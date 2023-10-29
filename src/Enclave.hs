{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Enclave(module Enclave) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Binary(Binary, encode, decode)
import Data.ByteString.Lazy(ByteString)
import Data.IORef
import Network.Simple.TCP
import System.IO(hFlush, stdout)
import App

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import Control.Concurrent
import Control.Exception
import Data.Bits (shift, (.|.))
import Data.Char (ord)
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.IO.Handle.Text


type Ref a = IORef a
newtype Enclave a = Enclave (IO a)

instance Functor (Enclave) where
  fmap f (Enclave x) = Enclave $ fmap f x

instance Applicative Enclave where
  pure x = Enclave (pure x)
  Enclave f <*> Enclave a = Enclave $ f <*> a

instance Monad Enclave where
  return = pure
  Enclave ma >>= k = Enclave $ do
    a <- ma
    let Enclave ka = k a
    ka


data Secure a = SecureDummy

inEnclaveConstant :: a -> App (Enclave a)
inEnclaveConstant = return . return

liftNewRef :: a -> App (Enclave (Ref a))
liftNewRef a = App $ do
  r <- liftIO $ newIORef a
  return (return r)

newRef :: a -> Enclave (Ref a)
newRef x = Enclave $ newIORef x

readRef :: Ref a -> Enclave a
readRef ref = Enclave $ readIORef ref

writeRef :: Ref a -> a -> Enclave ()
writeRef ref v = Enclave $ writeIORef ref v

inEnclave :: (Securable a) => a -> App (Secure a)
inEnclave f = App $ do
  (next_id, remotes) <- get
  put (next_id + 1, (next_id, \bs -> let Enclave n = mkSecure f bs in n) : remotes)
  return SecureDummy

ntimes :: (Securable a) => Int -> a -> App (Secure a)
ntimes n f = App $ do
  r <- liftIO $ newIORef n
  (next_id, remotes) <- get
  put (next_id + 1, (next_id, \bs ->
    let Enclave s = do
          c <- Enclave $ do atomicModifyIORef' r $ \i -> (i - 1, i)
          if c > 0
          then mkSecure f bs
          else return Nothing
    in s) : remotes)


  return SecureDummy

(<@>) :: Binary a => Secure (a -> b) -> a -> Secure b
(<@>) = error "Access to client not allowed"


class Securable a where
  mkSecure :: a -> ([ByteString] -> Enclave (Maybe ByteString))

instance (Binary a) => Securable (Enclave a) where
  mkSecure m = \_ -> fmap (Just . encode) m

instance (Binary a, Securable b) => Securable (a -> b) where
  mkSecure f = \(x:xs) -> mkSecure (f $ decode x) xs

data Client a = ClientDummy
  deriving (Functor, Applicative, Monad, MonadIO)

runClient :: Client a -> App Done
runClient _ = return Done

tryEnclave :: (Binary a) => Secure (Enclave a) -> Client (Maybe a)
tryEnclave _ = ClientDummy

gateway :: Binary a => Secure (Enclave a) -> Client a
gateway _ = ClientDummy

unsafeOnEnclave :: Binary a => Secure (Enclave a) -> Client a
unsafeOnEnclave _ = ClientDummy

{-@ The enclave's event loop. @-}
runApp :: App a -> IO a
runApp (App s) = do
  (a, (_, vTable)) <- runStateT s initAppState
  {- BLOCKING HERE -}
  _ <- serve (Host localhost) connectPort $
    \(connectionSocket, remoteAddr) -> do
      -- debug log
      putStrLn $ "TCP connection established from " ++ show remoteAddr
      hFlush stdout -- Gramine prints only if stdout is flushed
      req <- readTCPSocket connectionSocket
      onEvent vTable req connectionSocket
  {- BLOCKING ENDS -}
  return a -- the a is irrelevant



onEvent :: [(CallID, Method)] -> ByteString -> Socket -> IO ()
onEvent mapping incoming socket = do
  let (identifier, args) = decode incoming :: (CallID, [ByteString])
      Just f = lookup identifier mapping
  result <- encode <$> f args
  let res = handleVoidTy result -- the () type cannot be sent over wire
  sendLazy socket (BL.append (msgSize res) res) -- See NOTE 1
  where
    msgSize r = encode $ BL.length r
    handleVoidTy r = if (BL.length r == 0) -- the () type has msg length 0
                     then encode '\0'
                     else r

-- NOTE 1
-- We do not use `createPayload` because as a first step it
-- encodes the message body to a ByteString. In case of the
-- enclave the `result` is already a ByteString. So it further
-- encodes the ByteString and gives a wrong result.
-- We can modify the `createPayload` function so that depending
-- on the value of some identifier term that we send, it decides
-- to encode or not encode the message body but then the type
-- will become an actual dependent type. (Keep it simple!)


microsec :: Int -> Int
microsec = id

millisec :: Int -> Int
millisec x = (microsec x) * 1000

sec :: Int -> Int
sec x = (millisec x) * 1000

foreign import ccall "startServer" startServer
    :: Ptr CInt -> Ptr CChar -> IO CInt

foreign import ccall "unistd.h usleep"
  c_usleep :: CUInt -> IO CInt

sleepMilliseconds :: Int -> IO ()
sleepMilliseconds ms =
  c_usleep (fromIntegral (ms * 1000)) >> return () -- Convert milliseconds to microseconds

-- XXX: not portable;
-- 8 bytes for this machine
-- big-endian order used for the data packets
byteStrLength :: Ptr CChar -> IO Int
byteStrLength cptr = go 0 []
  where
    go 8 xs = do
      let (i0:i1:i2:i3:i4:i5:i6:i7:_) = reverse xs
      let y = (shift i0 56) .|. (shift i1 48) .|. (shift i2 40) .|.
              (shift i3 32) .|. (shift i4 24) .|. (shift i5 16) .|.
              (shift i6  8) .|. i7
      return y
    go i xs = do
      cchar <- peekElemOff cptr i
      go (i + 1) ((fromEnum cchar):xs)

dataPacketSize :: Int
dataPacketSize = 1024

runAppRA :: App a -> IO a
runAppRA (App s) = do
  (a, (_, vTable)) <- runStateT s initAppState
  flagptr  <- malloc :: IO (Ptr CInt)
  dataptr  <- mallocBytes dataPacketSize :: IO (Ptr CChar)
  poke flagptr 0
  tid <- myThreadId
  _   <- forkIO (ffiComp tid flagptr dataptr)
  result <-
    try (loop vTable flagptr dataptr) :: IO (Either SomeException ())
  case result of
    Left e  -> putStrLn $ "Caught exception : " ++ show e
    Right _ -> putStrLn "Exited loop with C server termination"
  free flagptr
  free dataptr
  return a
  where
    loop :: [(CallID, Method)] -> Ptr CInt -> Ptr CChar -> IO ()
    loop vTable fptr dptr = do
      int_val <- peek fptr
      if (fromEnum int_val == 0)
      then do
        -- threadDelay (millisec 200)
        -- sleepMilliseconds will force the whole thread to sleep
        -- instead of context switching (like threadDelay); it is
        -- a greasy hack to counter for the lack of timing APIs in Intel SGX
        sleepMilliseconds 200
        loop vTable fptr dptr
      else do
        -- find size of data to be read
        l <- byteStrLength dptr
        -- XXX: not portable 8 bytes for this machine
        byteString <- B.packCStringLen (dptr `plusPtr` 8, l)
        -- call the correct function from the lookup table
        res <- onEventRA vTable (BL.fromStrict byteString)
        -- clear dptr
        memsetToZero dptr dataPacketSize
        -- write result to dptr
        _ <- B.useAsCStringLen (BL.toStrict res) $ \(resptr, len) -> do
                memcpy dptr resptr (toEnum len)
        -- set fptr = 0 and set the C server in motion
        poke fptr 0
        -- continue Haskell's event loop
        loop vTable fptr dptr


ffiComp :: ThreadId -> Ptr CInt -> Ptr CChar -> IO ()
ffiComp tid fptr dptr = do
  errorcode <- startServer fptr dptr
  if (fromEnum errorcode /= 0)
  then throwTo tid (userError "C server terminated abnormally")
  else throwTo tid (userError "C server terminated gracefully") -- should not happen

gatewayRA :: Binary a => Secure (Enclave a) -> Client a
gatewayRA _ = ClientDummy

onEventRA :: [(CallID, Method)] -> ByteString -> IO (BL.ByteString)
onEventRA mapping incoming = do
  let (identifier, args) = decode incoming :: (CallID, [ByteString])
      Just f = lookup identifier mapping
  result <- encode <$> f args
  putStr "onEventRA :"
  printDecimalValues (BL.toStrict result)
  let res = handleVoidTy result -- the () type cannot be sent over wire
  return (BL.append (msgSize res) res)
  where
    msgSize r = encode $ BL.length r
    handleVoidTy r = if (BL.length r == 0) -- the () type has msg length 0
                     then encode '\0'
                     else r

-- Set all characters in the C string to \0
memsetToZero :: Ptr CChar -> Int -> IO ()
memsetToZero ptr len = go ptr 0
    where
        go :: Ptr CChar -> Int -> IO ()
        go p n
          | n == len = return ()
          | otherwise = do
              poke p (fromIntegral (ord '\0') :: CChar)
              go (p `plusPtr` 1) (n + 1)

printDecimalValues :: B.ByteString -> IO ()
printDecimalValues bs = do
  let decimalValues = map ord (BC.unpack bs)
  putStrLn (show decimalValues)
