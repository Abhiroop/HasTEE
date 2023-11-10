{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Client(module Client) where

import Data.Bits (shift, (.|.))
import Data.Maybe
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.ByteString.Lazy(ByteString)
import Data.Binary(Binary, encode, decode)
import Network.Simple.TCP
import App


import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL


data Ref l a = RefDummy
data Enclave l a = EnclaveDummy deriving (Functor, Applicative, Monad)
data Secure a = Secure CallID [ByteString]

(<@>) :: Binary a => Secure (a -> b) -> a -> Secure b
(Secure identifier args) <@> arg =
  Secure identifier (encode arg : args)

{- The Securable a constraint is necessary for the Enclave type -}
inEnclave :: (Securable a) => a -> App (Secure a)
inEnclave _ = App $ do
  (next_id, remotes) <- get
  put (next_id + 1, remotes)
  return $ Secure next_id []

ntimes :: (Securable a) => Int -> a -> App (Secure a)
ntimes _ = inEnclave

class Securable a where
  mkSecure :: a -> ([ByteString] -> Enclave l (Maybe ByteString))

-- instance (Binary a) => Securable (Enclave a) where
--   mkSecure m = \_ -> fmap (Just . encode) m

instance (Binary a) => Securable (Enclave l a) where
  mkSecure _ = \_ -> EnclaveDummy

instance (Binary a, Securable b) => Securable (a -> b) where
  mkSecure f = \(x:xs) -> mkSecure (f $ decode x) xs

inEnclaveConstant :: (Label l) => l -> a -> App (Enclave l (Labeled l a))
inEnclaveConstant _ _ = return EnclaveDummy


liftNewRef :: Label l
           => l -> a -> App (Enclave l (Ref l a))
liftNewRef _ _ = return EnclaveDummy


newRef :: Label l
       => l
       -> a
       -> Enclave l (Ref l a)
newRef _ _ = EnclaveDummy


readRef :: Label l => Ref l a -> Enclave l a
readRef _ = EnclaveDummy


writeRef :: Label l => Ref l a -> a -> Enclave l ()
writeRef _ _ = EnclaveDummy

data Labeled l t = LabeledDummy

taint :: Label l => l -> Enclave l ()
taint _ = EnclaveDummy

label :: Label l => l -> a -> Enclave l (Labeled l a)
label _ _ = EnclaveDummy

unlabel :: Label l => Labeled l a -> Enclave l a
unlabel _ = EnclaveDummy

toLabeled :: Label l => l -> Enclave l a -> Enclave l (Labeled l a)
toLabeled _ _ = EnclaveDummy


labelOf :: Label l => Labeled l a -> l
labelOf _ = error "Client not allowed to look at labels"

inEnclaveLabeledConstant :: Label l => l -> a -> App (Enclave l (Labeled l a))
inEnclaveLabeledConstant _ _ = return $ EnclaveDummy



type Client = IO

runClient :: Client a -> App Done
runClient cl = do
  v <- liftIO cl
  return $ v `seq` Done

tryEnclave :: Binary a => Secure (Enclave l a) -> Client (Maybe a)
tryEnclave (Secure identifier args) = do
  {- SENDING REQUEST HERE -}
  connect localhost connectPort $ \(connectionSocket, remoteAddr) -> do
    -- debug logs
    putStrLn $ "Connection established to " ++ show remoteAddr
    sendLazy connectionSocket $ createPayload (identifier, reverse args)
    resp <- readTCPSocket connectionSocket
    return $ fmap decode (decode resp :: Maybe ByteString)
  {- SENDING ENDS -}

gateway :: Binary a => Secure (Enclave l a) -> Client a
gateway closure = fromJust <$> tryEnclave closure

runApp :: App a -> IO a
runApp (App s) = evalStateT s initAppState

foreign import ccall "setup_ra_tls_send" setup_ra_tls_send
    :: Ptr CChar -> CSize -> Ptr CChar -> Ptr CChar -> IO CInt

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

raTryEnclave :: (Label l, Binary a) => Secure (Enclave l a) -> Client (Maybe a)
raTryEnclave (Secure identifier args) = do
  let inputBytes = BL.toStrict $ encode $ (identifier, reverse args)
  withCString "native" $ \cstring -> do
    B.useAsCStringLen inputBytes $ \(ptr, len) -> do
      respptr  <- mallocBytes dataPacketSize :: IO (Ptr CChar)
      cByteStr <- setup_ra_tls_send ptr (fromIntegral len) cstring respptr
      let errorcode = fromEnum cByteStr :: Int
      -- putStrLn $ "Hs to C and back " <> (show errorcode)
      if (errorcode == 1)
      then do
        free respptr
        return Nothing
      else do
        l <- byteStrLength respptr
        -- XXX: not portable 8 bytes for this machine
        byteString <- B.packCStringLen (respptr `plusPtr` 8, l)
        free respptr
        return $ fmap decode (decode $ BL.fromStrict byteString :: Maybe ByteString)

--return $ fmap decode $ Just $ encode errorcode
gatewayRA :: (Binary a, Label l) => Secure (Enclave l a) -> Client a
gatewayRA closure = (fromMaybe raerr) <$> (raTryEnclave closure)
  where
    raerr = error "ERR: Remote Attestation failed"


runAppRA :: App a -> IO a
runAppRA (App s) = evalStateT s initAppState



  -- {- SENDING REQUEST HERE -}
  -- connect localhost connectPort $ \(connectionSocket, remoteAddr) -> do
  --   -- debug logs
  --   putStrLn $ "Connection established to " ++ show remoteAddr
  --   sendLazy connectionSocket $ createPayload (identifier, reverse args)
  --   resp <- readTCPSocket connectionSocket
  --   return $ fmap decode (decode resp :: Maybe ByteString)
  -- {- SENDING ENDS -}
