{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
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
import DCLabel

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

import Control.Monad (ap, unless)
import Data.Dynamic
import Data.Maybe (fromMaybe)

import GHC.TypeLits

{- FLOATING LABEL Information Flow Control
   The floating is bounded by a clearance label. Bell
   and La Padula formalized clearance as a bound on
   the current label of a particular users’ processes.
-}


-- | Encriching the Enclave monad with labeled values
newtype Enclave l a = Enclave (IORef (LIOState l) -> IO a) deriving (Typeable)

instance Monad (Enclave l) where
  return = pure
  (Enclave ma) >>= k = Enclave $ \s -> do
    a <- ma s
    case k a of
      Enclave mb -> mb s


instance Functor (Enclave l) where
  fmap f (Enclave ma) = Enclave $ \s -> fmap f (ma s)


instance Applicative (Enclave l) where
  pure a = Enclave $ \_ -> pure a
  (<*>) = ap

-- XXX: For Debugging please remove XXX--
-- instance MonadIO (Enclave l) where
--   liftIO io = Enclave $ \_ -> io

getLIOStateTCB :: Enclave l (LIOState l)
getLIOStateTCB = Enclave readIORef

-- | Set internal state.
putLIOStateTCB :: LIOState l -> Enclave l ()
putLIOStateTCB s = Enclave $ \sp -> writeIORef sp $! s

-- | Update the internal state given some function.
modifyLIOStateTCB :: (LIOState l -> LIOState l) -> Enclave l ()
modifyLIOStateTCB f = do
  s <- getLIOStateTCB
  putLIOStateTCB (f s)

data Secure a = SecureDummy

data Labeled l t = LabeledTCB !l t deriving Typeable

inEnclaveConstant :: (Label l) => l -> a -> App (Enclave l (Labeled l a))
inEnclaveConstant l constant = return (label l constant)

-- ignoring exceptions for now
runLIO :: (Label l) => Enclave l a -> LIOState l -> IO (a, LIOState l)
runLIO (Enclave m) s0 = do
  sp <- newIORef s0
  a  <- m sp
  s1 <- readIORef sp
  unless ((lioLabel s1) `canFlowTo` (lioOutLabel s0)) $
    error $  "Data cannot flow from " <> (show (lioLabel s1))
          <> " to public channel labeled " <> (show (lioOutLabel s0))
  return (a, s1)

evalLIO :: (Label l) => Enclave l a -> Dynamic -> IO a
evalLIO lio s_dyn = do
  let s = fromMaybe dynError
             (fromDynamic s_dyn :: (Label l) => Maybe (LIOState l))
  (a, _) <- runLIO lio s
  return a
  where
    dynError = error "Incorrect label type supplied to evalLIO"

guardAlloc :: Label l => l -> Enclave l ()
guardAlloc newl = do
  LIOState { lioLabel = l_cur, lioClearance = c_cur } <- getLIOStateTCB
  unless (l_cur `canFlowTo` newl) $ error ("Can't flow to " <> (show newl))
  unless (newl `canFlowTo` c_cur) $ error ("Below clearance level " <>
                                           (show c_cur))

guardAllocP :: PrivDesc l p => Priv p -> l -> Enclave l ()
guardAllocP p newl = do
  LIOState { lioLabel = l_cur, lioClearance = c_cur } <- getLIOStateTCB
  unless (canFlowToP p l_cur newl) $ error ("Can't flow to " <> (show newl))
  unless (canFlowTo newl c_cur) $ error ("Below clearance level " <>
                                         (show c_cur))


{-| taint l
Taints the current context with l such that L_cur = (L_cur ⊔ l),
provided (L_cur ⊔ l) ⊑ C_cur
-}
taint :: Label l => l -> Enclave l ()
taint newl = do
  LIOState { lioLabel = l_cur, lioClearance = c_cur } <- getLIOStateTCB
  let l' = l_cur `lub` newl
  unless (l' `canFlowTo` c_cur) $
    error ((show l') <> " can't flow to " <> (show c_cur))
  modifyLIOStateTCB $ \s -> s { lioLabel = l' }

taintP :: PrivDesc l p => Priv p -> l -> Enclave l ()
taintP p newl = do
  LIOState { lioLabel = l_cur, lioClearance = c_cur } <- getLIOStateTCB
  let l' = l_cur `lub` downgradeP p newl
  unless (l' `canFlowTo` c_cur) $
    error ((show l') <> " can't flow to " <> (show c_cur))
  modifyLIOStateTCB $ \s -> s { lioLabel = l' }

{-| label l a
Given a label l such that L_cur ⊑ l ⊑ C_cur and a value v, the
action label l v returns a labeled value that protects v with l
-}
label :: Label l => l -> a -> Enclave l (Labeled l a)
label l a = do
  guardAlloc l
  return $ LabeledTCB l a

labelP :: PrivDesc l p => Priv p -> l -> a -> Enclave l (Labeled l a)
labelP p l a = do
  guardAllocP p l
  return $ LabeledTCB l a

{-| unlabel lv
raises the current label, clearance permitting (see `taint`) to the join of
of lv’s label and the current label, returning the value with the label removed.
-}
unlabel :: Label l => Labeled l a -> Enclave l a
unlabel (LabeledTCB l v) = do
  taint l
  return v

unlabelP :: PrivDesc l p => Priv p -> Labeled l a -> Enclave l a
unlabelP p (LabeledTCB l v) = do
  taintP p l
  return v

{-|
If lv is a labeled value with label l and value v, labelOf lv returns l
-}
labelOf :: Label l => Labeled l a -> l
labelOf (LabeledTCB l _) = l

{-|
Given a label l such that L_cur ⊑ l ⊑ C_cur and an LIO action m,
toLabeled l m executes m without raising L_cur.
-}
toLabeled :: Label l => l -> Enclave l a -> Enclave l (Labeled l a)
toLabeled l m = do
  -- | get the label and clearance before running the computation
  LIOState { lioLabel = l_cur
           , lioClearance = c_cur
           , lioOutLabel  = l_out
           } <- getLIOStateTCB
  -- | run the computation now (label will float up)
  res <- m
  -- | grab the current label
  LIOState { lioLabel = l_cur_new} <- getLIOStateTCB
  -- | check IFC violation
  unless (l_cur_new `canFlowTo` l) $
    error ("Label " <> (show l) <> " leads to IFC violation")
  -- | restore original label and clearance
  putLIOStateTCB (LIOState { lioLabel     = l_cur
                           , lioClearance = c_cur
                           , lioOutLabel  = l_out
                           })
  -- | wrap result in the desired label
  lRes <- label l res
  -- | wrap the `lRes` in the (l_cur, c_cur)'s context
  return lRes

toLabeledP :: PrivDesc l p =>
              Priv p -> l -> Enclave l a -> Enclave l (Labeled l a)
toLabeledP p l m = do
  -- | get the label and clearance before running the computation
  LIOState { lioLabel = l_cur
           , lioClearance = c_cur
           , lioOutLabel  = l_out
           } <- getLIOStateTCB
  -- | run the computation now (label will float up)
  res <- m
  -- | grab the current label
  LIOState { lioLabel = l_cur_new} <- getLIOStateTCB
  -- | check IFC violation
  unless (canFlowToP p l_cur_new l) $
    error ("Label " <> (show l) <> " leads to IFC violation")
  -- | restore original label and clearance
  putLIOStateTCB (LIOState { lioLabel = l_cur
                           , lioClearance = c_cur
                           , lioOutLabel  = l_out
                           })
  -- | wrap result in the desired label
  lRes <- labelP p l res
  -- | wrap the `lRes` in the (l_cur, c_cur)'s context
  return lRes






inEnclaveLabeledConstant :: Label l => l -> a -> App (Enclave l (Labeled l a))
inEnclaveLabeledConstant l a = return $ return $ LabeledTCB l a

data Ref l a = LIORef !l (IORef a)

newRef :: Label l
       => l                   -- ^ Label of reference
       -> a                   -- ^ Initial value
       -> Enclave l (Ref l a) -- ^ Mutable reference
newRef l a = do
  guardAlloc l
  Enclave $ \_ -> (LIORef l `fmap` newIORef a)


liftNewRef :: Label l
           => l -> a -> App (Enclave l (Ref l a))
liftNewRef l a = return $ newRef l a


readRef :: Label l => Ref l a -> Enclave l a
readRef (LIORef l ref) = do
  taint l
  Enclave (\_ -> readIORef ref)


writeRef :: Label l => Ref l a -> a -> Enclave l ()
writeRef (LIORef l ref) v = do
  guardAlloc l
  Enclave (\_ -> writeIORef ref v)


-- | The main monad type alias to use for 'LIO' computations that are
-- specific to 'DCLabel's.
type EnclaveDC = Enclave DCLabel

-- | An alias for 'Labeled' values labeled with a 'DCLabel'.
type DCLabeled = Labeled DCLabel

-- | 'DCLabel' privileges are expressed as a 'CNF' of the principals
-- whose authority is being exercised.
type DCPriv = Priv CNF



------ Previous Non-LIO---------

-- type Ref a = IORef a
-- newtype Enclave a = Enclave (IO a)

-- instance Functor (Enclave) where
--   fmap f (Enclave x) = Enclave $ fmap f x

-- instance Applicative Enclave where
--   pure x = Enclave (pure x)
--   Enclave f <*> Enclave a = Enclave $ f <*> a

-- instance Monad Enclave where
--   return = pure
--   Enclave ma >>= k = Enclave $ do
--     a <- ma
--     let Enclave ka = k a
--     ka


-- data Secure a = SecureDummy

-- inEnclaveConstant :: a -> App (Enclave a)
-- inEnclaveConstant = return . return

-- liftNewRef :: a -> App (Enclave (Ref a))
-- liftNewRef a = App $ do
--   r <- liftIO $ newIORef a
--   return (return r)

-- newRef :: a -> Enclave (Ref a)
-- newRef x = Enclave $ newIORef x

-- readRef :: Ref a -> Enclave a
-- readRef ref = Enclave $ readIORef ref

-- writeRef :: Ref a -> a -> Enclave ()
-- writeRef ref v = Enclave $ writeIORef ref v


inEnclave :: (Securable a, Label l) => LIOState l -> a -> App (Secure a)
inEnclave initState f = App $ do
  (next_id, remotes, ident) <- get
  put (next_id + 1, (next_id, \bs -> mkSecure initState f bs) : remotes, ident)
  return SecureDummy


(<@>) :: Binary a => Secure (a -> b) -> a -> Secure b
(<@>) = error "Access to client not allowed"


class Securable a where
  mkSecure :: (Label l)
           => LIOState l -> a -> ([ByteString] -> IO (Maybe ByteString))

instance (Binary a, Label l) => Securable (Enclave l a) where
  mkSecure s m = \_ -> fmap (Just . encode) (evalLIO m (toDyn s))


-- m :: Enclave l1 a
-- instance (Binary a) => Securable (Enclave l a) where
--   mkSecure :: LIOState l1 -> Enclave l a -> [ByteString] -> IO (Maybe ByteString)
--   mkSecure s m = \_ -> (fmap (Just . encode) (evalLIO m s))

instance (Binary a, Securable b) => Securable (a -> b) where
  mkSecure s f = \(x:xs) -> mkSecure s (f $ decode x) xs


-- | Term-level locations.
type LocTm = String

-- | Type-level locations.
type LocTy = Symbol

data Client (l :: LocTy) a = ClientDummy
  deriving (Functor, Applicative, Monad, MonadIO)

runClient :: Client l a -> App Done
runClient _ = return Done

tryEnclave :: (Binary a) => Secure (Enclave l a) -> Client loc (Maybe a)
tryEnclave _ = ClientDummy

gateway :: Binary a => Secure (Enclave l a) -> Client loc a
gateway _ = ClientDummy

unsafeOnEnclave :: Binary a => Secure (Enclave l a) -> Client loc a
unsafeOnEnclave _ = ClientDummy

{-@ The enclave's event loop. @-}
runApp :: Identifier -> App a -> IO a
runApp ident (App s) = do
  (a, (_, vTable, _)) <- runStateT s (initAppState ident)
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

runAppRA :: Identifier -> App a -> IO a
runAppRA ident (App s) = do
  (a, (_, vTable, _)) <- runStateT s (initAppState ident)
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

gatewayRA :: Binary a => Secure (Enclave l a) -> Client loc a
gatewayRA _ = ClientDummy

onEventRA :: [(CallID, Method)] -> ByteString -> IO (BL.ByteString)
onEventRA mapping incoming = do
  let (identifier, args) = decode incoming :: (CallID, [ByteString])
      Just f = lookup identifier mapping
  result <- encode <$> f args
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
