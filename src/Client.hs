{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
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
import DCLabel
import Label -- holds the Label typeclass


import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import GHC.TypeLits
import Data.Proxy

#ifdef INTEGRITY
import Crypto.Hash.Algorithms (SHA512)
import Crypto.PubKey.RSA.PKCS15
#endif

data Ref l a = RefDummy
data Enclave l p a = EnclaveDummy deriving (Functor, Applicative, Monad, MonadIO)
data Secure a = Secure CallID [ByteString]


(<@>) :: Binary a => Secure (a -> b) -> a -> Secure b
(Secure identifier args) <@> arg =
  Secure identifier (encode arg : args)

{- The Securable a constraint is necessary for the Enclave type -}
inEnclave :: (Securable a, Label l) => LIOState l p -> a -> App (Secure a)
inEnclave _ _ = App $ do
  (next_id, remotes, ident) <- get
  put (next_id + 1, remotes, ident)
  return $ Secure next_id []


getPrivilege :: Enclave l p (Priv p)
getPrivilege = EnclaveDummy


class Securable a where
  mkSecure :: (Label l)
           => LIOState l p -> a -> ([ByteString] -> Enclave l p (Maybe ByteString))

-- instance (Binary a) => Securable (Enclave a) where
--   mkSecure m = \_ -> fmap (Just . encode) m

instance (Binary a, Label l) => Securable (Enclave l p a) where
  mkSecure _ _ = \_ -> EnclaveDummy

instance (Binary a, Securable b) => Securable (a -> b) where
  mkSecure _ _  = \_ -> EnclaveDummy

inEnclaveConstant :: (Label l) => l -> a -> App (Enclave l p (Labeled l a))
inEnclaveConstant _ _ = return EnclaveDummy


liftNewRef :: Label l
           => l -> a -> App (Enclave l p (Ref l a))
liftNewRef _ _ = return EnclaveDummy

liftNewRefP :: p -> l -> a -> App (Enclave l p (Ref l a))
liftNewRefP _ _ _ = return EnclaveDummy


newRef :: Label l
       => l
       -> a
       -> Enclave l p (Ref l a)
newRef _ _ = EnclaveDummy

newRefP :: Priv p
        -> l
        -> a
        -> Enclave l p (Ref l a)
newRefP _ _ _ = EnclaveDummy


readRef :: Label l => Ref l a -> Enclave l p a
readRef _ = EnclaveDummy

readRefP :: Priv p ->  Ref l a -> Enclave l p a
readRefP _ _ = EnclaveDummy


writeRef :: Label l => Ref l a -> a -> Enclave l p ()
writeRef _ _ = EnclaveDummy

writeRefP :: Priv p -> Ref l a -> a -> Enclave l p ()
writeRefP _ _ _ = EnclaveDummy


-- data Labeled l t = LabeledDummy


-- | The main monad type alias to use for 'LIO' computations that are
-- specific to 'DCLabel's.
type EnclaveDC = Enclave DCLabel DCPriv

-- | An alias for 'Labeled' values labeled with a 'DCLabel'.
type DCLabeled = Labeled DCLabel

type DCPriv = CNF

type DCRef = Ref DCLabel

taint :: Label l => l -> Enclave l p ()
taint _ = EnclaveDummy

taintP :: Priv p -> l -> Enclave l p ()
taintP _ _ = EnclaveDummy

label :: Label l => l -> a -> Enclave l p (Labeled l a)
label _ _ = EnclaveDummy

labelP :: Priv p -> l -> a -> Enclave l p (Labeled l a)
labelP _ _ _ = EnclaveDummy

unlabel :: Label l => Labeled l a -> Enclave l p a
unlabel _ = EnclaveDummy

unlabelP :: Priv p -> Labeled l a -> Enclave l p a
unlabelP _ _ = EnclaveDummy


toLabeled :: Label l => l -> Enclave l p a -> Enclave l p (Labeled l a)
toLabeled _ _ = EnclaveDummy

toLabeledP :: Priv p -> l -> Enclave l p a -> Enclave l p (Labeled l a)
toLabeledP _ _ _ = EnclaveDummy


labelOf :: Label l => Labeled l a -> l
labelOf (LabeledTCB l _) = l

inEnclaveLabeledConstant :: Label l => l -> a -> App (Enclave l p (Labeled l a))
inEnclaveLabeledConstant _ _ = return $ EnclaveDummy



clientLabel :: (Label l, KnownSymbol loc, Binary l, Binary a)
            => l -> a -> Client loc (Labeled l a)
clientLabel l a = return (LabeledTCB l a)

clientUnLabel :: (Label l, KnownSymbol loc, Binary l, Binary a)
              => Labeled l a -> Client loc a
clientUnLabel (LabeledTCB _ a) = return a


-- | Generalising monads with locations


-- | Term-level locations.
type LocTm = String

-- | Type-level locations.
type LocTy = Symbol

-- | Convert a type-level location to a term-level location.
toLocTm :: forall (l :: LocTy). KnownSymbol l => Proxy l -> LocTm
toLocTm = symbolVal

data Client (l :: LocTy) a where
  Client :: (KnownSymbol l)
         => Proxy l -> IO a -> Client l a


instance (KnownSymbol l) => Functor (Client l) where
  fmap f (Client l comp) = Client l (fmap f comp)

instance (KnownSymbol l) => Applicative (Client l) where
  pure = Client Proxy . pure
  (Client l1 f) <*> Client l2 a
    | toLocTm l1 == toLocTm l2 = Client l1 (f <*> a)
    | otherwise = error "splatting different location types!"

instance (KnownSymbol l) => Monad (Client l) where
  return = pure
  Client l ma >>= k = Client l $ do
    a <- ma
    let (Client c comp) = k a
    if (c == l) then comp else error "binding wrong location types!"

instance (KnownSymbol l) => MonadIO (Client l) where
  liftIO = Client Proxy . liftIO

runClient :: Client l a -> App Done
runClient (Client loc cl) = App $ do
  (_, _, loctm) <- get
  if ((toLocTm loc) == loctm)
  then do
    v <- liftIO cl
    return $ v `seq` Done
  else return Done -- cl not executed



tryEnclave :: (Binary a, KnownSymbol loc)
           => Secure (Enclave l p a) -> Client loc (Maybe a)
tryEnclave (Secure identifier args) = Client Proxy $ do
  {- SENDING REQUEST HERE -}
  connect localhost connectPort $ \(connectionSocket, remoteAddr) -> do
    -- debug logs
    putStrLn $ "Connection established to " ++ show remoteAddr
    sendLazy connectionSocket $ createPayload (identifier, reverse args)
    resp <- readTCPSocket connectionSocket
    return $ fmap decode (decode resp :: Maybe ByteString)
  {- SENDING ENDS -}

gateway :: (Binary a, KnownSymbol loc) => Secure (Enclave l p a) -> Client loc a
gateway closure = fromJust <$> tryEnclave closure

runApp :: Identifier -> App a -> IO a
runApp ident (App s) = evalStateT s (initAppState ident)

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

raTryEnclave :: (Label l, Binary a, KnownSymbol loc)
             => Secure (Enclave l p a) -> Client loc (Maybe a)
raTryEnclave (Secure identifier args) = Client Proxy $ do
#ifdef INTEGRITY
  let inputBytes' = BL.toStrict $ encode $ (identifier, reverse args)
  inputBytes <- createSigMsg inputBytes'
#else
  let inputBytes = BL.toStrict $ encode $ (identifier, reverse args)
#endif
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
gatewayRA :: (Binary a, Label l, KnownSymbol loc)
          => Secure (Enclave l p a) -> Client loc a
gatewayRA closure = (fromMaybe raerr) <$> (raTryEnclave closure)
  where
    raerr = error "ERR: Remote Attestation failed"


runAppRA :: Identifier -> App a -> IO a
runAppRA ident (App s) = evalStateT s (initAppState ident)


#ifdef INTEGRITY
-- Integrity Checking with Digital Signatures
-- return type is (Signature, B.ByteString) where the orignal string
-- is the second element of the tuple
createSigMsg :: B.ByteString -> IO B.ByteString
createSigMsg msg = do
  privK <- read <$> readFile "ssl/private.key"
  case (sign Nothing (Nothing :: Maybe SHA512) privK msg) of
    Left err  -> putStrLn ("error signing" <> show err) >> return B.empty
    Right sig_m -> return $ BL.toStrict $ encode (sig_m, msg)
#endif


addLaplacianNoiseWithSensitivity :: Double -> Double -> Double -> EnclaveDC Double
addLaplacianNoiseWithSensitivity _ _ _ = EnclaveDummy


data Traceable = TraceableDummy

note :: Show a => a -> Traceable
note _ = TraceableDummy


traceCallI :: [Traceable] -> EnclaveDC ()
traceCallI _ = EnclaveDummy

traceCallO :: [Traceable] -> EnclaveDC ()
traceCallO _ = EnclaveDummy
