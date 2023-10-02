{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Client(module Client) where

import Data.Maybe
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.ByteString.Lazy(ByteString)
import Data.Binary(Binary, encode, decode)
import Network.Simple.TCP
import App


import GHC.TypeLits
import Data.Proxy


data Ref a = RefDummy
data Enclave a = EnclaveDummy deriving (Functor, Applicative, Monad)
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
  mkSecure :: a -> ([ByteString] -> Enclave (Maybe ByteString))

instance (Binary a) => Securable (Enclave a) where
  mkSecure m = \_ -> fmap (Just . encode) m

instance (Binary a, Securable b) => Securable (a -> b) where
  mkSecure f = \(x:xs) -> mkSecure (f $ decode x) xs

inEnclaveConstant :: a -> App (Enclave a)
inEnclaveConstant _ = return EnclaveDummy

liftNewRef :: a -> App (Enclave (Ref a))
liftNewRef _ = return EnclaveDummy

newRef :: a -> Enclave (Ref a)
newRef _ = EnclaveDummy

readRef :: Ref a -> Enclave a
readRef _ = EnclaveDummy

writeRef :: Ref a -> a -> Enclave ()
writeRef _ _ = EnclaveDummy




-- Generalising monads with locations

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

runClient :: LocTm -> Client l a -> App Done
runClient loctm (Client label cl)
  | (toLocTm label) == loctm = do
      v <- liftIO cl
      return $ v `seq` Done
  | otherwise = error $ "ERROR! wrong client computation label - "
             <> show (toLocTm label)


tryEnclave :: (Binary a, KnownSymbol l)
           => Secure (Enclave a) -> Client l (Maybe a)
tryEnclave (Secure identifier args) = Client Proxy $ do
  {- SENDING REQUEST HERE -}
  connect localhost connectPort $ \(connectionSocket, remoteAddr) -> do
    -- debug logs
    putStrLn $ "Connection established to " ++ show remoteAddr
    sendLazy connectionSocket $ createPayload (identifier, reverse args)
    resp <- readTCPSocket connectionSocket
    return $ fmap decode (decode resp :: Maybe ByteString)
  {- SENDING ENDS -}

gateway :: (Binary a, KnownSymbol l) => Secure (Enclave a) -> Client l a
gateway closure = fromJust <$> tryEnclave closure

runApp :: App a -> IO a
runApp (App s) = evalStateT s initAppState
