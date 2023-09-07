{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Client(module Client) where

import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.ByteString.Lazy(ByteString)
import Data.Binary(Binary, encode, decode)
import Network.Simple.TCP
import App


data Ref a = RefDummy
data Server a = ServerDummy deriving (Functor, Applicative, Monad)
data Remote a = Remote CallID [ByteString]

(<.>) :: Binary a => Remote (a -> b) -> a -> Remote b
(Remote identifier args) <.> arg =
  Remote identifier (encode arg : args)

{- The Remotable a constraint is necessary for the Server type -}
remote :: (Remotable a) => a -> App (Remote a)
remote _ = App $ do
  (next_id, remotes) <- get
  put (next_id + 1, remotes)
  return $ Remote next_id []

ntimes :: (Remotable a) => Int -> a -> App (Remote a)
ntimes _ = remote

class Remotable a where
  mkRemote :: a -> ([ByteString] -> Server (Maybe ByteString))

instance (Binary a) => Remotable (Server a) where
  mkRemote m = \_ -> fmap (Just . encode) m

instance (Binary a, Remotable b) => Remotable (a -> b) where
  mkRemote f = \(x:xs) -> mkRemote (f $ decode x) xs

inEnclaveConstant :: a -> App (Server a)
inEnclaveConstant _ = return ServerDummy

liftNewRef :: a -> App (Server (Ref a))
liftNewRef _ = return ServerDummy

newRef :: a -> Server (Ref a)
newRef _ = ServerDummy

readRef :: Ref a -> Server a
readRef _ = ServerDummy

writeRef :: Ref a -> a -> Server ()
writeRef _ _ = ServerDummy


type Client = IO


runClient :: Client a -> App Done
runClient cl = do
  v <- liftIO cl
  return $ v `seq` Done

tryServer :: (Binary a) => Remote (Server a) -> Client (Maybe a)
tryServer (Remote identifier args) = do
  {- SENDING REQUEST HERE -}
  connect localhost connectPort $ \(connectionSocket, remoteAddr) -> do
    -- debug logs
    putStrLn $ "Connection established to " ++ show remoteAddr
    sendLazy connectionSocket $ createPayload (identifier, reverse args)
    resp <- readTCPSocket connectionSocket
    return $ fmap decode (decode resp :: Maybe ByteString)
  {- SENDING ENDS -}

onServer :: Binary a => Remote (Server a) -> Client a
onServer closure = fromJust <$> tryServer closure

runApp :: App a -> IO a
runApp (App s) = evalStateT s initAppState
