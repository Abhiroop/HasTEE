{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Server(module Server) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Binary(Binary, encode, decode)
import Data.ByteString.Lazy(ByteString)
import Network.Simple.TCP

import App

-- the Reader was not being used
type Server = IO
data Remote a = RemoteDummy

liftServerIO :: IO a -> App (Server a)
liftServerIO m = App $ do
  x <- liftIO m
  return (return x)

remote :: (Remotable a) => a -> App (Remote a)
remote f = App $ do
  (next_id, remotes) <- get
  put (next_id + 1, (next_id, mkRemote f) : remotes)
  return RemoteDummy

(<.>) :: Binary a => Remote (a -> b) -> a -> Remote b
(<.>) = error "Access to client not allowed"


class Remotable a where
  mkRemote :: a -> ([ByteString] -> Server ByteString)

instance (Binary a) => Remotable (Server a) where
  mkRemote m = \_ -> fmap encode m

instance (Binary a, Remotable b) => Remotable (a -> b) where
  mkRemote f = \(x:xs) -> mkRemote (f $ decode x) xs

data Client a = ClientDummy deriving (Functor, Applicative, Monad, MonadIO)

runClient :: Client a -> App Done
runClient _ = return Done

onServer :: (Binary a) => Remote (Server a) -> Client a
onServer _ = ClientDummy

{-@ The server's event loop. @-}
runApp :: App a -> IO a
runApp (App s) = do
  (a, (_, vTable)) <- runStateT s initAppState
  {- BLOCKING HERE -}
  _ <- serve (Host localhost) connectPort $
    \(connectionSocket, remoteAddr) -> do
      -- debug log
      putStrLn $ "TCP connection established from " ++ show remoteAddr
      req <- readTCPSocket connectionSocket
      onEvent vTable req connectionSocket
  {- BLOCKING ENDS -}
  return a -- the a is irrelevant

onEvent :: [(CallID, Method)] -> ByteString -> Socket -> IO ()
onEvent mapping incoming socket = do
  let (identifier, args) = decode incoming :: (CallID, [ByteString])
      Just f = lookup identifier mapping
  result <- f args
  sendLazy socket (createPayload result)
