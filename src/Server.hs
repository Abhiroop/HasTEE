{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Server(module Server) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Aeson
import Lib

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

(<.>) :: ToJSON a => Remote (a -> b) -> a -> Remote b
(<.>) = error "Access to client not allowed"


class Remotable a where
  mkRemote :: a -> ([JSON] -> Server JSON)

instance (ToJSON a) => Remotable (Server a) where
  mkRemote m = \_ -> fmap toJSON m

instance (FromJSON a, Remotable b) => Remotable (a -> b) where
  mkRemote f = \(x:xs) -> mkRemote (f $ resFromJSON x) xs

-- we have to choose some library to do an RPC call here
webSocketSend :: JSON -> IO ()
webSocketSend _ = return ()

data Client a = ClientDummy deriving (Functor, Applicative, Monad, MonadIO)

runClient :: Client a -> App Done
runClient _ = return Done

onServer :: (FromJSON a) => Remote (Server a) -> Client a
onServer _ = ClientDummy

{-@ This is where the server can have an event loop.
    It can asynchronously wait for multiple requests. @-}
runApp :: App a -> IO a
runApp (App s) = do
  (a, (_, vTable)) <- runStateT s initAppState
  {- BLOCKING HERE -}
  let incomingRequest = undefined :: JSON
  {- BLOCKING ENDS -}
  onEvent vTable incomingRequest
  return a -- the a is irrelevant


onEvent :: [(CallID, Method)] -> JSON -> IO ()
onEvent mapping incoming = do
  let (nonce, identifier, args) = resFromJSON incoming :: (Int, CallID, [JSON])
      Just f = lookup identifier mapping
  result <- f args
  webSocketSend $ toJSON (nonce, result)
