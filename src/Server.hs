{-# LANGUAGE TypeSynonymInstances #-}
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

-- The server needs an event loop
onEvent :: [(CallID, Method)] -> JSON -> IO ()
onEvent mapping incoming = do
  let (nonce, identifier, args) = resFromJSON incoming :: (Int, CallID, [JSON])
      Just f = lookup identifier mapping
  result <- f args
  webSocketSend $ toJSON (nonce, result)

-- we have to choose some library to do an RPC call here
webSocketSend :: JSON -> IO ()
webSocketSend _ = return ()
