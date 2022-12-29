{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Client(module Client) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Aeson
import Lib

data Server a = ServerDummy deriving Functor
data Remote a = Remote CallID [JSON]

(<.>) :: ToJSON a => Remote (a -> b) -> a -> Remote b
(Remote identifier args) <.> arg =
  Remote identifier (toJSON arg : args)

{- The Remotable a constraint is necessary for the Server type -}
remote :: (Remotable a) => a -> App (Remote a)
remote _ = App $ do
  (next_id, remotes) <- get
  put (next_id + 1, remotes)
  return $ Remote next_id []

class Remotable a where
  mkRemote :: a -> ([JSON] -> Server JSON)

instance (ToJSON a) => Remotable (Server a) where
  mkRemote m = \_ -> fmap toJSON m

instance (FromJSON a, Remotable b) => Remotable (a -> b) where
  mkRemote f = \(x:xs) -> mkRemote (f $ getRes $ fromJSON x) xs
    where
      getRes :: Result a -> a
      getRes (Success a) = a
      getRes (Error str) = error $ "Runtime error while parsing JSON: " <> str


liftServerIO :: IO a -> App (Server a)
liftServerIO _ = App $ return ServerDummy



-- The client monad
-- MVar is used simply to emulate blocking; we might use a different mechanism
type Nonce = Int
type ClientState = (Nonce, [(Nonce, (MVar JSON))])
type Client = StateT ClientState IO -- did not use the continuation monad

newResult :: Client (Nonce, MVar JSON)
newResult = do
  (nonce, m) <- get
  mv <- liftIO newEmptyMVar
  put (nonce + 1, (nonce, mv):m)
  return (nonce, mv)

onServer :: (FromJSON a) => Remote (Server a) -> Client a
onServer (Remote identifier args) = do
  (nonce, mv) <- newResult
  liftIO $ webSocketSend $ toJSON (nonce, identifier, reverse args)
  liftIO $ resFromJSON <$> takeMVar mv

-- we have to choose some library to do an RPC call here
webSocketSend :: JSON -> IO ()
webSocketSend _ = return ()

-- the client side event loop
onMessage :: JSON -> Client ()
onMessage response = do
  let (nonce, result) = resFromJSON response
  (n, m) <- get
  put (n, removeNonce n m)
  liftIO $ putMVar (getRunningReq nonce m) result

removeNonce :: Nonce -> [(Nonce, MVar JSON)] -> [(Nonce, MVar JSON)]
removeNonce _ [] = []
removeNonce nonce ((n, mv):rest)
  | nonce == n = rest
  | otherwise = (n, mv) : removeNonce nonce rest

getRunningReq :: Nonce -> [(Nonce, MVar JSON)] -> MVar JSON
getRunningReq _ [] = error "Running request not found"
getRunningReq nonce ((n,mv):rest)
  | nonce == n = mv
  | otherwise = getRunningReq nonce rest
