{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Client(module Client) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.ByteString.Lazy(ByteString)
import Data.Binary(Binary, encode, decode)

import App

data Server a = ServerDummy deriving (Functor, Applicative, Monad, MonadIO)
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

class Remotable a where
  mkRemote :: a -> ([ByteString] -> Server ByteString)

instance (Binary a) => Remotable (Server a) where
  mkRemote m = \_ -> fmap encode m

instance (Binary a, Remotable b) => Remotable (a -> b) where
  mkRemote f = \(x:xs) -> mkRemote (f $ decode x) xs
    -- where
    --   getRes :: Result a -> a
    --   getRes (Success a) = a
    --   getRes (Error str) = error $ "Runtime error while parsing JSON: " <> str


liftServerIO :: IO a -> App (Server a)
liftServerIO _ = App $ return ServerDummy



-- The client monad
-- MVar is used simply to emulate blocking; we might use a different mechanism
-- type Nonce = Int
-- type ClientState = (Nonce, [(Nonce, (MVar JSON))])
type Client = IO

-- initClientState :: ClientState
-- initClientState = (0, [])

-- original types had `Client ()`
-- was it to enforce a `return ()`
-- relaxing this for now (might revise later)

runClient :: Client a -> App Done
runClient cl = do
  v <- liftIO cl
  return $ v `seq` Done

-- newResult :: Client (Nonce, MVar JSON)
-- newResult = do
--   (nonce, m) <- get
--   mv <- liftIO newEmptyMVar
--   put (nonce + 1, (nonce, mv):m)
  -- return (nonce, mv)

onServer :: (Binary a) => Remote (Server a) -> Client a
onServer (Remote identifier args) = do
  -- (nonce, mv) <- newResult
  webSocketSend $ encode (identifier, reverse args)
  {- BLOCKING HAPPENS HERE -}
  respFromServer <- (undefined :: IO ByteString)
  {- BLOCING ENDS -}
  return $ decode respFromServer

-- we have to choose some library to do an RPC call here
webSocketSend :: ByteString -> IO ()
webSocketSend _ = return ()

-- the client side event loop
-- onMessage :: JSON -> Client ()
-- onMessage response = do
--   let (nonce, result) = resFromJSON response
--   (n, m) <- get
--   put (n, removeNonce n m)
--   liftIO $ putMVar (getRunningReq nonce m) result

-- removeNonce :: Nonce -> [(Nonce, MVar JSON)] -> [(Nonce, MVar JSON)]
-- removeNonce _ [] = []
-- removeNonce nonce ((n, mv):rest)
--   | nonce == n = rest
--   | otherwise = (n, mv) : removeNonce nonce rest

-- getRunningReq :: Nonce -> [(Nonce, MVar JSON)] -> MVar JSON
-- getRunningReq _ [] = error "Running request not found"
-- getRunningReq nonce ((n,mv):rest)
--   | nonce == n = mv
--   | otherwise = getRunningReq nonce rest

runApp :: App a -> IO a
runApp (App s) = evalStateT s initAppState
