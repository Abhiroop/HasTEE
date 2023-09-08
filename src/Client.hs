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
data Secure a = Secure CallID [ByteString]

(<@>) :: Binary a => Secure (a -> b) -> a -> Secure b
(Secure identifier args) <@> arg =
  Secure identifier (encode arg : args)

{- The Securable a constraint is necessary for the Server type -}
inEnclave :: (Securable a) => a -> App (Secure a)
inEnclave _ = App $ do
  (next_id, remotes) <- get
  put (next_id + 1, remotes)
  return $ Secure next_id []

ntimes :: (Securable a) => Int -> a -> App (Secure a)
ntimes _ = inEnclave

class Securable a where
  mkSecure :: a -> ([ByteString] -> Server (Maybe ByteString))

instance (Binary a) => Securable (Server a) where
  mkSecure m = \_ -> fmap (Just . encode) m

instance (Binary a, Securable b) => Securable (a -> b) where
  mkSecure f = \(x:xs) -> mkSecure (f $ decode x) xs

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

tryServer :: (Binary a) => Secure (Server a) -> Client (Maybe a)
tryServer (Secure identifier args) = do
  {- SENDING REQUEST HERE -}
  connect localhost connectPort $ \(connectionSocket, remoteAddr) -> do
    -- debug logs
    putStrLn $ "Connection established to " ++ show remoteAddr
    sendLazy connectionSocket $ createPayload (identifier, reverse args)
    resp <- readTCPSocket connectionSocket
    return $ fmap decode (decode resp :: Maybe ByteString)
  {- SENDING ENDS -}

gateway :: Binary a => Secure (Server a) -> Client a
gateway closure = fromJust <$> tryServer closure

runApp :: App a -> IO a
runApp (App s) = evalStateT s initAppState
