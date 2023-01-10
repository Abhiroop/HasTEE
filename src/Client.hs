{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Client(module Client) where

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

class Remotable a where
  mkRemote :: a -> ([ByteString] -> Server ByteString)

instance (Binary a) => Remotable (Server a) where
  mkRemote m = \_ -> fmap encode m

instance (Binary a, Remotable b) => Remotable (a -> b) where
  mkRemote f = \(x:xs) -> mkRemote (f $ decode x) xs

serverConstant :: a -> App (Server a)
serverConstant _ = return ServerDummy

liftNewRef :: a -> App (Ref a)
liftNewRef _ = return RefDummy

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

onServer :: (Binary a) => Remote (Server a) -> Client a
onServer (Remote identifier args) = do
  {- SENDING REQUEST HERE -}
  connect localhost connectPort $ \(connectionSocket, remoteAddr) -> do
    -- debug logs
    putStrLn $ "Connection established to " ++ show remoteAddr
    sendLazy connectionSocket $ createPayload (identifier, reverse args)
    resp <- readTCPSocket connectionSocket
    return $ decode resp
  {- SENDING ENDS -}


runApp :: App a -> IO a
runApp (App s) = evalStateT s initAppState





----------------------Sec----------------------------------

newtype Sec a = MkSec a -- dont export MkSec

instance Monad Sec where
  return = pure

  MkSec a >>= k =
    MkSec $ let MkSec b = k a in b

instance Functor Sec where
  fmap = liftM

instance Applicative Sec where
  pure = sec
  MkSec ab <*> MkSec a = MkSec (ab a)


--used to protect value `a`
sec :: a -> Sec a
sec = MkSec

-- look at a protected value given
-- that you can produce a security
-- level `s`
-- open :: Sec s a -> s -> a
-- open _ _ = error "Client cannot open"

-- up :: forall a sl sh . Less sl sh => Sec sl a -> Sec sh a
-- up (MkSec x) = (less @sl @sh) `seq` sech
--   where
--     sech = (MkSec x)

-- -- only for trusted code
-- reveal :: Sec s a -> a
-- reveal (MkSec x) = x

declassify :: Sec a -> a
declassify _ = error "Client cannot declassify"

-- endorse :: a -> Sec H a
-- endorse _ = error "Client cannot endorse"
