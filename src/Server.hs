{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Server(module Server) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Binary(Binary, encode, decode)
import Data.ByteString.Lazy(ByteString)
import Data.IORef
import Data.Maybe
import Network.Simple.TCP
import System.IO(hFlush, stdout)
import App

import qualified Data.ByteString.Lazy as B

type Ref a = IORef a
newtype Server a = Server (IO a)

instance Functor (Server) where
  fmap f (Server x) = Server $ fmap f x

instance Applicative Server where
  pure x = Server (pure x)
  Server f <*> Server a = Server $ f <*> a

instance Monad Server where
  return = pure
  Server ma >>= k = Server $ do
    a <- ma
    let Server ka = k a
    ka


data Remote a = RemoteDummy

inEnclaveConstant :: a -> App (Server a)
inEnclaveConstant = return . return

liftNewRef :: a -> App (Server (Ref a))
liftNewRef a = App $ do
  r <- liftIO $ newIORef a
  return (return r)

newRef :: a -> Server (Ref a)
newRef x = Server $ newIORef x

readRef :: Ref a -> Server a
readRef ref = Server $ readIORef ref

writeRef :: Ref a -> a -> Server ()
writeRef ref v = Server $ writeIORef ref v

remote :: (Remotable a) => a -> App (Remote a)
remote f = App $ do
  (next_id, remotes) <- get
  put (next_id + 1, (next_id, \bs -> let Server n = mkRemote f bs in n) : remotes)
  return RemoteDummy

ntimes :: (Remotable a) => Int -> a -> App (Remote a)
ntimes n f = App $ do
  r <- liftIO $ newIORef n
  (next_id, remotes) <- get
  put (next_id + 1, (next_id, \bs ->
    let Server s = do c <- Server $ do atomicModifyIORef' r $ \i -> (i - 1, i)
                      if c > 0
                        then mkRemote f bs
                        else return Nothing
    in s) : remotes)


  return RemoteDummy

(<.>) :: Binary a => Remote (a -> b) -> a -> Remote b
(<.>) = error "Access to client not allowed"


class Remotable a where
  mkRemote :: a -> ([ByteString] -> Server (Maybe ByteString))

instance (Binary a) => Remotable (Server a) where
  mkRemote m = \_ -> fmap (Just . encode) m

instance (Binary a, Remotable b) => Remotable (a -> b) where
  mkRemote f = \(x:xs) -> mkRemote (f $ decode x) xs

data Client a = ClientDummy deriving (Functor, Applicative, Monad, MonadIO)

runClient :: Client a -> App Done
runClient _ = return Done

tryServer :: (Binary a) => Remote (Server a) -> Client (Maybe a)
tryServer _ = ClientDummy

onServer :: Binary a => Remote (Server a) -> Client a
onServer _ = ClientDummy

unsafeOnServer :: Binary a => Remote (Server a) -> Client a
unsafeOnServer _ = ClientDummy

{-@ The server's event loop. @-}
runApp :: App a -> IO a
runApp (App s) = do
  (a, (_, vTable)) <- runStateT s initAppState
  {- BLOCKING HERE -}
  _ <- serve (Host localhost) connectPort $
    \(connectionSocket, remoteAddr) -> do
      -- debug log
      putStrLn $ "TCP connection established from " ++ show remoteAddr
      hFlush stdout -- Gramine prints only if stdout is flushed
      req <- readTCPSocket connectionSocket
      onEvent vTable req connectionSocket
  {- BLOCKING ENDS -}
  return a -- the a is irrelevant



onEvent :: [(CallID, Method)] -> ByteString -> Socket -> IO ()
onEvent mapping incoming socket = do
  let (identifier, args) = decode incoming :: (CallID, [ByteString])
      Just f = lookup identifier mapping
  result <- encode <$> f args
  let res = handleVoidTy result -- the () type cannot be sent over wire
  sendLazy socket (B.append (msgSize res) res) -- See NOTE 1
  where
    msgSize r = encode $ B.length r
    handleVoidTy r = if (B.length r == 0) -- the () type has msg length 0
                     then encode '\0'
                     else r

-- NOTE 1
-- We do not use `createPayload` because as a first step it
-- encodes the message body to a ByteString. In case of the
-- server the `result` is already a ByteString. So it further
-- encodes the ByteString and gives a wrong result.
-- We can modify the `createPayload` function so that depending
-- on the value of some identifier term that we send, it decides
-- to encode or not encode the message body but then the type
-- will become an actual dependent type. (Keep it simple!)

