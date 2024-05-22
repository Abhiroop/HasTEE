{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Enclave(module Enclave) where

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
newtype Enclave a = Enclave (IO a)

instance Functor (Enclave) where
  fmap f (Enclave x) = Enclave $ fmap f x

instance Applicative Enclave where
  pure x = Enclave (pure x)
  Enclave f <*> Enclave a = Enclave $ f <*> a

instance Monad Enclave where
  return = pure
  Enclave ma >>= k = Enclave $ do
    a <- ma
    let Enclave ka = k a
    ka


data Secure a = SecureDummy

{-# NOINLINE inEnclaveConstant #-}
{-# RULES "HASTEERULES inEnclaveConstant/Enclave" forall (x :: a) . inEnclaveConstant x = return (return x) #-}
inEnclaveConstant :: a -> App (Enclave a)
inEnclaveConstant _ = error "inEnclaveConstant error"
-- inEnclaveConstant = return . return

{-# NOINLINE liftNewRef #-}
{-# RULES "HASTEERULES liftNewRef/Enclave" forall (x :: a) . liftNewRef x = App (do r <- liftIO $ newIORef x; return (return r)) #-}
liftNewRef :: a -> App (Enclave (Ref a))
liftNewRef _ = error "liftNewRef error"
-- liftNewRef a = App $ do
--   r <- liftIO $ newIORef a
--   return (return r)

{-# NOINLINE newRef #-}
{-# RULES "HASTEERULES newRef/Enclave" forall (v :: a) . newRef v = Enclave (newIORef v) #-}
newRef :: a -> Enclave (Ref a)
newRef _ = error "newRef error"
-- newRef x = Enclave $ newIORef x

{-# NOINLINE readRef #-}
{-# RULES "HASTEERULES readRef/Enclave" forall (ref :: Ref a) . readRef ref = Enclave (readIORef ref) #-}
readRef :: Ref a -> Enclave a
readRef _ = error "readRef error"
-- readRef ref = Enclave $ readIORef ref

{-# NOINLINE writeRef #-}
{-# RULES "HASTEERULES writeRef/Enclave" forall (ref :: Ref a) (v :: a) . writeRef ref v = Enclave (writeIORef ref v) #-}
writeRef :: Ref a -> a -> Enclave ()
writeRef _ _ = error "writeRef error"
-- writeRef ref v = Enclave $ writeIORef ref v

{-# NOINLINE inEnclave #-}
{-# RULES "HASTEERULES inEnclave/Enclave" forall (f :: a) . inEnclave f = App (do (next_id, remotes) <- get; put (next_id + 1, (next_id, \bs -> let Enclave n = mkSecure f bs in n) : remotes); return SecureDummy) #-}
inEnclave :: (Securable a) => a -> App (Secure a)
inEnclave _ = error "inEnclave error"
-- inEnclave f = App $ do
--   (next_id, remotes) <- get
--   put (next_id + 1, (next_id, \bs -> let Enclave n = mkSecure f bs in n) : remotes)
--   return SecureDummy

-- ntimes :: (Securable a) => Int -> a -> App (Secure a)
-- ntimes n f = App $ do
--   r <- liftIO $ newIORef n
--   (next_id, remotes) <- get
--   put (next_id + 1, (next_id, \bs ->
--     let Enclave s = do
--           c <- Enclave $ do atomicModifyIORef' r $ \i -> (i - 1, i)
--           if c > 0
--           then mkSecure f bs
--           else return Nothing
--     in s) : remotes)


--   return SecureDummy

{-# NOINLINE (<@>) #-}
{-# RULES "HASTEERULES HASTEERULES app/Enclave" forall (closure :: Secure (a -> b)) (arg :: a) . (<@>) closure arg = error "enclave can not apply client-side closures" #-}
(<@>) :: Binary a => Secure (a -> b) -> a -> Secure b
(<@>) = error "Access to client not allowed"


class Securable a where
  mkSecure :: a -> ([ByteString] -> Enclave (Maybe ByteString))

instance (Binary a) => Securable (Enclave a) where
  mkSecure m = \_ -> fmap (Just . encode) m

instance (Binary a, Securable b) => Securable (a -> b) where
  mkSecure f = \(x:xs) -> mkSecure (f $ decode x) xs

data Client a = ClientDummy deriving (Functor, Applicative, Monad, MonadIO)

{-# NOINLINE runClient #-}
{-# RULES "HASTEERULES runClient/Enclave" forall (client :: Client a) . runClient client = return Done #-}
runClient :: Client a -> App Done
runClient _ = error "runClient error"
-- runClient _ = return Done

{-# NOINLINE tryEnclave #-}
{-# RULES "HASTEERULES tryEnclave/Enclave" forall (closure :: Binary a => Secure (Enclave a)) . tryEnclave closure = ClientDummy #-}
tryEnclave :: (Binary a) => Secure (Enclave a) -> Client (Maybe a)
tryEnclave _ = error "tryEnclave error"
-- tryEnclave _ = ClientDummy

{-# NOINLINE gateway #-}
{-# RULES "HASTEERULES gateway/Enclave" forall (closure :: Secure (Enclave a)) . gateway closure = ClientDummy #-}
gateway :: Binary a => Secure (Enclave a) -> Client a
gateway _ = error "gateway error"
-- gateway _ = ClientDummy

{-# NOINLINE unsafeOnEnclave #-}
{-# RULES "HASTEERULES unsafeOnEnclave/Enclave" forall (closure :: Binary a => Secure (Enclave a)) . unsafeOnEnclave closure = ClientDummy #-}
unsafeOnEnclave :: Binary a => Secure (Enclave a) -> Client a
unsafeOnEnclave _ = error "unsafeOnEnclave error"
-- unsafeOnEnclave _ = ClientDummy

{-# NOINLINE runApp #-}
{-# RULES "HASTEERULES runApp/Enclave" forall (app :: App Done) . runApp app = do (_, (_,vTable)) <- runStateT (unApp app) initAppState
                                                                                  _ <- serve (Host localhost) connectPort $
                                                                                    \(connectionSocket, _)-> do
                                                                                      hFlush stdout
                                                                                      req <- readTCPSocket connectionSocket
                                                                                      onEvent vTable req connectionSocket
                                                                                  return () #-}
runApp :: App Done -> IO ()
runApp _ = putStrLn "I am not supposed to execute" -- return ()
-- runApp (App s) = do
--   (_, (_, vTable)) <- runStateT s initAppState
--   _ <- serve (Host localhost) connectPort $
--     \(connectionSocket, remoteAddr) -> do
--       hFlush stdout
--       req <- readTCPSocket connectionSocket
--       onEvent vTable req connectionSocket
--   return ()



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
-- enclave the `result` is already a ByteString. So it further
-- encodes the ByteString and gives a wrong result.
-- We can modify the `createPayload` function so that depending
-- on the value of some identifier term that we send, it decides
-- to encode or not encode the message body but then the type
-- will become an actual dependent type. (Keep it simple!)


{-#


Rule fired: Class op put (BUILTIN)
Rule fired: Class op get (BUILTIN)
Rule fired: USPEC $fEqList @Char (GHC.Classes)
Rule fired: Class op fmap (BUILTIN)
Rule fired: eqString (GHC.Base)
Rule fired: Class op >>= (BUILTIN)
Rule fired: Class op >>= (BUILTIN)
Rule fired: Class op >> (BUILTIN)
Rule fired: HASTEERULES runClient/Enclave (Enclave)
Rule fired: HASTEERULES inEnclave/Enclave (Enclave)
Rule fired: unpack (GHC.Base)
Rule fired: HASTEERULES inEnclaveConstant/Enclave (Enclave)
Rule fired: Class op >> (BUILTIN)
Rule fired: unpack (GHC.Base)
Rule fired: Class op >>= (BUILTIN)
Rule fired: +# (BUILTIN)
Rule fired: unpack-list (GHC.Base)
Rule fired: unpack-list (GHC.Base)



Rule fired: Class op put (BUILTIN)
Rule fired: Class op get (BUILTIN)
Rule fired: USPEC $fEqList @Char (GHC.Classes)
Rule fired: Class op fmap (BUILTIN)
Rule fired: eqString (GHC.Base)
Rule fired: Class op >>= (BUILTIN)
Rule fired: Class op >>= (BUILTIN)
Rule fired: Class op >> (BUILTIN)
Rule fired: HASTEERULES runClient/Enclave (Enclave)
Rule fired: HASTEERULES inEnclave/Enclave (Enclave)
Rule fired: unpack (GHC.Base)
Rule fired: HASTEERULES inEnclaveConstant/Enclave (Enclave)
Rule fired: Class op >> (BUILTIN)
Rule fired: unpack (GHC.Base)
Rule fired: Class op >>= (BUILTIN)
Rule fired: HASTEERULES runApp/Enclave (Enclave)
Rule fired: Class op >> (BUILTIN)
Rule fired: unpack (GHC.Base)
Rule fired: Class op return (BUILTIN)
Rule fired: unpack-list (GHC.Base)
Rule fired: unpack-list (GHC.Base)
Rule fired: unpack-list (GHC.Base)
Rule fired: unpack-list (GHC.Base)
Rule fired: +# (BUILTIN)


#-}