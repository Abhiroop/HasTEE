{-# LANGUAGE GeneralizedNewtypeDeriving, StaticPointers #-}
module App (module App) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict

import Data.ByteString.Lazy(ByteString, append, length, fromStrict)
import Data.Binary(Binary, encode, decode)
import Data.Maybe(fromMaybe)
import Network.Simple.TCP

{-@ The EnclaveIFC API for programmers

-- use the below functions to describe your server API

-- mutable references
liftNewRef :: a -> App (Server (Ref a))
newRef     :: a -> Server (Ref a)
readRef    :: Ref a -> Server a
writeRef   :: Ref a -> a -> Server ()
-- immutable value
serverConstant :: a -> App (Server a)
-- closures
-- create an escape hatch that can be used however many times you want
remote :: Remotable a => a -> App (Remote a)

-- create an escape hatch that can be used only a specific amount of times
ntimes :: Remotable a => Int -> a -> App (Remote a)

-- use the below function to introduce the Client monad
runClient :: Client () -> App Done


-- use the 3 functions below to talk to the server
-- inside the Client monad

-- try to extract a result from a server computation. Might fail if the
-- declassifier does not allow the information leak
tryServer :: Remote (Server a) -> Client (Maybe a)

-- Extract a result from a server computation, with the assumption
-- that it will not fail. Will throw an exception if the result is not
-- returned due to some policy violation.
onServer :: Remote (Server a) -> Client a
(<.>) :: Binary a => Remote (a -> b) -> a -> Remote b

-- call this from `main` to run the App monad
runApp :: App a -> IO a


@-}


type CallID = Int
type Method = [ByteString] -> IO (Maybe ByteString)
type AppState = (CallID, [(CallID, Method)])
newtype App a = App (StateT AppState IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

data Done = Done

initAppState :: AppState
initAppState = (0,[])

-- Client-server communication utils follow

localhost :: String
localhost = "127.0.0.1"

connectPort :: String
connectPort = "8000"

{-@ `createPayload` creates the  binary request to send over TCP.

     The payload comprise of:
     8 bytes message length followed by n bytes message body
     where n :: Int64 (from the length function in `bytestring`)

@-}
createPayload :: Binary a => a -> ByteString
createPayload msg = append bytstr msgBody
  where
    msgBody = encode msg
    msgSize = Data.ByteString.Lazy.length msgBody
    bytstr  = encode msgSize

readTCPSocket :: (MonadIO m) => Socket -> m ByteString
readTCPSocket socket = do
  -- first 8 bytes (Int64) encodes the msg size
  mSize <- recv socket 8
  let mSize_ = fromMaybe err mSize
  -- read the actual message body now
  msgBody <- recv socket (decode $ fromStrict mSize_)
  return $ fromStrict $ fromMaybe err msgBody
  where
    err = error "Error parsing request"
