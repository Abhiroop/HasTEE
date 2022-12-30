{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StaticPointers #-}
module App (module App) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.ByteString.Lazy(ByteString)

{-@ The EnclaveIFC API for programmers

-- use the below 2 function to describe your server API
liftServerIO :: IO a -> App (Server a)
remote :: Remotable a => a -> App (Remote a)

-- use the below function to introduce the Client monad
runClient :: Client () -> App Done


-- use the 2 functions below to talk to the server
-- inside the Client monad
onServer :: Remote (Server a) -> Client a
(<.>) :: Binary a => Remote (a -> b) -> a -> Remote b

-- call this from `main` to run the App monad
runApp :: App a -> IO a


@-}


type CallID = Int
type Method = [ByteString] -> IO ByteString
type AppState = (CallID, [(CallID, Method)])
newtype App a = App (StateT AppState IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

data Done = Done

initAppState :: AppState
initAppState = (0,[])
