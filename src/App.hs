{-# LANGUAGE GeneralizedNewtypeDeriving, StaticPointers #-}
module App (module App) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.ByteString.Lazy(ByteString)

{-
runClient :: Client () -> App Done
liftServerIO :: IO a -> App (Server a)
remote :: Remotable a => a -> App a
onServer :: Remote (Server a) -> Client a
(<.>) :: Serialize a => Remote (a -> b) -> a -> Remote b
getSessionID :: Server SessionID


-}

{- This whole logic is handled by StaticPtrs -}
type CallID = Int
type Method = [ByteString] -> IO ByteString
type AppState = (CallID, [(CallID, Method)])
newtype App a = App (StateT AppState IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

data Done = Done

initAppState :: AppState
initAppState = (0,[])

-- a util function for JSON
-- resFromJSON :: FromJSON a => JSON -> a
-- resFromJSON = getRes . fromJSON
--   where
--     getRes :: Result a -> a
--     getRes (Success a) = a
--     getRes (Error str) = error $ "Runtime error while parsing JSON: " <> str
