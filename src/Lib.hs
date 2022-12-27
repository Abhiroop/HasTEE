module Lib ( someFunc ) where

{-
runClient :: Client () -> App Done
liftServerIO :: IO a -> App (Server a)
remote :: Remotable a => a -> App a
onServer :: Remote (Server a) -> Client a
(<.>) :: Serialize a => Remote (a -> b) -> a -> Remote b
getSessionID :: Server SessionID


-}

someFunc :: IO ()
someFunc = putStrLn "someFunc"
