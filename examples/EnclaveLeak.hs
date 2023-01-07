{-# LANGUAGE CPP #-}
module EnclaveLeak where

import Control.Monad.IO.Class(liftIO)
import Data.List(genericLength)
import GHC.Float(int2Float)
import App


#ifdef ENCLAVE
import Server
#else
import Client
#endif



getData :: Ref [Int] -> Int -> Server Int
getData secret idx = do
  s <- readRef secret
  return (s !! idx)

releaseAvg :: Ref Bool -> Server Bool
releaseAvg bool = do
  writeRef bool True
  r <- readRef bool
  return r

doAvg :: [Int] -> Float
doAvg xs = realToFrac (sum xs) / genericLength xs

getAvg :: Ref Bool -> Ref [Int] -> Server Float
getAvg bool secret = do
  b <- readRef bool
  if b
  then do
    s <- readRef secret
    let avg = doAvg s
    return avg
  else return 0.0


printCl :: String -> Client ()
printCl = liftIO . putStrLn

app :: App Done
app = do
  remoteSec1 <- liftNewRef [15,30,11,6] :: App (Ref [Int])
  remoteSec2 <- liftNewRef False :: App (Ref Bool)
  gD <- remote $ getData remoteSec1
  rA <- remote $ releaseAvg remoteSec2
  gA <- remote $ getAvg remoteSec2 remoteSec1
  runClient $ do
    data1 <- onServer (gD <.> 3)
    _     <- onServer rA
    avg   <- onServer gA
    let b = dummyCompOnData data1 avg
    printCl $ "Is data less than avg? " <> show b
  where
    dummyCompOnData i av = int2Float i < av


main :: IO ()
main = do
  res <- runApp app
  return $ res `seq` ()
