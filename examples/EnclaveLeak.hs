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



getData :: Server (Ref [Int]) -> Int -> Server Int
getData secret idx = do
  r <- secret
  s <- readRef r
  return (s !! idx)

releaseAvg :: Server (Ref Bool) -> Server Bool
releaseAvg bool = do
  ref <- bool
  writeRef ref True
  r <- readRef ref
  return r

doAvg :: [Int] -> Float
doAvg xs = realToFrac (sum xs) / genericLength xs

getAvg :: Server (Ref Bool) -> Server (Ref [Int]) -> Server Float
getAvg bool' secret' = do
  bool <- bool'
  secret <- secret'
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
  remoteSec1 <- liftNewRef [15,30,11,6] :: App (Server (Ref [Int]))
  remoteSec2 <- liftNewRef False :: App (Server (Ref Bool))
  gD <- inEnclave $ getData remoteSec1
  rA <- inEnclave $ releaseAvg remoteSec2
  gA <- inEnclave $ getAvg remoteSec2 remoteSec1
  runClient $ do
    data1 <- gateway (gD <@> 3)
    _     <- gateway rA
    avg   <- gateway gA
    let b = dummyCompOnData data1 avg
    printCl $ "Is data less than avg? " <> show b
  where
    dummyCompOnData i av = int2Float i < av


main :: IO ()
main = do
  res <- runApp app
  return $ res `seq` ()
