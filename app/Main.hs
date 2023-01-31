{-# LANGUAGE CPP #-}
module Main where

import qualified Data.Vector as V
import Control.Monad
import Control.Monad.IO.Class(liftIO)
import Data.List(genericLength)
import GHC.Float(int2Float)
import App
import Security.Sec
import LogisticRegression
import FedLearnUtils

#ifdef ENCLAVE
import Server
#else
import Client
#endif

getData :: Server (Ref (Sec [Int])) -> Int -> Server Int
getData serv_secret idx = do
  secret <- serv_secret
  sech <- readRef secret
  let sec_i = fmap (\s -> s !! idx) sech
  return (declassify sec_i)

releaseAvg :: Server (Ref Bool) -> Server ()
releaseAvg sbool = do
  bool <- sbool
  writeRef bool True

doAvg :: [Int] -> Float
doAvg xs = realToFrac (sum xs) / genericLength xs

getAvg :: Server (Ref Bool) -> Server (Ref (Sec [Int])) -> Server Float
getAvg serv_bool serv_secret = do
  bool   <- serv_bool
  secret <- serv_secret
  b <- readRef bool
  if b
  then do
    s <- readRef secret
    let s' = declassify s
    let avg = doAvg s'
    return avg
  else return 0.0


printCl :: String -> Client ()
printCl = liftIO . putStrLn

app :: App Done
app = do
  remoteSec1 <- liftNewRef (sec [15,30,11,6]) :: App (Server (Ref (Sec [Int])))
  remoteSec2 <- liftNewRef False :: App (Server (Ref Bool))
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
  (x,y) <- parseDataSet testDataSet
  let w = weights $ fit baseConfig x y
  zipWithM_ (\w o -> putStrLn (show w) >> putStrLn (show o) >> putStrLn "") (V.toList w) otherout
--  putStrLn $ show w

otherout = [ 1.13168292, -0.21312993,  0.09354092, -0.26391138, -0.17312359,  0.05455268,
  -0.23395698, -0.33054974, -0.44401806,  0.20446647,  0.12975334, -0.14920247,
   0.11039067, -0.19896242, -0.14983745,  0.02879726, -0.35788109, -0.46187986,
  -0.50990152,  0.06687542,  0.47857546, -0.11103028,  0.6573644,  -0.15545855,
  -0.01532559,  0.71608367,  0.02366001,  0.17972896, -0.07736812,  0.5992046,
   0.40205282]
