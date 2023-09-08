{-# LANGUAGE CPP #-}
module NaiveDeclassifier where

import Control.Monad.IO.Class(liftIO)
import Data.List(genericLength)
import GHC.Float(int2Float)
import App

#ifdef ENCLAVE
import Enclave
#else
import Client
#endif



getData :: Ref (Sec [Int]) -> Int -> Enclave Int
getData secret idx = do
  sech <- readRef secret
  let sec_i = fmap (\s -> s !! idx) sech
  return (declassify sec_i)

releaseAvg :: Ref Bool -> Enclave ()
releaseAvg bool = writeRef bool True

doAvg :: [Int] -> Float
doAvg xs = realToFrac (sum xs) / genericLength xs

getAvg :: Ref Bool -> Ref (Sec [Int]) -> Enclave Float
getAvg bool secret = do
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
  remoteSec1 <- liftNewRef (sec [15,30,11,6]) :: App (Ref (Sec [Int]))
  remoteSec2 <- liftNewRef False :: App (Ref Bool)
  gD <- inEnclave $ getData remoteSec1
  rA <- inEnclave $ releaseAvg remoteSec2
  gA <- inEnclave $ getAvg remoteSec2 remoteSec1
  runClient $ do
    data1 <- gateway (gD <@> 3) -- the data memory `3` can be
                                 -- modified by the hacker
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
