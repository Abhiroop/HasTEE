{-# LANGUAGE CPP #-}
module Main where

import Control.Monad.IO.Class(liftIO)
import Data.List(genericLength)
import GHC.Float(int2Float)
import App


#ifdef ENCLAVE
import Enclave
#else
import Client
#endif


getData :: Enclave (Ref [Int]) -> Int -> Enclave Int
getData secret idx = do
  r <- secret
  s <- readRef r
  return (s !! idx)

releaseAvg :: Enclave (Ref Bool) -> Enclave Bool
releaseAvg bool = do
  ref <- bool
  writeRef ref True
  r <- readRef ref
  return r

doAvg :: [Int] -> Float
doAvg xs = realToFrac (sum xs) / genericLength xs

getAvg :: Enclave (Ref Bool) -> Enclave (Ref [Int]) -> Enclave Float
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
  remoteSec1 <- liftNewRef [15,30,11,6] :: App (Enclave (Ref [Int]))
  remoteSec2 <- liftNewRef False :: App (Enclave (Ref Bool))
  liftIO $ putStrLn "registering getData"
  gD <- inEnclave $ getData remoteSec1
  rA <- inEnclave $ releaseAvg remoteSec2
  gA <- inEnclave $ getAvg remoteSec2 remoteSec1
  runClient $ do
    data1 <- gateway (gD <@> 3)
    liftIO $ putStrLn $ show data1
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




-- {-# LANGUAGE CPP #-}
-- module Main where

-- import Control.Monad.IO.Class(liftIO)

-- import App

-- #ifdef ENCLAVE
-- import Enclave
-- #else
-- import Client
-- #endif
 
-- pwdChkr :: Enclave String -> String -> Enclave Bool
-- pwdChkr pwd guess = fmap (== guess) pwd


-- passwordChecker :: App Done
-- passwordChecker = do
--   paswd <- inEnclaveConstant ("secret") :: App (Enclave String) -- see NOTE 1
--   enclaveFunc <- inEnclave $ pwdChkr paswd
--   runClient $ do
--     liftIO $ putStrLn "Enter your password"
--     userInput <- liftIO getLine
--     res <- gateway (enclaveFunc <@> userInput)
--     liftIO $ putStrLn $ "Your login attempt returned " <> (show res)


-- main :: IO ()
-- main = do
--   putStrLn "before"
--   res <- runApp passwordChecker
--   putStrLn "after"
--   return $ res `seq` ()
