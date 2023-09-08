{-# LANGUAGE CPP #-}
module Main where

-- import Control.Monad.IO.Class(liftIO)
-- import Data.List(genericLength)
-- import GHC.Float(int2Float)
-- import App
-- import Security.Sec

-- #ifdef ENCLAVE
-- import Server
-- #else
-- import Client
-- #endif



-- getData :: Server (Ref (Sec [Int])) -> Int -> Server Int
-- getData serv_secret idx = do
--   secret <- serv_secret
--   sech <- readRef secret
--   let sec_i = fmap (\s -> s !! idx) sech
--   return (declassify sec_i)

-- releaseAvg :: Server (Ref Bool) -> Server ()
-- releaseAvg sbool = do
--   bool <- sbool
--   writeRef bool True

-- doAvg :: [Int] -> Float
-- doAvg xs = realToFrac (sum xs) / genericLength xs

-- getAvg :: Server (Ref Bool) -> Server (Ref (Sec [Int])) -> Server Float
-- getAvg serv_bool serv_secret = do
--   bool   <- serv_bool
--   secret <- serv_secret
--   b <- readRef bool
--   if b
--   then do
--     s <- readRef secret
--     let s' = declassify s
--     let avg = doAvg s'
--     return avg
--   else return 0.0


-- printCl :: String -> Client ()
-- printCl = liftIO . putStrLn

-- app :: App Done
-- app = do
--   remoteSec1 <- liftNewRef (sec [15,30,11,6]) :: App (Server (Ref (Sec [Int])))
--   remoteSec2 <- liftNewRef False :: App (Server (Ref Bool))
--   gD <- inEnclave $ getData remoteSec1
--   rA <- inEnclave $ releaseAvg remoteSec2
--   gA <- inEnclave $ getAvg remoteSec2 remoteSec1
--   runClient $ do
--     data1 <- gateway (gD <@> 3)
--     _     <- gateway rA
--     avg   <- gateway gA
--     let b = dummyCompOnData data1 avg
--     printCl $ "Is data less than avg? " <> show b
--   where
--     dummyCompOnData i av = int2Float i < av


-- main :: IO ()
-- main = do
--   res <- runApp app
--   return $ res `seq` ()

import Control.Monad.IO.Class(liftIO)

import App

#ifdef ENCLAVE
import Server
#else
import Client
#endif

pwdChkr :: Server String -> String -> Server Bool
pwdChkr pwd guess = fmap (== guess) pwd


passwordChecker :: App Done
passwordChecker = do
  paswd <- inEnclaveConstant ("secret") :: App (Server String) -- see NOTE 1
  serverFunc <- inEnclave $ pwdChkr paswd
  runClient $ do
    liftIO $ putStrLn "Enter your password"
    userInput <- liftIO getLine
    res <- gateway (serverFunc <@> userInput)
    liftIO $ putStrLn $ "Your login attempt returned " <> (show res)


main :: IO ()
main = do
  res <- runApp passwordChecker
  return $ res `seq` ()
