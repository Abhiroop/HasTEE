{-# LANGUAGE CPP #-}
module Main where

import FedLearnWorker

-- #ifdef ENCLAVE
-- import Server
-- #else
-- import Client
-- #endif

-- import App
-- import Control.Concurrent


-- trainingDataSet1 :: FilePath
-- trainingDataSet1 = "fed_dataset/breast_homo_guest1.csv"

-- trainingDataSet2 :: FilePath
-- trainingDataSet2 = "fed_dataset/breast_homo_host1.csv"

-- dummyEnclaveTrainingFile :: FilePath
-- dummyEnclaveTrainingFile = ""

-- train :: FilePath -> IO ()
-- train fp = do
--   res <- runApp (app fp)
--   return $ res `seq` ()

-- startEnclave :: IO ()
-- startEnclave = do
--   res <- runApp (app dummyEnclaveTrainingFile)
--   -- training not done in enclave
--   return $ res `seq` ()

-- main :: IO ()
-- main = do
-- #ifdef ENCLAVE
--   startEnclave
-- #else
--   forkIO $ train trainingDataSet1
--   train trainingDataSet2
-- #endif

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
  paswd <- serverConstant ("secret") :: App (Server String) -- see NOTE 1
  serverFunc <- remote $ pwdChkr paswd
  runClient $ do
    userInput <- liftIO $ putStrLn "Enter your password" >> liftIO getLine
    res <- onServer (serverFunc <.> userInput)
    liftIO $ putStrLn $ "Your login attempt returned " <> (show res)


main :: IO Done
main = runApp passwordChecker
