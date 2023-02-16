{-# LANGUAGE CPP #-}
module Main where

import FedLearnWorker

#ifdef ENCLAVE
import Server
#else
import Client
#endif

import App
import Control.Concurrent


trainingDataSet1 :: FilePath
trainingDataSet1 = "fed_dataset/breast_homo_guest1.csv"

trainingDataSet2 :: FilePath
trainingDataSet2 = "fed_dataset/breast_homo_host1.csv"

dummyEnclaveTrainingFile :: FilePath
dummyEnclaveTrainingFile = ""

train :: FilePath -> IO ()
train fp = do
  res <- runApp (app fp)
  return $ res `seq` ()

startEnclave :: IO ()
startEnclave = do
  res <- runApp (app dummyEnclaveTrainingFile)
  -- training not done in enclave
  return $ res `seq` ()

main :: IO ()
main = do
#ifdef ENCLAVE
  startEnclave
#else
  forkIO $ train trainingDataSet1
  train trainingDataSet2
#endif
