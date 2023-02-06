{-# LANGUAGE CPP #-}
module Main where

import FedLearnWorker

#ifdef ENCLAVE
import Server
#else
import Client
#endif

import App

main :: IO ()
main = do
  res <- runApp app
  return $ res `seq` ()

