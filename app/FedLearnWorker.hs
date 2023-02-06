{-# LANGUAGE CPP #-}
module FedLearnWorker where


#ifdef ENCLAVE
import Server
#else
import Client
#endif

import App

import FedLearnServer

import Control.Monad.IO.Class(liftIO)

printCl :: String -> Client ()
printCl = liftIO . putStrLn

noClients :: Int
noClients = 2

app :: App Done
app = do
  server_st <- liftNewRef initSrvState :: App (Server (Ref SrvSt))
  initSt    <- remote $ initTEEState server_st
  getPubK   <- remote $ getPubKey server_st
  aggrModel <- remote $ aggregateModel server_st
  validateM <- remote $ validate server_st
  runClient $ do
    _    <- onServer (initSt <.> noClients)
    pubK <- onServer getPubK
    printCl "Hello"
