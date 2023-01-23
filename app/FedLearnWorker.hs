{-# LANGUAGE CPP #-}
module FedLearnWorker where

#ifdef ENCLAVE
import Server
#else
import Client
#endif


