{-# LANGUAGE CPP #-}
module FedLearnServer where

#ifdef ENCLAVE
import Server
#else
import Client
#endif

import Crypto.Paillier
