{-# LANGUAGE CPP #-}
module Library (setup) where

import Control.Monad.IO.Class(liftIO)

import App

#ifdef ENCLAVE
import Server
#else
import Client
#endif

setup :: App (Client (Maybe Int))
setup = do
  remoteRef <- liftNewRef 0 :: App (Ref Int)
  count' <- remote $ count remoteRef
  hatch <- ntimes 3 onServer
  return (hatch count')

count :: Ref Int -> Server Int
count ref = do
    v <- readRef ref
    writeRef ref $ v + 1
    return v
