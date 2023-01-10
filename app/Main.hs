{-# LANGUAGE CPP #-}
module Main where

import Control.Monad.IO.Class(liftIO)
import Data.List(genericLength)
import GHC.Float(int2Float)
import App

#ifdef ENCLAVE
import Server
#else
import Client
#endif



app :: App Done
app = do
  remoteRef <- liftNewRef 0 :: App (Ref Int)
  count <- ntimes 3 $ do
    v <- readRef remoteRef
    writeRef remoteRef (v + 1)
    return v
  runClient $ do
    sequence $ replicate 4 $ do
      visitors <- onServer count
      liftIO $ putStrLn $ "You are visitor number #" ++ show visitors


main :: IO ()
main = do
  res <- runApp app
  return $ res `seq` ()
