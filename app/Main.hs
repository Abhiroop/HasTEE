{-# LANGUAGE CPP #-}
module Main where

import Control.Monad.IO.Class(liftIO)

import App

#ifdef ENCLAVE
import Server
#else
import Client
#endif

import Library

app :: App Done
app = do
  count <- setup

  runClient $ do
    v1 <- count 
    v2 <- count
    v3 <- count
    v4 <- count
    liftIO $ putStrLn $ show v1
    liftIO $ putStrLn $ show v2
    liftIO $ putStrLn $ show v3
    liftIO $ putStrLn $ show v4
--    liftIO $ putStrLn $ "You are visitor number #" ++ show visitors


main :: IO ()
main = do
  res <- runApp app
  return $ res `seq` ()
