{-# LANGUAGE CPP #-}
module Main where

import Control.Monad.IO.Class(liftIO)

import App

#ifdef ENCLAVE
import Server
#else
import Client
#endif

count :: Ref Int -> Server Int
count ref = do
    v <- readRef ref
    writeRef ref $ v + 1
    return v

app :: App Done
app = do
  remoteRef <- liftNewRef 0 :: App (Ref Int)
  count <- remote $ count remoteRef

  hatch <- ntimes 3 onServer

  runClient $ do
    v1 <- hatch count
    v2 <- hatch count
    v3 <- hatch count
    v4 <- hatch count
    liftIO $ putStrLn $ show v1
    liftIO $ putStrLn $ show v2
    liftIO $ putStrLn $ show v3
    liftIO $ putStrLn $ show v4
--    liftIO $ putStrLn $ "You are visitor number #" ++ show visitors


main :: IO ()
main = do
  res <- runApp app
  return $ res `seq` ()
