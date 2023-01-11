{-# LANGUAGE CPP #-}
module SecureCounter (main) where

-- copy and paste this program into the Main.hs file

import Control.Monad.IO.Class(liftIO)

import App

#ifdef ENCLAVE
import Server
#else
import Client
#endif


app :: App Done
app = do
  remoteRef <- liftNewRef 0 :: App (Ref Int)
  count <- remote $ do
    v <- readRef remoteRef
    writeRef remoteRef (v + 1)
    return v
  runClient $ do
    visitors <- onServer count
    liftIO $ putStrLn $ "You are visitor number #" ++ show visitors


main :: IO ()
main = do
  res <- runApp app
  return $ res `seq` ()
