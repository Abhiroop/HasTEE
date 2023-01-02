{-# LANGUAGE CPP #-}
module SecureCounter (main) where

-- copy and paste this program into the Main.hs file

import Control.Monad.IO.Class
import Data.IORef

import App

#ifdef ENCLAVE
import Server
#else
import Client
#endif


app :: App Done
app = do
  remoteRef <- liftServerIO (newIORef 0) :: App (Server (IORef Int))
  count <- remote $ do
    r <- remoteRef
    liftIO $ atomicModifyIORef r (\v -> (v+1, v+1))
  runClient $ do
    visitors <- onServer count
    liftIO $ putStrLn $ "You are visitor number #" ++ show visitors


main :: IO ()
main = do
  res <- runApp app
  return $ res `seq` ()
