module NTimes where

import App

import Control.Monad.IO.Class(liftIO)

#ifdef ENCLAVE
import Enclave
#else
import Client
#endif


app :: App Done
app = do
  remoteRef <- liftNewRef 0 :: App (Enclave (Ref Int))
  count <- ntimes 3 $ do
    r <- remoteRef
    v <- readRef r
    writeRef r (v + 1)
    return v
  runClient $ do
    sequence $ replicate 4 $ do
      visitors <- tryEnclave count
      liftIO $ putStrLn $ "You are visitor number #" ++ show visitors


main :: IO ()
main = do
  res <- runApp app
  return $ res `seq` ()

