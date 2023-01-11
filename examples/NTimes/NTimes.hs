module NTimes where

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

  hatch <- ntimes 3 onServer

  runClient $ do
    v1 <- hatch count
    v2 <- hatch count
    v3 <- hatch count
    v4 <- hatch count
    putStrLn $ show v1
    putStrLn $ show v2
    putStrLn $ show v3
    putStrLn $ show v4
--    liftIO $ putStrLn $ "You are visitor number #" ++ show visitors


main :: IO ()
main = do
  res <- runApp app
  return $ res `seq` ()
