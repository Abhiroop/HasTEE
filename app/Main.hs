-- {-# LANGUAGE CPP #-}
-- module Main (main) where

-- import Control.Monad.IO.Class

-- import App

-- #ifdef ENCLAVE
-- import Server
-- #else
-- import Client
-- #endif

-- pwdChkr :: Server String -> String -> Server Bool
-- pwdChkr pwd guess = fmap (== guess) pwd


-- passwordChecker :: App Done
-- passwordChecker = do
--   paswd <- serverConstant "secret" :: App (Server String)
--   serverFunc <- remote $ pwdChkr paswd
--   runClient $ do
--     liftIO $ putStrLn "Enter your password"
--     userInput <- liftIO getLine
--     res <- onServer (serverFunc <.> userInput)
--     liftIO $ putStrLn $ "Your login attempt returned " <> (show res)


-- main :: IO ()
-- main = do
--   res <- runApp passwordChecker
--   return $ res `seq` ()





{-# LANGUAGE CPP #-}
module Main (main) where

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
  remoteRef <- liftNewRef 0 :: App (Server (Ref Int))
  count <- remote $ do
    r <- remoteRef
    v <- readRef r
    writeRef r (v + 1)
    return v
  runClient $ do
    visitors <- onServer count
    liftIO $ putStrLn $ "You are visitor number #" ++ show visitors


main :: IO ()
main = do
  res <- runApp app
  return $ res `seq` ()
