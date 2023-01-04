{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Monad.IO.Class

import App

#ifdef ENCLAVE
import Server
#else
import Client
#endif

pwdChkr :: Server String -> String -> Server Bool
pwdChkr pwd guess = fmap (== guess) pwd


passwordChecker :: App Done
passwordChecker = do
  paswd <- serverConstant "secret" :: App (Server String)
  serverFunc <- remote $ pwdChkr paswd
  runClient $ do
    liftIO $ putStrLn "Enter your password"
    userInput <- liftIO getLine
    res <- onServer (serverFunc <.> userInput)
    liftIO $ putStrLn $ "Your login attempt returned " <> (show res)


main :: IO ()
main = do
  res <- runApp passwordChecker
  return $ res `seq` ()
