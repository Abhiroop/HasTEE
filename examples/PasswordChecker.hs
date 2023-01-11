{-# LANGUAGE CPP #-}
module PasswordChecker (main) where

import Control.Monad.IO.Class(liftIO)

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
  paswd <- serverConstant ("secret") :: App (Server String) -- see NOTE 1
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

-- NOTE 1
-- If the question is about the untrusted client accessing the actual source code
-- it is quite possible to instead give the client
-- `liftServerIO undefined` -- see the Client.hs definition of liftServerIO

