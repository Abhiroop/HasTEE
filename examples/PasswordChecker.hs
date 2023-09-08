{-# LANGUAGE CPP #-}
module PasswordChecker (main) where

import Control.Monad.IO.Class(liftIO)

import App

#ifdef ENCLAVE
import Enclave
#else
import Client
#endif

pwdChkr :: Enclave String -> String -> Enclave Bool
pwdChkr pwd guess = fmap (== guess) pwd


passwordChecker :: App Done
passwordChecker = do
  paswd <- enclaveConstant ("secret") :: App (Enclave String) -- see NOTE 1
  enclaveFunc <- inEnclave $ pwdChkr paswd
  runClient $ do
    liftIO $ putStrLn "Enter your password"
    userInput <- liftIO getLine
    res <- gateway (enclaveFunc <@> userInput)
    liftIO $ putStrLn $ "Your login attempt returned " <> (show res)


main :: IO ()
main = do
  res <- runApp passwordChecker
  return $ res `seq` ()

-- NOTE 1
-- If the question is about the untrusted client accessing the actual source code
-- it is quite possible to instead give the client
-- `liftEnclaveIO undefined` -- see the Client.hs definition of liftEnclaveIO

