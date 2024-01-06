{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Benchmark where

import Control.Monad.IO.Class(liftIO)

import App
import DCLabel
#ifdef ENCLAVE
import Enclave
#else
import Client
#endif

import System.Clock

data API =
  API { checkpwd :: Secure (String -> EnclaveDC Bool) }


pwdLabel :: DCLabel
pwdLabel = "Alice" %% "Alice"

pwdChecker :: EnclaveDC (DCLabeled String) -> String -> EnclaveDC Bool
pwdChecker pwd guess = do
  l_pwd <- pwd
  priv  <- getPrivilege
  p     <- unlabelP priv l_pwd
  if p == guess
  then return True
  else return False

-- pwdChecker :: String -> String -> EnclaveDC Bool
-- pwdChecker pwd guess = return (guess == pwd)

client :: API -> Client "client" ()
client api = do
  let userInput = "password"
  res <- gateway ((checkpwd api) <@> userInput) -- switch with gatewayRA
  liftIO $ putStrLn ("Login returned " ++ show res)

ifctest :: App Done
ifctest = do
  pwd   <- inEnclaveLabeledConstant pwdLabel "password"
  let priv = toCNF "Alice"
  efunc <- inEnclave (dcDefaultState priv) $ pwdChecker pwd
  -- efunc <- inEnclave (dcDefaultState priv) $ pwdChecker "password"
  runClient (client (API efunc))


timeItmsec :: IO a -> IO a
timeItmsec ioa = do
  startTime <- getTime Monotonic
  a <- ioa
  endTime <- getTime Monotonic

  let elapsedTime = toNanoSecs $ diffTimeSpec endTime startTime
  putStrLn $ "CPU Time: " ++ show (elapsedTime `div` 1000000) ++ " msec"
  return a

main :: IO ()
main = do
  res <- timeItmsec $ runApp "client" ifctest -- switch to runAppRA with gatewayRA
  return $ res `seq` ()
