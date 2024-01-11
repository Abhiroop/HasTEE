module PwdCheckerLIOFail where

import Control.Monad.IO.Class(liftIO)

import App
import DCLabel
#ifdef ENCLAVE
import Enclave
#else
import Client
#endif

data API =
  API { checkpwd :: Secure (String -> EnclaveDC Bool) }

s_t_label :: DCLabel
s_t_label = False %% False

pwdChecker :: EnclaveDC (DCLabeled String) -> String -> EnclaveDC Bool
pwdChecker pwd guess = do
  l_pwd <- pwd
  p     <- unlabel l_pwd
  if p == guess
  then return True
  else return False

client :: API -> Client "client" ()
client api = do
  liftIO $ putStrLn "Enter your password:"
  userInput <- liftIO getLine
  res <- gatewayRA ((checkpwd api) <@> userInput)
  liftIO $ putStrLn ("Login returned " ++ show res)

ifctest :: App Done
ifctest = do
  pwd   <- inEnclaveLabeledConstant s_t_label "password"
  efunc <- inEnclave dcDefaultState $ pwdChecker pwd
  runClient (client (API efunc))

main :: IO ()
main = do
  res <- runAppRA "client" ifctest
  return $ res `seq` ()
