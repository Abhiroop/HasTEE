module PwdCheckerLIOPriv where

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

client :: API -> Client "client" ()
client api = do
  liftIO $ putStrLn "Enter your password:"
  userInput <- liftIO getLine
  res <- gatewayRA ((checkpwd api) <@> userInput)
  liftIO $ putStrLn ("Login returned " ++ show res)

ifctest :: App Done
ifctest = do
  pwd   <- inEnclaveLabeledConstant pwdLabel "password"
  let priv = toCNF "Alice"
  efunc <- inEnclave (dcDefaultState priv) $ pwdChecker pwd
  runClient (client (API efunc))

main :: IO ()
main = do
  res <- runAppRA "client" ifctest
  return $ res `seq` ()
