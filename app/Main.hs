{-# LANGUAGE CPP #-}

{-# LANGUAGE DataKinds #-}
module Main where

import Control.Monad.IO.Class(liftIO)

import App
import DCLabel
#ifdef ENCLAVE
import Enclave
#else
import Client
#endif

-- dataTillNow :: [Int]
-- dataTillNow = []

-- computeAvg :: Enclave (Ref [Int]) -> Enclave Int
-- computeAvg enc_ref_ints = do
--   ref_ints <- enc_ref_ints
--   vals     <- readRef ref_ints
--   return (avg vals)
--   where
--     avg datas
--       | (length datas) == 0 = 0
--       | otherwise = sum datas `div` (length datas)

-- sendData :: Enclave (Ref [Int]) -> Int -> Enclave ()
-- sendData enc_ref_ints n = do
--   ref_ints <- enc_ref_ints
--   vals     <- readRef ref_ints
--   writeRef ref_ints (n : vals)


-- data API =
--   API { sendToEnclave :: Secure (Int -> Enclave ())
--       , compAvg       :: Secure (Enclave Int)
--       }



-- client1 :: API -> Client ()
-- client1 api = do
--   gatewayRA ((sendToEnclave api) <@> 1700)
--   res <- gatewayRA (compAvg api)
--   liftIO $ putStrLn $ "Computed result " <> (show res)


-- privateAverage :: App Done
-- privateAverage = do
--   initialData <- liftNewRef dataTillNow
--   sD <- inEnclave $ sendData initialData
--   cA <- inEnclave $ computeAvg initialData
--   runClient (client1 (API sD cA))


data API =
  API { checkpwd :: Secure (String -> EnclaveDC Bool) }

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
  pwd   <- inEnclaveLabeledConstant (False %% True) "password"
  efunc <- inEnclave dcDefaultState $ pwdChecker pwd
  runClient (client (API efunc))

main :: IO ()
main = do
  res <- runAppRA "client" ifctest
  return $ res `seq` ()
