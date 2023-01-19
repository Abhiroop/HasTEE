{-# LANGUAGE CPP #-}
module Main where

import Control.Monad.IO.Class(liftIO)
import Data.List(genericLength)
import GHC.Float(int2Float)
import App
import Security.Sec

#ifdef ENCLAVE
import Server
#else
import Client
#endif


type Salary = Int


data ServerState = ServerState { average :: Float
                               , clients :: Int
                               } deriving Show

initServer :: ServerState
initServer = ServerState { average = 0.0, clients = 0}

sendData :: Server (Ref ServerState) -> Salary -> Server ()
sendData serverState salary = do
  state_ref <- serverState
  ServerState {average = avg, clients = cl} <- readRef state_ref
  let new_avg = (avg + (realToFrac salary)) / (int2Float $ cl + 1)
  writeRef state_ref (ServerState {average = new_avg, clients = cl + 1})

getAvg :: Server (Ref ServerState) -> Server Float
getAvg serverState = do
  state_ref <- serverState
  ServerState {average = avg, clients = cl} <- readRef state_ref
  return avg


printCl :: String -> Client ()
printCl = liftIO . putStrLn

app :: App Done
app = do
  remoteSt <- liftNewRef initServer :: App (Server (Ref ServerState))
  sD    <- remote $ sendData remoteSt
  getAv <- remote $ getAvg remoteSt
  runClient $ do
    printCl "Input salary : "
    salary <- liftIO $ readLn :: Client Int
    _   <- onServer (sD <.> salary)
    avg <- onServer getAv
    printCl $ "Average salary : " <> show avg


main :: IO ()
main = do
  res <- runApp app
  return $ res `seq` ()
