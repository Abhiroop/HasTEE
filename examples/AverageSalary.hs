{-# LANGUAGE CPP #-}
module AverageSalary where

import Control.Monad.IO.Class(liftIO)
import GHC.Float(int2Float)
import App

#ifdef ENCLAVE
import Server
#else
import Client
#endif

type Salary = Int

type ServerState = [Int]

sendData :: Server (Ref ServerState) -> Server () -> Salary -> Server ()
sendData serverState unlock salary = do
  state_ref <- serverState
  state <- readRef state_ref
  writeRef state_ref (salary : state)
  when (length state + 1 > 2) unlock

when :: Monad m => Bool -> m () -> m ()
when True ma = ma
when False _ = return ()

getAvg :: Server (Ref ServerState) -> Server Float
getAvg serverState = do
  state <- readRef =<< serverState
  return $ fromIntegral (sum state) / fromIntegral (length state)

data API = API
    { send :: Remote (Salary -> Server ())
    , avg  :: Remote (Server Float)
    }

putStrLnC :: String -> Client ()
putStrLnC str = liftIO $ putStrLn str

clientApp :: API -> Client ()
clientApp api = do
  putStrLnC "input salary..."
  salary <- liftIO $ readLn :: Client Int
  onServer (send api <.> salary)
  average <- tryServer (avg api)
  case average of
    Nothing -> putStrLnC "can not release average, leaky!"
    Just a  -> putStrLnC $ "average : " ++ show a

app :: App Done
app = do
  remoteSt <- liftNewRef [] :: App (Server (Ref ServerState))
  (getAv, unlock, lock) <- flowlock $ getAvg remoteSt
  sD                    <- remote $ sendData remoteSt unlock

  runClient $ clientApp $ API sD getAv

main :: IO ()
main = do
  res <- runApp app
  return $ res `seq` ()
