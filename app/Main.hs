{-# LANGUAGE CPP #-}

module Main where

import Control.Monad.IO.Class(liftIO)
import Data.Binary
import Data.List (find, delete)

import App

#ifdef ENCLAVE
import Enclave
#else
import Client
#endif

----- Enclave side logic -----

dataTillNow :: [UserInfo]
dataTillNow = []

computeAvg :: Enclave (Ref [UserInfo]) -> Enclave Result
computeAvg enc_ref_users = do
  ref_users <- enc_ref_users
  vals      <- readRef ref_users
  return (avg vals)
  where
    avg datas
      | (length datas) <= 4 = Nothing -- malicious code wouldn't check this
      | otherwise = Just $ sum (map salary datas) `div` (length datas)

sendData :: Enclave (Ref [UserInfo]) -> UserInfo -> Enclave ()
sendData enc_ref_users u = do
  ref_users <- enc_ref_users
  vals      <- readRef ref_users
  writeRef ref_users (u : vals)

deleteData :: Enclave (Ref [UserInfo]) -> UserName -> Enclave ()
deleteData enc_ref_users u = do
  ref_users <- enc_ref_users
  users     <- readRef ref_users
  case (find (\(UserInfo uname _) -> uname == u) users) of
    Nothing -> return ()
    Just uinfo ->
      writeRef ref_users (delete uinfo users)

{-@ malicious delete
deleteData :: Enclave (Ref [UserInfo]) -> UserName -> Enclave ()
deleteData _ _ = pure ()
@-}


------ API between Client and Enclave -----

data API =
  API { sendToEnclave :: Secure (UserInfo -> Enclave ())
      , compAvg       :: Secure (Enclave Result)
      , delData       :: Secure (UserName -> Enclave ())
      }

data UserInfo =
  UserInfo { username :: UserName -- has to be unique; think UUID
           , salary   :: Int
           -- timestamp not sent by Client because
           -- I expect the weaving will log that
           } deriving (Show, Eq)

type Result   = Maybe Int
type UserName = String

instance Binary UserInfo where
  put (UserInfo uname sal) = do
    put uname
    put sal

  get = UserInfo <$> get <*> get

----- Client side logic -----

client :: API -> Client ()
client api = do
  gatewayRA ((sendToEnclave api) <@> user1)
  res <- gatewayRA (compAvg api)
  liftIO $ putStrLn $ "Computed result " <> (show res)


user1 :: UserInfo
user1 = UserInfo { username = "James", salary = 4200 }

user2 :: UserInfo
user2 = UserInfo { username = "Matz", salary = 4000 }

user3 :: UserInfo
user3 = UserInfo { username = "Guido", salary = 3000 }

user4 :: UserInfo
user4 = UserInfo { username = "Brendan", salary = 500 }

user5 :: UserInfo
user5 = UserInfo { username = "Larry", salary = 300 }

user6 :: UserInfo
user6 = UserInfo { username = "Simon", salary = 10000 }


----- Setup and deploy -----

privateAverage :: App Done
privateAverage = do
  initialState <- liftNewRef dataTillNow
  sD <- inEnclave $ sendData   initialState
  cA <- inEnclave $ computeAvg initialState
  dD <- inEnclave $ deleteData initialState
  runClient (client (API sD cA dD))

main :: IO ()
main = do
  res <- runAppRA privateAverage
  return $ res `seq` ()

{- LINEAR HASKELL primer

{-# LANGUAGE LinearTypes #-}

import Prelude.Linear

g :: (a %1-> b) -> a -> b
g f x = body -- f can be used at most once in the body
  where
   body = f x

-}
