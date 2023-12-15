{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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

sendData :: Enclave (Ref [UserInfo]) -> UserInfo -> Enclave Result
sendData enc_ref_users u = do
  ref_users <- enc_ref_users
  vals      <- readRef ref_users
  writeRef ref_users (u : vals)
  pure Nothing

deleteData :: Enclave (Ref [UserInfo]) -> UserName -> Enclave Result
deleteData enc_ref_users u = do
  ref_users <- enc_ref_users
  users     <- readRef ref_users
  case (find (\(UserInfo uname _) -> uname == u) users) of
    Nothing -> pure Nothing
    Just uinfo -> do
      writeRef ref_users (delete uinfo users)
      pure Nothing

{-@ malicious delete
deleteData :: Enclave (Ref [UserInfo]) -> UserName -> Enclave ()
deleteData _ _ = pure ()
@-}


------ API between Client and Enclave -----

data API =
  API { sendToEnclave :: Secure (UserInfo -> Enclave Result)
      , compAvg       :: Secure (Enclave Result)
      , delData       :: Secure (UserName -> Enclave Result)
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

-- tracing info
instance MonadTrace UserInfo where
  traceFmt (UserInfo uname sal) =
    uname <> "," <> show sal

instance MonadTrace Result where
  traceFmt m_int =
    case m_int of
      Nothing -> "-1"
      (Just x) -> show x

instance MonadTrace UserName where
  traceFmt = id

----- Client side logic -----

client :: API -> Client ()
client api = do
  gatewayRA ((sendToEnclave api) <@> user1)
  res <- gatewayRA (compAvg api)
  liftIO $ putStrLn $ "Computed result " <> (show res)


user1 :: UserInfo
user1 = UserInfo { username = "Alice", salary = 4200 }

user2 :: UserInfo
user2 = UserInfo { username = "Bob", salary = 3000 }

user3 :: UserInfo
user3 = UserInfo { username = "Eve", salary = 4000 }

user4 :: UserInfo
user4 = UserInfo { username = "Grace", salary = 4500 }

user5 :: UserInfo
user5 = UserInfo { username = "Mary", salary = 5300 }

user6 :: UserInfo
user6 = UserInfo { username = "John", salary = 5000 }


----- Setup and deploy -----

privateAverage :: App Done
privateAverage = do
  initialState <- liftNewRef dataTillNow
  sD <- inEnclave "sendData"   $ sendData initialState
  cA <- inEnclave "computeAvg" $ computeAvg initialState
  dD <- inEnclave "deleteData" $ deleteData initialState
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
