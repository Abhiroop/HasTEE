{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
module Main where

import Control.Monad.IO.Class(liftIO)
import System.IO
import Text.Read
import Data.Binary
import GHC.Generics


import App

#ifdef ENCLAVE
import Server as API
#else
import Client as API
#endif

-- * Return codes

data ReturnCode
  = Success
  | PasswordOutOfRange
  | WalletAlreadyExists
  | CannotSaveWallet
  | CannotLoadWallet
  | WrongMasterPassword
  | WalletFull
  | ItemDoesNotExist
  | ItemAlreadyExists
  | ItemTooLong
  | FailSeal
  | FailUnseal
  deriving Generic

instance Binary ReturnCode

-- * Static values

maxItems :: Int
maxItems = 100

maxItemSize :: Int
maxItemSize = 100

wallet :: String
wallet = "/home/abhir00p/gramine/CI-Examples/hask-wallet/db/wallet.seal"

-- * Data types

data Item = Item
    { title    :: String
    , username :: String
    , password :: String
    }
  deriving (Show, Read)

data Wallet = Wallet
    { items          :: [Item]
    , size           :: Int
    , masterPassword :: String
    }
  deriving (Show, Read)

newWallet :: Password -> Wallet
newWallet mp = Wallet [] 0 mp

type Password = String

-- * Enclave code

passwordPolicy :: Password -> Bool
passwordPolicy pass = length pass >= 8 && length pass + 1 <= maxItemSize

loadWallet :: Server (Maybe Wallet)
loadWallet = do
  b <- API.doesFileExist wallet
  if b
    then do contents <- API.readFile wallet
            return $ (readMaybe contents :: Maybe Wallet)
    else return Nothing

saveWallet :: Wallet -> Server ReturnCode
saveWallet w = API.writeFile wallet (show w) >> return Success

createWallet :: Password -> Server ReturnCode
createWallet mp
  | not $ passwordPolicy mp = return PasswordOutOfRange
  | otherwise = saveWallet (newWallet mp)-- do
    -- w <- loadWallet
    -- case w of
    --     Just _  -> return WalletAlreadyExists
    --     Nothing -> saveWallet (newWallet mp)

changeMasterPassword :: Password -> Password -> Server ReturnCode
changeMasterPassword old new
  | not $ passwordPolicy new = return PasswordOutOfRange
  | otherwise = do
    w <- loadWallet
    case w of
      Nothing -> return CannotLoadWallet
      Just w -> if masterPassword w == old
                    then saveWallet (w { masterPassword = new })
                    else return WrongMasterPassword

addItem :: Password -> String -> String -> Password -> Server ReturnCode
addItem mp item username pass
  | length item + 1     > maxItemSize ||
    length username + 1 > maxItemSize ||
    length pass + 1 > maxItemSize = return ItemTooLong
  | otherwise = do
    w <- loadWallet
    case w of
        Nothing -> return CannotLoadWallet
        Just w | not (masterPassword w == mp) -> return WrongMasterPassword
        Just w | itemExists item username (items w) -> return ItemAlreadyExists
        Just w -> saveWallet (w { items = (Item item username pass) : items w, size = size w + 1})

removeItem :: Password -> String -> String -> Server ReturnCode
removeItem mp title' username' = do
  w <- loadWallet
  case w of
      Nothing -> return CannotLoadWallet
      Just w | not (itemExists title' username' (items w)) -> return ItemDoesNotExist
      Just w | not (masterPassword w == mp) -> return WrongMasterPassword
      Just w -> let newitems = removeItem' title' username' (items w)
                in saveWallet (w { items = newitems, size = size w - 1})
  where    
    removeItem' :: String -> String -> [Item] -> [Item]
    removeItem' title' uname items =
      filter (\t -> title t /= title' && username t /= uname) items

showItem :: Password -> String -> String -> Server (Either ReturnCode Password)
showItem mp title' username' = do
  w <- loadWallet
  case w of
    Nothing -> return $ Left CannotLoadWallet
    Just w | not (itemExists title' username' (items w)) -> return $ Left ItemDoesNotExist
    Just w | not (masterPassword w == mp) -> return $ Left WrongMasterPassword
    Just w -> return $ Right (findPass title' username' (items w))
  where
    findPass :: String -> String -> [Item] -> Password
    findPass title' username' items =
      let singleton = filter (\t -> title t == title' && username t == username') items
      in password (head singleton)

itemExists :: String -> String -> [Item] -> Bool
itemExists title' uname items =
  any (\t -> title t == title' && username t == uname) items

-- * The application

data Api = Api
    { create     :: Remote (Password -> Server ReturnCode)
    , changePass :: Remote (Password -> Password -> Server ReturnCode)
    , add        :: Remote (Password -> String -> String -> Password -> Server ReturnCode)
    , remove     :: Remote (Password -> String -> String -> Server ReturnCode)
    , showP      :: Remote (Password -> String -> String -> Server (Either ReturnCode String))
    }

app :: App Done
app = do
    -- The api
    create     <- ntimes 1 $ createWallet
    changePass <- ntimes 1 $ changeMasterPassword
    add        <- ntimes 1 $ addItem
    remove     <- ntimes 1 $ removeItem
    showP      <- ntimes 1 $ showItem

    -- Client code
    runClient $ clientApp $ Api create changePass add remove showP

data Command
    = Create Password
    | Change Password Password
    | Add Password String String Password
    | Remove Password String String
    | Show Password String String
    | Shutoff
  deriving Show

clientApp :: Api -> Client ()
clientApp api = do
    cmd <- getCommand
    case cmd of
        Shutoff -> return ()
        Create mp -> do
          r <- onServer $ create api <.> mp
          printCode r
        Change old new -> do
          r <- onServer $ changePass api <.> old <.> new
          printCode r
        Add mp title username password -> do
          r <- onServer $ add api <.> mp <.> title <.> username <.> password
          printCode r
        Remove mp title username -> do
          r <- onServer $ remove api <.> mp <.> title <.> username
          printCode r
        Show mp title username -> do
          p <- onServer $ showP api <.> mp <.> title <.> username
          case p of
            Left code -> printCode code
            Right pass -> liftIO $ putStrLn pass

getCommand :: Client Command
getCommand = do
    input <- liftIO $ getContents
    case input of
        [] -> usage "<empty input>" >> return Shutoff
        s -> case tryParse s of
            Just c -> return c
            Nothing -> usage s >> return Shutoff
  where
    tryParse :: String -> Maybe Command
    tryParse input = case words input of
        ["-create", mp]                        -> Just (Create mp)
        ["-p", old, "-c", new]            -> Just (Change old new)
        [ "-p", mp, "-a", "-title", title, "-username", username, "-password", password] -> Just (Add mp title username password)
        [ "-p", mp, "-r", "-title", title, "-username", username] -> Just (Remove mp title username)
        [ "-p", mp, "-s", "-title", title, "-username", username] -> Just (Show mp title username)
        otherwise                -> Nothing

    usage :: String -> Client ()
    usage str = liftIO $ putStrLn $ "<some helpful user manual>" ++ " : " ++ str

printCode :: ReturnCode -> Client ()
printCode Success             = liftIO $ putStrLn $ "# ok"
printCode PasswordOutOfRange  = liftIO $ putStrLn $ "= password out of range"
printCode WalletAlreadyExists = liftIO $ putStrLn $ "= wallet already exists"
printCode CannotSaveWallet    = liftIO $ putStrLn $ "= cannot save wallet"
printCode CannotLoadWallet    = liftIO $ putStrLn $ "= cannot load wallet"
printCode WrongMasterPassword = liftIO $ putStrLn $ "= wrong master password"
printCode WalletFull          = liftIO $ putStrLn $ "= wallet is full"
printCode ItemDoesNotExist    = liftIO $ putStrLn $ "= item does not exist"
printCode ItemAlreadyExists   = liftIO $ putStrLn $ "= item already exists"
printCode ItemTooLong         = liftIO $ putStrLn $ "= item is too long"
printCode FailSeal            = liftIO $ putStrLn $ "= failure while sealing wallet"
printCode FailUnseal          = liftIO $ putStrLn $ "= failure while unsealing wallet"

main :: IO ()
main = do
  res <- runApp app
  return $ res `seq` ()
