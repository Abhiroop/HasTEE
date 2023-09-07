{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
module Main where

import Control.Monad.IO.Class(liftIO)
import Text.Read ( readMaybe )
import Data.Binary
import GHC.Generics
import Data.Time.Clock

import System.IO
import Control.Exception
import System.Process

import App

#ifdef ENCLAVE
import Server as API
#else
import Client as API
#endif

-- * Return codes

data ReturnCode
  = Success | PasswordOutOfRange | WalletAlreadyExists | CannotSaveWallet | CannotLoadWallet
  | WrongMasterPassword | WalletFull | ItemDoesNotExist | ItemAlreadyExists | ItemTooLong
  | FailSeal | FailUnseal deriving (Generic, Show)

instance Binary ReturnCode

-- * Static values

maxItems :: Int
maxItems = 100

maxItemSize :: Int
maxItemSize = 100

wallet :: SecureFilePath
wallet = createSecurePath "wallet.seal.pf"

-- * Data types

-- | A single entry of authentication tokens
data Item = Item
    { title    :: String
    -- ^ Title of the online service, e.g Twitter, Youtune
    , username :: String
    , password :: String
    }
  deriving (Show, Read)

-- | The secure wallet
data Wallet = Wallet
    { items          :: [Item]
    -- ^ All authentication tokens
    , size           :: Int
    -- ^ The size of the wallet
    , masterPassword :: String
    -- ^ The master password that unlocks the wallet
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
  b <- API.doesSecureFileExist wallet
  if b
    then do contents <- API.readSecureFile wallet
            return $ (readMaybe contents :: Maybe Wallet)
    else return Nothing


dFx :: IO Bool
dFx = (openFile fp ReadMode >>= hClose >> return True) `catch` \ex -> let e = ex :: SomeException
                                                                    in return False
  where fp = "db/wallet.seal.pf"

rF :: String -> IO String
rF fp = (do handle <- openFile fp ReadMode
            contents <- hGetContents' handle
            hClose handle
            return contents)
  where
    hGetContents' :: Handle -> IO String
    hGetContents' h = do
      eof <- hIsEOF h
      if eof
        then
          return []
        else do
          c <- hGetChar h
          fmap (c:) $ hGetContents' h

loadWallet' :: IO (Maybe Wallet)
loadWallet' = do
  b <- dFx
  if b
    then do contents <- rF "db/wallet.seal.pf"
            return (readMaybe contents :: Maybe Wallet)
    else return Nothing

saveWallet :: Wallet -> Server ReturnCode
saveWallet w = API.writeSecureFile wallet (show w) >> return Success

-- | Create a new password wallet
createWallet :: Password -> Server ReturnCode
createWallet mp | not $ passwordPolicy mp = return PasswordOutOfRange
                | otherwise = do w <- loadWallet -- does a wallet already exist?
                                 case w of
                                     Just _  -> return WalletAlreadyExists
                                     Nothing -> saveWallet (newWallet mp)

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

sI :: Password -> String -> String -> IO (Either ReturnCode Password)
sI mp title' username' = do
  w <- loadWallet'
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

handleCommand :: Command -> Server (Either ReturnCode String)
handleCommand cmd = case cmd of
  Create s        -> fmap Left $ createWallet s
  Change s str    -> fmap Left $ changeMasterPassword s str
  Add s str cs s' -> fmap Left $ addItem s str cs s'
  Remove s str cs -> fmap Left $ removeItem s str cs
  Show s str cs   -> showItem s str cs
  Shutoff         -> return $ Left Success

hC :: Command -> IO (Either ReturnCode String)
hC cmd = case cmd of
  Create s        -> fmap Left $ return PasswordOutOfRange
  Change s str    -> fmap Left $ return PasswordOutOfRange
  Add s str cs s' -> fmap Left $ return ItemTooLong
  Remove s str cs -> fmap Left $ return CannotLoadWallet
  Show s str cs   -> sI s str cs
  Shutoff         -> return $ Left Success

-- * The application

data Api = Api { execute :: Remote (Command -> Server (Either ReturnCode String)) }

app :: App Done
app = do
    execute <- remote handleCommand
    runClient $ clientApp $ Api execute

-- | Commands exposed by the password wallet
data Command
    = Create Password
    -- ^ Create a new wallet with the given master password
    | Change Password Password
    -- ^ Change the master password
    | Add Password String String Password
    -- ^ Add an item to the password wallet
    | Remove Password String String
    -- ^ Remove an item from the password wallet
    | Show Password String String
    -- ^ Retrieve a password from the password wallet
    | Shutoff
    -- ^ No-op
  deriving Show

instance Binary Command where
  put (Create pw) = put (1 :: Word8) >> put pw
  put (Change pw npw) = put (2 :: Word8) >> put pw >> put npw
  put (Add pw title uname pass) = put (3 :: Word8) >> put pw >> put title >> put uname >> put pass
  put (Remove pw title uname) = put (4 :: Word8) >> put pw >> put title >> put uname
  put (Show pw title uname) = put (5 :: Word8) >> put pw >> put title >> put uname
  put Shutoff = put (6 :: Word8)

  get = do
    c <- get :: Get Word8
    case c of
      1 -> Create <$> get
      2 -> Change <$> get <*> get
      3 -> Add <$> get <*> get <*> get <*> get
      4 -> Remove <$> get <*> get <*> get
      5 -> Show <$> get <*> get <*> get
      6 -> return Shutoff
      _ -> error "unrecognized command"

clientApp :: Api -> Client ()
clientApp api = do
    cmd <- getCommand
    runCommand shell -- decrypt
    --codeOrPass <- onServer $ execute api <.> cmd
    codeOrPass <- hC cmd
    --let codeOrPass = Right "strongpassword" :: Either ReturnCode Password
    case codeOrPass of
      Left code -> liftIO $ putStrLn $ show code
      Right pass -> liftIO $ putStrLn pass
    where
      shell = "gpg --batch --output db/wallet.seal.pf --passphrase mypassword --decrypt db/wallet.gpg"

getCommand :: Client Command
getCommand = do
    input <- liftIO $ getContents
    case input of
        [] -> return Shutoff
        s -> case tryParse s of
            Just c -> return c
            Nothing -> return Shutoff
  where
    tryParse :: String -> Maybe Command
    tryParse input = case words input of
        ["-create", mp]                        -> Just (Create mp)
        ["-p", old, "-c", new]            -> Just (Change old new)
        [ "-p", mp, "-a", "-title", title, "-username", username, "-password", password] -> Just (Add mp title username password)
        [ "-p", mp, "-r", "-title", title, "-username", username] -> Just (Remove mp title username)
        [ "-p", mp, "-s", "-title", title, "-username", username] -> Just (Show mp title username)
        [ "-shutoff"]    -> Just Shutoff
        _                -> Nothing


timeit :: IO Done -> IO NominalDiffTime
timeit doit=do
    start  <- getCurrentTime
    doit
    end    <- getCurrentTime
    return (diffUTCTime end start)

main :: IO ()
main = do
  res <- timeit $ runApp app
  putStrLn $ "Time : " ++ show res
  return $ res `seq` ()
