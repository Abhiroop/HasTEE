module SecureWallet where

import App

#ifdef ENCLAVE
import Server
#else
import Client
#endif

-- * Return codes

retSuccess, passwordOutOfRange, walletAlreadyExists :: Int
retSuccess          = 0
passwordOutOfRange  = 1
walletAlreadyExists = 2

cannotSaveWallet, cannotLoadWallet, wrongMasterPassword :: Int
cannotSaveWallet    = 3
cannotLoadWallet    = 4
wrongMasterPassword = 5

walletFull, itemDoesNotExist, itemTooLong, failSeal, failUnseal :: Int
walletFull       = 6
itemDoesNotExist = 7
itemTooLong      = 8
failSeal         = 9
failUnseal       = 10

-- * Static values

maxItems :: Int
maxItems = 100

maxItemSize :: Int
maxItemSize = 100

wallet :: String
wallet = "wallet.seal"

-- * Data types

data Item = Item
    { title    :: String
    , username :: String
    , password :: String
    }

data Wallet = Wallet
    { items          :: [Item]
    , size           :: Int
    , masterPassword :: String
    }

newWallet :: Password -> Wallet
newWallet mp = Wallet [] 0 mp

type Password = String

-- * Enclave code

passwordPolicy :: Password -> Bool
passwordPolicy pass = length pass >= 8 && length pass + 1 <= maxItemSize

createWallet :: Server (Ref (Maybe Wallet)) -> Password -> Server Int
createWallet srw mp
  | not $ passwordPolicy mp = return passwordOutOfRange
  | otherwise = do
    w <- readRef =<< srw
    case w of
        Just _  -> return walletAlreadyExists
        Nothing -> srw >>= \r -> writeRef r (Just $ newWallet mp) >> return retSuccess

changeMasterPassword :: Server (Ref (Maybe Wallet)) -> Password -> Password -> Server Int
changeMasterPassword srw old new
  | not $ passwordPolicy new = return passwordOutOfRange
  | otherwise = do
    w <- readRef =<< srw
    if masterPassword w == old
        then do r <- srw 
                writeRef r (Just $ w { masterPassword = new })
                return retSuccess
        else return wrongMasterPassword

addItem :: Server (Ref (Maybe Wallet)) -> Password -> String -> String -> Password -> Server Int
addItem srw mp item username pass
  | length item + 1     > maxItemSize ||
    length username + 1 > maxItemSize ||
    length password + 1 > maxItemSize = return itemTooLong
  | otherwise = do
    w <- readRef =<< srw
    case w of
        Nothing -> return cannotLoadWallet
        Just w ->
            if masterPassword w == mp
                then do r <- srw
                        writeRef r (Just $ w { items = (Item item username pass) : items w, size = size w + 1})
                        return retSuccess
                else return wrongMasterPassword

removeItem :: Server (Ref (Maybe Wallet)) -> Password -> String -> Server Int
removeItem srw mp item = do
    w <- readRef =<< srw
    case w of
        Nothing -> return cannotLoadWallet
        Just w | not (item `elem` (map title (items w))) -> return itemDoesNotExist
        Just w | not (masterPassword w == mp) -> return wrongMasterPassword
        Just w -> do r <- srw
                     writeRef r $ Just $ w { items = filter (not . (==) item . title) items, size = size w - 1}
                     return retSuccess

-- * The application

data Api = Api
    { create     :: Server Int
    , changePass :: Password -> Password -> Server Int
    , add        :: String -> String -> Password -> Server Int
    , remove     :: String -> Server Int}

app :: App Done
app = do
    -- The data
    walletref <- liftNewRef Nothing

    -- The api
    create     <- remote $ createWallet walletref
    changePass <- remote $ changeMasterPassword walletref
    add        <- remote $ addItem walletref
    remove     <- remote $ removeItem walletref

    -- Client code
    runClient $ clientApp $ Api create changePass add remove

data Command
    = Create
    | Change Password Password
    | Add String String Password
    | Remove String
    | Shutoff

clientApp :: Api -> Client ()
clientApp api = do
    cmd <- getCommand
    case cmd of
        Shutoff -> liftIO (putStrLn "turning off...") >> return ()
        _       -> r <- onServer $ case cmd of
                          Create                      -> create api
                          Change old new              -> change api old new
                          Add title username password -> add api title username password
                          Remove title                -> remove api title
                   printCode r

getCommand :: Client Command
getCommand = do
    liftIO $ putStr ">"
    input <- liftIO $ getLine
    case input of
        [] -> getCommand
        s -> case tryParse s of
            Just c -> return C
            Nothing -> getCommand
  where
    tryParse :: String -> Just Command
    tryParse input = case words input of
        ["create"]                         -> Just Create
        ["change", old, new]               -> Just $ Change old new
        ["add", title, username, password] -> Just $ Add title username password
        ["remote", title]                  -> Just $ Remove title
        ["shutoff"]                        -> Just Shutoff
        otherwise                          -> Nothing

printCode :: Int -> Client ()
printCode c
  | c == retSuccess          = liftIO $ putStrLn $ "# ok"
  | c == passwordOutOfRange  = liftIO $ putStrLn $ "= password out of range"
  | c == walletAlreadyExists = liftIO $ putStrLn $ "= wallet already exists"
  | c == cannotSaveWallet    = liftIO $ putStrLn $ "= cannot save wallet"
  | c == cannotLoadWallet    = liftIO $ putStrLn $ "= cannot load wallet"
  | c == wrongMasterPassword = liftIO $ putStrLn $ "= wrong master password"
  | c == walletFull          = liftIO $ putStrLn $ "= wallet is full"
  | c == itemDoesNotExists   = liftIO $ putStrLn $ "= item does not exist"
  | c == itemTooLong         = liftIO $ putStrLn $ "= item is too long"
  | c == failSeal            = liftIO $ putStrLn $ "= failure while sealing wallet"
  | c == failUnseal          = liftIO $ putStrLn $ "= failure while unsealing wallet"

main :: IO ()
main = do
  res <- runApp app
  return $ res `seq` ()
