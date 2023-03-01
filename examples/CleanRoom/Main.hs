{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

#ifdef ENCLAVE
import Server
#else
import Client
#endif
import App

import Crypto.Paillier as CP
import IFCIO

import Data.Char
import Text.Read hiding (get)
import Data.Binary
import Control.Monad.IO.Class
import Control.Monad
import GHC.Float
import qualified Test.QuickCheck as QC

-- * Binary instance for public keys so that they can be transmitted to the client

instance Binary PubKey where
  put (PubKey {..}) = put bits >> put nModulo >> put generator >> put nSquare >> put maxInt
  get = PubKey <$> get <*> get <*> get <*> get <*> get

-- * Data types

data CleanRoomSt = CleanRoomSt
    { pubkey  :: PubKey
    , prvkey  :: PrvKey
    , users   :: [User]
    , stdgen  :: Gen Server
    , epsilon :: Double
    }

data User = User
    { name       :: String
    , occupation :: Occupation
    , salary     :: Integer
    , gender     :: Gender
    , age        :: Integer
    , origin     :: Country
    } deriving (Show, Read)

data Occupation = Developer | Cashier | Painter | Carpenter | Musician deriving (Show, Read, Eq)

data Gender = Male | Female | Nonbinary deriving (Show, Read, Eq)

data Country = Sweden | Norway | Denmark | Iceland | Finland deriving (Show, Read, Eq)

-- * Encryption/decryotion

-- | Encrypt a string. This is done by converting each character to an integer and
-- encrypting each integer with the public key
encryptString :: String -> PubKey -> Client [Integer]
encryptString str key = mapM (CP.encrypt key) $ map (toInteger . ord) str

-- | Encrypt a user
encryptUser :: User -> PubKey -> Client [Integer]
encryptUser user key = encryptString (show user) key

-- | Decrypt a user
decryptUser :: [CipherText] -> PrvKey -> PubKey -> Server (Maybe User)
decryptUser cts prvkey' pubkey' =
    let asString = map (chr . fromInteger . CP.decrypt prvkey' pubkey') cts :: [Char]
    in return (readMaybe asString) 

-- * Initialization of enclave

-- | Initialize the enclave with a specific privacy budget
initEnclave :: Server (Ref CleanRoomSt) -> Double -> Server ()
initEnclave refst eps = do
    ref <- refst
    (pub,prv) <- genKey 1024
    g <- newGen
    writeRef ref $ CleanRoomSt pub prv [] g eps

-- * Public key management

-- | Fetch the enclaves public key
getPublicKey :: Server (Ref CleanRoomSt) -> Server PubKey
getPublicKey refst = do
    ref <- refst
    st <- readRef ref
    return $ pubkey st

-- * Provisioning of users to the enclave

-- | Provision an encrypted user to the enclave
provisionUserEnclave :: Server (Ref CleanRoomSt) -> [CipherText] -> Server Bool
provisionUserEnclave refst ctuser = do
    ref <- refst
    st <- readRef ref
    user <- decryptUser ctuser (prvkey st) (pubkey st)
    case user of
        Nothing -> return False
        Just u -> writeRef ref (st { users = u :users st}) >> return True

-- * Random number generation

-- | Given a range, generate a random number within that range, uniformly
getRandom :: Server (Ref CleanRoomSt) -> (Int, Int) -> Server Int
getRandom refst range = do
    ref <- refst
    st <- readRef ref
    (n,g) <- uniFromGen range (stdgen st)
    writeRef ref $ st { stdgen = g }
    return n

-- | Flip a weighted coin. Returns True if the coin landed on the weighted side
flipCoin :: Server (Ref CleanRoomSt) -> Double -> Server Bool
flipCoin refst prob = do
    n <- getRandom refst (0,100)
    let l = double2Int $ 100 * prob
    return (n <= l)

-- The algorithm

countingQuery :: Server (Ref CleanRoomSt) -> (User -> Bool) -> Server Int
countingQuery refst q = do
    st <- readRef =<< refst
    return $ length $ filter id $ map q (users st)

laplaceDistribution :: Server (Ref CleanRoomSt) -> Double -> Server Double
laplaceDistribution refst b = do
    z <- int2Double <$> getRandom refst (0,1)
    u <- ((/) 1000 . int2Double) <$> getRandom refst (1,1000)
    return $ (2 * z - 1) * (b * log u)

laplaceMechanism :: Server (Ref CleanRoomSt) -> (User -> Bool) -> Server Double
laplaceMechanism refst q = do
    st <- readRef =<< refst
    true <- int2Double <$> countingQuery refst q
    noise <- laplaceDistribution refst (1 / (epsilon st))
    return $ true + noise

-- | Randomized response algorithm
randomizedResponse :: Server (Ref CleanRoomSt) -> (User -> Bool) -> Server Double
randomizedResponse refst q = do
    ref <- refst
    st <- readRef ref

    -- get the responses
    num <- foldM (\acc u -> do b <- flipCoin refst (prob (epsilon st))
                               if b
                                 then if q u then return (acc + 1) else return acc
                                 else if not (q u) then return (acc + 1) else return acc) 0 (users st)
    return $ num / (fromInteger $ toInteger (length (users st)))
  where
    prob :: Double -> Double
    prob eps = (exp eps) / (1 + exp eps)

-- * Queries

fromCountry :: Country -> User -> Bool
fromCountry c u = c == origin u

ofGender :: Gender -> User -> Bool
ofGender g u = g == gender u

ofAge :: Integer -> User -> Bool
ofAge a u = a == age u

withinAge :: Integer -> Integer -> User -> Bool
withinAge l h u = l <= age u && age u <= h

ofOccupation :: Occupation -> User -> Bool
ofOccupation o u = o == occupation u

withSalary :: Integer -> User -> Bool
withSalary s u = s == salary u

salaryWithin :: Integer -> Integer -> User -> Bool
salaryWithin l h u = l <= salary u && salary u <= h

-- * Application code

app :: App Done
app = do
    ref    <- liftNewRef undefined
    initSt <- remote $ initEnclave ref
    prov'  <- remote $ provisionUserEnclave ref
    pkey   <- remote $ getPublicKey ref
    rr     <- remote $ laplaceMechanism ref $ fromCountry Sweden--salaryWithin 10000 50000
    dataset <- liftIO $ sequence $ replicate 100 (QC.generate QC.arbitrary)
    runClient $ do
        onServer $ initSt <.> 0.1 -- initialize enclave with privacy budget
        key <- onServer pkey    -- fetch public key
        mapM_ (\u -> do ct <- encryptUser u key
                        onServer $ prov' <.> ct) dataset -- provision users
        results <- sequence $ replicate 10 $ onServer rr
        liftIO $ putStrLn $ concat ["randomized response: ", show results]

main :: IO ()
main = do
    res <- runApp app
    return $ res `seq` ()

-- * QuickCheck stuff

instance QC.Arbitrary Occupation where
    arbitrary = QC.elements [Developer, Cashier, Painter, Carpenter, Musician]

instance QC.Arbitrary Country where
    arbitrary = QC.elements [Sweden, Norway, Denmark, Iceland, Finland]

instance QC.Arbitrary Gender where
    arbitrary = QC.elements [Male, Female, Nonbinary]

instance QC.Arbitrary User where
    arbitrary = do
        occupation <- QC.arbitrary
        salary     <- QC.chooseInteger (0, 300000)
        name       <- QC.arbitrary
        gender     <- QC.arbitrary
        age        <- QC.chooseInteger (18,65)
        origin     <- QC.arbitrary
        return $ User name occupation salary gender age origin
