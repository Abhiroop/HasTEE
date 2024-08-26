{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Monad.IO.Class
import Data.Binary
import GHC.Generics (Generic)

import App
import DCLabel
#ifdef ENCLAVE
import Enclave
#else
import Client
#endif

import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.Map as M

data ClaimCause =
  VehicleAccident | VehicleVandalism |
  HomeFire | HomeTheft | PropertyDamage
  deriving (Ord, Show, Eq, Generic)


data FraudDetection = Regression | DecisionTree | SVM
  deriving (Show, Eq, Generic)

type TPS = Int -- transaction per second

data Location = C1 | C2 | C3 | C4 deriving (Ord, Show, Eq, Generic)

data FraudHistory =
  FraudHistory { claimCause           :: ClaimCause
               , detectionMethod      :: FraudDetection
               , transactionFrequency :: TPS
               , location             :: Location
               }
  deriving (Show, Eq, Generic)

instance Binary ClaimCause
instance Binary FraudDetection
instance Binary Location
instance Binary FraudHistory

type UserId      = Int
type ClaimId     = Int
type ClaimAmount = Int

data InsuranceData = InsuranceData { userId       :: UserId
                                   , claimId      :: ClaimId
                                   , claimAmount  :: ClaimAmount
                                   , fraudHistory :: FraudHistory
                                   }
                     deriving (Show, Eq, Generic)

instance Binary InsuranceData

instance Arbitrary InsuranceData where
  arbitrary :: Gen InsuranceData
  arbitrary = do
    uid <- choose (1, 10000)
    cid <- choose (1, 100000)
    let reasonableClaim = choose (10000, claimAmountThreshold)
    let fraudClaim =
          choose (claimAmountThreshold + 1, 2 * claimAmountThreshold)
    cA <- frequency [(3, reasonableClaim), (1, fraudClaim)]
    fraudHist <- arbitrary
    return (InsuranceData uid cid cA fraudHist)

instance Arbitrary FraudHistory where
  arbitrary :: Gen FraudHistory
  arbitrary = do
    cCause <- arbitrary
    dMeth  <- arbitrary
    tFreq  <- arbitrary `suchThat` (\tps -> tps > 100 && tps < 1000)
    loc    <- arbitrary
    return (FraudHistory cCause dMeth tFreq loc)

instance Arbitrary ClaimCause where
  arbitrary :: Gen ClaimCause
  arbitrary = oneof [ return VehicleAccident
                    , return VehicleVandalism
                    , return HomeFire
                    , return HomeTheft
                    , return PropertyDamage]

instance Arbitrary Location where
  arbitrary :: Gen Location
  arbitrary = oneof [ return C1, return C2
                    , return C3, return C4]

instance Arbitrary FraudDetection where
  arbitrary :: Gen FraudDetection
  arbitrary = frequency [ (3, return SVM)
                        , (2, return DecisionTree)
                        , (1, return Regression)]


{-@
    This is a database of fraudulent claims collected
    from various insurance organisations I1, I2, I3.
@-}
type DB = [InsuranceData]

fraudDB :: DB
fraudDB = []

collect :: EnclaveDC (DCRef DB) -> InsuranceData -> EnclaveDC ()
collect enc_ref_db idata = do
  ref_db <- enc_ref_db
  datas  <- readRef ref_db
  writeRef ref_db (idata : datas)

batchCollect :: EnclaveDC (DCRef DB) -> [InsuranceData] -> EnclaveDC ()
batchCollect enc_ref_db idatas = do
  ref_db <- enc_ref_db
  datas  <- readRef ref_db
  writeRef ref_db (idatas ++ datas)

data PotentialFraudData =
  PotentialFraudData { fraud_uid        :: UserId
                     , fraud_claimAmt   :: ClaimAmount
                     , fraud_claimCause :: ClaimCause
                     , fraud_tps        :: TPS
                     , fraud_loc        :: Location
                     }
  deriving (Show, Eq, Generic)

instance Binary PotentialFraudData

instance Arbitrary PotentialFraudData where
  arbitrary :: Gen PotentialFraudData
  arbitrary = do
    f_uid        <- choose (1, 10000)
    f_claimAmt   <- choose (claimAmountThreshold + 1
                               , 2 * claimAmountThreshold)
    f_claimCause <- arbitrary
    f_tps <- arbitrary `suchThat` (\tps -> tps > 100 && tps < 1000)
    f_loc <- arbitrary
    return PotentialFraudData { fraud_uid        = f_uid
                              , fraud_claimAmt   = f_claimAmt
                              , fraud_claimCause = f_claimCause
                              , fraud_tps = f_tps
                              , fraud_loc = f_loc
                              }

{-@ Mock Fraud Detection Algorithm
    The algorithm assigns a score based on
    1. history of previous fraud
    2. claim amount
    3. claim cause and detection method accuracy
    4. transaction frequency
    5. location
    If the score exceeds a particular threshold we
    say positive or negative for fraud
@-}
fraudDetect :: EnclaveDC (DCRef DB)
            -> PotentialFraudData
            -> EnclaveDC Bool
fraudDetect enc_ref_db fraud_data = do
  ref_db <- enc_ref_db
  datas  <- readRef ref_db
  let score1 = fraudHistScore  datas fraud_data
  let score2 = claimAmountCheck      fraud_data
  let score3 = claimCauseCheck datas fraud_data
  let score4 = tranFreqCheck   datas fraud_data
  let score5 = locCheck        datas fraud_data
  let meanScore =
        fromIntegral (score1 + score2 +
                      score3 + score4 + score5) / 5.0 :: Double
  -- 0.8 <= mean score <= 2.6
  if meanScore >= 2.0
  then pure True
  else pure False

fraudHistScore :: [InsuranceData] -> PotentialFraudData -> Int
fraudHistScore datas fraud_data =
  case getUser datas (fraud_uid fraud_data) of
    Nothing -> 0
    Just _  -> 1

claimAmountThreshold :: Int
claimAmountThreshold = 100000

claimAmountCheck :: PotentialFraudData -> Int
claimAmountCheck fraud_data
  | (fraud_claimAmt fraud_data) > claimAmountThreshold = 2
  | otherwise = 0

claimCauseCheck :: [InsuranceData] -> PotentialFraudData -> Int
claimCauseCheck datas fraud_data =
  let cCauseMap =
        foldr (\(InsuranceData _ _ _ fH) m ->
           M.insertWith (\_ (oldVal, fd) -> (oldVal + 1, fd))
           (claimCause fH) (1, detectionMethod fH) m)
        (M.empty :: M.Map ClaimCause (Int, FraudDetection)) datas
   in case M.lookup (fraud_claimCause fraud_data) cCauseMap of
        Nothing -> 0
        Just (v, detMeth) -> if (v > 5)
                             then 2 + detectionRanking detMeth
                             else 1 + detectionRanking detMeth

detectionRanking :: FraudDetection -> Int
detectionRanking SVM          = 3
detectionRanking DecisionTree = 2
detectionRanking Regression   = 1

tranFreqCheck :: [InsuranceData] -> PotentialFraudData -> Int
tranFreqCheck datas fraud_data =
  case getUser datas (fraud_uid fraud_data) of
    Nothing -> 1
    Just (InsuranceData _ _ _ fH) ->
      if (fraud_tps fraud_data) >= (transactionFrequency fH) then 3 else 2

getUser :: [InsuranceData] -> UserId -> Maybe InsuranceData
getUser datas u_id
  | length res == 0 = Nothing
  | otherwise       = Just (head res)
  where
    res = filter (\idata -> (userId idata) == u_id) datas

locCheck :: [InsuranceData] -> PotentialFraudData -> Int
locCheck datas fraud_data =
  let locFreqMap =
        foldr (\(InsuranceData _ _ _ fH) m ->
           M.insertWith (\_ oldVal -> oldVal + 1) (location fH) 1 m)
        (M.empty :: M.Map Location Int) datas
   in case M.lookup (fraud_loc fraud_data) locFreqMap of
        Nothing -> 0
        Just v  -> if (v > 20) then 2 else 1


-- batchCollect :: EnclaveDC (DCRef DB) -> [InsuranceData]    -> EnclaveDC ()
-- fraudDetect  :: EnclaveDC (DCRef DB) -> PotentialFraudData -> EnclaveDC Bool

data API =
  API { datasend :: Secure ([InsuranceData] -> EnclaveDC ())
      , runQ     :: Secure (PotentialFraudData -> EnclaveDC Bool)
      }


-- this is where the random data generation and testing should happen
prop_NI :: [InsuranceData]
        -> PotentialFraudData
        -> PotentialFraudData
        -> Property
prop_NI ids pfd1 pfd2 = monadicIO $ do
  (res1, res2) <- run $ runAppRA "testClient" $ do
    db    <- liftNewRef dcPublic fraudDB
    let initState = dcDefaultState cTrue
    sfunc <- inEnclave initState $ batchCollect db
    qfunc <- inEnclave initState $ fraudDetect  db
    let api = API sfunc qfunc
    {- Sends private data -}
    _  <- runClient (testClientCollect api ids)
    {- Query first time (public query) -}
    r1 <- runClient (testClientQuery api pfd1)
    {- Query second time (public query) -}
    r2 <- runClient (testClientQuery api pfd2)
    return (r1, r2)
  assert (res1 == res2)
{-

[]
PotentialFraudData {fraud_uid = 2813, fraud_claimAmt = 171528, fraud_claimCause = HomeFire, fraud_tps = 101, fraud_loc = C4}
PotentialFraudData {fraud_uid = 730, fraud_claimAmt = 161041, fraud_claimCause = HomeTheft, fraud_tps = 102, fraud_loc = C4}



[]
PotentialFraudData {fraud_uid = 1885, fraud_claimAmt = 197068, fraud_claimCause = HomeTheft, fraud_tps = 105, fraud_loc = C2}
PotentialFraudData {fraud_uid = 42,   fraud_claimAmt = 104722, fraud_claimCause = HomeTheft, fraud_tps = 105, fraud_loc = C2}

-}



testClientCollect :: API -> [InsuranceData] -> Client "testClient" ()
testClientCollect api ids = gatewayRA ((datasend api) <@> ids)

testClientQuery :: API -> PotentialFraudData -> Client "testClient" Bool
testClientQuery api fraud_data = gatewayRA ((runQ api) <@> fraud_data)




testClient :: API -> Client "testClient" ()
testClient api = do
  gatewayRA ((datasend api) <@> [(InsuranceData 1 1 100 (FraudHistory VehicleAccident SVM 100 C1))])
  fraudTest <- gatewayRA ((runQ api) <@> (PotentialFraudData 1 100 PropertyDamage 100 C1))
  if fraudTest
  then liftIO $ putStrLn "Fraud Detected"
  else liftIO $ putStrLn "Fraud not found!"

main :: IO ()
main = quickCheck prop_NI



-------------------------NOT IMPORTANT--------------------------------

type Public = Int
type Private = Bool

testProg :: Public -> Private -> Public
testProg pub priv =
  if priv
  then pub + 2
  else pub - 2

propNI2 :: Public -> Private -> Private -> Bool
propNI2 pub priv1 priv2 =
  testProg pub priv1 == testProg pub priv2

-- main :: IO ()
-- main = quickCheck propNI2
