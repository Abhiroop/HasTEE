{-# LANGUAGE CPP #-}

{-# LANGUAGE DataKinds #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (join)
import Data.Binary
import Data.List (groupBy, sortBy)

import App
import DCLabel
#ifdef ENCLAVE
import Enclave
#else
import Client
#endif


{-@ COVID Variant and Age Correlation

    There are 2 organisations;
    org1 is the data provider and org2 carries out the analytics.
    Because org1 is providing confidential data it uses the new
    ```
    clientLabel :: (Label l , Binary l , Binary a)
                => l -> a -> Labeled l a
    ```
    function to label its data from the client side.
    The database at the server side only holds labeled data.
    Because this is confidental data; no one is allowed to
    declassify this. There is only one analytics query audited and approved \
    by org1, which is `query1` and only this query is capable of
    declassification. The declassification happens with the `runQuery`
    special function that captures org1's declassification privilege
    within a closure.

    org2 wishes to run the analytics `query1` and when the partitioning
    happens we can ensure that the `privInit` part of the code is
    compiled out. Ideally proper partitioning should eliminate client1's
    `dataProvider` DCLabel that captures the underlying representation
    of the privilege. We take the dynamic approach a la HasChor. But this is
    handled easily with a symmetric encryption key that org1 holds.

    Also, one shouldn't use strings in DCLabels as well as Privileges.
    2048 bit hashes should be used. A DC Label should be of the form
    type DCLabel = Hash
    where hash :: Hash
          hash = hash (hash secrecy, hash integrity)

@-}

data CovidVariant = BA275
                  | XBB15
                  | DV71
                  | B1525
                  deriving (Show, Eq, Ord)

instance Binary CovidVariant where
  put variant = case variant of
    BA275 -> putWord8 0
    XBB15 -> putWord8 1
    DV71  -> putWord8 2
    B1525 -> putWord8 3

  get = do
    tag <- getWord8
    case tag of
      0 -> return BA275
      1 -> return XBB15
      2 -> return DV71
      3 -> return B1525
      _ -> fail "Invalid tag for CovidVariant"


type Age = Word8
data Row = Row { covidVar   :: CovidVariant
               , patientAge :: Age
               } deriving (Show, Eq)

instance Binary Row where
  put (Row var age) = put var >> put age

  get = do
    var <- get
    age <- get
    return (Row var age)


type DB     = [DCLabeled Row]
type Result = [(CovidVariant, Age)] -- gives rounded up mean age

database :: DB
database = []

sendData :: EnclaveDC (DCRef DB) -> DCLabeled Row -> EnclaveDC ()
sendData enc_ref_db labeledRow = do
  ref_db     <- enc_ref_db
  datas      <- readRef ref_db
  writeRef ref_db (labeledRow : datas)


runQuery :: EnclaveDC (DCRef DB) -> Priv CNF -> EnclaveDC Result
runQuery enc_ref_db priv = do
  labeled_rows <- join $ readRef <$> enc_ref_db
  rows <- mapM (unlabelP priv) labeled_rows
  return (query1 rows)


query1 :: [Row] -> Result
query1 = map collate
       . groupBy (\(Row cov1 _) (Row cov2 _) -> cov1 == cov2)
       . sortBy  (\(Row cov1 _) (Row cov2 _) -> compare cov1 cov2)
  where
    collate :: [Row] -> (CovidVariant, Age)
    collate rows = (covidVar (head rows), meanAge)
      where meanAge = sum (map patientAge rows) `div` (fromIntegral $ length rows)


data API =
  API { datasend :: Secure (DCLabeled Row -> EnclaveDC ())
      , runQ     :: Secure (EnclaveDC Result)
      }


client1 :: API -> Client "org1" ()
client1 api = do
  d1 <- clientLabel dataProvider row1
  d2 <- clientLabel dataProvider row2
  d3 <- clientLabel dataProvider row3
  d4 <- clientLabel dataProvider row4
  d5 <- clientLabel dataProvider row5
  d6 <- clientLabel dataProvider row6
  gatewayRA ((datasend api) <@> d1)
  gatewayRA ((datasend api) <@> d2)
  gatewayRA ((datasend api) <@> d3)
  gatewayRA ((datasend api) <@> d4)
  gatewayRA ((datasend api) <@> d5)
  gatewayRA ((datasend api) <@> d6)
  where
    dataProvider :: DCLabel
    dataProvider = "org1" %% "org1"



client2 :: API -> Client "org2" ()
client2 api = do
  res <- gatewayRA (runQ api)
  liftIO $ putStrLn "Analytics result"
  liftIO $ putStrLn (show res)

-- Assume rows are fetched from the database
row1, row2, row3 :: Row
row4, row5, row6 :: Row

row1 = Row XBB15 77
row2 = Row BA275 57
row3 = Row DV71  39
row4 = Row XBB15 82
row5 = Row BA275 53
row6 = Row B1525 37


type OrgName = String

-- data provider
org1 :: OrgName
org1 = "org1"

-- analytics provider
org2 :: OrgName
org2 = "org2"


ifctest :: App Done
ifctest = do
  db <- liftNewRef dcPublic database -- db kept permissive because all
                                     -- data is labeled
  sfunc    <- inEnclave initState $ sendData db
  org1Priv <- liftIO $ privInit (toCNF org1)
  qfunc    <- inEnclave initState $ runQuery db org1Priv
  let api = API sfunc qfunc
  runClient (client1 api)
  runClient (client2 api)
  where
    initState = dcDefaultState cTrue

main :: IO ()
main = do
  res <- runAppRA "org1" ifctest
  return $ res `seq` ()
