{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (join)
import Data.Binary
import Data.List (groupBy, sortBy)


import Crypto.PubKey.RSA.PKCS15
import Crypto.PubKey.RSA.Types (PublicKey)
import Data.Foldable (traverse_)
import qualified Data.ByteString as B

import App
import DCLabel
#ifdef ENCLAVE
import Enclave
#else
import Client
#endif


{-@ COVID Variant and Age Correlation

    There are 3 organisations;
    org1 and org2 are the data provider and org3 carries out the analytics.
    Analytics combines ONLY the covid strains common between org1 and org2
    and gives the mean infected age associated with that strain.
    Because org1 and org2 are providing confidential data they uses the new
    ```
    clientLabel :: (Label l , Binary l , Binary a)
                => l -> a -> Labeled l a
    ```
    function to label the data from the client side.
    The database at the server side only holds labeled data.
    Because this is confidental data; no one is allowed to
    declassify this. There is only one analytics query audited and approved
    by org1 and org2, which is `query1` and only this query is capable of
    declassification. The declassification happens with the `runQuery`
    special function that captures org1 and org2's declassification privilege
    within a closure.

    org3 wishes to run the analytics `query1` and when the partitioning
    happens we can ensure that the `privInit` part of the code is
    compiled out. Ideally proper partitioning should eliminate client1's
    `dataProvider` DCLabel that captures the underlying representation
    of the privilege. We take the dynamic approach a la HasChor. But this is
    handled easily with a symmetric encryption key that org1 holds.

    Also, one shouldn't use strings in DCLabels as well as Privileges.
    512 bit hashes should be used. A DC Label should be of the form
    type DCLabel = Hash
    where hash :: Hash
          hash = hash (hash secrecy, hash integrity)

    Problem
    -------
    Anyone who calls `runQ :: Secure (EnclaveDC Result)` gets the analytics
    result, that can partially (or sometimes entirely) reveal the
    confidential data.
    Solution
    --------
    Public-Private cryptography
    runQ :: Secure (EnclaveDC Hash)
    runQ should internally do `encrpyt org3_pubK result` and now
    except org3, no one can decrypt this data as they dont have the private
    key. org3 can do `decrypt org3_privK result`



@-}

data CovidVariant = BA275
                  | XBB15
                  | DV71
                  | B1525
                  | P681
                  | B318
                  deriving (Show, Eq, Ord)

instance Binary CovidVariant where
  put variant = case variant of
    BA275 -> putWord8 0
    XBB15 -> putWord8 1
    DV71  -> putWord8 2
    B1525 -> putWord8 3
    P681  -> putWord8 4
    B318  -> putWord8 5

  get = do
    tag <- getWord8
    case tag of
      0 -> return BA275
      1 -> return XBB15
      2 -> return DV71
      3 -> return B1525
      4 -> return P681
      5 -> return B318
      _ -> fail "Invalid tag for CovidVariant"

type Party = String
type Age = Word8
type MeanAge = Double
data Row = Row { owner      :: Party
               , covidVar   :: CovidVariant
               , patientAge :: Age
               } deriving (Show, Eq)

instance Binary Row where
  put (Row o var age) = put o >> put var >> put age

  get = do
    o   <- get
    var <- get
    age <- get
    return (Row o var age)


type DB     = [DCLabeled Row]
type Result = [(CovidVariant, MeanAge)] -- mean age with noise
type ResultEncrypted = B.ByteString

database :: DB
database = []

sendData :: EnclaveDC (DCRef DB) -> Priv CNF -> Priv CNF -> DCLabeled Row -> EnclaveDC ()
sendData enc_ref_db p1 p2 labeledRow = do
  ref_db     <- enc_ref_db
  datas      <- readRef ref_db

  -- DPBA ADDITION
  row <- unlabelRow labeledRow
  traceCallI [ note (owner row)
             , note (patientAge row)
             , note (show (covidVar row)) ]
  -- DPBA ADDITION
  writeRef ref_db (labeledRow : datas)
  where
    unlabelRow = unlabelFunc p1 p2


runQuery :: EnclaveDC (DCRef DB) -> PublicKey -> Priv CNF -> Priv CNF -> EnclaveDC ResultEncrypted
runQuery enc_ref_db pubK priv1 priv2  = do
  labeled_rows <- join $ readRef <$> enc_ref_db
  rows         <- mapM unlabelRow labeled_rows
  result       <- addNoise $ query1 rows
  res_enc      <- liftIO $ encrypt pubK (B.toStrict $ encode result)
  case res_enc of
    Left err -> do
      liftIO $ putStrLn (show err)
      return B.empty
    Right bytestr -> do
      -- DPBA ADDITION
      mapM_ (\(cov,age) ->
               traceCallO [note o3, note age, note (show cov)]) result
      -- DPBA ADDITION
      return bytestr
  where
    unlabelRow = unlabelFunc priv1 priv2




addNoise :: [(CovidVariant, MeanAge, Int)] -> EnclaveDC Result
addNoise datas =
  mapM (\(cov, mA, s) -> addNoise' s mA >>= (\mA' -> pure (cov, mA'))) datas

-- Local Sensitivity Δf = (Max Change in Age​) / (Number of entries)

-- NOTE 1: Local sensitivity
-- The maximum a record can change if we substitute
-- one for the other, I am assuming is 100. Eg:
-- you substitue the record of an infant (0) with a
-- 100 year old person's covid data

-- NOTE 2
-- chose a magic epsilon number (needs tweaking)
-- the probability of getting a certain output is
-- at most e^epsilon times the probability of getting
-- the output from the altered database

--XXX: datasetsize should never be 0
-- the consumer of this function insures this
addNoise' :: Int -> MeanAge -> EnclaveDC MeanAge
addNoise' datasetsize meanAge =
  addLaplacianNoiseWithSensitivity meanAge sensitivity epsilon
  where
    sensitivity = 100.0 / (fromIntegral datasetsize :: Double) -- NOTE 1
    epsilon = 0.9 -- NOTE 2

-- | Declassification
unlabelFunc :: Priv CNF -> Priv CNF -> DCLabeled Row -> EnclaveDC Row
unlabelFunc p1 p2 lrow =
  case orgName of
    "org1" -> unlabelP p1 lrow
    "org2" -> unlabelP p2 lrow
    _      -> unlabel lrow -- label will float
  where
    dclabel = labelOf lrow
    orgName = extractOrgName dclabel

extractOrgName :: DCLabel -> String
extractOrgName dclabel =
  if dcSecrecy dclabel == dcIntegrity dclabel
  then filter (/= '"') $ show $ dcSecrecy dclabel
  else show dclabel

query1 :: [Row] -> [(CovidVariant, MeanAge, Int)]
query1 = map collate
       . filter (\rs -> length rs > 1)
       . groupBy (\(Row _ cov1 _) (Row _ cov2 _) -> cov1 == cov2)
       . sortBy  (\(Row _ cov1 _) (Row _ cov2 _) -> compare cov1 cov2)
  where
    collate :: [Row] -> (CovidVariant, MeanAge, Int)
    collate rows = (covidVar (head rows), meanAge, length rows)
      where meanAge = (fromIntegral (sum (map patientAge rows)) :: Double) /
                      (fromIntegral $ length rows)


data API =
  API { datasend :: Secure (DCLabeled Row -> EnclaveDC ())
      , runQ     :: Secure (EnclaveDC ResultEncrypted)
      }


client1 :: API -> Client "org1" ()
client1 api = do
  labeledDs <- mapM (clientLabel dataProvider)
               [row1, row2, row3, row4, row5, row6]
  traverse_ (\lRow -> gatewayRA ((datasend api) <@> lRow)) labeledDs
  where
    dataProvider :: DCLabel
    dataProvider = "org1" %% "org1"

client2 :: API -> Client "org2" ()
client2 api = do
  labeledDs <- mapM (clientLabel dataProvider)
               [row7, row8, row9, row10]
  traverse_ (\lRow -> gatewayRA ((datasend api) <@> lRow)) labeledDs
  where
    dataProvider :: DCLabel
    dataProvider = "org2" %% "org2"



client3 :: API -> Client "org3" ()
client3 api = do
  res_enc <- gatewayRA (runQ api)
  privK   <- liftIO $ read <$> readFile "ssl/private.key"
  res     <- liftIO $ decryptSafer privK res_enc
  case res of
    Left err -> liftIO $ putStrLn $ show err
    Right bytestr -> do
      let result = decode (B.fromStrict bytestr) :: Result
      liftIO $ putStrLn "Analytics result"
      liftIO $ putStrLn (show result)

-- Assume rows are fetched from the database
-- org1 rows
row1, row2, row3 :: Row
row4, row5, row6 :: Row

o1, o2, o3 :: Party
o1 = "client1"
o2 = "client2"
o3 = "client3"


row1 = Row o1 XBB15 77
row2 = Row o1 BA275 57
row3 = Row o1 DV71  39
row4 = Row o1 XBB15 82
row5 = Row o1 BA275 53
row6 = Row o1 B1525 37

-- org2 rows
row7, row8, row9, row10 :: Row

row7  = Row o2 XBB15 83
row8  = Row o2 BA275 52
row9  = Row o2 P681  22
row10 = Row o2 B318 32

type OrgName = String

-- data provider
org1 :: OrgName
org1 = "org1"

-- data provider
org2 :: OrgName
org2 = "org2"

-- analytics provider
org3 :: OrgName
org3 = "org3"


ifctest :: App Done
ifctest = do
  db <- liftNewRef dcPublic database -- db kept permissive because all
                                     -- data is labeled
  pubK     <- liftIO $ read <$> readFile "ssl/public.key"
  org1Priv <- liftIO $ privInit (toCNF org1)
  org2Priv <- liftIO $ privInit (toCNF org2)
  sfunc    <- inEnclave initState $ sendData db org1Priv org2Priv
  qfunc    <- inEnclave initState $ runQuery db pubK org1Priv org2Priv
  let api = API sfunc qfunc
  runClient (client1 api)
  runClient (client2 api)
  runClient (client3 api)
  where
    initState = dcDefaultState cTrue

main :: IO ()
main = do
  res <- runAppRA org3 ifctest
  return $ res `seq` ()
