module Examples where


{-@ Ex 1

Insurance Fraud Detection

Enclave
-------

data ClaimCause =
  A | B | C | ......

data FraudHistory = FH { cause :: FraudCause
                         detectionMethod :: Method
                         resolutionId :: Int
                         caseNum :: Int}

type InsuranceData =
  (UserId, ClaimId, ClaimAmount, ClaimCause, FraudHistory)

collect :: InsuranceData -> Enclave ()
-- aggregate data


type PotentialFraudData =
  (UserId, ClaimAmount, ClaimCause)

fraudDetect :: PotentialFraudData -> Enclave Bool

-- Besides this the testRig can also generate
IO (Trace, Bool) and then the traces can be
compared to see what the attacker learns
testRig :: [InsuranceData]
        -> PotentialFraudData
        -> IO Bool
testRig ids fraudData = runAppRA org1 $ do
  -- general setup
  mapM_ collect ids     -- inside client
  res <- fraudDetect fraudData -- another client
  return res
-- need a variant of runApp that can take down and
-- restore an enclave; testAppRA


prop_NI :: [InsuranceData]
        -> PotentialFraudData
        -> PotentialFraudData
        -> Property
prop_NI ids fd1 fd2 = monadicIO $ do
  result1 <- run $ testRig ids fd1
  result2 <- run $ testRig ids fd2
  return (result1 == result2)

if FraudData is well generated this should always hold

the key is `==`; I think instead of == we should use `equivalence`
The equivalence should check more then the result; like trace, state, etc.

By observing the cases where this equality doesn't hold;
a passive attacker can learn by looking at the difference between fd1 and fd2 to learn about the policy of the model




There are metamorphic relations that can be tested. Be careful of delving too much into the metamorphic testing stuff because this is more of a QC feature than testing hyperproperties with QC.

-- Metamorphic property: Permutation invariance
prop_permutation_invariance :: [InsuranceData] -> PotentialFraudData -> Property
prop_permutation_invariance ids fd = monadicIO $ do
  originalResult <- run $ testRig ids fd
  permutedResult <- run $ testRig (reverse ids) fd
  return (originalResult == permutedResult)

-- Metamorphic property: Subset property; If a subset of insurance data results in a positive detection of fraud, then the full set should also detect fraud.

prop_subset_property ::
[InsuranceData] -> PotentialFraudData -> Property
prop_subset_property ids fd = 
  length ids > 1 ==> monadicIO $ do
    let (subset, rest) = splitAt (length ids `div` 2) ids
    subsetResult <- run $ testRig subset fd
    fullResult <- run $ testRig ids fd
    return (not subsetResult || fullResult)

@-}

--------------------------------------------------------

{-@ Ex 2

Medical Data Analysis (example in paper)

Keeping the example unaltered;

type Row = (Name, Age, Gender, CovidStrain, Income)
send :: Row    -> Enclave ()

runQ :: Enclave Result

runQ

The Income group allows the attacker to pinpoint areas where COVID outbreaks are high;

Encryption doesn't change anything. Encryption protect the data from being snooped but it is about what the querying party learns. In this case depending on the statistics in question it can infer several things. So, we will use differential privacy. And use QC to test for differential privacy:

The benefits of QC should be deadly easy here; simply alter the parameters and the QC test should fail.

In the paper we are kind of selling Haskell. So the point we should make is that Haskell is allowing us:
1. type safety
2. Hosting things in the enclave so the parties are confident that computation are being carried out in a safe environment;
3. IFC is preventing data from being leaked to libraries and as a development tool to prevent us from making silly mistakes
4. QC employs the purity of Haskell; algebraic data types; etc to offer a good alternative to randomised testing/fuzzing


-- Define a simple query type
data Query = CountCases deriving (Show, Eq)

-- Example query function
queryDatabase :: Query -> [TestData] -> IO Double
queryDatabase CountCases db = return . fromIntegral . length $ db

-- Differential Privacy Mechanism
addLaplaceNoise :: Double -> IO Double
addLaplaceNoise epsilon = do
  noise <- laplaceNoise epsilon
  return noise
  where
    scale = 1 / epsilon
    laplaceNoise b = do
      u <- randomRIO (-1, 1) :: IO Double
      return $ -b * signum u * log (1 - 2 * abs u)

-- Function to compute the differentially private result
computeWithPrivacy :: Double -> Query -> [TestData] -> IO Double
computeWithPrivacy epsilon query db = do
  queryResult <- queryDatabase query db
  noise <- addLaplaceNoise epsilon
  return $ queryResult + noise

-- Example data type
data TestData = TestData
  { value :: Int
  } deriving (Show, Eq)

-- Generate test databases
generateTestDatabases :: IO ([TestData], [TestData])
generateTestDatabases = do
  db1 <- generateData
  db2 <- generateDataWithOneChange db1
  return (db1, db2)

generateData :: IO [TestData]
generateData = return [TestData 100, TestData 101]

generateDataWithOneChange :: [TestData] -> IO [TestData]
generateDataWithOneChange (TestData v : rest) = return $ TestData (v + 1) : rest
generateDataWithOneChange _ = return []

-- Function to test differential privacy
testPrivacyMechanism :: Double -> Query -> [TestData] -> [TestData] -> IO Bool
testPrivacyMechanism epsilon query db1 db2 = do
  result1 <- computeWithPrivacy epsilon query db1
  result2 <- computeWithPrivacy epsilon query db2

  let observedPrivacyLoss = abs (result1 - result2)
      acceptablePrivacyLoss = epsilon -- Simplified; in practice, compute bounds

  return $ observedPrivacyLoss <= acceptablePrivacyLoss

-- Property to check
prop_DifferentialPrivacy :: [TestData] -> [TestData] -> Bool
prop_DifferentialPrivacy db1 db2 = 
  let epsilon = 1.0  -- Example epsilon
      query = CountCases -- Example query
  in (==) <$> testPrivacyMechanism epsilon query db1 db2

-- Running QuickCheck
main :: IO ()
main = quickCheck prop_DifferentialPrivacy



@-}
--------------------------------------------------------

{-@ Ex 3

LLM Context in ChatBots

Public Data
-----------
LLM_Model like Llama

Private Data
------------
There are grades:

Semi-confidential:
Query   - UserInput;
User might choose to make their current query confidential or not
Might be public;

Highly confidential:
Gathered by organisations like Google
Context - User context ; users history & interests


getResponseRich :: LLM_Model
                -> Context
                -> Query -> Enclave Response

getResponse     :: LLM_Model
                -> Query
                -> Enclave Response

Multiple clients not needed.

We are approaching Differential Privacy here; but the general idea is that we want to provide bounds on the information leakage:


getResponseRich :: LLM_Model -> Context -> Query -> IO Response


-- Number of samples for the probabilistic test
numSamples = 1000

-- Collect a list of responses from running getResponseRich multiple times
collectResponses :: LLM_Model -> Context -> Query -> IO [Response]
collectResponses model context query = replicateM numSamples (getResponseRich model context query)

-- Convert responses to a numerical representation for comparison (hypothetical function)
responseToNumeric :: Response -> Double
responseToNumeric = undefined

-- Compute Kullback-Leibler divergence as a measure of information leakage
klDivergenceResponses :: [Response] -> [Response] -> Double
klDivergenceResponses responses1 responses2 =
  let dist1 = map responseToNumeric responses1
      dist2 = map responseToNumeric responses2
  in klDivergence dist1 dist2

-- Property to ensure information leakage is within bounds
prop_information_leakage :: LLM_Model -> Query -> Property
prop_information_leakage model query = monadicIO $ do
  responses1 <- run $ collectResponses model Context1 query
  responses2 <- run $ collectResponses model Context2 query
  let leakage = klDivergenceResponses responses1 responses2
  return $ leakage < acceptableLeakageBound

-- Define an acceptable leakage bound (this value should be determined based on the application's privacy requirements)
acceptableLeakageBound :: Double
acceptableLeakageBound = 0.1  -- Example value

-- Example usage
main :: IO ()
main = quickCheck (prop_information_leakage someModel someQuery)
  where
    someModel = LLM_Model
    someQuery = Query

@-}

--------------------------------------------------------

{-@ Regarding partitioning

Now we want to prove some properties related to the code partitioning.

1. Isolation
2. Controlled Communication
3. Integration Testing (simulating an attack) - Simulate programs that require access to I/O and show that it is not possible

-- Define types for partitions
data Partition = Partition { name :: String, dataStore :: Int } deriving (Show, Eq)

-- Define a simple mechanism for communication
communicate :: Partition -> Partition -> Int -> Partition
communicate sender receiver msg = receiver { dataStore = msg }

-- Define a property to test isolation
prop_Isolation :: Int -> Int -> Bool
prop_Isolation senderData receiverData =
  let sender = Partition "Sender" senderData
      receiver = Partition "Receiver" receiverData
      newReceiver = communicate sender receiver 42
  in dataStore newReceiver == receiverData

-- Define a property to test controlled communication
prop_CommunicationIntegrity :: Int -> Int -> Bool
prop_CommunicationIntegrity senderData receiverData =
  let sender = Partition "Sender" senderData
      receiver = Partition "Receiver" receiverData
      newReceiver = communicate sender receiver 42
  in (dataStore newReceiver == 42) && (dataStore newReceiver /= receiverData) -- what if receiverData generated as 42

-- Run QuickCheck tests
main :: IO ()
main = do
  quickCheck prop_Isolation
  quickCheck prop_CommunicationIntegrity



@-}
