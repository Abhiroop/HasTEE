module LogisticRegression where

import Data.List

data Config = Config
    { epochs       :: Int
    , alpha        :: Double
    , learningRate :: Double
    , iterN        :: Int
    , weights      :: [Double]
    }
  deriving Show

baseConfig :: Config
baseConfig = Config 100 0.01 0.15 0 (error "<uninitialized weights>")

fit :: Config -> [[Double]] -> [Int] -> Config
fit cfg x y =
    let m = length x
        x' = map ((:) 1.0) x
        n = length (head x')
        x'' = transpose x'
        lw = replicate n 1.0
    in handleSingle 0 (epochs cfg) x'' (cfg { weights = lw })
  where
    handleSingle :: Int -> Int -> [[Double]] -> Config -> Config
    handleSingle n m x cfg
        | n == m    = cfg
        | otherwise = handleSingle (n+1) m x (updateModel (cfg { iterN = n }) $ computeGradient cfg x y)

computeGradient :: Config -> [[Double]] -> [Int] -> [Double]
computeGradient cfg x y =
    let m = length (head x)
        yPred = map sigmoid $ dotprod (weights cfg) x
     -- they pass in transpose here, not sure that we have to? double check
    in map (\w -> w / (fromIntegral m)) $ dotprod (zipWith (\y1 y2 -> y1 - fromIntegral y2) yPred y) (transpose x)

dotprod :: [Double] -> [[Double]] -> [Double]
dotprod w x = let x' = transpose x -- easier to do in Haskell if it is transposed here
                  mapped = map (\dp -> zipWith (*) dp w) x'
              in map sum mapped

updateModel :: Config -> [Double] -> Config
updateModel cfg grad =
    let lr = learningRate cfg / sqrt (1 + fromIntegral (iterN cfg))
        gr = zipWith (+) grad (map (alpha cfg *) (weights cfg))
        nw = zipWith (-) (weights cfg) (map (lr *) gr)
    in cfg { weights = nw }

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

-- * test data

-- did student pass the exam?
testy :: [Int]
testy = [0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1]

-- how many hours did student study?
testx :: [[Double]]
testx = [ [0.5]
        , [0.75]
        , [1.0]
        , [1.25]
        , [1.5]
        , [1.75]
        , [1.75]
        , [2.0]
        , [2.25]
        , [2.5]
        , [2.75]
        , [3.0]
        , [3.25]
        , [3.5]
        , [4.0]
        , [4.25]
        , [4.5]
        , [4.75]
        , [5.0]
        , [5.5]
        ]

-- for the above test data, the returned weights should be -4.1 and 1.5

eval :: IO ()
eval = let cfg = fit baseConfig testx testy
       in putStrLn $ concat [show (head (weights cfg)), " + ", show (last (weights cfg)), " * #hours-studied"]
