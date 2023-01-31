module LogisticRegression where

import Data.List


data Config = Config
    { epochs       :: Int
    , alpha        :: Double
    , learningRate :: Double
    , iterN        :: Int
    , weights      :: [Double]
    }

fit :: Config -> [[Double]] -> [Int] -> Config
fit cfg x y =
    let m = length x
        x' = map ((:) 1.0) x
        n = length (head x')
        x'' = transpose x'
        lw = replicate n 1.0
    in handleSingle 0 (epochs cfg) cfg
  where
    handleSingle :: Int -> Int -> Config -> Config
    handleSingle n m cfg
        | n == m    = cfg
        | otherwise = handleSingle (n+1) m (updateModel cfg $ computeGradient cfg x y)

computeGradient :: Config -> [[Double]] -> [Int] -> [Double]
computeGradient cfg x y =
    let m = length (head x)
        yPred = map sigmoid $ dotprod (weights cfg) x
     -- they pass in transpose here, not sure that we have to? double check
    in map (\w -> w / (fromIntegral m)) $ dotprod (zipWith (\y1 y2 -> y1 - fromIntegral y2) yPred y) x

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
