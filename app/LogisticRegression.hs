module LogisticRegression (eval, fit, Config(..), baseConfig) where

import Data.Vector as V
import Data.Matrix as M

import Debug.Trace
import FedLearnUtils

data Config = Config
    { epochs       :: Int
    , alpha        :: Double
    , learningRate :: Double
    , iterN        :: Int
    , weights      :: Vector Double
    }
  deriving Show

baseConfig :: Config
baseConfig = Config 100 0.01 0.15 0 (error "<uninitialized weights>")

fit :: Config -> Matrix Double -> Vector Int -> Config
fit cfg x y =
    let m = nrows x
        x' = colVector (V.replicate m 1.0) <|> x
        n = ncols x'
        x'' = M.transpose x'
        lw = V.replicate n 1.0
    in handleSingle 0 (epochs cfg) x'' (cfg { weights = lw })
  where
    handleSingle :: Int -> Int -> Matrix Double -> Config -> Config
    handleSingle n m x' cfg'
        | n == m    = cfg'
        | otherwise = handleSingle (n+1) m x' (updateModel (cfg' { iterN = n }) $ computeGradient cfg' x' y)

computeGradient :: Config -> Matrix Double -> Vector Int -> Vector Double
computeGradient cfg x y =
    let m = ncols x
        yPred = V.map sigmoid $ dotprod (weights cfg) x
     -- they pass in transpose here, not sure that we have to? double check
    in V.map (\w -> w / (fromIntegral m)) $ dotprod (V.zipWith (\y1 y2 -> y1 - fromIntegral y2) yPred y) (M.transpose x)

updateModel :: Config -> Vector Double -> Config
updateModel cfg grad =
    let lr = learningRate cfg / sqrt (1 + fromIntegral (iterN cfg))
        gr = V.zipWith (+) grad (V.map (alpha cfg *) (weights cfg))
        nw = V.zipWith (-) (weights cfg) (V.map (lr *) gr)
    in cfg { weights = nw }

-- * test data

-- did student pass the exam?
testy :: Vector Int --[Int]
testy = V.fromList [0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1]

-- how many hours did student study?
testx :: Matrix Double
testx = M.fromLists 
        [ [0.5]
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

eval :: IO ()
eval = let cfg = fit baseConfig testx testy
       in putStrLn $ Prelude.concat [show (V.head (weights cfg)), " + ", show (V.last (weights cfg)), " * #hours-studied"]
