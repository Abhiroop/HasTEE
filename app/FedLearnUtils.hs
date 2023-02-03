module FedLearnUtils ( parseDataSet, testDataSet, trainingDataSet
                     , dotprod, sigmoid, go2I, go2D) where

import Data.List.Split (splitOn)
import Data.Matrix ( Matrix, fromLists, submatrix, nrows
                   , ncols, getCol, getRow, transpose)
import qualified Data.Vector as V
import GHC.Float (double2Int)

testDataSet :: FilePath
testDataSet = "fed_dataset/breast_homo_test.csv"

trainingDataSet :: FilePath
trainingDataSet = "fed_dataset/breast_homo_host.csv"

csvToMatrix :: String -> Matrix Double
csvToMatrix csv = fromLists [[read x :: Double | x <- splitOn "," row] | row <- tail (lines csv)]

parseDataSet :: FilePath -> IO (Matrix Double, V.Vector Int)
parseDataSet fp = do
  csv <- readFile fp
  let mat = csvToMatrix csv
  let x = submatrix 1 (nrows mat) 3 (ncols mat) mat
  let y = V.map double2Int $ getCol 2 mat
  return (x, y)

dotprod :: V.Vector Double -> Matrix Double -> V.Vector Double
dotprod w x = let x' = transpose x
                  i  = nrows x'
                  mapped = V.fromList $ Prelude.map (\i' -> V.sum $ V.zipWith (*) (getRow i' x') w) [1..i]
              in mapped

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

-- 6 digits of precision
precision :: Double
precision = 1000000

go2I :: Double -> Integer
go2I = truncate . (* precision)

go2D :: Integer -> Double
go2D x = fromInteger x / precision
