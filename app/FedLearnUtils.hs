module FedLearnUtils (parseDataSet, testDataSet) where

import Data.List.Split (splitOn)
import Data.Matrix (Matrix, fromLists, submatrix, nrows, ncols, getCol)
import Data.Vector (Vector, map)
import GHC.Float (double2Int)

testDataSet :: FilePath
testDataSet = "/home/abhiroop/Haskell/EnclaveIFC/fed_dataset/breast_homo_test.csv"

csvToMatrix :: String -> Matrix Double
csvToMatrix csv = fromLists [[read x :: Double | x <- splitOn "," row] | row <- tail (lines csv)]

parseDataSet :: FilePath -> IO (Matrix Double, Vector Int)
parseDataSet fp = do
  csv <- readFile fp
  let mat = csvToMatrix csv
  let x = submatrix 1 (nrows mat) 3 (ncols mat) mat
  let y = Data.Vector.map double2Int $ getCol 2 mat
  return (x, y)
