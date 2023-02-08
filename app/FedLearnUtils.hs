module FedLearnUtils ( parseDataSet, testDataSet
                     , dotprod, dotprodHE, sigmoid, sigmoid_taylor_expand
                     , go2I, go2D, i2I, runProperties) where

import Control.Monad.IO.Class
import Crypto.Paillier
import Data.List.Split (splitOn)
import Data.Matrix ( Matrix, fromLists, submatrix, nrows
                   , ncols, getCol, getRow, transpose)
import qualified Data.Vector as V
import GHC.Float (double2Int)

import Test.QuickCheck

testDataSet :: FilePath
testDataSet = "fed_dataset/breast_homo_test.csv"

csvToMatrix :: String -> Matrix Double
csvToMatrix csv = fromLists [[read x :: Double | x <- splitOn "," row] | row <- tail (lines csv)]

parseDataSet :: FilePath -> IO (Matrix Double, V.Vector Int)
parseDataSet fp = do
  csv <- readFile fp
  let mat = csvToMatrix csv
  let x = submatrix 1 (nrows mat) 3 (ncols mat) mat
  let y = V.map double2Int $ getCol 2 mat
  return (x, y)

dotprodHE :: (MonadIO m)
          => PubKey
          -> V.Vector CipherText
          -> Matrix Double
          -> m (V.Vector CipherText)
dotprodHE pubk w x = do
  let zero = go2I 0.0
  enc_zero <- liftIO $ encrypt pubk zero
  return $ V.fromList $
    Prelude.map (\i' ->
                   V.foldr (\c c' -> cipherMul pubk c c')
                   enc_zero
                   (V.zipWith (\d cipher -> cipherExp pubk cipher (go2I d))
                    (getRow i' x')
                    w)
                )
    [1..i]
   where
     x' = transpose x
     i  = nrows x'


dotprod :: V.Vector Double -> Matrix Double -> V.Vector Double
dotprod w x = let x' = transpose x
                  i  = nrows x'
                  mapped = V.fromList $ Prelude.map (\i' -> V.sum $ V.zipWith (*) (getRow i' x') w) [1..i]
              in mapped

-- `sigmoid` cannot work on encrypted values
-- so we need `sigmoid_taylor_expand`
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

sigmoid_taylor_expand :: MonadIO m => PubKey -> CipherText -> m CipherText
sigmoid_taylor_expand pubK cipher = do
  let val = 0.5
  let enc_val = go2I val
  return $ cipherMul pubK enc_val (cipherExp pubK cipher (go2I 0.25))

-- 6 digits of precision
precision :: Double
precision = 1000000

go2I :: Double -> Integer
go2I = truncate . (* precision)

go2D :: Integer -> Double
go2D x = fromInteger x / precision

i2I  :: Int -> Integer
i2I = toInteger

eps :: Double
eps = 0.000001

-- * testing

prop_d2i_id :: Double -> Bool
prop_d2i_id d = abs (go2D (go2I d) - d) <= eps

prop_i2d_id :: Integer -> Bool
prop_i2d_id i = go2I (go2D i) == i

-- this reports that the conversions work as expected, which is great
runProperties :: IO ()
runProperties = do
  quickCheck $ withMaxSuccess 1000000 prop_d2i_id
  quickCheck $ withMaxSuccess 1000000 prop_i2d_id
