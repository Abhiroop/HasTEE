{-# LANGUAGE CPP #-}
module FedLearnServer where

import FedLearnUtils

#ifdef ENCLAVE
import Server
#else
import Client
#endif

import Crypto.Paillier
import Control.Monad.IO.Class(liftIO)
import Data.Matrix
import GHC.Float (int2Double)

import qualified Data.Vector as V

data SrvSt = SrvSt { publicKey  :: PubKey
                   , privateKey :: PrvKey
                   , updWts     :: V.Vector Double
                   , wtsDict    :: Map
                   , numClients :: Int
                   }

-- no of clients-
--               |
--               v
initTEEState :: Int -> Server (Ref SrvSt)
initTEEState num = do
  (pubK, prvK) <- liftIO $ genKey 1024
  let st = SrvSt { publicKey  = pubK
                 , privateKey = prvK
                 , updWts     = V.empty
                 , wtsDict    = []
                 , numClients = num
                 }
  newRef st

getPubKey :: Ref SrvSt -> Server PubKey
getPubKey ref_st = do
  srvst <- readRef ref_st
  return (publicKey srvst)

type Id = Int
type IterN = Int

-- this function updates the inner state `updWts` that
-- stores the updated_weights variable and returns the
-- updated value back; if all the clients haven't responded
-- it encrypts and send the old value back
aggregateModel :: Ref SrvSt
               -> IterN
               -> V.Vector CipherText
               -> Server (V.Vector CipherText)
aggregateModel ref_st iter_n wts = do
  srvst <- readRef ref_st
  let puK = publicKey  srvst
  let dict = wtsDict srvst
  let dict' = addWt iter_n wts dict
  atomicWriteRef ref_st (srvst { wtsDict = dict'})
  if length (dict' ~> iter_n) == numClients srvst
  then do -- this point onwards logic needs review
    let prK = privateKey srvst
    let data_plain = map (V.map (decrypt prK puK)) (dict' ~> iter_n)
    let data_plain_d = map (V.map go2D) data_plain -- going to Double
    let sum_vec = foldr (V.zipWith (+)) V.empty data_plain_d
    let aggr_vec = V.map (/ int2Double (numClients srvst)) sum_vec
    atomicWriteRef ref_st (srvst { updWts = aggr_vec })
    reEncrypt puK aggr_vec
  else reEncrypt puK (updWts srvst)
  where
    reEncrypt :: PubKey
              -> V.Vector Double
              -> Server (V.Vector CipherText)
    reEncrypt pk d = V.mapM (encM pk) (V.map go2I d)

    encM :: PubKey -> PlainText -> Server CipherText
    encM pk t = liftIO $ encrypt pk t


type Accuracy = Double
type Loss = Double
type DataSet = [[Double]] -- some dummy type


-- parse dataset; get x,y and call internal validate
-- calculate acc and loss using self.updated_weights
-- see NOTE 1
validate :: Ref SrvSt -> Server (Accuracy, Loss)
validate ref_st = do
  srvst <- readRef ref_st
  (x, y) <- liftIO $ parseDataSet testDataSet
  let m = nrows x
  let x' = colVector (V.replicate m 1.0) <|> x
  -- let n = ncols x'
  let yPred = V.map sigmoid $ dotprod (updWts srvst) x' -- check x or x'
  let loss = (-1 / (int2Double m))
           * (V.foldr (+) 0 $ addP (mulP (toD y) (logP yPred))
                                   (mulP (subP 1 (toD y))
                                         (logP (subP 1 yPred))))
  let yPred' = V.map (\v -> if v < 0.5 then 0 else 1) yPred
  let acc = (1 / (int2Double m))
          * (V.sum $ V.map (\x -> if x then 1 else 0) $ V.zipWith (==) yPred' y)
  return (acc, loss)
  where
    subP d vecd    = V.map (d -) vecd
    mulP vec1 vec2 = V.zipWith (*) vec1 vec2
    addP vec1 vec2 = V.zipWith (+) vec1 vec2
    logP = V.map log
    toD  = V.map int2Double

finish :: Server ()
finish = return () -- a no op to begin with; not splitting
                   -- the application into two parts now

-- NOTE 1
-- The validate function actually uses the weights to
-- calculate the accuracy and loss. It is important to
-- see if there is any information leak by leaking the
-- accuracy and loss values


-- I don't want to import `containers` X(
type Map = [(IterN, [V.Vector CipherText])]

member :: IterN -> Map -> Bool
member _ [] = False
member iter_n ((i,_):xs)
  | i == iter_n = True
  | otherwise = member iter_n xs

addWt :: IterN -> V.Vector CipherText -> Map -> Map
addWt iter_n wts [] = [(iter_n, [wts])]
addWt iter_n wts ((i, vecs):xs)
  | i == iter_n = (i, wts:vecs) : xs
  | otherwise = (i, vecs) : addWt iter_n wts xs

(~>) :: Map -> IterN -> [V.Vector CipherText]
(~>) [] _ = error "Cannot arise as key is added before lookup always"
(~>) ((i, v):xs) iter_n
  | i == iter_n = v
  | otherwise = xs ~> iter_n
