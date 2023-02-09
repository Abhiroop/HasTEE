{-# LANGUAGE CPP #-}
module FedLearnWorker where


#ifdef ENCLAVE
import Server
#else
import Client
#endif

import App

import FedLearnServer

import Control.Concurrent(threadDelay)
import Control.Monad.IO.Class(liftIO)

import Crypto.Paillier
import Data.Vector as V
import Data.Matrix as M

import FedLearnUtils

data Config = Config
    { epochs       :: Int
    , alpha        :: Double
    , learningRate :: Double
    , iterN        :: Int
    , weights      :: Vector CipherText
    , pubKey       :: PubKey
    }
  deriving Show

baseConfig :: Config
baseConfig = Config 100 0.01 0.15 0 wt pk
  where
    wt = error "<uninitialized weights>"
    pk = error "<uninitialized public key>"

fit :: API -> Config -> Matrix Double -> Vector Int -> Client Config
fit api cfg x y = do
    let m   = nrows x
    let x'  = colVector (V.replicate m 1.0) <|> x
    let n   = ncols x'
    let x'' = M.transpose x'
    let one = go2I 1.0
    enc_one <- liftIO $ encrypt (pubKey cfg) one
    let lw  = V.replicate n enc_one
    handleSingle 0 (epochs cfg) x'' (cfg { weights = lw })
  where
    handleSingle :: Int -> Int
                 -> Matrix Double
                 -> Config
                 -> Client Config
    handleSingle n m x' cfg'
        | n == m    = return cfg'
        | otherwise = do
            grad <- computeGradient cfg' x' y
            let cfgNew = updateModel (cfg' { iterN = n }) grad
            wt' <- retryOnServer $ (aggrM api) <.> n <.> (weights cfgNew)
            (acc, loss) <- onServer (valM api)
            printCl $ "Iteration no: " <> show n
                    <> " Accuracy: "   <> show acc
                    <> " Loss : "      <> show loss
            handleSingle (n+1) m x' (cfgNew { weights = wt' })

retryOnServer :: Remote (Server (Maybe (V.Vector CipherText)))
              -> Client (V.Vector CipherText)
retryOnServer rem_srv = do
  res <- onServer rem_srv
  case res of
    Nothing -> do
      liftIO $ threadDelay 1000000
      retryOnServer rem_srv
    Just a -> return a

computeGradient :: Config
                -> Matrix Double
                -> Vector Int
                -> Client (Vector CipherText)
computeGradient cfg x y = do
  let m = ncols x
  prod  <- dotprodHE pubK (weights cfg) x
  yPred <- V.mapM (sigmoid_taylor_expand pubK) prod
  yEnc  <- V.mapM ((\e -> liftIO $ encrypt pubK e) . i2I) y
  prod' <- dotprodHE pubK (subPHE yPred yEnc) (M.transpose x)
  return $ V.map (\c -> cipherExp pubK c (go2I (1 / (fromIntegral m)))) prod'
  where
    pubK = pubKey cfg
    subPHE  = V.zipWith (\v1 v2 -> homoSub pubK v1 v2)

updateModel :: Config -> Vector CipherText -> Config
updateModel cfg grad =
    let lr = learningRate cfg / sqrt (1 + fromIntegral (iterN cfg))
        gr = addP grad (V.map (\w -> cipherExp pubK w (go2I (alpha cfg))) (weights cfg))
        nw = subP (weights cfg) (V.map (\g -> cipherExp pubK g (go2I lr)) gr)
    in cfg { weights = nw }
    where
      pubK = pubKey cfg
      addP = V.zipWith (\v1 v2 -> cipherMul pubK v1 v2)
      subP = V.zipWith (\v1 v2 -> homoSub   pubK v1 v2)

printCl :: String -> Client ()
printCl = liftIO . putStrLn

noClients :: Int
noClients = 2

data API = API { aggrM :: Remote (IterN ->
                                  V.Vector CipherText ->
                                  Server (Maybe (V.Vector CipherText)))
               , valM  :: Remote (Server (Accuracy, Loss))
               }

app :: FilePath -> App Done
app fp = do
  server_st <- liftNewRef initSrvState :: App (Server (Ref SrvSt))
  initSt    <- remote $ initTEEState server_st
  getPubK   <- remote $ getPubKey server_st
  aggrModel <- remote $ aggregateModel server_st
  validateM <- remote $ validate server_st
  fin       <- remote $ finish   server_st
  let api   = API aggrModel validateM
  runClient $ do
    (x, y) <- liftIO $ parseDataSet fp
    _      <- onServer (initSt <.> noClients)
    pubK   <- onServer getPubK
    let config' = baseConfig { pubKey = pubK }
    _      <- fit api config' x y
    _      <- onServer fin
    printCl "Goodbye!"
