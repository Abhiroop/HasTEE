{-# LANGUAGE CPP #-}
module FedLearnWorker where


#ifdef ENCLAVE
import Server
#else
import Client
#endif

import App

import FedLearnServer

import Control.Concurrent(threadDelay, myThreadId)
import Control.Monad.IO.Class(liftIO)

import Crypto.PaillierRealNum
import Data.Vector as V
import Data.Matrix as M
import GHC.Float (int2Double)

import FedLearnUtils

import qualified Crypto.Paillier as P

data Config = Config
    { epochs       :: Int
    , alpha        :: Double
    , learningRate :: Double
    , iterN        :: Int
    , weights      :: Vector CT
    , pubKey       :: P.PubKey
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
    --let one = go2I 1.0
    enc_one <- liftIO $ encrypt (pubKey cfg) 1.0
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
            tid <- liftIO $ myThreadId
            prv <- onServer (gpK api)
            grad <- computeGradient api prv cfg' x' y
            -- liftIO $ print "Abhi"
            -- liftIO $ print $ V.map (decrypt (pubKey cfg') prv) grad
            cfgNew <- updateModel api (cfg' { iterN = n }) grad
            wt' <- retryOnServer $ (aggrM api) <.> n <.> (weights cfgNew)
            (acc, loss) <- onServer (valM api)
            printCl $  "Client no: "     <> (show tid) -- $ clno cfg')
                    <> " Iteration no: " <> show n
                    <> " Accuracy: "     <> show acc
                    <> " Loss : "        <> show loss
            handleSingle (n+1) m x' (cfgNew { weights = wt' })

retryOnServer :: Remote (Server (Maybe (V.Vector CT)))
              -> Client (V.Vector CT)
retryOnServer rem_srv = do
  res <- onServer rem_srv
  case res of
    Nothing -> do
      liftIO $ threadDelay 1000000
      retryOnServer rem_srv
    Just a -> return a

computeGradient :: API
                -> P.PrvKey
                -> Config
                -> Matrix Double
                -> Vector Int
                -> Client (Vector CT)
computeGradient api prv cfg x y = do
  let m = ncols x
  prod  <- dotprodHEC api pubK (weights cfg) x
  yPred <- V.mapM (sigmoid_taylor_expandC api pubK) prod
  yEnc  <- V.mapM ((\e -> liftIO $ encrypt pubK e) . int2Double) y
  prod' <- do
    op <- subPHE yPred yEnc
    dotprodHEC api pubK op (M.transpose x)
  onServer $ recryptMany api <.> V.map (\c -> homoMul pubK c (1 / (fromIntegral m))) prod'
  where
    pubK = pubKey cfg
    subPHE xs ys = onServer $ recryptMany api <.> V.zipWith (\v1 v2 -> homoSub pubK v1 v2) xs ys

-- | Dot product that reencrypts the CT between any two operations
dotprodHEC :: API
           -> P.PubKey
           -> V.Vector CT
           -> Matrix Double
           -> Client (V.Vector CT)
dotprodHEC api pubk w x = do
  -- let zero = go2I 0.0
  enc_zero <- liftIO $ encrypt pubk 0.0
  vec <- flip Prelude.mapM [1..i] $ \i' -> do
    let dots = V.zipWith (\d cipher -> homoMul pubk cipher d) (getRow i' x') w
    dots' <- onServer $ recryptMany api <.> dots
    V.foldM (\c c' -> onServer $ recrypt api <.> homoAdd pubk c c') enc_zero dots'
  return $ V.fromList vec
   where
     x' = transpose x
     i  = nrows x'

sigmoid_taylor_expandC :: API -> P.PubKey -> CT -> Client CT
sigmoid_taylor_expandC api pubK cipher = do
  let val = 0.5
  enc_val <- liftIO $ encrypt pubK val --go2I val
  first <- onServer $ recrypt api <.> homoMul pubK cipher 0.25
  onServer $ recrypt api <.> homoAdd pubK enc_val first

updateModel :: API -> Config -> Vector CT -> Client Config
updateModel api cfg grad = do
    let lr = learningRate cfg / sqrt (1 + fromIntegral (iterN cfg))
    gr <- do
      let op = V.map (\w -> homoMul pubK w (alpha cfg)) (weights cfg)
      op' <- onServer $ recryptMany api <.> op
      addP grad op'
    nw <- do
      let op = V.map (\g -> homoMul pubK g lr) gr
      op' <- onServer $ recryptMany api <.> op
      subP (weights cfg) op'
        -- gr = addP grad (V.map (\w -> homoMul pubK w (alpha cfg)) (weights cfg))
        -- nw = subP (weights cfg) (V.map (\g -> homoMul pubK g lr) gr)
    return cfg { weights = nw }
    where
      pubK = pubKey cfg
      addP xs ys = do
        let res = V.zipWith (\v1 v2 -> homoAdd pubK v1 v2) xs ys
        onServer $ recryptMany api <.> res
      subP xs ys = do
        let res = V.zipWith (\v1 v2 -> homoSub pubK v1 v2) xs ys
        onServer $ recryptMany api <.> res

printCl :: String -> Client ()
printCl = liftIO . putStrLn

noClients :: Int
noClients = 1

data API = API { aggrM :: Remote (IterN ->
                                  V.Vector CT ->
                                  Server (Maybe (V.Vector CT)))
               , valM  :: Remote (Server (Accuracy, Loss))
               , gpK   :: Remote (Server P.PrvKey)
               , recrypt :: Remote (CT -> Server CT)
               , recryptMany :: Remote (V.Vector CT -> Server (V.Vector CT))
               }

app :: FilePath -> App Done
app fp = do
  server_st <- liftNewRef initSrvState :: App (Server (Ref SrvSt))
  initSt    <- remote $ initTEEState server_st
  getPubK   <- remote $ getPubKey server_st
  getPrK    <- remote $ getPrvKey server_st
  aggrModel <- remote $ aggregateModel server_st
  validateM <- remote $ validate server_st
  fin       <- remote $ finish   server_st
  re        <- remote $ reEncrypt server_st
  remany    <- remote $ reEncryptMany server_st
  let api   = API aggrModel validateM getPrK re remany
  runClient $ do
    (x, y) <- liftIO $ parseDataSet fp
    _      <- onServer (initSt <.> noClients)
    pubK   <- onServer getPubK
    let config' = baseConfig { pubKey = pubK }
    _      <- fit api config' x y
    _      <- onServer fin
    printCl "Goodbye!"
