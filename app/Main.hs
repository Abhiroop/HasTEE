{-# LANGUAGE CPP #-}

{-# LANGUAGE DataKinds #-}
module Main where

import Data.Binary
import Data.Bits (shift, (.|.))
import Data.Word (Word8)
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable (peekElemOff)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Control.Concurrent (forkIO, threadDelay, yield)
import Data.IORef
import Foreign.Marshal.Alloc
import Foreign.Storable
-- import Control.Monad.IO.Class(liftIO)
-- import Data.List(genericLength)
-- import GHC.Float(int2Float)
-- import App
-- import Security.Sec

-- #ifdef ENCLAVE
-- import Enclave
-- #else
-- import Client
-- #endif



-- getData :: Enclave (Ref (Sec [Int])) -> Int -> Enclave Int
-- getData serv_secret idx = do
--   secret <- serv_secret
--   sech <- readRef secret
--   let sec_i = fmap (\s -> s !! idx) sech
--   return (declassify sec_i)

-- releaseAvg :: Enclave (Ref Bool) -> Enclave ()
-- releaseAvg sbool = do
--   bool <- sbool
--   writeRef bool True

-- doAvg :: [Int] -> Float
-- doAvg xs = realToFrac (sum xs) / genericLength xs

-- getAvg :: Enclave (Ref Bool) -> Enclave (Ref (Sec [Int])) -> Enclave Float
-- getAvg serv_bool serv_secret = do
--   bool   <- serv_bool
--   secret <- serv_secret
--   b <- readRef bool
--   if b
--   then do
--     s <- readRef secret
--     let s' = declassify s
--     let avg = doAvg s'
--     return avg
--   else return 0.0


-- printCl :: String -> Client ()
-- printCl = liftIO . putStrLn

-- app :: App Done
-- app = do
--   remoteSec1 <- liftNewRef (sec [15,30,11,6]) :: App (Enclave (Ref (Sec [Int])))
--   remoteSec2 <- liftNewRef False :: App (Enclave (Ref Bool))
--   gD <- inEnclave $ getData remoteSec1
--   rA <- inEnclave $ releaseAvg remoteSec2
--   gA <- inEnclave $ getAvg remoteSec2 remoteSec1
--   runClient $ do
--     data1 <- gateway (gD <@> 3)
--     _     <- gateway rA
--     avg   <- gateway gA
--     let b = dummyCompOnData data1 avg
--     printCl $ "Is data less than avg? " <> show b
--   where
--     dummyCompOnData i av = int2Float i < av


-- main :: IO ()
-- main = do
--   res <- runApp app
--   return $ res `seq` ()




import Control.Monad.IO.Class(liftIO)

import App

#ifdef ENCLAVE
import Enclave
#else
import Client
#endif

dataTillNow :: [Int]
dataTillNow = []

computeAvg :: Enclave (Ref [Int]) -> Enclave Int
computeAvg enc_ref_ints = do
  ref_ints <- enc_ref_ints
  vals     <- readRef ref_ints
  return (avg vals)
  where
    avg datas
      | (length datas) == 0 = 0
      | otherwise = sum datas `div` (length datas)

sendData :: Enclave (Ref [Int]) -> Int -> Enclave ()
sendData enc_ref_ints n = do
  ref_ints <- enc_ref_ints
  vals     <- readRef ref_ints
  writeRef ref_ints (n : vals)


data API =
  API { sendToEnclave :: Secure (Int -> Enclave ())
      , compAvg       :: Secure (Enclave Int)
      }



client1 :: API -> Client ()
client1 api = do
  gatewayRA ((sendToEnclave api) <@> 1700)
  res <- gatewayRA (compAvg api)
  liftIO $ putStrLn $ "Computed result " <> (show res)


privateAverage :: App Done
privateAverage = do
  initialData <- liftNewRef dataTillNow
  sD <- inEnclave $ sendData initialData
  cA <- inEnclave $ computeAvg initialData
  runClient (client1 (API sD cA))

main :: IO ()
main = do
  res <- runAppRA privateAverage
  return $ res `seq` ()



-- foreign import ccall "add" c_add :: CInt -> CInt -> IO CInt
-- foreign import ccall "processByteArray" processByteArray
--     :: Ptr CChar -> CSize -> IO (Ptr CChar)
-- foreign import ccall "processByteArrayDyn" processByteArrayDyn
--     :: Ptr CChar -> IO (Ptr CChar)
-- foreign import ccall "stdlib.h free" c_free :: Ptr CChar -> IO ()
-- foreign import ccall "test_ref" test_ref
--     :: Ptr CInt -> Ptr CChar -> IO ()

-- -- XXX: not portable;
-- -- 8 bytes for this machine
-- -- LSB/MSB order for this machine
-- byteStrLength :: Ptr CChar -> IO Int
-- byteStrLength cptr = go 0 []
--   where
--     go 8 xs = do
--       let (i0:i1:i2:i3:i4:i5:i6:i7:_) = xs
--       let y = (shift i0 56) .|. (shift i1 48) .|. (shift i2 40) .|.
--               (shift i3 32) .|. (shift i4 24) .|. (shift i5 16) .|.
--               (shift i6  8) .|. i7
--       return y
--     go i xs = do
--       cchar <- peekElemOff cptr i
--       go (i + 1) ((fromEnum cchar):xs)

-- data Foo = A Int | B Bool deriving (Show, Eq)


-- instance Binary Foo where
--   put (A i) = do
--     put (0 :: Word8)
--     put i
--   put (B b) = do
--     put (1 :: Word8)
--     put b

--   get = do
--     t <- get :: Get Word8
--     case t of
--       0 -> do
--         i <- get
--         return (A i)
--       1 -> do
--         b  <- get
--         return (B b)

-- foo = A 3
-- bar = B True

-- baz :: [Foo]
-- baz = [foo, bar]

-- microsec :: Int -> Int
-- microsec = id

-- millisec :: Int -> Int
-- millisec x = (microsec x) * 1000

-- sec :: Int -> Int
-- sec x = (millisec x) * 1000

-- main :: IO ()
-- main = do
--   res <- c_add 5 3
--   putStrLn $ "Haskell : " <> show res
--   intptr  <- malloc :: IO (Ptr CInt)
--   dataptr <- mallocBytes 1024 :: IO (Ptr CChar)
--   poke intptr 0
--   _ <- forkIO $ test_ref intptr dataptr
--   loop intptr dataptr
--   -- putStrLn $ "Haskell Land flag : " <> (show $ fromEnum int_val)
--   where
--     loop :: Ptr CInt -> Ptr CChar -> IO ()
--     loop iptr dptr = do
--       int_val <- peek iptr
--       if (fromEnum int_val == 0)
--       then do
--         -- yield
--         threadDelay (millisec 200)
--         loop iptr dptr
--       else do
--         cchar <- peekElemOff dptr 0
--         putStrLn $ "Haskell Land data : " <> (show cchar)
--         free iptr
--         free dptr

  -- let inputBytes = B.pack [1, 2, 3, 4, 11]
  {- Binary's encode is interesting
     It uses 1 word to store the length of the data about to come
     1 word is 8 bytes in this machine;
     Hence its encode is platform dependent
  -}
  -- let inputBytes = BL.toStrict $ encode baz
  -- B.useAsCStringLen inputBytes $ \(ptr, len) -> do
  --   cByteStr <- processByteArray ptr (fromIntegral len)
  --   putStrLn "Haskell Land"
  --   l <- byteStrLength cByteStr
  --   byteString <- B.packCStringLen (cByteStr `plusPtr` 8, l) -- XXX: not portable 8 bytes for this machine
  --   c_free cByteStr
  --   let result = decode (BL.fromStrict byteString) :: [Foo]
  --   putStrLn $ "Hs to C and back : " <> (show result)



  -- B.useAsCString inputBytes $ \ptr -> do
  --   cByteStr   <- processByteArrayDyn ptr
  --   putStrLn "Haskell Land"
  --   byteString <- B.packCStringLen (cByteStr, 19)
  --   let result = decode (B.fromStrict byteString) :: [Foo]
  --   putStrLn $ "Hs to C and back : " <> (show result)

{- LINEAR HASKELL primer

{-# LANGUAGE LinearTypes #-}

import Prelude.Linear

g :: (a %1-> b) -> a -> b
g f x = body -- f can be used at most once in the body
  where
   body = f x

-}
