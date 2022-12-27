{-# LANGUAGE StaticPointers #-}
module Main (main) where

import GHC.StaticPtr

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

main = do
  let sptr :: StaticPtr (Int -> Int)
      sptr = static fact
  print $ staticPtrInfo sptr
  print $ deRefStaticPtr sptr 10

-- main :: IO ()
-- main = putStrLn "Hello World"
