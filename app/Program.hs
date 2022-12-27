{-# OPTIONS_GHC -Wno-missing-methods #-}
module Program where

import Data.Char
import Lib
import Sec
import SecIO

{-@
 Non-interferent; Public output is not influenced
 by private
@-}
f1 :: (Char, Int) -> (Char, Int)
f1 (c, i) = (chr (ord c + i), i + 3)

{-@
 Information Leak; Public output is
 influenced by private
@-}
f2 :: (Char, Int) -> (Char, Int)
f2 (c, _) = (c, ord c)

{-@
 Information Leak; Public output is
 influenced by private
@-}
f3 :: (Char, Int) -> (Char, Int)
f3 (c, _) = (c, if ord c > 31 then 1 else 0)

data H = H

f4 :: (Sec H Char, Int) -> (Sec H Char, Int)
f4 (sc, i) = (fmap ordinc sc, i + 3)
  where
    ordinc = \c -> chr (ord c + i)

{-@ The `open` function needs to be restricted
    in terms of who can open it.
@-}
f5 :: (Sec H Char, Int) -> (Sec H Char, Int)
f5 (sc, _) = (sc, open (fmap ord sc) H)



file1, file2 :: File s
file1 = undefined
file2 = undefined
writeToAFile :: Sec H String -> Sec H (SecIO H ())
writeToAFile secs =
  (\s -> if length s < 10
         then writeSecIO file1 s
         else writeSecIO file2 s) `fmap` secs


foo = do
  let secret = undefined :: Sec H String
  let secStr = readSecIO file1
  writeToAFile secret
--  let secStr2 = readSecIO file2
