{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
module Sec (Sec, sec, open, up
           , reveal -- only for trusted code
           )where

import Control.Monad
import Data.IORef
import Lattice

newtype Sec s a = MkSec a

instance Monad (Sec s) where
  return = pure

  MkSec a >>= k =
    MkSec $ let MkSec b = k a in b

instance Functor (Sec s) where
  fmap = liftM

instance Applicative (Sec s) where
  pure = sec
  MkSec ab <*> MkSec a = MkSec (ab a)


--used to protect value `a`
sec :: a -> Sec s a
sec = MkSec

-- look at a protected value given
-- that you can produce a security
-- level `s`
open :: Sec s a -> s -> a
open (MkSec a) s = s `seq` a

up :: forall a sl sh . Less sl sh => Sec sl a -> Sec sh a
up (MkSec x) = (less @sl @sh) `seq` sech
  where
    sech = (MkSec x)

{-

the original modifcation to the
paper to make the code compile

up' :: forall a sl sh . Less sl sh => Sec sl a -> Sec sh a
up' secl@(MkSec x) = (less sl sh) `seq` sech
  where
    sech :: Sec sh a
    sech = (MkSec x)
    sl = unsecType secl
    sh = unsecType sech

unsecType :: Sec s a -> s
unsecType _ = undefined
-}

-- only for trusted code
reveal :: Sec s a -> a
reveal (MkSec x) = x


{-@ Declassification @-}

type Hatch sh sl a b = Sec sh a -> IO (Maybe (Sec sl b))
--                                 ^     ^
--                                 |     |
--                                 |  sometimes declassification maybe impossible
--                           run time checks

hatch :: Less sl sh => (a -> b) -> Hatch sh sl a b
hatch f = return . Just . return . f . reveal

ntimes :: Int -> Hatch sh sl a b -> IO (Hatch sh sl a b)
ntimes n f = do
  ref <- newIORef n
  return (\sa -> do
             k <- readIORef ref
             if (k <= 0)
             then return Nothing
             else do
                writeIORef ref (k - 1)
                f sa
         )
