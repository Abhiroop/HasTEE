{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
module SecIO where

import Control.Monad
import Lattice
import Sec

newtype SecIO s a = MkSecIO (IO (Sec s a))

instance Functor (SecIO s) where
  fmap = liftM

instance Applicative (SecIO s) where
  pure = MkSecIO . pure . pure
  (MkSecIO ab) <*> (MkSecIO a) = MkSecIO $ do
    secab <- ab
    seca  <- a
    pure (secab <*> seca)

instance Monad (SecIO s) where
  return = pure

  MkSecIO m >>= k =
    MkSecIO $ do
      sa <- m
      let MkSecIO iosecsecb = k (reveal sa)
      secsecb <- iosecsecb
      return secsecb

      -- let MkSecIO iosecsecb = flipper $ fmap k sa
      -- secsecb <- iosecsecb
      -- return $ join secsecb

{-@ Attempt to break the Sec monad but
    had to use unsafePerformIO @-}
-- flipper :: Sec s (SecIO s a) -> SecIO s (Sec s a)
-- flipper secsio = fmap (unsafePerformIO . run) $ value secsio



{-@ Look at protected value in the current
    security level @-}
value :: Sec s a -> SecIO s a
value sa = MkSecIO $ pure sa


run :: SecIO s a -> IO (Sec s a)
run (MkSecIO iosa) = iosa

plug :: forall sl sh a . Less sl sh => SecIO sh a -> SecIO sl (Sec sh a)
plug (MkSecIO m) = less @sl @sh `seq` ss_sl
  where
    ss_sl :: SecIO sl (Sec sh a)
    ss_sl = MkSecIO $ do
                sha <- m :: IO (Sec sh a) -- type sig not needed
                                          -- this part does the side effect
                                          -- but does not reveal the secret
                return (sec sha)

newtype File s = MkFile FilePath

{-@ when read from a file at security level s'
    the secret is also at level s' so Sec s' String;
    now you want to expose this protected secret
    at any security level s. If they are at the right
    security level they should be able to open the monad @-}
readSecIO :: File s' -> SecIO s (Sec s' String)
readSecIO (MkFile file) =
  MkSecIO $ fmap (sec . sec) (readFile file) -- :: IO Sec s (Sec s' String)

writeSecIO :: File s -> String -> SecIO s ()
writeSecIO (MkFile file) = MkSecIO . fmap sec . writeFile file


s_read :: Less sl sh => File sl -> SecIO sh String
s_read file = do
  sl_str <- readSecIO file -- :: SecIO sh (Sec sl String)
  value $ up sl_str

{-@ write to a high security file like the shadow pwd while
    residing in a low security env like your local user @-}
s_write :: Less sl sh => File sh -> String -> SecIO sl (Sec sh ())
s_write file str = plug $ writeSecIO file str
