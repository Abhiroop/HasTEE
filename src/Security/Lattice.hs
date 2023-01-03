{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unused-top-binds#-}
module Security.Lattice (L, Less, less) where

data L = L
data H = H

class Less sl sh where
  less :: sl -> sh -> ()

instance Less L L where
  less _ _ = ()

instance Less L H where
  less _ _ = ()

instance Less H H where
  less _ _ = ()

-- no Less H L as that would break non-interference

