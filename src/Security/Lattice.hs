{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unused-top-binds#-}
module Security.Lattice (H(), L(..), Less, less) where

#ifdef ENCLAVE
data L = L
data H = H
#else
data L = L
data H
#endif


class Less sl sh where
  less :: sl -> sh -> ()

instance Less L L where
  less _ _ = ()

instance Less L H where
  less _ _ = ()

instance Less H H where
  less _ _ = ()

-- no Less H L as that would break non-interference

