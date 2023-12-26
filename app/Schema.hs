{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Schema where

import Data.Typeable

import qualified Data.ByteString as B

import App
import DCLabel
#ifdef ENCLAVE
import Enclave
#else
import Client
#endif

-- check binary-typed https://hackage.haskell.org/package/binary-typed provides
-- typed :: Typeable a => TypeFormat -> a -> Typed a
-- Typed a is serializable
-- Then there is erase :: Typed a -> a -- loses type information
-- No Proxies needed
-- Typed a allows serlializing TypeRep by removing the unserializable parts

{-@

check out https://hackage.haskell.org/package/hashabler-1.2.1

Very tediously written hand-crafted implementation of hashing types

@-}





foo :: B.ByteString
foo = let intTypeRep = typeOf (1 :: Int)
       in B.toStrict $ encode (show intTypeRep)

bar :: B.ByteString -> Maybe (Proxy Int)
bar typerepBytstr = reifyCol inttyperep
  where
    inttyperep    = undefined -- read inttyperepstr :: TypeRep (not allowed)
    inttyperepstr = decode (B.fromStrict typerepBytstr) :: String

reifyCol :: Typeable a => TypeRep -> Maybe (Proxy a)
reifyCol rep = cast rep

-- fixed size scheme to begin with
schema2 :: Typeable a => Proxy a -> Proxy a -> Proxy (a, a)
schema2 Proxy Proxy = Proxy
