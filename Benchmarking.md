diff --git a/EnclaveIFC.cabal b/EnclaveIFC.cabal
   if (flag(integrity-check))
     cpp-options: -DINTEGRITY
-    build-depends: crypton
+    build-depends: cryptonite
   else
     cpp-options: -DUMMY
 
     , bytestring
+    , clock
     , containers
-    , crypton
+    , cryptonite
     , network-simple



diff --git a/src/Client.hs b/src/Client.hs
-  withCString "native" $ \cstring -> do
+  withCString "epid" $ \cstring -> do

