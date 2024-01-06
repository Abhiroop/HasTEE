diff --git a/EnclaveIFC.cabal b/EnclaveIFC.cabal
-    , crypton
+    , cryptonite
diff --git a/app/Main.hs b/app/Main.hs
index dac6e03..201983c 100644
--- a/app/Main.hs
+++ b/app/Main.hs
@@ -12,6 +12,7 @@ import Data.List (groupBy, sortBy)
 import qualified Data.ByteString as B
+import qualified Data.ByteString.Lazy as BL
 
@@ -134,7 +135,7 @@ runQuery enc_ref_db priv1 priv2  = do
-  res_enc      <- liftIO $ encrypt pubK (B.toStrict $ encode $ query1 rows)
+  res_enc      <- liftIO $ encrypt pubK (BL.toStrict $ encode $ query1 rows)
@@ -202,7 +203,7 @@ client3 api = do
-      let result = decode (B.fromStrict bytestr) :: Result
+      let result = decode (BL.fromStrict bytestr) :: Result
diff --git a/src/Client.hs b/src/Client.hs
index d0dd6c7..83fae19 100644
--- a/src/Client.hs
+++ b/src/Client.hs
@@ -252,7 +252,7 @@ raTryEnclave :: (Label l, Binary a, KnownSymbol loc)
-  withCString "native" $ \cstring -> do
+  withCString "epid" $ \cstring -> do
