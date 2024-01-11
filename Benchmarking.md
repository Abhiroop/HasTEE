For running the benchmarks on an actual SGX machine, the following fixes are required (very minimal):

```
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
```

The trusted GHC runtime is a patched version of GHC 8.8 and the `crypton` package is a later fork of `cryptonite`. So, it's better to switch to the old `cryptonite`. The last change uses EPID-based remote attestation.

For reproducing the benchmarks in paper 2, simply substitute `app/Main.hs` with [Benchmark.hs](https://github.com/Abhiroop/HasTEE/blob/master/app/Benchmark.hs). This depends on the `clock` package seen above and implements a timing combinator to report the API timing in milliseconds.

#### mbedtls setup

In the root of the project, creating a directory named `ssl`. Place this `ca_config.conf` file inside `ssl`:

```
[ req ]
default_bits       = 4096
default_md         = sha512
default_keyfile    = example.com.key
prompt             = no
encrypt_key        = no
distinguished_name = req_distinguished_name

[ req_distinguished_name ]
countryName            = "XX"             # C=
localityName           = "XXXXX"          # L=
organizationName       = "My Company"     # O=
organizationalUnitName = "Department"     # OU=
commonName             = "localhost"      # CN=
emailAddress           = "me@example.com" # email
```

Place the `mbedtls-3.2.1.tar.gz` in the folder `cbits`. Extract it and run the following from the root of the project.

```
        openssl genrsa -out ssl/ca.key 2048
        openssl req -x509 -new -nodes -key ssl/ca.key -sha256 -days 1024 -out ssl/ca.crt -config ssl/ca_config.conf
        openssl genrsa -out ssl/server.key 2048
        openssl req -new -key ssl/server.key -out ssl/server.csr -config ssl/ca_config.conf
        openssl x509 -req -days 360 -in ssl/server.csr -CA ssl/ca.crt -CAkey ssl/ca.key -CAcreateserial -out ssl/server.crt
```

