
# EnclaveIFC

Can we use IFC policies to stop enclave programs from doing silly things?

### Building

NOTE: For building this on any machine, remove the `cabal.project` file. The current `cabal.project` expects the trusted GHC at a particular location.

The executable supports conditional compilation and can compile into 2 binaries
#### Using cabal
```
-- For the server
cabal run -f enclave

-- For the client
cabal run
```

#### Using stack
```
-- Build and run the server (the flag is called `enclave`)
stack build --flag EnclaveIFC:enclave
.stack-work/install/x86_64-linux-tinfo6/16c183811171455bbb9119194450e5a4a4679f74605e9f4e1a47fbd54088f2b5/9.2.5/bin/EnclaveIFC-exe

-- Build and run the client (default build is for client)
stack run EnclaveIFC-exe
```

Follow the above order - run server first and then the client. The server is stateful and can be tested by running the server first and then calling the client repeatedly for the program in `Main.hs`.

#### Installed Binary Location

```
cabal exec which EnclaveIFC-exe
```
