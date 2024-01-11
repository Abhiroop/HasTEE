
# HasTEE⁺

A Haskell DSL for programming Trusted Execution Environments (TEEs). 

See `app/Main.hs` and the `examples` directory for sample programs.

### Papers on HasTEE⁺:

[HasTEE: Programming Trusted Execution Environments with Haskell](https://dl.acm.org/doi/10.1145/3609026.3609731) - Version 1 of the DSL.

HasTEE⁺: Confidential Cloud Computing and Analytics with Haskell - Submitted to ESORICS 2024, ArXiV link coming soon. Version 2 of the DSL.


### Building

#### SGX Machine Dependencies

For running on Intel SGX-enabled machines there are two complex dependencies. Note the DSL can be run without these two dependencies on standard machines (but not on SGX). Read past the two bullets for standard non-SGX setup.

- The remote attestation infrastructure of HasTEE⁺ relies on the ARM MbedTLS implementation of TLS 1.2. This implementation implements [Intel's RA-TLS protocol](https://arxiv.org/pdf/1801.05863.pdf). Experiments have been conducted on mbedtls version 3.2.1. Available [here](https://github.com/Mbed-TLS/mbedtls/tree/v3.2.1) and [here](https://packages.gramineproject.io/distfiles/mbedtls-3.2.1.tar.gz). `mbedtls` functions as a C library and there is some setup involved explained here ().
- Trusted GHC - A patched GHC runtime capable of running on Intel SGX machines. Available on request.

NOTE: The current `cabal.project` expects the trusted GHC at a particular location. For building this on your local machine that doesn't have SGX or the custom GHC, use - `cabal build --project-file=cabal-nosgx.project`.

The executable supports conditional compilation and can compile into 2 binaries
#### Using cabal
```
-- For the enclave
cabal run -f enclave

-- For the client
cabal run
```

Follow the above order - run enclave first and then the client. The enclave is stateful and can be tested by running the enclave first and then calling the client repeatedly for the program in `Main.hs`.


#### Installed Binary Location

```
cabal exec which EnclaveIFC-exe
```

#### Client integrity check
Enabled with `-fintegrity-check`. Disabled by default. Works with the `mbed-tls` version.


#### Using stack

LATEST: DO NOT USE `stack`. Stick to `cabal`, all recent developments have been done with `cabal`

Very hard (or impossible) to make the latest `stack` pick up a custom GHC because of the snapshot mechanism (perhaps that requires all the necessary packages be compiled with the custom GHC). Approaches in this thread https://github.com/commercialhaskell/stack/issues/725#issuecomment-364624897 are no longer functional in the latest `stack` incarnations.

```
-- Build and run the enclave (the flag is called `enclave`)
stack build --flag EnclaveIFC:enclave
.stack-work/install/x86_64-linux-tinfo6/16c183811171455bbb9119194450e5a4a4679f74605e9f4e1a47fbd54088f2b5/9.2.5/bin/EnclaveIFC-exe

-- Build and run the client (default build is for client)
stack run EnclaveIFC-exe
```

