if [ $# -eq 0 ]
  then cabal run EnclaveIFC-exe --project-file=cabal-nosgx.project
  else echo "$@" | cabal run EnclaveIFC-exe --project-file=cabal-nosgx.project
fi
