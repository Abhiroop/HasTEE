module Main where

import System.Directory
import System.Process
import System.Exit

main :: IO ()
main = compileEnclave "EnclaveLeak"--putStrLn "Test suite not yet implemented"

app :: String -> IO ()
app target = undefined

allExamples :: IO [String]
allExamples = undefined

exampleFolder :: String
exampleFolder = "example/"

compileEnclave :: String -> IO ()
compileEnclave target = do
    let exe   = target <> "-exe"
        flags = [exe, "-f testing", "-f enclave", "--project-file=cabal-nosgx.project"]
    (code, out, _) <- readCreateProcessWithExitCode (proc "cabal build" flags) []
    case code of
        ExitSuccess -> putStrLn "compilation OK"
        ExitFailure n -> do putStrLn $ "compilation failed with code: " ++ show n
                            putStrLn out

runClient :: IO ()
runClient = undefined

runServer :: IO ()
runServer = undefined

givenInput :: IO String
givenInput = undefined

expectedOutput :: IO String
expectedOutput = undefined
