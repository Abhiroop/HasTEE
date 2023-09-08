## DESIGN

### Line of defence in the enclaves

1. No I/O; it is not possible to do I/O in the Enclave monad
2. Sec values; Only enclave can declassify Sec values
3. Safe Haskell? Should we compile Enclave.hs with `{-# SafeHaskell #-}` to prevent unsafePerformIO, unsafeCoerce etc?
4. `ntimes` hatch; cap the number of times the enclave can be called

## Old discussion

Lets take an example. Say the password checker

```haskell
pwdChkr :: Enclave String -> String -> Enclave Bool
pwdChkr pwd guess = fmap (== guess) pwd


passwordChecker :: App Done
passwordChecker = do
  paswd <- liftEnclaveIO (return "secret") :: App (Enclave String)
  enclaveFunc <- inEnclave $ pwdChkr paswd
  runClient $ do
    liftIO $ putStrLn "Enter your password"
    userInput <- liftIO getLine
    res <- gateway (enclaveFunc <@> userInput)
    liftIO $ putStrLn $ "Your login attempt returned " <> (show res)
```

What can go wrong?

```haskell
pwdChkr pwd guess = do
   p <- pwd
   liftIO $ sendToWorld p -- the attack (think of the offline dictionary attack)
   return (guess == p)
```

In the enclave attack model we trust the enclave to do the right thing. As we fully trust the enclave, the Enclave monad is like `MAC H`. Maybe, like MAC H, we should restrict the Enclave monad from doing bad IO.

Enclave should be a restrictive IO monad where you can do very basic things. In SIO, we track data with different labels, that is not the case in Enclave because all data in 
Enclave Monad is H.

We need two things
- First we need to add escape hatches or declassifiers within the Enclave monad. A naive type signature could be:

```
gateway :: Secure (Enclave a) -> Client (Maybe a)
```

 The `Maybe` exists for the fact that occasionally it might be impossible to declassify data.
 
- We also need a restricted IO monad. One from which IO calls like the ones shown in the attack are not possible.



1. average calculation of salaries; clients are threads and the enclave is the enclave that exposes only the average
2. password checker
