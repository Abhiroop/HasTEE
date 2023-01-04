## DESIGN

Lets take an example. Say the password checker

```haskell
pwdChkr :: Server String -> String -> Server Bool
pwdChkr pwd guess = fmap (== guess) pwd


passwordChecker :: App Done
passwordChecker = do
  paswd <- liftServerIO (return "secret") :: App (Server String)
  serverFunc <- remote $ pwdChkr paswd
  runClient $ do
    liftIO $ putStrLn "Enter your password"
    userInput <- liftIO getLine
    res <- onServer (serverFunc <.> userInput)
    liftIO $ putStrLn $ "Your login attempt returned " <> (show res)
```

What can go wrong?

```haskell
pwdChkr pwd guess = do
   p <- pwd
   sendToWorld p -- the attack (think of the offline dictionary attack)
   return (guess == p)
```

In the enclave attack model we trust the enclave to do the right thing. As we fully trust the enclave, the Server monad is like `MAC H`. Maybe, like MAC H, we should restrict the Server monad from doing bad IO.

Server should be a restrictive IO monad where you can do very basic things. In SIO, we track data with different labels, that is not the case in Server because all data in 
Server Monad is H.

We need two things
- First we need to add escape hatches or declassifiers within the Server monad. A naive type signature could be:

```
onServer :: Remote (Server a) -> Client (Maybe a)
```

 The `Maybe` exists for the fact that occasionally it might be impossible to declassify data.
 
- We also need a restricted IO monad. One from which IO calls like the ones shown in the attack are not possible.

