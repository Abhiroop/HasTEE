{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import App
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Prelude hiding (lookup)


#ifdef ENCLAVE
import Enclave
#else
import Client
#endif

type Price = Int
type Day = Int
type BookStore = Map String (Price, Day)
storeLookup :: Enclave (Ref BookStore) -> String -> Enclave (Price, Day)
storeLookup secBooks book = do
    booksRef <- secBooks         :: Enclave (Ref BookStore)
    books    <- readRef booksRef :: Enclave BookStore
    return . fromJust $ Map.lookup book books

getPrice :: Enclave (Ref BookStore) -> String -> Enclave Price
getPrice books book = do
    (price,_ ) <- storeLookup books book
    return price

getDay :: Enclave (Ref BookStore) -> String -> Enclave Day
getDay books book = do
    (_, deliveryDate) <- storeLookup books book
    return deliveryDate

defaultBooks :: BookStore
defaultBooks =
    Map.fromList
        [ ("Types and Programming Languages", (80, 20221219))
        , ("Homotopy Type Theory", (120, 20230101))
        ]

clientBudget :: Int
clientBudget = 100

bookseller :: App Done
bookseller = do
    remoteRef <- liftNewRef defaultBooks        :: App (Enclave (Ref BookStore))
    getPrice  <- inEnclave (getPrice remoteRef) :: App (Secure (String -> Enclave Price))
    getDay    <- inEnclave (getDay remoteRef)   :: App (Secure (String -> Enclave Day))

    runClient $ do
        liftIO    $ putStrLn "Enter the title of the book to buy"
        userInput <- liftIO getLine
        price     <- gateway (getPrice <@> userInput) :: Client Price

        if price < clientBudget then do
            day    <- gateway (getDay <@> userInput) :: Client Day
            liftIO $ putStrLn $ "The book will be delivered on " <> show day
        else do
            liftIO $ putStrLn "The book's price is out of the budget!"

main :: IO ()
main = do
    res <- runApp bookseller
    return $ res `seq` ()
