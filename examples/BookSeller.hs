{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import App
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Prelude hiding (lookup)

import Client

type Price = Int
type Day = Int
type BookStore = Map String (Price, Day)
storeLookup :: Enclave (Ref BookStore) -> String -> Enclave (Price, Day)
storeLookup books book =
    fromJust . Map.lookup book
        <$> (readRef =<< books)

getPrice :: Enclave (Ref BookStore) -> String -> Enclave Price
getPrice books book = fst <$> storeLookup books book

getDay :: Enclave (Ref BookStore) -> String -> Enclave Day
getDay books book = snd <$> storeLookup books book

defaultBooks :: BookStore
defaultBooks =
    Map.fromList
        [ ("Types and Programming Languages", (80, 20221219))
        , ("Homotopy Type Theory", (120, 20230101))
        ]

-- Maybe remove this and let app have a state?
budget :: Int
budget = 100

bookseller :: App Done
bookseller = do
    remoteRef <- liftNewRef defaultBooks        :: App (Enclave (Ref BookStore))
    getPrice  <- inEnclave (getPrice remoteRef) :: App (Secure (String -> Enclave Price))
    getDay    <- inEnclave (getDay remoteRef)   :: App (Secure (String -> Enclave Day))
    runClient $ do
        liftIO $ putStrLn "Enter the title of the book to buy"
        userInput <- liftIO getLine
        price <- gateway (getPrice <@> userInput) :: Client Price

        if price < budget
            then do
                day <- gateway (getDay <@> userInput) :: Client Day
                -- should this modify the enclave state, removing the book from the list?
                liftIO . putStrLn $ "The book will be delivered on " <> show day
            else do
                liftIO $ putStrLn "The book's price is out of the budget!"

main :: IO ()
main = do
    res <- runApp bookseller
    return $ res `seq` ()
