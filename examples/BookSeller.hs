{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import App
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time
import System.Exit (exitFailure)
import Prelude hiding (lookup)

import Client

dayToInt :: Day -> Integer
dayToInt = toModifiedJulianDay

intToDay :: Integer -> Day
intToDay = ModifiedJulianDay

type BookStore = Map String (Int, Day)
storeLookup :: Enclave (Ref BookStore) -> String -> Enclave (Maybe (Int, Day))
storeLookup books book = do
    lookup <- flip Map.lookup <$> (readRef =<< books)
    case lookup book of
        Just res -> pure $ Just res
        Nothing -> pure Nothing

getPrice :: Enclave (Ref BookStore) -> String -> Enclave (Maybe Int)
getPrice books book = fmap fst <$> storeLookup books book

getDay :: Enclave (Ref BookStore) -> String -> Enclave (Maybe Integer)
getDay books book = fmap (dayToInt . snd) <$> storeLookup books book

defaultBooks :: BookStore
defaultBooks =
    Map.fromList
        [ ("Types and Programming Languages", (80, fromGregorian 2022 12 19))
        , ("Homotopy Type Theory", (120, fromGregorian 2023 01 01))
        ]

-- Maybe remove this and let app have a state?
budget :: Int
budget = 100

bookseller :: App Done
bookseller = do
    remoteRef <- liftNewRef defaultBooks :: App (Enclave (Ref BookStore))
    getPrice <- inEnclave $ getPrice remoteRef
    getDay <- inEnclave $ getDay remoteRef
    runClient $ do
        liftIO $ putStrLn "Enter the title of the book to buy"
        userInput <- liftIO getLine
        priceRes <- gateway (getPrice <@> userInput)
        price <- liftIO $ maybe (putStrLn "non-existent book" >> exitFailure) pure priceRes

        if price < budget
            then do
                day <- intToDay . fromJust <$> gateway (getDay <@> userInput)
                -- should this modify the enclave state, removing the book from the list?
                liftIO . putStrLn $ "The book will be delivered on " <> show day
            else do
                liftIO $ putStrLn "The book's price is out of the budget!"

main :: IO ()
main = do
    res <- runApp bookseller
    return $ res `seq` ()
