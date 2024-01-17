{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List
import qualified Data.Vector as V
import qualified Data.Text as T
import Text.HTML.Scalpel
import GHC.Generics (Generic)

-- Scrape h1 text and class attribute
scraper :: IO (Maybe T.Text)
scraper = scrapeURL "https://en.wikipedia.org/wiki/List_of_largest_cities" heading
  where
    heading :: Scraper T.Text T.Text
    heading = do
      headingText <- text "h1"
      headingAttr <- attr "class" "h1"
      pure $ "Attr: " <> headingAttr <> " | Text: " <> headingText

-- Run scraper
main1 :: IO ()
main1 = do
  result <- scraper
  case result of
    Just x -> print x
    Nothing -> print "Didn't find the necessary items"


-- Main example

data City = City 
  { name :: T.Text
  , country :: T.Text
  , population :: T.Text
  }
  deriving (Show, Generic)

-- To create CSV file
-- Using deriving with generics doesn't allow you to use capitals in the header, 
-- so replaced generic deriving with manual

-- instance ToNamedRecord City 

instance ToNamedRecord City where
  toNamedRecord City{..} =
    namedRecord
      [ "Name" .= name
      , "Country" .= country
      , "Population" .= population
      ]


allCities :: IO (Maybe ([City]))
allCities = scrapeURL "https://en.wikipedia.org/wiki/List_of_largest_cities" table
  where

    table :: Scraper T.Text [City]
    table = chroot ("table" @: [hasClass "static-row-numbers"]) cities

    cities :: Scraper T.Text [City]
    cities =
      chroots ("tr" @: [notP (hasClass "static-row-header")]) $ do
      contents <- html "th"
      guard (not ("style=" `T.isInfixOf` contents))
      city

    city :: Scraper T.Text City
    city = do
      name <- text "th"
      rows <- texts "td"
      let country = getCountry rows
      let population = getPopulation rows
      return $ City (T.strip name) country population

    getCountry (x:xs) = T.strip x
    getCountry (_) = "Not available"

    getPopulation (x:y:xs) = T.strip y
    getPopulation (_) = "Not available"

writeToFile :: [City] -> IO ()
writeToFile cities = do
  BL.writeFile "cities.csv" $  encodeByName (V.fromList ["Name", "Country", "Population"]) cities

main :: IO ()
main = do
  result <- allCities
  case result of
    Just x -> do 
      print x
      writeToFile x
    Nothing -> print "Didn't find the necessary items"



