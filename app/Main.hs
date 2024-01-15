{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Text.HTML.Scalpel

scraper :: IO (Maybe T.Text)
scraper = scrapeURL "https://en.wikipedia.org/wiki/List_of_largest_cities" heading
  where
    heading :: Scraper T.Text T.Text
    heading = do
      headingText <- text "h1"
      headingAttr <- attr "class" "h1"
      pure $ "Attr: " <> headingAttr <> " | Text: " <> headingText
       

main :: IO ()
main = do
  result <- scraper
  case result of
    Just x -> print x
    Nothing -> print "Didn't find the necessary items"
  