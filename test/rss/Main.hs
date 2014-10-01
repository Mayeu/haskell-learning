module Main where

import Data.Maybe

import Text.Feed.Import
import Text.Feed.Query

import System.Environment

main :: IO ()
main = do
  (x:_) <- getArgs
  feed  <- parseFeedFromFile x
  let items = getFeedItems feed
      item = head items
  putStrLn $ fromMaybe "Nothing. Lol" $ getItemDescription item
