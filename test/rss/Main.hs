module Main where

import Data.Maybe

import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Text.Feed.Constructor

import System.Environment

main :: IO ()
main = do
  (x:_) <- getArgs
  feed  <- parseFeedFromFile x
  -- Change all authors of the feed to Mayeu
  let myFeed = changeAuthors feed "Mayeu"
  -- just print all author to check if it is working
  mapM_ ((putStrLn . fromMaybe "Nothing") . getItemAuthor) $ getFeedItems myFeed

-- Dumb function change all authors to a new one
changeAuthors :: Feed -> String -> Feed
changeAuthors feed author = withFeedItems (mapAuthor $ getFeedItems feed) feed
  where mapAuthor = map (withItemAuthor author)
