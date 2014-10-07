module Main where

import Data.Maybe
import Data.List

import Text.Feed.Import
import Text.Feed.Export
import Text.Feed.Query
import Text.Feed.Types
import Text.Feed.Constructor

import Text.XML.Light
import Text.XML.HXT.Core

import Text.HandsomeSoup

import System.Environment
import System.IO.Unsafe

main :: IO ()
main = do
  (x:_) <- getArgs
  feed  <- parseFeedFromFile x
  let items = getFeedItems feed
  let urls = map getUrl items
  imgs <- mapM getComic urls
  let newFeed = withFeedItems (insertComics imgs items) feed
  --mapM_ putStrLn imgs
  writeFile "newFeed.xml" (ppTopElement $ xmlFeed newFeed)

--expandFeed :: Feed -> Feed
--expandFeed feed = withFeedItems (insertComics items) feed
--  where items = getFeedItems feed

insertComics :: [String] -> [Item] -> [Item]
insertComics urls its = map expandItem $ zip urls its

expandItem :: (String, Item) -> Item
expandItem (url, it) = withItemDescription newDesc it
  where
      --oldDesc = fromJust $ getItemDescription it
      newDesc = updateDescription url it

updateDescription :: String -> Item -> String
updateDescription url it =
    let desc = fromJust $ getItemDescription it
        doc = readString [] desc
    in head $ unsafePerformIO $ runX . xshow $
         doc >>> processTopDown (ifA (hasName "a") (injectImage url) this)

--injectImage :: String -> String
injectImage url = replaceChildren ( imgElement <+> getChildren )
  where imgElement = mkelem "img" [ sattr "src" url ] []

getUrl :: Item -> String
getUrl = fromJust . getItemLink

-- Get the comic at an url
getComic :: String -> IO String
getComic url = do let doc = fromUrl url
                  img <- runX $ doc >>> css "img#comic" ! "src"
                  return $ head img

-- Dumb function change all authors to a new one
changeAuthors :: Feed -> String -> Feed
changeAuthors feed author = withFeedItems (mapAuthor $ getFeedItems feed) feed
  where mapAuthor = map (withItemAuthor author)
