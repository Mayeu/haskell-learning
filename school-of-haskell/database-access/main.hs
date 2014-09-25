{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Data.Text (Text)
--import Database.Persist
import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
                            share, sqlSettings)

-- imports for dumpTable
import Database.Persist.Sql (rawQuery, insert)
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Author
  name     Text
  email    Text
  EmailKey email
  deriving Show
Tutorial
  title    Text
  url      Text
  school   Bool
  author   AuthorId
  deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    buildDb
    -- Playing with select
    -- tuts <- select $ from $ \(a, t) -> do
    --   where_ (t ^. TutorialAuthor ==. a ^. AuthorId)
    --   groupBy (a ^. AuthorId)
    --   let cnt = countRows :: SqlExpr (Value Int)
    --   orderBy [desc cnt]
    --   return (a ^. AuthorEmail, cnt)
    --
    -- Simple delete
    -- delete $ from $ \t ->
    --   where_ (t ^. TutorialTitle ==. val "Basic Haskell" &&. t ^. TutorialSchool ==. val False)
    --
    -- Delete every tuts from an author
    -- delete $ from $ \t -> do
    --   where_ $ (t ^. TutorialAuthor) ==.
    --            (sub_select $ from $ \a -> do
    --                where_ (a ^. AuthorEmail ==. val "anne@example.com")
    --                return (a ^. AuthorId))
    -- tuts <- select $ from $ \t -> do
    --   where_ (t ^. TutorialSchool !=. val True)
    --   return (t ^. TutorialTitle)
    -- liftIO $ print tuts
    update $ \a -> do
      set a [AuthorEmail =. val "anna@example.com"]
      where_ (a ^. AuthorEmail ==. val "anne@example.com")
    auths <- select $ from $ \a -> return a
    liftIO $ mapM_ (print . authorEmail . entityVal) auths

buildDb = do
    runMigrationSilent migrateTables{-hi-}
    school <- insert $ Author "School of Haskell" "school@example.com"
    anne <- insert $ Author "Ann Author" "anne@example.com"
    insert $ Tutorial "Basic Haskell" "https://fpcomplete.com/school/basic-haskell-1" True school
    insert $ Tutorial "A monad tutorial" "https://fpcomplete.com/user/anne/monads" False anne
    insert $ Tutorial "Yesod usage" "https://fpcomplete.com/school/basic-yesod" True school
    insert $ Tutorial "Putting the FUN in functors" "https://fpcomplete.com/user/anne/functors" False anne
    insert $ Tutorial "Basic Haskell" "https://fpcomplete/user/anne/basics" False anne
