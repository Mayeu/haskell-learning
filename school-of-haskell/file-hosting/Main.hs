{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent.STM
import Data.IntMap

import Yesod

import Foundation
import Dispatch ()

main :: IO ()
main = do
    tstore <- atomically $ newTVar empty
    tident <- atomically $ newTVar 0
    warp 3000 $ App tident tstore
