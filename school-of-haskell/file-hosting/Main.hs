{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent.STM

import Yesod

import Foundation
import Dispatch ()

main :: IO ()
main = do
    tfilenames <- atomically $ newTVar []
    warp 3000 $ App tfilenames
