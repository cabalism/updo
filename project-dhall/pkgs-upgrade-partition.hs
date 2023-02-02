#!/usr/bin/env stack
{- stack --resolver lts-18.27 script
   --package dhall
   --package text
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List ((\\), intersect)
import Dhall (Text, Generic, FromDhall, ToDhall, input, auto, embed, inject)
import Dhall.Core (pretty)
import System.Environment (getArgs)
import qualified Data.Text as T (pack)
import qualified Data.Text.IO as T (putStrLn)

data PkgUpgrade =
    PkgUpgrade
        { pkgs :: [Text]
        , done :: [Text]
        , todo :: [Text]
        }
    deriving (Show, Generic, ToDhall, FromDhall)

main :: IO ()
main = do
    file : _ <- getArgs
    pkgs :: [Text] <- input auto (T.pack file)
    remaining :: [Text] <- input auto "./project-dhall/pkgs-upgrade-todo.dhall"
    let todo = pkgs `intersect` remaining
    let done = pkgs \\ todo
    T.putStrLn . pretty $ embed inject PkgUpgrade{..}