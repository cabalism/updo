#!/usr/bin/env stack
-- stack script --resolver=lts-18.27 --package=base --package=dhall --package=filepath --package=text

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Description: Convert each package group from .dhall to .config with upgrade
-- conditionals if needed.
module Main where

import Data.List (concat, filter, intersect, sortOn, (\\))
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.IO as T (putStrLn)
import Dhall (FromDhall, Generic, Text, ToDhall, auto, embed, inject, input)
import Dhall.Core (pretty)
import System.Environment (getArgs)
import System.FilePath ((<.>), (</>))

type UpgradeConfig = Text -> Text -> PkgUpgrade -> Text

data PkgUpgrade = PkgUpgrade
    { pkgs :: [Text]
    , done :: [Text]
    , todo :: [Text]
    }
    deriving (Show, Generic, ToDhall, FromDhall)

groupUpgrade :: [Text] -> String -> IO PkgUpgrade
groupUpgrade remaining name = do
    pkgs :: [Text] <- input auto (T.pack $ "./project-dhall/pkgs" </> name <.> "dhall")
    let todo = pkgs `intersect` remaining
    let done = pkgs \\ todo
    return PkgUpgrade{..}

main :: IO ()
main = do
    ghcVersion : ghcUpgrade : _ <- getArgs
    remaining :: [Text] <- input auto "./project-dhall/pkgs-upgrade-todo.dhall"
    pkgUpgrade :: UpgradeConfig <- input auto "./updo/project-dhall/pkgs-upgrade.dhall"
    pkgGroups :: [String] <- input auto "./project-dhall/pkg-groups.dhall"
    sequence_
        [ do
            up <- groupUpgrade remaining p
            let config = pkgUpgrade (T.pack ghcVersion) (T.pack ghcUpgrade) up
            writeFile ("./project-cabal/pkgs" </> p <.> "config") (T.unpack config)
        | p <- pkgGroups
        ]
