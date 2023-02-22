#!/usr/bin/env stack
-- stack script --resolver=lts-18.27 --package=base --package=dhall --package=filepath --package=text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Description: Sorts packages as a flat list.
module Main where

import Data.List (concat, sortOn)
import qualified Data.Text as T (filter, pack)
import qualified Data.Text.IO as T (putStrLn)
import Dhall (Text, auto, embed, inject, input)
import Dhall.Core (pretty)
import System.FilePath ((<.>), (</>))

grab :: String -> IO [Text]
grab name = input auto (T.pack $ "./project-dhall/pkgs" </> name <.> "dhall")

main :: IO ()
main = do
    pkgGroups :: [String] <- input auto "./project-dhall/pkg-groups.dhall"
    xs <- traverse grab pkgGroups
    -- Package items are (usually in this project) paths to folders containing a
    -- .cabal file.  These aren't canonical so may or may not have a trailing
    -- slash.  To sort them consistently, we filter out the slashes.
    let sortedPkgPaths = sortOn (T.filter (/= '/')) $ concat xs
    T.putStrLn . pretty $ embed inject sortedPkgPaths
