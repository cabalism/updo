#!/usr/bin/env stack
-- stack script --resolver=lts-18.27 --package=base --package=dhall --package=filepath --package=text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Description: Sorts packages as a flat list.
module Main where

import Data.List (concat, filter, sortOn)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.IO as T (putStrLn)
import Dhall (Text, auto, embed, inject, input)
import Dhall.Core (pretty)
import System.FilePath ((<.>), (</>))

grab :: String -> IO [Text]
grab name = input auto (T.pack $ "./project-dhall/pkgs" </> name <.> "dhall")

main :: IO ()
main = do
    pkgGroups :: [String] <- input auto "./project-dhall/pkg-groups.dhall"
    xs <- sequence [do grab p | p <- pkgGroups]
    T.putStrLn . pretty $ embed inject (sortOn (filter (/= '/') . T.unpack) $ concat xs)
