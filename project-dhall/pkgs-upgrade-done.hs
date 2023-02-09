#!/usr/bin/env cabal
{- cabal:
build-depends: base, dhall, text
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Description: The difference between all packages and those in the todo
--  list, the packgaes that have been upgraded.

import Data.List ((\\))
import qualified Data.Text as T (pack)
import qualified Data.Text.IO as T (putStrLn)
import Dhall (Text, auto, embed, inject, input)
import Dhall.Core (pretty)
import System.Environment (getArgs)

main = do
  sortedFile : todoFile : _ <- getArgs
  sorted :: [Text] <- input auto (T.pack sortedFile)
  todo :: [Text] <- input auto (T.pack todoFile)
  T.putStrLn . pretty $ embed inject (sorted \\ todo)
