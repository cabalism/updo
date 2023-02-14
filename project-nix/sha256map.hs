#!/usr/bin/env -S cabal run --verbose=0
{- cabal:
build-depends: aeson, base, dhall, text, turtle, utf8-string
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Description: Generates a sha256map.
module Main where

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Either (partitionEithers)
import Data.Text (pack, unlines, unpack)
import Dhall (FromDhall, Generic, Text, ToDhall, auto, embed, inject, input)
import GHC.Generics
import System.Exit (ExitCode (..))
import Turtle (Line, echo, empty, parallel, shellStrictWithErr, sort, stdout, unsafeTextToLine, void)
import Prelude hiding (unlines)

data SourceRepoPkg = SourceRepoPkg
    { loc :: Text
    , tag :: Text
    , sub :: [Text]
    }
    deriving (Eq, Ord, Show, Generic, ToDhall, FromDhall)

data Warning
    = NixPrefetchGitFailed Int Text
    | InvalidPrefetchGitOutput Text
    deriving (Eq, Ord, Show)

data NixPrefetchGitOutput = NixPrefetchGitOutput
    { url :: Text
    , rev :: Text
    , sha256 :: Text
    , date :: Text
    }
    deriving (Eq, Ord, Show, Generic, FromJSON)

prefetchSoureRepoPkg :: SourceRepoPkg -> IO (Either Warning NixPrefetchGitOutput)
prefetchSoureRepoPkg SourceRepoPkg{loc, tag} = nixPrefetchGit (loc, tag)
{-# INLINE prefetchSoureRepoPkg #-}

nixPrefetchGit :: (Text, Text) -> IO (Either Warning NixPrefetchGitOutput)
nixPrefetchGit (repo, commit) = do
    (exitCode, stdout, stderr) <-
        shellStrictWithErr
            ("nix-prefetch-git " <> repo <> " " <> commit)
            empty
    case exitCode of
        ExitFailure e -> return (Left $ NixPrefetchGitFailed e stderr)
        ExitSuccess ->
            return $
                maybe
                    (Left $ InvalidPrefetchGitOutput stdout)
                    Right
                    (decode (fromString $ unpack stdout))

mkSha256Map :: [NixPrefetchGitOutput] -> [Line]
mkSha256Map xs =
    unsafeTextToLine
        <$> "{"
            : [ "  \"" <> url <> "\".\"" <> rev <> "\" = \"" <> sha256 <> "\";"
              | NixPrefetchGitOutput{..} <- xs
              ]
                <> ["}"]

main :: IO ()
main = do
    s <- getContents
    repos :: [[SourceRepoPkg]] <- input auto (pack s)
    fetches <- sort . parallel $ prefetchSoureRepoPkg <$> concat repos
    let (_errs, xs) = partitionEithers fetches
    sequence_ $ echo <$> mkSha256Map xs
    return ()
