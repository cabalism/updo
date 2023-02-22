#!/usr/bin/env stack
-- stack script --resolver=lts-18.27 --package=aeson --package=base --package=dhall --package=text --package=turtle --package=utf8-string
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Description: Generates a sha256map.
module Main where

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Either (partitionEithers)
import Data.Text (pack, unpack)
import qualified Data.Text.IO as T (getContents)
import Dhall (FromDhall, Generic, Text, ToDhall, auto, input)
import GHC.Generics
import System.Exit (ExitCode (..))
import Turtle (empty, parallel, shellStrictWithErr, sort)
import Turtle.Format (printf, (%), s)

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

prefetchSourceRepoPkg :: SourceRepoPkg -> IO (Either Warning NixPrefetchGitOutput)
prefetchSourceRepoPkg SourceRepoPkg{..} = nixPrefetchGit (loc, tag)
{-# INLINE prefetchSourceRepoPkg #-}

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

printUrlTagSha :: NixPrefetchGitOutput -> IO ()
printUrlTagSha NixPrefetchGitOutput{..} =
    printf ("  \""%s%"\".\""%s%"\" = \""%s%"\";\n") url rev sha256

main :: IO ()
main = do
    repos :: [[SourceRepoPkg]] <- input auto =<< T.getContents
    fetches <- sort . parallel $ prefetchSourceRepoPkg <$> concat repos
    let (_errs, xs) = partitionEithers fetches
    printf "{\n"
    mapM_ printUrlTagSha xs
    printf "}\n"
