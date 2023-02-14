#!/bin/sh
set -eu

for f in ghc-*.stack.* ghc-*.dhall2cabal.* ghc-*.dhall2config.* ghc-*.dhall2stack.* ghc-*.stack2cabal.* ghc-*.cabal2stack.*
do
    if [ -e "${f}" ];
        then rm "${f}" || true;
    fi
done