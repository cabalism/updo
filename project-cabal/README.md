# Updo Cabal

The `Makefile` has targets for `ghc-x.y.z.dhall2config.project` that is mostly
imports of `.config` files from the `.dhall` configuration, something like:

```
import: project-stackage/stackage-resolver.config

import: project-cabal/pkgs.config

import: project-cabal/ghc-x.y.z/constraints.config
import: project-cabal/ghc-x.y.z/deps-external.config
import: project-cabal/ghc-x.y.z/deps-internal.config
import: project-cabal/ghc-x.y.z/forks-external.config
import: project-cabal/ghc-x.y.z/forks-internal.config

build-info: True
```