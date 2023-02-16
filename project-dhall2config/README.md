# Updo Cabal

We offer two ways of generating cabal projects, `dhall2cabal` and
`dhall2config`. This folder deals with the later.

The `Makefile` has targets for `ghc-x.y.z.dhall2config.project` that is mostly
imports of `.config` files generated from the `.dhall` configuration, something
like:

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

For this rule using `make --jobs`[^parallel-make] will speed up the recipe a
lot.

[^parallel-make]: https://www.gnu.org/software/make/manual/html_node/Parallel.html