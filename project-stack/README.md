# Updo Stack

The `Makefile` has targets for `ghc-x.y.z.dhall2yaml2stack.yaml` and its `.lock`
file.

The `.dhall` files are mirrored to `.yaml` files followed by a very lightweight
stitching together of these fragments with
[yq](https://mikefarah.gitbook.io/yq/), removing comments and sorting
dependencies.

We include a `sha256map-regenerate.py` script for use with
[haskell.nix](https://input-output-hk.github.io/haskell.nix/).