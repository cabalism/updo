# Updo Dhall2Yaml2Stack

The `Makefile` has targets for `ghc-x.y.z.dhall2yaml2stack.yaml` and its `.lock`
file.

The `.dhall` files are mirrored to `.yaml` files[^working-dir] followed by a very lightweight
stitching together of these fragments with
[yq](https://mikefarah.gitbook.io/yq/), removing comments and sorting
dependencies.

[^working-dir]: The `.yaml` fragments are written to `.updo/dhall2yaml2stack`,
copied to a bash `$(mktemp -d)` temporary directory and then stitched together
there. There's a note in the make file explaining how to keep these fragments.
