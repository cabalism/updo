### 2.0.0
* Add make recipes in the readme that bootstrap updo from source.
* Allow all inputs to be omitted and default to empty lists for the omissions.
* Add a choice of stackage import location for `dhall2cabal` and `dhall2config`
  text templates:
    - `StackageWeb`: "import: https://stackage.org/resolver/cabal.config"
    - `StackageLocal`: "import: ./project-stackage/resolver.config"
* For version equality constraints, strip `@sha256:...` revision suffixes for
  Cabal as we already do for `@rev:...` revisions as the [revision
  feature](https://github.com/haskell/cabal/issues/7833) is not yet implemented
  for Cabal.
* The grammar of the comment describing source package dependencies and forks is
  better, no longer saying 1 packages. Same goes for the comment describing
  local packages.
* With `dhall2cabal` or `dhall2stack` generated projects, source dependencies
  and their commentary are only included if there are dependencies in a
  category; internal or external, dependency or fork.
* Show comments with `Verbosity.Info` or not with `Verbosity.Quiet`.

### 1.0.0
* First release