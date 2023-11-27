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

### 1.0.0
* First release