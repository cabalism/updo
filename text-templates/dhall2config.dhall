\(stackage-resolver : Text) ->
\(ghc-version : Text) ->
  ''
  import: ./project-stackage/${stackage-resolver}.config

  import: ./project-cabal/pkgs.config

  import: ./project-cabal/ghc-${ghc-version}/constraints.config
  import: ./project-cabal/ghc-${ghc-version}/deps-external.config
  import: ./project-cabal/ghc-${ghc-version}/deps-internal.config
  import: ./project-cabal/ghc-${ghc-version}/forks-external.config
  import: ./project-cabal/ghc-${ghc-version}/forks-internal.config

  build-info: True
  ''
