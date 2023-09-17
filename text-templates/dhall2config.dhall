let TYPES = ../types.dhall

let null = https://prelude.dhall-lang.org/List/null

in  \(stackage-location : TYPES.Stackage) ->
    \(stackage-resolver : Text) ->
    \(ghc-version : Text) ->
    \ ( pkg-config
      : { constraints : List TYPES.PkgVer
        , source-pkgs :
            { deps-external : List TYPES.SourceRepoPkg
            , deps-internal : List TYPES.SourceRepoPkg
            , forks-external : List TYPES.SourceRepoPkg
            , forks-internal : List TYPES.SourceRepoPkg
            }
        }
      ) ->
      let import-stackage =
            merge
              { StackageWeb =
                  "import: https://stackage.org/${stackage-resolver}.config"
              , StackageProject =
                  "import: ./project-stackage/${stackage-resolver}.config"
              }
              stackage-location

      let import-constraints =
            if    null TYPES.PkgVer pkg-config.constraints
            then  ""
            else  ''
                  import: ./project-cabal/ghc-${ghc-version}/constraints.config
                  ''

      let import-source =
            \(sources : List TYPES.SourceRepoPkg) ->
            \(source : Text) ->
              if    null TYPES.SourceRepoPkg sources
              then  ""
              else  ''
                    import: ./project-cabal/ghc-${ghc-version}/${source}.config
                    ''

      let pkgs = pkg-config.source-pkgs

      in      ''
              ${import-stackage}

              import: ./project-cabal/pkgs.config
              ''
          ++  import-constraints
          ++  import-source pkgs.deps-external "deps-external"
          ++  import-source pkgs.deps-internal "deps-internal"
          ++  import-source pkgs.forks-external "forks-external"
          ++  import-source pkgs.forks-internal "forks-internal"
          ++  ''
              build-info: True
              ''
