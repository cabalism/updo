let TYPES = ./../../types.dhall

in  \(pkg-import : TYPES.CabalRelativity) ->
    \(pkg-groups : List Text) ->
      let null = https://prelude.dhall-lang.org/List/null

      let concatMapSep = https://prelude.dhall-lang.org/Text/concatMapSep

      let import-pkg =
            merge
              { CabalProjectRelative =
                  \(pkg : Text) -> "import: ./project-cabal/pkgs/${pkg}.config"
              , CabalImportRelative =
                  \(pkg : Text) -> "import: ./pkgs/${pkg}.config"
              }
              pkg-import

      in  if    null Text pkg-groups
          then  ""
          else  concatMapSep "\n" Text import-pkg pkg-groups
