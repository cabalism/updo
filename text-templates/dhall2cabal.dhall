let TYPES = ../types.dhall

let length = https://prelude.dhall-lang.org/List/length

let show = https://prelude.dhall-lang.org/Natural/show

let concatMapSep = https://prelude.dhall-lang.org/Text/concatMapSep

let counts = ./internal/comments/counts.dhall

in  \(stackage-location : TYPES.Stackage) ->
    \(stackage-resolver : Text) ->
    \(pkg-set : TYPES.PkgSet) ->
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
      let deps-external = pkg-config.source-pkgs.deps-external

      let deps-internal = pkg-config.source-pkgs.deps-internal

      let forks-external = pkg-config.source-pkgs.forks-external

      let forks-internal = pkg-config.source-pkgs.forks-internal

      let source-deps =
            deps-external # deps-internal # forks-external # forks-internal

      let count =
            \(xs : List TYPES.SourceRepoPkg) -> length TYPES.SourceRepoPkg xs

      let countPkgs = \(xs : List Text) -> show (length Text xs)

      let pkgs =
            merge
              { AllPkgs = \(pkgs : List Text) -> pkgs
              , PkgUpgrade = \(pkgs : TYPES.PkgTodoList) -> pkgs.done
              }
              pkg-set

      let pkgs-comment =
            merge
              { AllPkgs =
                  \(pkgs : List Text) ->
                    "-- We have ${countPkgs pkgs} packages."
              , PkgUpgrade =
                  \(pkgs : TYPES.PkgTodoList) ->
                    "-- We have upgraded ${countPkgs
                                             pkgs.done} packages and have ${countPkgs
                                                                              pkgs.todo} yet to do."
              }
              pkg-set

      let cabal = ./cabal/package.dhall

      let deps-count-comment =
            concatMapSep
              "\n"
              Text
              (\(s : Text) -> "-- ${s}")
              ( counts
                  { deps-external = count deps-external
                  , deps-internal = count deps-internal
                  , forks-external = count forks-external
                  , forks-internal = count forks-internal
                  }
              )

      in  ''
          ${./import-stackage.dhall stackage-location stackage-resolver}

          ${pkgs-comment}
          ${cabal.packages pkgs}

          ${deps-count-comment}
          -- Source Packages, external (3rd party).
          ${cabal.repo-items deps-external}

          -- Source Packages, internal to this organisation (private and public).
          ${cabal.repo-items deps-internal}

          -- Source Packages, external (3rd party) forks of other repositories.
          -- Can we help upstream?
          ${cabal.repo-items forks-external}

          -- Source Packages, internal forks of other repositories.
          -- Can we upstream and unfork?
          ${cabal.repo-items forks-internal}

          -- Constraints are equivalent to stack package-version extra dependencies.
          ${cabal.constraints pkg-config.constraints}
          ''
