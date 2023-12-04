let TYPES = ../types.dhall

let L = https://prelude.dhall-lang.org/List/package.dhall

let N = https://prelude.dhall-lang.org/Natural/package.dhall

let concatMapSep = https://prelude.dhall-lang.org/Text/concatMapSep

let counts = ./internal/comments/counts.dhall

let intros = ./internal/comments/intros.dhall

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
            \(xs : List TYPES.SourceRepoPkg) -> L.length TYPES.SourceRepoPkg xs

      let countPkgs = \(xs : List Text) -> N.show (L.length Text xs)

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

      let comment = concatMapSep "\n" Text (\(s : Text) -> "-- ${s}")

      let dep-counts =
            { deps-external = count deps-external
            , deps-internal = count deps-internal
            , forks-external = count forks-external
            , forks-internal = count forks-internal
            }

      let deps-count-comment = comment (counts dep-counts)

      in      ''
              ${./import-stackage.dhall stackage-location stackage-resolver}

              ${pkgs-comment}
              ${cabal.packages pkgs}''
          ++  ( if    L.null TYPES.SourceRepoPkg source-deps
                then  ""
                else      ''

                          ${deps-count-comment}''
                      ++  ( if    N.isZero dep-counts.deps-external
                            then  ""
                            else  ''


                                  ${comment intros.deps-external}
                                  ${cabal.repo-items deps-external}''
                          )
                      ++  ( if    N.isZero dep-counts.deps-internal
                            then  ""
                            else  ''


                                  ${comment intros.deps-internal}
                                  ${cabal.repo-items deps-internal}''
                          )
                      ++  ( if    N.isZero dep-counts.forks-external
                            then  ""
                            else  ''


                                  ${comment intros.forks-external}
                                  ${cabal.repo-items forks-external}''
                          )
                      ++  ( if    N.isZero dep-counts.forks-internal
                            then  ""
                            else  ''


                                  ${comment intros.forks-internal}
                                  ${cabal.repo-items forks-internal}''
                          )
              )
          ++  ''


              -- Version equality constraints.
              ${cabal.constraints pkg-config.constraints}
              ''
