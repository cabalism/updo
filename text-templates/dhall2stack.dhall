let TYPES = ../types.dhall

let COMMENTS = ./internal/comments.dhall

let stack = ./stack/package.dhall

let L = https://prelude.dhall-lang.org/List/package.dhall

let N = https://prelude.dhall-lang.org/Natural/package.dhall

let concatMapSep = https://prelude.dhall-lang.org/Text/concatMapSep

in  \(verbosity : TYPES.Verbosity) ->
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
            COMMENTS.pkg-counts
              verbosity
              pkg-set
              ( \(comments : List Text) ->
                      "\n"
                  ++  concatMapSep "\n" Text (\(s : Text) -> "# ${s}") comments
              )

      let nested-comment =
            merge
              { Quiet = \(_ : List Text) -> ""
              , Info =
                  \(xs : List Text) ->
                        "\n"
                    ++  concatMapSep "\n" Text (\(s : Text) -> "  # ${s}") xs
              }
              verbosity

      let null-source-deps = L.null TYPES.SourceRepoPkg source-deps

      let dep-counts =
            { deps-external = count deps-external
            , deps-internal = count deps-internal
            , forks-external = count forks-external
            , forks-internal = count forks-internal
            }

      let deps-count-comment =
                "\n"
            ++  merge
                  { Quiet = ""
                  , Info =
                      if    null-source-deps
                      then  ""
                      else      concatMapSep
                                  "\n"
                                  Text
                                  (\(s : Text) -> "# ${s}")
                                  (COMMENTS.dep-counts dep-counts)
                            ++  "\n"
                  }
                  verbosity

      let constraints-intro =
            merge
              { Quiet = [] : List Text
              , Info =
                [ "Package versions for published packages either not on Stackage or"
                , "not matching the version on Stackage for the resolver we use."
                , "These package-version extra dependencies are equivalent to cabal constraints."
                ]
              }
              verbosity

      in      "resolver: ${stackage-resolver}"
          ++  ''

              ${pkgs-comment}''
          ++  ''

              ${stack.packages pkgs}''
          ++  "${deps-count-comment}"
          ++  ( if        null-source-deps
                      &&  L.null TYPES.PkgVer pkg-config.constraints
                then  "extra-deps: []"
                else      "extra-deps:"
                      ++  ( if    N.isZero dep-counts.deps-external
                            then  ""
                            else  ''
                                  ${nested-comment
                                      COMMENTS.intros.deps-external}
                                  ${stack.repo-items deps-external}''
                          )
                      ++  ( if    N.isZero dep-counts.deps-internal
                            then  ""
                            else  ''
                                  ${nested-comment
                                      COMMENTS.intros.deps-internal}
                                  ${stack.repo-items deps-internal}''
                          )
                      ++  ( if    N.isZero dep-counts.forks-external
                            then  ""
                            else  ''
                                  ${nested-comment
                                      COMMENTS.intros.forks-external}
                                  ${stack.repo-items forks-external}''
                          )
                      ++  ( if    N.isZero dep-counts.forks-internal
                            then  ""
                            else  ''
                                  ${nested-comment
                                      COMMENTS.intros.forks-internal}
                                  ${stack.repo-items forks-internal}''
                          )
                      ++  ''
                          ${nested-comment constraints-intro}
                          ${stack.constraints pkg-config.constraints}''
              )
