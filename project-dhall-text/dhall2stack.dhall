let TYPES = ../types.dhall

let length = https://prelude.dhall-lang.org/List/length

let show = https://prelude.dhall-lang.org/Natural/show

in  \(stackage-resolver : Text) ->
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
            \(xs : List TYPES.SourceRepoPkg) ->
              show (length TYPES.SourceRepoPkg xs)

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
                  \(pkgs : List Text) -> "# We have ${countPkgs pkgs} packages."
              , PkgUpgrade =
                  \(pkgs : TYPES.PkgTodoList) ->
                    "# We have upgraded ${countPkgs
                                            pkgs.done} packages and have ${countPkgs
                                                                             pkgs.todo} yet to do."
              }
              pkg-set

      let stack = ./stack/package.dhall

      in  ''
          resolver: ${stackage-resolver}

          ${pkgs-comment}
          ${stack.packages pkgs}
          # We have ${count source-deps} source packages listed in this order:
          #   * external ${count deps-external}
          #   * internal ${count deps-internal}
          #   * external forks ${count forks-external}
          #   * internal forks ${count forks-internal}
          extra-deps:

            # Source Packages, external (3rd party).
          ${stack.repo-items deps-external}
            # Source Packages, internal to this organisation (private and public).
          ${stack.repo-items deps-internal}
            # Source Packages, external (3rd party) forks of other repositories.
            # Can we help upstream?
          ${stack.repo-items forks-external}
            # Source Packages, internal forks of other repositories.
            # Can we upstream and unfork?
          ${stack.repo-items forks-internal}
            # Package versions for published packages either not on Stackage or
            # not matching the version on Stackage for the resolver we use.
            # These package-version extra dependencies are equivalent to cabal constraints.
          ${stack.constraints pkg-config.constraints}''
