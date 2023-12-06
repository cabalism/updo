let TYPES = ./../../../updo/types.dhall

let empty-constraints = ./../../../updo/empty/constraints.dhall

let empty-source-pkgs = ./../../../updo/empty/source-pkgs.dhall

in  \(stackage-resolver : Text) ->
    \(ghc-version : Text) ->
      let project-dhall2config = ../../../updo/text-templates/dhall2config.dhall

      let pkg-config =
            { constraints = ./../constraints.dhall ? empty-constraints
            , source-pkgs =
              { deps-external = ./../deps-external.dhall ? empty-source-pkgs
              , deps-internal = ./../deps-internal.dhall ? empty-source-pkgs
              , forks-external = ./../forks-external.dhall ? empty-source-pkgs
              , forks-internal = ./../forks-internal.dhall ? empty-source-pkgs
              }
            }

      in  ''
          ${./cabal-snippet.dhall}
          ${project-dhall2config
              TYPES.Stackage.StackageWeb
              stackage-resolver
              ghc-version
              pkg-config}''
