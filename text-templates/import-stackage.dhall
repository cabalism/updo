let TYPES = ../types.dhall

in  \(stackage-location : TYPES.Stackage) ->
    \(stackage-resolver : Text) ->
      merge
        { StackageWeb =
            "import: https://stackage.org/${stackage-resolver}/cabal.config"
        , StackageLocal =
            "import: ./project-stackage/${stackage-resolver}.config"
        }
        stackage-location
