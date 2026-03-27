\(_ghc-version : Text) ->
\(_stackage-resolver : Optional Text) ->
\(generated-project : Text) ->
  ''
  -- WARNING: This cabal project is generated.
  program-options
    ghc-options: -fhide-source-paths
  ${generated-project}
  ''
