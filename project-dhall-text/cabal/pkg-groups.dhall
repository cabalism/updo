\(pkg-groups : List Text) ->
  let null = https://prelude.dhall-lang.org/List/null

  let concatMapSep = https://prelude.dhall-lang.org/Text/concatMapSep

  in  if    null Text pkg-groups
      then  ""
      else  concatMapSep
              "\n"
              Text
              (\(pkg : Text) -> "import: project-cabal/pkgs/${pkg}.config")
              pkg-groups
