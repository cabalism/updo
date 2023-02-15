\(separator : Text) ->
\(dep-ver-template : { dep : Text, ver : Text } -> Text) ->
\(xs : List { dep : Text, ver : Text }) ->
  let null = https://prelude.dhall-lang.org/List/null

  let concatMapSep = https://prelude.dhall-lang.org/Text/concatMapSep

  in  if    null { dep : Text, ver : Text } xs
      then  ""
      else  concatMapSep
              ''

              ${separator}''
              { dep : Text, ver : Text }
              dep-ver-template
              xs
