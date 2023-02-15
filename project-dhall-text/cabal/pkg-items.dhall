\(xs : List Text) ->
  let null = https://prelude.dhall-lang.org/List/null

  let concatSep = https://prelude.dhall-lang.org/Text/concatSep

  in  if    null Text xs
      then  ""
      else  concatSep
              ''

              ${"  "}, ''
              xs
