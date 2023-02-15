\(sub-field : Text) ->
\(sub-template : Text -> Text) ->
\(xs : List Text) ->
  let L = https://prelude.dhall-lang.org/List/package.dhall

  let T = https://prelude.dhall-lang.org/Text/package.dhall

  in  if    L.null Text xs
      then  ""
      else      ''

                ${sub-field}
                ''
            ++  T.concatMapSep "\n" Text sub-template xs
