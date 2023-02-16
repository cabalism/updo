\(null-content : Text) ->
\(first-separator : Text) ->
\(separator : Text) ->
\(pkgs : List Text) ->
  let null = https://prelude.dhall-lang.org/List/null

  in  if    null Text pkgs
      then  null-content
      else  ''
            packages:
            ${first-separator}${./pkg-items.dhall separator pkgs}
            ''
