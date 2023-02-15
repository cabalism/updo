\(null-content : Text) ->
\(separator : Text) ->
\(pkgs : List Text) ->
  let null = https://prelude.dhall-lang.org/List/null

  in  if    null Text pkgs
      then  null-content
      else  ''
            packages:
              - ${./pkg-items.dhall "  - " pkgs}
            ''
