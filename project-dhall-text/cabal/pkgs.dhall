\(pkgs : List Text) ->
  let null = https://prelude.dhall-lang.org/List/null

  in  if    null Text pkgs
      then  ""
      else  ''
            packages:
                ${../pkg-items.dhall "  , " pkgs}
            ''
