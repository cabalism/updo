\(xs : List Text) ->
  let concatMapSep = https://prelude.dhall-lang.org/Text/concatMapSep

  let null = https://prelude.dhall-lang.org/List/null

  let pkgs =
        concatMapSep
          ''

          ${"  "}''
          Text
          (\(s : Text) -> "- ${s}")
          xs

  in  if    null Text xs
      then  "packages: []"
      else  ''
            packages:
              ${pkgs}
            ''
