\(xs : List Text) ->
  let concatMapSep = https://prelude.dhall-lang.org/Text/concatMapSep

  let pkgs =
        concatMapSep
          ''

          ${"  "}, ''
          Text
          (\(s : Text) -> "${s}")
          xs

  in  ''
      packages:
          ${pkgs}''
