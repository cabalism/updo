\(xs : List Text) ->
  let concatSep = https://prelude.dhall-lang.org/Text/concatSep

  let pkgs =
        concatSep
          ''

          ${"  "}, ''
          xs

  in  ''
      packages:
          ${pkgs}''
