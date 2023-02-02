\(xs : List { dep : Text, ver : Text }) ->
  let null = https://prelude.dhall-lang.org/List/null

  let concatMapSep = https://prelude.dhall-lang.org/Text/concatMapSep

  in  if    null { dep : Text, ver : Text } xs
      then  ""
      else      ''
                constraints:
                    ''
            ++  concatMapSep
                  ''

                  ${"  "}, ''
                  { dep : Text, ver : Text }
                  (\(c : { dep : Text, ver : Text }) -> "${c.dep} ==${c.ver}")
                  xs
