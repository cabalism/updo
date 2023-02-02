\(constraints : List { dep : Text, ver : Text }) ->
  let null = https://prelude.dhall-lang.org/List/null

  in  if    null { dep : Text, ver : Text } constraints
      then  "extra-deps: []"
      else  ''
            extra-deps:
            ${./constraint-items.dhall constraints}
            ''
