\(null-content : Text) ->
\(leader : Text) ->
\(separator : Text) ->
\(dep-ver-template : { dep : Text, ver : Text } -> Text) ->
\(constraints : List { dep : Text, ver : Text }) ->
  let null = https://prelude.dhall-lang.org/List/null

  in  if    null { dep : Text, ver : Text } constraints
      then  null-content
      else  "${leader}${./constraint-items.dhall
                          separator
                          dep-ver-template
                          constraints}"
