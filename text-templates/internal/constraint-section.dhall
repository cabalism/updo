let TYPES = ../../types.dhall

in  \(null-content : Text) ->
    \(leader : Text) ->
    \(separator : Text) ->
    \(dep-ver-template : TYPES.PkgVer -> Text) ->
    \(constraints : List TYPES.PkgVer) ->
      let null = https://prelude.dhall-lang.org/List/null

      in  if    null TYPES.PkgVer constraints
          then  null-content
          else  "${leader}${./constraint-items.dhall
                              separator
                              dep-ver-template
                              constraints}"
