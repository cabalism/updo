let TYPES = ../../types.dhall

in  \(separator : Text) ->
    \(dep-ver-template : TYPES.PkgVer -> Text) ->
    \(xs : List TYPES.PkgVer) ->
      let null = https://prelude.dhall-lang.org/List/null

      let concatMapSep = https://prelude.dhall-lang.org/Text/concatMapSep

      in  if    null TYPES.PkgVer xs
          then  ""
          else  concatMapSep
                  ''

                  ${separator}''
                  TYPES.PkgVer
                  dep-ver-template
                  xs
