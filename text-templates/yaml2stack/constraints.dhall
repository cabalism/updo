let TYPES = ../../types.dhall

in  \(xs : List TYPES.PkgVer) ->
      let L = https://prelude.dhall-lang.org/List/package.dhall

      let x = ../stack/package.dhall

      in  if    L.null TYPES.PkgVer xs
          then  "extra-deps: []"
          else  ''
                extra-deps:
                ${x.constraints xs}
                ''
