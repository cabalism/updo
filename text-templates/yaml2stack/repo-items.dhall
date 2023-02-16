let TYPES = ../../types.dhall

in  \(xs : List TYPES.SourceRepoPkg) ->
      let L = https://prelude.dhall-lang.org/List/package.dhall

      let x = ../stack/package.dhall

      in  if    L.null TYPES.SourceRepoPkg xs
          then  "extra-deps: []"
          else  ''
                extra-deps:
                ${x.repo-items xs}
                ''
