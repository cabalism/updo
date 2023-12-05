let TYPES = ../../../types.dhall

let N = https://prelude.dhall-lang.org/Natural/package.dhall

in  \(counts : TYPES.DepCounts) ->
      let total-deps =
              counts.deps-external
            + counts.deps-internal
            + counts.forks-external
            + counts.forks-internal

      in  if    N.isZero total-deps
          then  [] : List Text
          else  if N.equal total-deps 1
          then  if    N.equal counts.deps-external 1
                then  [ "We have 1 external dependency source package." ]
                else  if N.equal counts.deps-internal 1
                then  [ "We have 1 internal dependency source package." ]
                else  if N.equal counts.forks-external 1
                then  [ "We have 1 external fork source package." ]
                else  if N.equal counts.forks-internal 1
                then  [ "We have 1 internal fork source package." ]
                else  [ "We have 1 source package:"
                      , "  * external ${N.show counts.deps-external}"
                      , "  * internal ${N.show counts.deps-internal}"
                      , "  * external forks ${N.show counts.forks-external}"
                      , "  * internal forks ${N.show counts.forks-internal}"
                      ]
          else  [ "We have ${N.show total-deps} source packages:"
                , "  * external ${N.show counts.deps-external}"
                , "  * internal ${N.show counts.deps-internal}"
                , "  * external forks ${N.show counts.forks-external}"
                , "  * internal forks ${N.show counts.forks-internal}"
                ]
