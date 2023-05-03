-- NOTE: With cabal-3.10, imports are relative to each other but imported
-- packages are not, they are still relative to the project root. With
-- cabal-3.8, the first version with imports, imports are relative to the
-- project root too.
let TYPES = ../types.dhall

in  \(pkg-import : TYPES.CabalRelativity) ->
    \(ghc-version : Text) ->
    \(ghc-upgrade : Text) ->
    \(x : { pkgs : List Text, done : List Text, todo : List Text }) ->
      let N = https://prelude.dhall-lang.org/Natural/package.dhall

      let L = https://prelude.dhall-lang.org/List/package.dhall

      let concatMapSep = https://prelude.dhall-lang.org/Text/concatMapSep

      let relativePkg =
            merge
              { CabalProjectRelative = \(s : Text) -> "./${s}"
              , CabalImportRelative = \(s : Text) -> "./${s}"
              }
              pkg-import

      let pkgList =
            \(indent : Text) ->
            \(xs : List Text) ->
              concatMapSep
                ''

                ${indent}, ''
                Text
                relativePkg
                xs

      let pkgListComment =
            \(xs : List Text) ->
              concatMapSep "\n" Text (\(s : Text) -> "-- ${relativePkg s}") xs

      in  if        L.null Text x.todo
                ||  N.equal (L.length Text x.pkgs) (L.length Text x.done)
          then  ''
                packages:
                    ${pkgList "  " x.pkgs}
                ''
          else      ''
                    if impl(ghc <= ${ghc-version})
                      packages:
                          ${pkgList "    " x.pkgs}
                    ''
                ++  ( if    L.null Text x.done
                      then  ""
                      else  ''
                            else
                              packages:
                                  ${pkgList "    " x.done}
                            ''
                    )
                ++  ( if    L.null Text x.done
                      then  ""
                      else  ''

                            -- TODO for impl(ghc >= ${ghc-upgrade})
                            ${pkgListComment x.todo}
                            ''
                    )
