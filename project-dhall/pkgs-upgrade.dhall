\(ghc-version : Text) ->
\(ghc-upgrade : Text) ->
\(x : { pkgs : List Text, done : List Text, todo : List Text }) ->
  let N = https://prelude.dhall-lang.org/Natural/package.dhall

  let L = https://prelude.dhall-lang.org/List/package.dhall

  let concatMapSep = https://prelude.dhall-lang.org/Text/concatMapSep

  let pkgList =
        \(indent : Text) ->
        \(xs : List Text) ->
          concatMapSep
            ''

            ${indent}, ''
            Text
            (\(s : Text) -> "${s}")
            xs

  let pkgListComment =
        \(xs : List Text) ->
          concatMapSep "\n" Text (\(s : Text) -> "-- ${s}") xs

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
