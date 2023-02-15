\(deps-git : List { loc : Text, tag : Text, sub : List Text }) ->
  let L = https://prelude.dhall-lang.org/List/package.dhall

  let T = https://prelude.dhall-lang.org/Text/package.dhall

  let subdirs =
        \(xs : List Text) ->
          if    L.null Text xs
          then  ""
          else      ''

                    ${"    "}subdirs:
                    ${"    "}  ''
                ++  T.concatMapSep
                      ''

                      ${"      "}''
                      Text
                      (\(s : Text) -> "- ${s}")
                      xs

  let repos =
        L.map
          { loc : Text, tag : Text, sub : List Text }
          Text
          ( \(s : { loc : Text, tag : Text, sub : List Text }) ->
              ''
                - git: ${s.loc}
                  commit: ${s.tag}${subdirs s.sub}
              ''
          )
          deps-git

  in  if L.null Text repos then "" else T.concat repos
