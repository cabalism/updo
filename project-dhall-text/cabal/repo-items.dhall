\(deps-git : List { loc : Text, tag : Text, sub : List Text }) ->
  let L = https://prelude.dhall-lang.org/List/package.dhall

  let T = https://prelude.dhall-lang.org/Text/package.dhall

  let subdirs =
        \(xs : List Text) ->
          if    L.null Text xs
          then  ""
          else      ''

                    ${"  "}subdir:
                    ${"  "}  ''
                ++  T.concatSep
                      ''

                      ${"    "}''
                      xs

  let repos =
        L.map
          { loc : Text, tag : Text, sub : List Text }
          Text
          ( \(s : { loc : Text, tag : Text, sub : List Text }) ->
              ''
              source-repository-package
                type: git
                location: ${s.loc}
                tag: ${s.tag}${subdirs s.sub}''
          )
          deps-git

  in  T.concatSep "\n" repos
