\(deps-git : List { loc : Text, tag : Text, sub : List Text }) ->
  let L = https://prelude.dhall-lang.org/List/package.dhall

  let concatMapSep = https://prelude.dhall-lang.org/Text/concatMapSep

  let subdirs =
        \(xs : List Text) ->
          if    L.null Text xs
          then  ""
          else      ''

                    ${"  "}subdir:
                    ${"  "}  ''
                ++  concatMapSep
                      ''

                      ${"    "}''
                      Text
                      (\(s : Text) -> "${s}")
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

  in  concatMapSep "\n" Text (\(s : Text) -> "${s}") repos
