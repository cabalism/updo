\(sub-template : Text -> Text) ->
\(deps-git : List { loc : Text, tag : Text, sub : List Text }) ->
  let L = https://prelude.dhall-lang.org/List/package.dhall

  let T = https://prelude.dhall-lang.org/Text/package.dhall

  let repos =
        L.map
          { loc : Text, tag : Text, sub : List Text }
          Text
          ( \(s : { loc : Text, tag : Text, sub : List Text }) ->
              ''
                - git: ${s.loc}
                  commit: ${s.tag}${../internal/sub-items.dhall
                                      "    subdirs:"
                                      sub-template
                                      s.sub}
              ''
          )
          deps-git

  in  if L.null Text repos then "" else T.concat repos
