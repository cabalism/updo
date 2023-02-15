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
              source-repository-package
                type: git
                location: ${s.loc}
                tag: ${s.tag}${../internal/sub-items.dhall
                                 "  subdir:"
                                 sub-template
                                 s.sub}''
          )
          deps-git

  in  T.concatSep "\n" repos
