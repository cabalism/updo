let TYPES = ../../types.dhall

in  \(sub-template : Text -> Text) ->
    \(deps-git : List TYPES.SourceRepoPkg) ->
      let L = https://prelude.dhall-lang.org/List/package.dhall

      let T = https://prelude.dhall-lang.org/Text/package.dhall

      in  if    L.null TYPES.SourceRepoPkg deps-git
          then  ""
          else  T.concatMap
                  TYPES.SourceRepoPkg
                  ( \(s : TYPES.SourceRepoPkg) ->
                      ''
                        - git: ${s.loc}
                          commit: ${s.tag}${../internal/sub-items.dhall
                                              "    subdirs:"
                                              sub-template
                                              s.sub}
                      ''
                  )
                  deps-git
