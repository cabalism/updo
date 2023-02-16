let TYPES = ../../types.dhall

in  \(sub-template : Text -> Text) ->
    \(deps-git : List TYPES.SourceRepoPkg) ->
      let T = https://prelude.dhall-lang.org/Text/package.dhall

      in  T.concatMapSep
            "\n"
            TYPES.SourceRepoPkg
            ( \(s : TYPES.SourceRepoPkg) ->
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
