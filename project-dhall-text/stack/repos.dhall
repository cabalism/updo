\(deps-git : List { loc : Text, tag : Text, sub : List Text }) ->
  let null = https://prelude.dhall-lang.org/List/null

  in  if    null { loc : Text, tag : Text, sub : List Text } deps-git
      then  "extra-deps: []"
      else  ''
            extra-deps:
            ${./repo-items.dhall deps-git}
            ''
