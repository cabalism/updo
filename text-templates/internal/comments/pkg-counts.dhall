let TYPES = ../../../types.dhall

let L = https://prelude.dhall-lang.org/List/package.dhall

let N = https://prelude.dhall-lang.org/Natural/package.dhall

in  \(verbosity : TYPES.Verbosity) ->
    \(pkg-set : TYPES.PkgSet) ->
    \(format-comments : List Text -> Text) ->
      let pkgs =
            merge
              { AllPkgs = \(pkgs : List Text) -> pkgs
              , PkgUpgrade = \(pkgs : TYPES.PkgTodoList) -> pkgs.done
              }
              pkg-set

      in  merge
            { Quiet = ""
            , Info =
                let comments =
                      merge
                        { AllPkgs =
                            \(pkgs : List Text) ->
                              let count-pkgs = L.length Text pkgs

                              in  if    N.isZero count-pkgs
                                  then  [ "We have no packages" ]
                                  else  let package-or-packages =
                                              if    N.equal 1 count-pkgs
                                              then  "package"
                                              else  "packages"

                                        in  [ "We have ${N.show
                                                           count-pkgs} ${package-or-packages}."
                                            ]
                        , PkgUpgrade =
                            \(pkgs : TYPES.PkgTodoList) ->
                              let count-done = L.length Text pkgs.done

                              let count-todo = L.length Text pkgs.todo

                              let package-or-packages =
                                    if    N.equal 1 count-done
                                    then  "package"
                                    else  "packages"

                              in  [ "We have upgraded ${N.show
                                                          count-done} ${package-or-packages} and have ${N.show
                                                                                                          count-todo} yet to do."
                                  ]
                        }
                        pkg-set

                in  format-comments comments
            }
            verbosity
