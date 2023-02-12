let TYPES = ../types.dhall

let concat = https://prelude.dhall-lang.org/List/concat

in  \(xs : List (List TYPES.SourceRepoPkg)) -> concat TYPES.SourceRepoPkg xs
