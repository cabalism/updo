{ constraints =
    ../constraint-section.dhall
      ""
      "constraints:"
      "  , "
      (\(c : { dep : Text, ver : Text }) -> "${c.dep} ==${c.ver}")
, packages = ../pkg-section.dhall "" "  , "
, repo-items = ./repo-items.dhall
}
