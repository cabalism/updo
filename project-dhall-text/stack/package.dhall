{ constraints =
    ../constraint-section.dhall
      ""
      "  - "
      "  - "
      (\(c : { dep : Text, ver : Text }) -> "${c.dep}-${c.ver}")
, packages = ../pkg-section.dhall "packages: []" "  - "
, repo-items = ./repo-items.dhall
}
