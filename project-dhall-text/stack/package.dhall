{ constraints =
    ../constraints.dhall
      ""
      "  - "
      "  - "
      (\(c : { dep : Text, ver : Text }) -> "${c.dep}-${c.ver}")
, packages = ../pkgs.dhall "packages: []" "  - "
, repo-items = ./repo-items.dhall
, repos = ./repos.dhall
}
