{ constraints =
    ../constraints.dhall
      ""
      "constraints:"
      "  , "
      (\(c : { dep : Text, ver : Text }) -> "${c.dep} ==${c.ver}")
, packages = ../pkgs.dhall "" "  , "
, repos = ./repos.dhall
}
