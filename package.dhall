{ name = "updo"
, version = "1.0.0"
, synopsis = "A tool for maintenance and upgrading Haskell projects"
, description =
    ''
    From configuration in .dhall, generate projects (stack.yaml and
    cabal.project) and progressively upgrade projects.
    ''
, category = "Development"
, github = "cabalism/updo"
, author = "Phil de Joux"
, maintainer = "phil.dejoux@blockscope.com"
, copyright = "© 2023 Phil de Joux, © 2023 Block Scope Limited"
, extra-source-files =
  [ "**/*.md", "**/*.dhall", "**/Makefile", "**/*hs", "**/*.py", "**/*.sh" ]
, dependencies =
  [ "aeson >= 2.1.2 && <2.2"
  , "base >=4.12 && <5"
  , "dhall >= 1.41.1 && <1.42"
  , "filepath >= 1.4.2 && <1.5"
  , "text >= 1.2.3 && <2.1"
  , "turtle >= 1.6.1 && <1.7"
  , "utf8-string >= 1.0.2 && <1.1"
  ]
, executables =
  { updo-pkgs-sorted.main = "project-dhall/pkgs-sorted.hs"
  , updo-pkgs-upgrade-done.main = "project-dhall/pkgs-upgrade-done.hs"
  , updo-pkgs-upgrade-partition.main = "project-dhall/pkgs-upgrade-partition.hs"
  , updo-pkg-groups.main = "project-dhall2config/pkg-groups.hs"
  , updo-sha256map.main = "project-nix/sha256map.hs"
  }
}
