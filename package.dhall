{ name = "updo"
, version = "1.0.0"
, synopsis = "A style of maintaining and upgrading Haskell projects"
, description =
    ''
    From configuration in .dhall, generate projects (stack.yaml and
    cabal.project) and progressively upgrade projects.
    ''
, category = "Development"
, github = "cabalism/updo"
, author = "Phil de Joux"
, maintainer = "phil.dejoux@blockscope.com"
, copyright = "© 2023 - 2024 Phil de Joux, © 2023 - 2024 Block Scope Limited"
, tested-with =
  [ "GHC == 9.2.8", "GHC == 9.4.8", "GHC == 9.6.5", "GHC == 9.8.2" ]
, extra-source-files =
  [ "**/*.md"
  , "**/*.dhall"
  , "**/Makefile"
  , "project-dhall/**/*hs"
  , "project-dhall2config/**/*hs"
  , "project-nix/**/*hs"
  , "**/*.py"
  , "**/*.sh"
  ]
, dependencies =
  [ "aeson >= 2.1.2 && <2.3"
  , "base >=4.12 && <5"
  , "dhall ^>= 1.42"
  , "filepath >= 1.4.2 && <1.6"
  , "text >= 1.2.3 && <2.2"
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
