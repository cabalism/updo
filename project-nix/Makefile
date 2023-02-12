project-sha256maps: \
  ghc-$(GHC_VERSION).sha256map.nix \
  ghc-$(GHC_UPGRADE).sha256map.nix

ghc-%.sha256map.nix: \
  project-dhall/ghc-%/deps-external.dhall \
  project-dhall/ghc-%/deps-internal.dhall \
  project-dhall/ghc-%/forks-external.dhall \
  project-dhall/ghc-%/forks-internal.dhall
	echo '[$(^:%=./%,) ([] : List {loc : Text, tag : Text, sub : List Text})]' | ./updo/project-nix/sha256map.hs > $@
