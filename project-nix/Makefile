UPDO_TMP := .updo

ifeq ($(SHA256MAP_HS_EXE), true)
SHA256MAP_HS := updo-sha256map
else
SHA256MAP_HS := ./updo/project-nix/sha256map.hs
endif

project-sha256maps: \
  ghc-$(GHC_VERSION).sha256map.nix \
  ghc-$(GHC_UPGRADE).sha256map.nix

ghc-%.sha256map.nix: \
  $(UPDO_TMP)/ghc-%/deps-external.dhall \
  $(UPDO_TMP)/ghc-%/deps-internal.dhall \
  $(UPDO_TMP)/ghc-%/forks-external.dhall \
  $(UPDO_TMP)/ghc-%/forks-internal.dhall
	echo '[$(^:%=./%,) ([] : List {loc : Text, tag : Text, sub : List Text})]' | $(SHA256MAP_HS) > $@

project-versions.nix:
	echo './updo/project-nix/project-versions.dhall "$(GHC_VERSION)" "$(GHC_UPGRADE)"' | dhall text > $@