.PHONY: project-files-dhall2config

project-files-dhall2config: \
  ghc-$(GHC_VERSION).dhall2config.project \
  ghc-$(GHC_UPGRADE).dhall2config.project

# SEE: https://stackoverflow.com/questions/14289513/makefile-rule-that-depends-on-all-files-under-a-directory-including-within-subd
PKG_GROUP_CONFIGS = \
  $(shell find project-dhall/pkgs/ -type f -name '*.dhall' -printf "project-cabal/pkgs/%f\n" | sed 's/\.dhall/.config/g')

project-cabal/pkgs.config: \
  updo/text-templates/cabal/pkg-groups.dhall \
  project-dhall/pkg-groups.dhall \
  $(PKG_GROUP_CONFIGS)
	echo './$< ./project-dhall/pkg-groups.dhall' | dhall text --output $@

# Mirror packages (package groups) from dhall to config.
project-cabal/pkgs/%.config: \
  project-dhall/pkgs/%.dhall \
  project-dhall/pkgs-upgrade-done.dhall
	echo './updo/project-dhall/pkgs-upgrade.dhall "$(GHC_VERSION)" "$(GHC_UPGRADE)" $(shell updo/project-dhall/pkgs-upgrade-partition.hs ./project-dhall/pkgs/$(<F))' | dhall text --output $@

# Mirror constraints from dhall to config.
project-cabal/ghc-%/constraints.config : \
  updo/text-templates/cabal/constraints.dhall \
  project-dhall/ghc-%/constraints.dhall
	echo "$(^:%=./%)" | dhall text --output $@

REPOS = updo/text-templates/cabal/repos.dhall
# Mirror source repositories from dhall to config.
project-cabal/ghc-%/deps-internal.config: REPOS project-dhall/ghc-%/deps-internal.dhall; echo "$(^:%=./%)" | dhall text --output $@
project-cabal/ghc-%/deps-external.config: REPOS project-dhall/ghc-%/deps-external.dhall; echo "$(^:%=./%)" | dhall text --output $@
project-cabal/ghc-%/forks-internal.config: REPOS project-dhall/ghc-%/deps-internal.dhall; echo "$(^:%=./%)" | dhall text --output $@
project-cabal/ghc-%/forks-external.config: REPOS project-dhall/ghc-%/forks-external.dhall; echo "$(^:%=./%)" | dhall text --output $@

ghc-$(GHC_VERSION).dhall2config.project: \
  updo/text-templates/dhall2config.dhall \
  project-cabal/ghc-$(GHC_VERSION)/constraints.config \
  project-cabal/ghc-$(GHC_VERSION)/deps-external.config \
  project-cabal/ghc-$(GHC_VERSION)/deps-internal.config \
  project-cabal/ghc-$(GHC_VERSION)/forks-external.config \
  project-cabal/ghc-$(GHC_VERSION)/forks-internal.config \
  project-cabal/pkgs.config
	echo './$< "$(STACKAGE_VERSION)" "$(GHC_VERSION)"' | dhall text --output $@

ghc-$(GHC_UPGRADE).dhall2config.project: \
  project-cabal/ghc-$(GHC_UPGRADE)/project-dhall2config.dhall \
  updo/text-templates/dhall2config.dhall \
  project-cabal/ghc-$(GHC_UPGRADE)/constraints.config \
  project-cabal/ghc-$(GHC_UPGRADE)/deps-external.config \
  project-cabal/ghc-$(GHC_UPGRADE)/deps-internal.config \
  project-cabal/ghc-$(GHC_UPGRADE)/forks-external.config \
  project-cabal/ghc-$(GHC_UPGRADE)/forks-internal.config \
  project-cabal/pkgs.config
	echo './$< "$(STACKAGE_UPGRADE)" "$(GHC_UPGRADE)"' | dhall text --output $@