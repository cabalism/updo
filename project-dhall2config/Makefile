.PHONY: project-files-dhall2config

project-files-dhall2config: \
  ghc-$(GHC_VERSION).dhall2config.project \
  ghc-$(GHC_UPGRADE).dhall2config.project

project-cabal/pkgs.config: \
  updo/text-templates/cabal/pkg-groups.dhall \
  project-dhall/pkg-groups.dhall \
  $(patsubst project-dhall/pkgs/%.dhall, project-cabal/pkgs/%.config, $(wildcard project-dhall/pkgs/*.dhall))
	echo './$< ./project-dhall/pkg-groups.dhall' | dhall text --output $@

generate-pkg-configs: \
  project-dhall/pkg-groups.dhall \
  project-dhall/pkgs-upgrade-todo.dhall
	mkdir -p project-cabal/pkgs && ./updo/project-dhall2config/pkg-groups.hs "$(GHC_VERSION)" "$(GHC_UPGRADE)"

# Mirror packages (package groups) from dhall to config with
# generate-pkg-configs doing the work once for all package groups.
#
# The "target: prerequisites ;" explicitly defines an empty recipe.
# SEE: https://www.gnu.org/software/make/manual/html_node/Empty-Recipes.html
project-cabal/pkgs/%.config: project-dhall/pkgs/%.dhall generate-pkg-configs ;

# Mirror constraints from dhall to config.
project-cabal/ghc-$(GHC_VERSION)/constraints.config : \
  updo/text-templates/cabal/package.dhall \
  project-dhall/ghc-$(GHC_VERSION)/constraints.dhall
	echo "let f = ./$< in f.constraints ./project-dhall/ghc-$(GHC_VERSION)/constraints.dhall" | dhall text --output $@

# Mirror constraints from dhall to config.
project-cabal/ghc-$(GHC_UPGRADE)/constraints.config : \
  updo/text-templates/cabal/package.dhall \
  project-dhall/ghc-$(GHC_UPGRADE)/constraints.dhall
	echo "let f = ./$< in f.constraints ./project-dhall/ghc-$(GHC_UPGRADE)/constraints.dhall" | dhall text --output $@

# Mirror source repositories from dhall to config.
project-cabal/ghc-%.config: \
  updo/text-templates/cabal/package.dhall \
  project-dhall/ghc-%.dhall
	echo "let f = ./$< in f.repo-items ./project-dhall/ghc-$*.dhall" | dhall text --output $@

ghc-$(GHC_VERSION).dhall2config.project: \
  project-dhall/ghc-$(GHC_VERSION)/text-templates/dhall2config.dhall \
  updo/text-templates/dhall2config.dhall \
  project-cabal/ghc-$(GHC_VERSION)/constraints.config \
  project-cabal/ghc-$(GHC_VERSION)/deps-external.config \
  project-cabal/ghc-$(GHC_VERSION)/deps-internal.config \
  project-cabal/ghc-$(GHC_VERSION)/forks-external.config \
  project-cabal/ghc-$(GHC_VERSION)/forks-internal.config \
  project-cabal/pkgs.config
	echo './$< "$(STACKAGE_VERSION)" "$(GHC_VERSION)"' | dhall text --output $@

ghc-$(GHC_UPGRADE).dhall2config.project: \
  project-dhall/ghc-$(GHC_UPGRADE)/text-templates/dhall2config.dhall \
  updo/text-templates/dhall2config.dhall \
  project-cabal/ghc-$(GHC_UPGRADE)/constraints.config \
  project-cabal/ghc-$(GHC_UPGRADE)/deps-external.config \
  project-cabal/ghc-$(GHC_UPGRADE)/deps-internal.config \
  project-cabal/ghc-$(GHC_UPGRADE)/forks-external.config \
  project-cabal/ghc-$(GHC_UPGRADE)/forks-internal.config \
  project-cabal/pkgs.config
	echo './$< "$(STACKAGE_UPGRADE)" "$(GHC_UPGRADE)"' | dhall text --output $@