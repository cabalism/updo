ifeq ($(PKGS_SORTED_HS_EXE), true)
PKGS_SORTED_HS := updo-pkgs-sorted
else
PKGS_SORTED_HS := ./updo/project-dhall/pkgs-sorted.hs
endif

ifeq ($(PKGS_UPGRADE_DONE_HS_EXE), true)
PKGS_UPGRADE_DONE_HS := updo-pkgs-upgrade-done
else
PKGS_UPGRADE_DONE_HS := ./updo/project-dhall/pkgs-upgrade-done.hs
endif

config-version: \
  $(UPDO_TMP)/ghc-$(GHC_VERSION)/constraints.dhall \
  $(UPDO_TMP)/ghc-$(GHC_VERSION)/deps-external.dhall \
  $(UPDO_TMP)/ghc-$(GHC_VERSION)/deps-internal.dhall \
  $(UPDO_TMP)/ghc-$(GHC_VERSION)/forks-external.dhall \
  $(UPDO_TMP)/ghc-$(GHC_VERSION)/forks-internal.dhall

config-upgrade: \
  $(UPDO_TMP)/ghc-$(GHC_UPGRADE)/constraints.dhall \
  $(UPDO_TMP)/ghc-$(GHC_UPGRADE)/deps-external.dhall \
  $(UPDO_TMP)/ghc-$(GHC_UPGRADE)/deps-internal.dhall \
  $(UPDO_TMP)/ghc-$(GHC_UPGRADE)/forks-external.dhall \
  $(UPDO_TMP)/ghc-$(GHC_UPGRADE)/forks-internal.dhall

.PHONY: dhall2stack-projects
dhall2stack-projects: \
  ghc-$(GHC_VERSION).dhall2stack.yaml \
  ghc-$(GHC_VERSION).dhall2stack.yaml.lock \
  ghc-$(GHC_UPGRADE).dhall2stack.yaml \
  ghc-$(GHC_UPGRADE).dhall2stack.yaml.lock \

.PHONY: dhall2cabal-projects
dhall2cabal-projects: \
  ghc-$(GHC_VERSION).dhall2cabal.project \
  ghc-$(GHC_UPGRADE).dhall2cabal.project

ghc-$(GHC_VERSION).dhall2stack.yaml: \
  project-dhall/ghc-$(GHC_VERSION)/text-templates/dhall2stack.dhall \
  $(UPDO_TMP)/pkgs-sorted.dhall \
  config-version \
  updo/text-templates/stack/*.dhall
	echo './$< ./$(UPDO_TMP)/pkgs-sorted.dhall "$(STACKAGE_VERSION)"' | dhall text --output $@

ghc-$(GHC_UPGRADE).dhall2stack.yaml: \
  project-dhall/ghc-$(GHC_UPGRADE)/text-templates/dhall2stack.dhall \
  $(UPDO_TMP)/pkgs-upgrade-done.dhall \
  config-upgrade \
  updo/text-templates/stack/*.dhall
	echo './$< ./$(UPDO_TMP)/pkgs-upgrade-done.dhall "$(STACKAGE_UPGRADE)"' | dhall text --output $@

ghc-$(GHC_VERSION).dhall2cabal.project: \
  project-dhall/ghc-$(GHC_VERSION)/text-templates/dhall2cabal.dhall \
  $(UPDO_TMP)/pkgs-sorted.dhall \
  config-version \
  updo/text-templates/cabal/*.dhall
	echo './$< ./$(UPDO_TMP)/pkgs-sorted.dhall "$(STACKAGE_VERSION)"' | dhall text --output $@

ghc-$(GHC_UPGRADE).dhall2cabal.project: \
  project-dhall/ghc-$(GHC_UPGRADE)/text-templates/dhall2cabal.dhall \
  $(UPDO_TMP)/pkgs-upgrade-done.dhall \
  config-upgrade \
  updo/text-templates/cabal/*.dhall
	echo './$< ./$(UPDO_TMP)/pkgs-upgrade-done.dhall "$(STACKAGE_UPGRADE)"' | dhall text --output $@

ghc-%.dhall2stack.yaml.lock: ghc-%.dhall2stack.yaml
	stack build  --test --no-run-tests --bench --no-run-benchmarks --dry-run --stack-yaml $<
