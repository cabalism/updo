UPDO_TMP := ./.updo

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

.PHONY: pkgs-sorted
pkgs-sorted: $(UPDO_TMP)/pkgs-sorted.dhall

.PHONY: pkgs-upgrade-done
pkgs-upgrade-done: $(UPDO_TMP)/pkgs-upgrade-done.dhall

$(UPDO_TMP)/pkgs-sorted.dhall: \
  project-dhall/pkg-groups.dhall \
  project-dhall/pkgs/*.dhall
	mkdir -p $(@D) && updo/project-dhall/pkgs-sorted.hs > $@

$(UPDO_TMP)/pkgs-upgrade-done.dhall: \
  project-dhall/pkgs/*.dhall \
  $(UPDO_TMP)/pkgs-sorted.dhall \
  project-dhall/pkgs-upgrade-todo.dhall
	./updo/project-dhall/pkgs-upgrade-done.hs \
	  ./$(UPDO_TMP)/pkgs-sorted.dhall \
	  ./project-dhall/pkgs-upgrade-todo.dhall \
	  > $@

ghc-$(GHC_VERSION).dhall2stack.yaml: \
  project-dhall/ghc-$(GHC_VERSION)/text-templates/dhall2stack.dhall \
  $(UPDO_TMP)/pkgs-sorted.dhall \
  project-dhall/ghc-$(GHC_VERSION)/*.dhall \
  updo/text-templates/stack/*.dhall
	echo './$< ./$(UPDO_TMP)/pkgs-sorted.dhall "$(STACKAGE_VERSION)"' | dhall text --output $@

ghc-$(GHC_UPGRADE).dhall2stack.yaml: \
  project-dhall/ghc-$(GHC_UPGRADE)/text-templates/dhall2stack.dhall \
  $(UPDO_TMP)/pkgs-upgrade-done.dhall \
  project-dhall/ghc-$(GHC_UPGRADE)/*.dhall \
  updo/text-templates/stack/*.dhall
	echo './$< ./$(UPDO_TMP)/pkgs-upgrade-done.dhall "$(STACKAGE_UPGRADE)"' | dhall text --output $@

ghc-$(GHC_VERSION).dhall2cabal.project: \
  project-dhall/ghc-$(GHC_VERSION)/text-templates/dhall2cabal.dhall \
  $(UPDO_TMP)/pkgs-sorted.dhall \
  project-dhall/ghc-$(GHC_VERSION)/*.dhall \
  updo/text-templates/cabal/*.dhall
	echo './$< ./$(UPDO_TMP)/pkgs-sorted.dhall "$(STACKAGE_VERSION)"' | dhall text --output $@

ghc-$(GHC_UPGRADE).dhall2cabal.project: \
  project-dhall/ghc-$(GHC_UPGRADE)/text-templates/dhall2cabal.dhall \
  $(UPDO_TMP)/pkgs-upgrade-done.dhall \
  project-dhall/ghc-$(GHC_UPGRADE)/*.dhall \
  updo/text-templates/cabal/*.dhall
	echo './$< ./$(UPDO_TMP)/pkgs-upgrade-done.dhall "$(STACKAGE_UPGRADE)"' | dhall text --output $@

ghc-%.dhall2stack.yaml.lock: ghc-%.dhall2stack.yaml
	stack build --dry-run --stack-yaml $<
