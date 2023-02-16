.PHONY: \
  project-files-dhall2stack \
  project-files-dhall2cabal

project-files-dhall2stack: \
  ghc-$(GHC_VERSION).dhall2stack.yaml \
  ghc-$(GHC_VERSION).dhall2stack.yaml.lock \
  ghc-$(GHC_UPGRADE).dhall2stack.yaml \
  ghc-$(GHC_UPGRADE).dhall2stack.yaml.lock \

project-files-dhall2cabal: \
  ghc-$(GHC_VERSION).dhall2cabal.project \
  ghc-$(GHC_UPGRADE).dhall2cabal.project

project-dhall/pkgs-sorted.dhall: \
  project-dhall/pkg-groups.dhall \
  project-dhall/pkgs/*.dhall
	updo/project-dhall/pkgs-sorted.hs > $@

project-dhall/pkgs-upgrade-done.dhall: \
  project-dhall/pkgs/*.dhall \
  project-dhall/pkgs-sorted.dhall \
  project-dhall/pkgs-upgrade-todo.dhall
	./updo/project-dhall/pkgs-upgrade-done.hs ./project-dhall/pkgs-sorted.dhall ./project-dhall/pkgs-upgrade-todo.dhall > $@

ghc-$(GHC_VERSION).dhall2stack.yaml: \
  project-dhall/ghc-$(GHC_VERSION)/text-templates/dhall2stack.dhall \
  project-dhall/pkgs-sorted.dhall \
  project-dhall/ghc-$(GHC_VERSION)/*.dhall \
  updo/text-templates/stack/*.dhall
	echo './$< "$(STACKAGE_VERSION)"' | dhall text --output $@

ghc-$(GHC_UPGRADE).dhall2stack.yaml: \
  project-dhall/ghc-$(GHC_UPGRADE)/text-templates/dhall2stack.dhall \
  project-dhall/pkgs-upgrade-done.dhall \
  project-dhall/ghc-$(GHC_UPGRADE)/*.dhall \
  updo/text-templates/stack/*.dhall
	echo './$< "$(STACKAGE_UPGRADE)"' | dhall text --output $@

ghc-$(GHC_VERSION).dhall2cabal.project: \
  project-dhall/ghc-$(GHC_VERSION)/text-templates/dhall2cabal.dhall \
  project-dhall/pkgs-sorted.dhall \
  project-dhall/ghc-$(GHC_VERSION)/*.dhall \
  updo/text-templates/cabal/*.dhall
	echo './$< "$(STACKAGE_VERSION)"' | dhall text --output $@

ghc-$(GHC_UPGRADE).dhall2cabal.project: \
  project-dhall/ghc-$(GHC_UPGRADE)/text-templates/dhall2cabal.dhall \
  project-dhall/pkgs-upgrade-done.dhall \
  project-dhall/ghc-$(GHC_UPGRADE)/*.dhall \
  updo/text-templates/cabal/*.dhall
	echo './$< "$(STACKAGE_UPGRADE)"' | dhall text --output $@

ghc-%.dhall2stack.yaml.lock: ghc-%.dhall2stack.yaml
	stack build --dry-run --stack-yaml $<
