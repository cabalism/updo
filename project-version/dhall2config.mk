ghc-$(GHC_VERSION).dhall2config.project: \
  project-dhall/ghc-$(GHC_VERSION)/text-templates/dhall2config.dhall \
  project-dhall/ghc-$(GHC_VERSION)/text-templates/wrap-cabal.dhall \
  updo/text-templates/dhall2config.dhall \
  project-cabal/ghc-$(GHC_VERSION)/constraints.config \
  project-cabal/ghc-$(GHC_VERSION)/deps-external.config \
  project-cabal/ghc-$(GHC_VERSION)/deps-internal.config \
  project-cabal/ghc-$(GHC_VERSION)/forks-external.config \
  project-cabal/ghc-$(GHC_VERSION)/forks-internal.config \
  project-cabal/pkgs.config
	echo './$< "$(STACKAGE_VERSION)" "$(GHC_VERSION)"' \
		| dhall text --output $(UPDO_TMP)/gen-config.txt
	echo 'let generated-project = "$${./$(UPDO_TMP)/gen-config.txt as Text}" in ./project-dhall/ghc-$(GHC_VERSION)/text-templates/wrap-cabal.dhall "$(GHC_VERSION)" (Some "$(STACKAGE_VERSION)") generated-project' \
		| dhall text --output $@
