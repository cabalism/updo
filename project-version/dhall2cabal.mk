config-version: \
  $(UPDO_TMP)/ghc-$(GHC_VERSION)/constraints.dhall \
  $(UPDO_TMP)/ghc-$(GHC_VERSION)/deps-external.dhall \
  $(UPDO_TMP)/ghc-$(GHC_VERSION)/deps-internal.dhall \
  $(UPDO_TMP)/ghc-$(GHC_VERSION)/forks-external.dhall \
  $(UPDO_TMP)/ghc-$(GHC_VERSION)/forks-internal.dhall

ghc-$(GHC_VERSION).dhall2cabal.project: \
  project-dhall/ghc-$(GHC_VERSION)/text-templates/dhall2cabal.dhall \
  project-dhall/ghc-$(GHC_VERSION)/text-templates/wrap-cabal.dhall \
  $(UPDO_TMP)/pkgs-sorted.dhall \
  config-version \
  updo/text-templates/cabal/*.dhall
	echo './$< ./$(UPDO_TMP)/pkgs-sorted.dhall "$(STACKAGE_VERSION)"' \
    	| dhall text \
	    | sed -E "s/@(rev|sha256):.*$$//" \
		> $(UPDO_TMP)/gen-cabal.txt
	echo 'let generated-project = "$${./$(UPDO_TMP)/gen-cabal.txt as Text}" in ./project-dhall/ghc-$(GHC_VERSION)/text-templates/wrap-cabal.dhall "$(GHC_VERSION)" (Some "$(STACKAGE_VERSION)") generated-project' \
    	| dhall text --output $@
