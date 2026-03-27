config-version: \
  $(UPDO_TMP)/ghc-$(GHC_VERSION)/constraints.dhall \
  $(UPDO_TMP)/ghc-$(GHC_VERSION)/deps-external.dhall \
  $(UPDO_TMP)/ghc-$(GHC_VERSION)/deps-internal.dhall \
  $(UPDO_TMP)/ghc-$(GHC_VERSION)/forks-external.dhall \
  $(UPDO_TMP)/ghc-$(GHC_VERSION)/forks-internal.dhall

ghc-$(GHC_VERSION).dhall2stack.yaml: \
  project-dhall/ghc-$(GHC_VERSION)/text-templates/dhall2stack.dhall \
  project-dhall/ghc-$(GHC_VERSION)/text-templates/wrap-stack.dhall \
  $(UPDO_TMP)/pkgs-sorted.dhall \
  config-version \
  updo/text-templates/stack/*.dhall
	echo './$< ./$(UPDO_TMP)/pkgs-sorted.dhall "$(STACKAGE_VERSION)"' \
	    | dhall text --output $(UPDO_TMP)/gen-stack.txt
	echo 'let generated-project = "$${./$(UPDO_TMP)/gen-stack.txt as Text}" in ./project-dhall/ghc-$(GHC_VERSION)/text-templates/wrap-stack.dhall "$(GHC_VERSION)" (Some "$(STACKAGE_VERSION)") generated-project' \
		| dhall text --output $@