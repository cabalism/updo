config-upgrade: \
  $(UPDO_TMP)/ghc-$(GHC_UPGRADE)/constraints.dhall \
  $(UPDO_TMP)/ghc-$(GHC_UPGRADE)/deps-external.dhall \
  $(UPDO_TMP)/ghc-$(GHC_UPGRADE)/deps-internal.dhall \
  $(UPDO_TMP)/ghc-$(GHC_UPGRADE)/forks-external.dhall \
  $(UPDO_TMP)/ghc-$(GHC_UPGRADE)/forks-internal.dhall

ghc-$(GHC_UPGRADE).dhall2stack.yaml: \
  project-dhall/ghc-$(GHC_UPGRADE)/text-templates/dhall2stack.dhall \
  project-dhall/ghc-$(GHC_UPGRADE)/text-templates/wrap-stack.dhall \
  $(UPDO_TMP)/pkgs-upgrade-done.dhall \
  config-upgrade \
  updo/text-templates/stack/*.dhall
	echo './$< ./$(UPDO_TMP)/pkgs-upgrade-done.dhall "$(STACKAGE_UPGRADE)"' \
    	| xargs echo 'project-dhall/ghc-$(GHC_UPGRADE)/text-templates/wrap-stack.dhall "$(GHC_UPGRADE)" "$(STACKAGE_UPGRADE)"' \
    	| dhall text --output $@
