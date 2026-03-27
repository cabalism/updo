config-upgrade: \
  $(UPDO_TMP)/ghc-$(GHC_UPGRADE)/constraints.dhall \
  $(UPDO_TMP)/ghc-$(GHC_UPGRADE)/deps-external.dhall \
  $(UPDO_TMP)/ghc-$(GHC_UPGRADE)/deps-internal.dhall \
  $(UPDO_TMP)/ghc-$(GHC_UPGRADE)/forks-external.dhall \
  $(UPDO_TMP)/ghc-$(GHC_UPGRADE)/forks-internal.dhall

ghc-$(GHC_UPGRADE).dhall2cabal.project: \
  project-dhall/ghc-$(GHC_UPGRADE)/text-templates/dhall2cabal.dhall \
  project-dhall/ghc-$(GHC_UPGRADE)/text-templates/wrap-cabal.dhall \
  $(UPDO_TMP)/pkgs-upgrade-done.dhall \
  config-upgrade \
  updo/text-templates/cabal/*.dhall
	echo './$< ./$(UPDO_TMP)/pkgs-upgrade-done.dhall "$(STACKAGE_UPGRADE)"' \
    	| dhall text \
    	| sed -E "s/@(rev|sha256):.*$$//" \
    	| xargs echo 'project-dhall/ghc-$(GHC_UPGRADE)/text-templates/wrap-cabal.dhall "$(GHC_UPGRADE)" "$(STACKAGE_UPGRADE)"' \
    	| dhall text --output $@
