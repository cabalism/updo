ghc-$(GHC_UPGRADE).dhall2config.project: \
  project-dhall/ghc-$(GHC_UPGRADE)/text-templates/dhall2config.dhall \
  project-dhall/ghc-$(GHC_UPGRADE)/text-templates/wrap-cabal.dhall \
  updo/text-templates/dhall2config.dhall \
  project-cabal/ghc-$(GHC_UPGRADE)/constraints.config \
  project-cabal/ghc-$(GHC_UPGRADE)/deps-external.config \
  project-cabal/ghc-$(GHC_UPGRADE)/deps-internal.config \
  project-cabal/ghc-$(GHC_UPGRADE)/forks-external.config \
  project-cabal/ghc-$(GHC_UPGRADE)/forks-internal.config \
  project-cabal/pkgs.config
	echo './$< "$(STACKAGE_UPGRADE)" "$(GHC_UPGRADE)"' \
    	| dhall text \
    	| xargs echo 'project-dhall/ghc-$(GHC_UPGRADE)/text-templates/wrap-cabal.dhall "$(GHC_UPGRADE)" "$(STACKAGE_UPGRADE)"' \
    	| dhall text --output $@
