.PHONY: stack2cabal-projects
stack2cabal-projects: \
  ghc-$(GHC_VERSION).stack2cabal.project \
  ghc-$(GHC_UPGRADE).stack2cabal.project

ghc-$(GHC_VERSION).stack2cabal.project: ghc-$(GHC_VERSION).$(STACK_VIA).yaml
	stack2cabal --file $< --output-file $@

ghc-$(GHC_UPGRADE).stack2cabal.project: ghc-$(GHC_UPGRADE).$(STACK_VIA).yaml
	stack2cabal --file $< --output-file $@

ghc-%.stack2cabal.project.freeze: ghc-%.stack2cabal.project
