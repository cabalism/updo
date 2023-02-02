.PHONY: project-files-stack2cabal

project-files-stack2cabal: \
  ghc-$(GHC_VERSION).stack2cabal.project \
  ghc-$(GHC_UPGRADE).stack2cabal.project

ghc-$(GHC_VERSION).stack2cabal.project: ghc-$(GHC_VERSION).$(STACK_DEFAULT).yaml
	stack2cabal --file $< --output-file $@

ghc-$(GHC_UPGRADE).stack2cabal.project: ghc-$(GHC_UPGRADE).$(STACK_DEFAULT).yaml
	stack2cabal --file $< --output-file $@

ghc-%.stack2cabal.project.freeze: ghc-%.stack2cabal.project
