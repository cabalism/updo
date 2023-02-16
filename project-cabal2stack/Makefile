.PHONY: project-files-cabal2stack

project-files-cabal2stack: \
  ghc-$(GHC_VERSION).cabal2stack.yaml \
  ghc-$(GHC_VERSION).cabal2stack.yaml.lock \
  ghc-$(GHC_UPGRADE).cabal2stack.yaml \
  ghc-$(GHC_UPGRADE).cabal2stack.yaml.lock

ghc-$(GHC_VERSION).cabal2stack.yaml: ghc-$(GHC_VERSION).$(CABAL_VIA).project
	rm -rf dist-newstyle/cache || true
	cabal build all --enable-tests --project-file=$< --dry-run
	cabal2stack --resolver=$(STACKAGE_VERSION) --allow-newer --output $@ --plan-json dist-newstyle/cache/plan.json

ghc-$(GHC_UPGRADE).cabal2stack.yaml: ghc-$(GHC_UPGRADE).$(CABAL_VIA).project
	rm -rf dist-newstyle/cache || true
	cabal build all --enable-tests --project-file=$< --dry-run
	cabal2stack --resolver=$(STACKAGE_UPGRADE) --allow-newer --output $@ --plan-json dist-newstyle/cache/plan.json

ghc-%.cabal2stack.yaml.lock: ghc-%.cabal2stack.yaml
	stack build --dry-run --stack-yaml $<
