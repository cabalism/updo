.PHONY: cabal2stack-projects
cabal2stack-projects: \
  ghc-$(GHC_VERSION).cabal2stack.yaml \
  ghc-$(GHC_VERSION).cabal2stack.yaml.lock \
  ghc-$(GHC_UPGRADE).cabal2stack.yaml \
  ghc-$(GHC_UPGRADE).cabal2stack.yaml.lock

ghc-$(GHC_VERSION).cabal2stack.yaml: ghc-$(GHC_VERSION).$(CABAL_VIA).project
	rm -rf dist-newstyle/cache
	cabal build all --enable-tests --project-file=$< --dry-run
	cabal2stack --resolver=$(STACKAGE_VERSION) --allow-newer --output $@ --plan-json dist-newstyle/cache/plan.json

ghc-$(GHC_UPGRADE).cabal2stack.yaml: ghc-$(GHC_UPGRADE).$(CABAL_VIA).project
	rm -rf dist-newstyle/cache
	cabal build all --enable-tests --project-file=$< --dry-run
	cabal2stack --resolver=$(STACKAGE_UPGRADE) --allow-newer --output $@ --plan-json dist-newstyle/cache/plan.json

ghc-%.cabal2stack.yaml.lock: ghc-%.cabal2stack.yaml
	stack build --test --no-run-tests --bench --no-run-benchmarks --dry-run --stack-yaml $<
