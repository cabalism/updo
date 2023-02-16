# When the prerequisites are a function and a list, we echo a dhall expression
# that calls this function with the list. Both function and list are in dhall
# files that must be ./ prefixed to be imported into the expression.
#
# $(^:%=./%) does the ./ prefixing that dhall needs to recognize the imports.
#
# "The import resolution phase replaces all imports with the expression located
# at that import, transitively resolving imports within the imported expression
# if necessary."
# SOURCE:https://github.com/dhall-lang/dhall-lang/blob/master/standard/imports.md#import-resolution-judgment

project-files-dhall2yaml2stack: \
  ghc-$(GHC_VERSION).dhall2yaml2stack.yaml \
  ghc-$(GHC_VERSION).dhall2yaml2stack.yaml.lock \
  ghc-$(GHC_UPGRADE).dhall2yaml2stack.yaml \
  ghc-$(GHC_UPGRADE).dhall2yaml2stack.yaml.lock

# Mirror from pkgs dhall to yaml.
.INTERMEDIATE: project-stack/ghc-$(GHC_VERSION)/pkgs.yaml
project-stack/ghc-$(GHC_VERSION)/pkgs.yaml: \
  updo/project-dhall-text/stack/pkgs.dhall \
  project-dhall/pkgs-sorted.dhall
	echo "$(^:%=./%)" | dhall text --output $@

.INTERMEDIATE: project-stack/ghc-$(GHC_UPGRADE)/pkgs.yaml
project-stack/ghc-$(GHC_UPGRADE)/pkgs.yaml: \
  updo/project-dhall-text/stack/pkgs.dhall \
  project-dhall/pkgs-upgrade-done.dhall
	echo "$(^:%=./%)" | dhall text --output $@

# Mirror constraints from dhall to yaml.
.INTERMEDIATE: project-stack/ghc-%/constraints.yaml
project-stack/ghc-%/constraints.yaml: \
  updo/project-dhall-text/stack/constraints.dhall \
  project-dhall/ghc-%/constraints.dhall
	echo "$(^:%=./%)" | dhall text --output $@

# Mirror source repositories from dhall to yaml.
.INTERMEDIATE: project-stack/ghc-%.yaml
project-stack/ghc-%.yaml: \
  updo/project-dhall-text/stack/repos.dhall \
  project-dhall/ghc-%.dhall
	echo "$(^:%=./%)" | dhall text --output $@

# Join the YAML fragments into a single file, the stack project.
ghc-%.dhall2yaml2stack.yaml: \
  project-stack/ghc-%/base.yaml \
  project-stack/ghc-%/pkgs.yaml \
  project-stack/ghc-%/constraints.yaml \
  project-stack/ghc-%/deps-external.yaml \
  project-stack/ghc-%/deps-internal.yaml \
  project-stack/ghc-%/forks-external.yaml \
  project-stack/ghc-%/forks-internal.yaml \
  updo/alternatives/yaml2stack/join-sort-fragments.sh
	./updo/alternatives/yaml2stack/join-sort-fragments.sh $(<D) >$@

ghc-%.dhall2yaml2stack.yaml.lock: ghc-%.dhall2yaml2stack.yaml
	stack build --dry-run --stack-yaml $<

.INTERMEDIATE: project-stack/ghc-$(GHC_VERSION)/base.yaml
project-stack/ghc-$(GHC_VERSION)/base.yaml: \
  project-dhall/ghc-$(GHC_VERSION)/project-stacksnippet.dhall
	echo '$(<:%=./%) (Some "$(STACKAGE_VERSION)" : Optional Text)' | dhall text --output $@

.INTERMEDIATE: project-stack/ghc-$(GHC_UPGRADE)/base.yaml
project-stack/ghc-$(GHC_UPGRADE)/base.yaml: \
  project-dhall/ghc-$(GHC_UPGRADE)/project-stacksnippet.dhall
	echo '$(<:%=./%) (Some "$(STACKAGE_UPGRADE)" : Optional Text)' | dhall text --output $@