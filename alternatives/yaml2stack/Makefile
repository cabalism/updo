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

Y2S_TMP := ./.updo/dhall2yaml2stack

project-files-dhall2yaml2stack: \
  ghc-$(GHC_VERSION).dhall2yaml2stack.yaml \
  ghc-$(GHC_VERSION).dhall2yaml2stack.yaml.lock \
  ghc-$(GHC_UPGRADE).dhall2yaml2stack.yaml \
  ghc-$(GHC_UPGRADE).dhall2yaml2stack.yaml.lock

.INTERMEDIATE: $(Y2S_TMP)/ghc-$(GHC_VERSION)/base.yaml
$(Y2S_TMP)/ghc-$(GHC_VERSION)/base.yaml: \
  project-dhall/ghc-$(GHC_VERSION)/text-templates/stacksnippet.dhall
	[ -d $(@D) ] || mkdir -p $(@D) && echo '$(<:%=./%) (Some "$(STACKAGE_VERSION)" : Optional Text)' | dhall text --output $@

.INTERMEDIATE: $(Y2S_TMP)/ghc-$(GHC_UPGRADE)/base.yaml
$(Y2S_TMP)/ghc-$(GHC_UPGRADE)/base.yaml: \
  project-dhall/ghc-$(GHC_UPGRADE)/text-templates/stacksnippet.dhall
	[ -d $(@D) ] || mkdir -p $(@D) && echo '$(<:%=./%) (Some "$(STACKAGE_UPGRADE)" : Optional Text)' | dhall text --output $@

# Mirror from pkgs dhall to yaml.
.INTERMEDIATE: $(Y2S_TMP)/ghc-$(GHC_VERSION)/pkgs.yaml
$(Y2S_TMP)/ghc-$(GHC_VERSION)/pkgs.yaml: \
  updo/text-templates/yaml2stack/pkgs.dhall \
  project-dhall/pkgs-sorted.dhall
	echo "$(^:%=./%)" | dhall text --output $@

.INTERMEDIATE: $(Y2S_TMP)/ghc-$(GHC_UPGRADE)/pkgs.yaml
$(Y2S_TMP)/ghc-$(GHC_UPGRADE)/pkgs.yaml: \
  updo/text-templates/yaml2stack/pkgs.dhall \
  project-dhall/pkgs-upgrade-done.dhall
	echo "$(^:%=./%)" | dhall text --output $@

# Mirror constraints from dhall to yaml.
$(Y2S_TMP)/ghc-%/constraints.yaml: \
  updo/text-templates/yaml2stack/constraints.dhall \
  project-dhall/ghc-%/constraints.dhall
	echo "$(^:%=./%)" | dhall text --output $@

# Mirror source repositories from dhall to yaml.
$(Y2S_TMP)/ghc-%.yaml: \
  updo/text-templates/yaml2stack/repo-items.dhall \
  project-dhall/ghc-%.dhall
	echo "$(^:%=./%)" | dhall text --output $@

# Join the YAML fragments into a single file, the stack project.
ghc-%.dhall2yaml2stack.yaml: \
  $(Y2S_TMP)/ghc-%/base.yaml \
  $(Y2S_TMP)/ghc-%/pkgs.yaml \
  $(Y2S_TMP)/ghc-%/constraints.yaml \
  $(Y2S_TMP)/ghc-%/deps-external.yaml \
  $(Y2S_TMP)/ghc-%/deps-internal.yaml \
  $(Y2S_TMP)/ghc-%/forks-external.yaml \
  $(Y2S_TMP)/ghc-%/forks-internal.yaml \
  updo/alternatives/yaml2stack/join-sort-fragments.sh
	./updo/alternatives/yaml2stack/join-sort-fragments.sh $(<D) >$@

ghc-%.dhall2yaml2stack.yaml.lock: ghc-%.dhall2yaml2stack.yaml
	stack build --dry-run --stack-yaml $<

# To keep dhall to yaml intermediate files, uncomment the following (no need to
# comment out the .INTERMEDIATE lines above):
.SECONDARY: $(wildcard $(Y2S_TMP)/*.yaml)