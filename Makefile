# How are we going to generate stack.yaml and stack.yaml.lock files?
#
# Choices are:
#  - dhall2stack = via a temporary ghc-x.y.z.dhall2stack.yaml
#  - dhall2yaml2stack = via a temporary ghc-x.y.z.dhall2yaml2stack.yaml
STACK_VIA ?= dhall2stack

# How are we going to generate the cabal.project file?
#
# Choices are:
#  - dhall2config = via a temporary ghc-x.y.z.dhall2config.project
#  - dhall2cabal = via a temporary ghc-x.y.z.dhall2cabal.project
#
# dhall2config is good when upgrading to a new GHC version. The
# <pkg-group>.config files will show how many in a group are yet to be upgraded.
# With the cabal imports it is possible to try things out quickly by editing the
# config files themselves if you want to momentarily sidestep Updo's make cabal
# project generation.
#
# dhall2cabal is good if you want the cabal project to closely match the layout
# of the stack project when that is generated using dhall2stack.

# dhall2cabal is also good if you need to use cabal < 3.8 where everything needs
# to be in one file. To achieve this, the
# project-dhall/project-dhall2cabal.dhall template will need to bring in the
# contents of the stackage import like this:
#
# ${../../project-stackage/$(STACKAGE_VERSION).config as Text}
CABAL_VIA ?= dhall2config

# Updo Dhall gives us these targets:
#  - dhall2stack-projects
#  - dhall2cabal-projects
#  - dhall2config-projects
#  - project-sha256maps
include updo/temp/Makefile
include updo/project-dhall/Makefile
include updo/project-dhall2config/Makefile
include updo/project-nix/Makefile

# Project (and related sha256map) files used in production, not in GHC upgrade.
.PHONY: projects
projects: \
  ghc-$(GHC_VERSION).sha256map.nix \
  stack.yaml \
  stack.yaml.lock \
  cabal.project

.PHONY: upgrade-projects
upgrade-projects: \
  project-nix/ghc-$(GHC_UPGRADE)/sha256map.nix \
  stack.upgrade.yaml \
  stack.upgrade.yaml.lock \
  cabal.upgrade.project \
  ghc-$(GHC_UPGRADE).dhall2stack.yaml \
  ghc-$(GHC_UPGRADE).dhall2stack.yaml.lock \
  ghc-$(GHC_UPGRADE).dhall2cabal.project

# All the kinds of project files we might want to generate.
#
# These are alternative methods we could include but don't.
#  - dhall2yaml2stack-projects
.PHONY: all-possible-projects
all-possible-projects: \
  projects \
  dhall2config-projects \
  dhall2cabal-projects \
  dhall2stack-projects

.PHONY: clean
clean:
	rm -f ghc-*.stack.* ghc-*.dhall2config.* ghc-*.dhall2cabal.* ghc-*.dhall2stack.* ghc-*.stack2cabal.* ghc-*.cabal2stack.* ghc-*.dhall2yaml2stack.* ghc-*.sha256map.nix

.PHONY: cabal
cabal: \
  ghc-$(GHC_VERSION).$(CABAL_VIA).project \
  ghc-$(GHC_UPGRADE).$(CABAL_VIA).project

.PHONY: stack
stack: \
  ghc-$(GHC_VERSION).$(STACK_VIA).yaml \
  ghc-$(GHC_VERSION).$(STACK_VIA).yaml.lock \
  ghc-$(GHC_UPGRADE).$(STACK_VIA).yaml \
  ghc-$(GHC_UPGRADE).$(STACK_VIA).yaml.lock

cabal.project: ghc-$(GHC_VERSION).$(CABAL_VIA).project
	cp $^ $@

cabal.upgrade.project: ghc-$(GHC_UPGRADE).$(CABAL_VIA).project
	cp $^ $@

stack.yaml: ghc-$(GHC_VERSION).$(STACK_VIA).yaml
	cp $< $@

stack.upgrade.yaml: ghc-$(GHC_UPGRADE).$(STACK_VIA).yaml
	cp $< $@

stack.yaml.lock: stack.yaml
	stack build --test --no-run-tests --bench --no-run-benchmarks --dry-run --stack-yaml $<

stack.upgrade.yaml.lock: stack.upgrade.yaml
	stack build --test --no-run-tests --bench --no-run-benchmarks --dry-run --stack-yaml $<
