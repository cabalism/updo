# How are we going to generate stack.yaml and stack.yaml.lock files?
#
# Choices are:
#  - dhall2stack = via a temporary ghc-x.y.z.dhall2stack.yaml
#  - dhall2yaml2stack = via a temporary ghc-x.y.z.dhall2yaml2stack.yaml
STACK_VIA := dhall2stack

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
CABAL_VIA := dhall2config

# How to generate nix/services/stackProject/sha256map.nix?
#  - True to generate from *.dhall inputs.
#  - False to generate from stack.yaml.
DHALL_SHA256MAP := true

# Are we going to run cabal2stack or stack2cabal on the default project names?
# These mirrors (cabal2stack and stack2cabal) are slow.
CABAL2STACK := false
STACK2CABAL := false
include updo/alternatives/_2_mirrors/Makefile

# Updo Dhall gives us these targets:
#  - project-files-dhall2stack
#  - project-files-dhall2cabal
#  - project-files-dhall2config
#  - project-sha256maps
include updo/project-dhall/Makefile
include updo/alternatives/yaml2stack/Makefile
include updo/project-dhall2config/Makefile
include updo/project-nix/Makefile

ifeq ($(DHALL_SHA256MAP), true)
nix/services/stackProject/sha256map.nix: ghc-$(GHC_VERSION).sha256map.nix
	cp $^ $@
else
nix/services/stackProject/sha256map.nix: stack.yaml
	updo/project-nix/sha256map-regenerate.py <$^ >$@
endif

# Project files used inproduction, not in GHC upgrade.
#
# By waiting for targets to the left of .WAIT, we ensure that what cabal
# downloads is not read before it is completely written.
.PHONY: all
all: \
  project-dhall/pkgs-sorted.dhall \
  nix/services/stackProject/sha256map.nix \
  .WAIT \
  stack.yaml \
  stack.yaml.lock \
  cabal.project

# "You can create an actual target .WAIT in your makefile for portability but
# this is not required to use this feature. If a .WAIT target is created it
# should not have prerequisites or commands."
# SOURCE: https://www.gnu.org/software/make/manual/html_node/Parallel.html
#
# A .WAIT target is not meant to be required but we need it to avoid:
# make: *** No rule to make target '.WAIT', needed by 'project-files'.  Stop.
.WAIT:

.NOTPARALLEL: \
  project-dhall/pkgs-sorted.dhall \
  ghc-%.sha256map.nix \
  nix/services/stackProject/sha256map.nix 

.PHONY: upgrade
upgrade: \
  ghc-$(GHC_UPGRADE).stack.yaml \
  ghc-$(GHC_UPGRADE).stack.yaml.lock \
  ghc-$(GHC_UPGRADE).cabal.project \
  ghc-$(GHC_UPGRADE).dhall2stack.yaml \
  ghc-$(GHC_UPGRADE).dhall2stack.yaml.lock \
  ghc-$(GHC_UPGRADE).dhall2cabal.project

# All the kinds of project files we might want to generate.
#
#  - project-files-mirror is included but will only generate projects if
#    CABAL2STACK or STACK2CABAL is true.
#
# These are alternative methods we could include but don't.
#  - project-files-dhall2yaml2stack
.PHONY: all-possible-projects
all-possible-projects: \
  project-files \
  project-files-mirror \
  project-files-dhall2config \
  project-files-dhall2cabal \
  project-files-dhall2stack

.PHONY: clean
clean:
	rm -f ghc-*.stack.* ghc-*.dhall2config.* ghc-*.dhall2cabal.* ghc-*.dhall2stack.* ghc-*.stack2cabal.* ghc-*.cabal2stack.* ghc-*.dhall2yaml2stack.*

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

stack.yaml: ghc-$(GHC_VERSION).$(STACK_VIA).yaml
	cp $< $@

stack.yaml.lock: stack.yaml
	stack build --dry-run --stack-yaml $<
