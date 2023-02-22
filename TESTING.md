# Testing Updo

Assuming this layout of make files in updo and at the root of your project.

```
├── updo
│   ├── Makefile
│   ├── project-dhall
│   │   └── Makefile
│   ├── project-dhall2config
│   │   ├── Makefile
│   └── project-nix
│       └── Makefile
├── project-files.mk
└── project-versions.mk
```

We'll need some versions, something like:

```
$ cat project-versions.mk 
# Versions of GHC and stackage resolver, the ones we're on and the next ones
# we're upgrading to.
GHC_VERSION := 8.10.7
GHC_UPGRADE := 9.2.5
STACKAGE_VERSION := lts-18.27
STACKAGE_UPGRADE := lts-20.5
```

We'll need to set up our `project-files.mk`:

```
$ cat project-files.mk
# How to generate nix/services/stackProject/sha256map.nix?
#  - True to generate from *.dhall inputs.
#  - False to generate from stack.yaml.
DHALL_SHA256MAP := true

include project-versions.mk
include updo/Makefile

ifeq ($(DHALL_SHA256MAP), true)
nix/services/stackProject/sha256map.nix: ghc-$(GHC_VERSION).sha256map.nix
	cp $^ $@
else
nix/services/stackProject/sha256map.nix: stack.yaml
	updo/project-nix/sha256map.py <$^ >$@
endif

.PHONY: all
all: \
  projects \
  nix/services/stackProject/sha256map.nix

# To make stack.yaml or cabal.project and no other, mark the file we copy from
# as intermediate. This is all we want when not doing a GHC upgrade.
#
# Comment out these .INTERMEDIATE targets to allow these files to be kept.
.INTERMEDIATE: ghc-$(GHC_VERSION).$(CABAL_VIA).project
.INTERMEDIATE: ghc-$(GHC_VERSION).$(STACK_VIA).yaml
.INTERMEDIATE: ghc-$(GHC_VERSION).sha256map.nix

# Alternative targets for generating project files (not recommended):
#  - ghc-x.y.z.stack2cabal.project
#  - ghc-x.y.z.cabal2stack.yaml
#  - ghc-x.y.z.dhall2yaml2stack.yaml
include updo/alternatives/cabal2stack/Makefile
include updo/alternatives/stack2cabal/Makefile
include updo/alternatives/yaml2stack/Makefile
```

## Default Targets

The default targets are `stack.yaml` and `cabal.project`. Can we build just
those without leaving behind[^check-temps] any temporary files?

[^check-temp-files]: You could check for unwanted files with `git status`.

```
$ make -f project-files.mk all --always-make
$ make -f project-files.mk all
```

## All Possible Targets

Comment out the `.INTERMEDIATE` targets in `project-files.mk` and then:

```
$ make -f project-files.mk all-possible-projects --always-make --jobs
```

This should make two sets of:
- `ghc-x.y.z.dhall2cabal.project`
- `ghc-x.y.z.dhall2config.project`
- `ghc-x.y.z.dhall2stack.yaml`
- `ghc-x.y.z.dhall2stack.yaml.lock`
- `ghc-x.y.z.sha256map.nix`

## Clean

After making `all-possible-projects`, can we clean all of these?

```
$ make -f project-files.mk clean
```

## SHA Map Targets

Can we build `ghc-x.y.z.sha256map.nix`?

```
$ make -f project-files.mk project-sha256maps
```

Try again flipping the sense of the variable DHALL_SHA256MAP. Is it generating
maps using another script, the Python one instead of the Haskell one? Are the
entries in the generated map sorted in both cases?

This should make two of `ghc-x.y.z.sha256map.nix`. You might like to use those
file names as targets explicitly.

## Sorting Packages Works

We need to run a Haskell script to generate `project-dhall/pkgs-sorted.dhall`.
After a cabal clean, this script will need to download dependencies. Can it do
that without polluting the generated file with status updates from cabal saying
that it has downloaded this or that dependency? The Haskell script must run
silently for this to work.

```
$ cabal clean
$ make -f project-files.mk project-dhall/pkgs-sorted.dhall --always-make
updo/project-dhall/pkgs-sorted.hs > project-dhall/pkgs-sorted.dhall
```

## Alternative Targets

Can we build these targets?

- `ghc-x.y.z.dhall2yaml2stack.yaml`
- `ghc-x.y.z.stack2cabal.project`
- `ghc-x.y.z.cabal2stack.yaml`