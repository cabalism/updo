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

```make
# How to generate project-nix/ghc-$(GHC_VERSION)/sha256map.nix?
# This is copied from ghc-$(GHC_VERSION).sha256map.nix.
#  - false to generate from *.dhall inputs via sha256map.hs.
#  - true to generate from stack.yaml via sha256map.py.
SHA256MAP_VIA_PYTHON := false

include project-versions.mk
include updo/Makefile

project-nix/ghc-%/sha256map.nix: ghc-%.sha256map.nix
	mkdir -p $(@D) && cp $^ $@

.PHONY: all
all: \
  projects \
  project-nix/ghc-$(GHC_VERSION)/sha256map.nix

# To make stack.yaml or cabal.project and no other, mark the file we copy from
# as intermediate. This is all we want when not doing a GHC upgrade.
#
# Comment out these .INTERMEDIATE targets to allow these files to be kept.
.INTERMEDIATE: ghc-$(GHC_VERSION).$(CABAL_VIA).project
.INTERMEDIATE: ghc-$(GHC_UPGRADE).$(CABAL_VIA).project
.INTERMEDIATE: ghc-$(GHC_VERSION).$(STACK_VIA).yaml
.INTERMEDIATE: ghc-$(GHC_UPGRADE).$(STACK_VIA).yaml
.INTERMEDIATE: ghc-$(GHC_VERSION).sha256map.nix
.INTERMEDIATE: ghc-$(GHC_UPGRADE).sha256map.nix

# Alternative targets for generating project files (not recommended):
#  - ghc-x.y.z.stack2cabal.project
#  - ghc-x.y.z.cabal2stack.yaml
#  - ghc-x.y.z.dhall2yaml2stack.yaml
include updo/alternatives/cabal2stack/Makefile
include updo/alternatives/stack2cabal/Makefile
include updo/alternatives/yaml2stack/Makefile

# If true, generate the sha256map from the stack.yaml with python,
# overriding the recipe for this target.
ifeq ($(SHA256MAP_VIA_PYTHON), true)
ghc-$(GHC_VERSION).sha256map.nix: stack.yaml
	updo/project-nix/sha256map.py <$^ >$@
ghc-$(GHC_UPGRADE).sha256map.nix: stack.yaml
	updo/project-nix/sha256map.py <$^ >$@
endif

.DEFAULT_GOAL := all
```

## Default Targets

The default targets are `stack.yaml` and `cabal.project`.

* [ ] Can we build just those without leaving behind[^check-temp-files] any
      temporary files?

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

* [ ] Does this make two sets of each of the following?

- `ghc-x.y.z.dhall2cabal.project`
- `ghc-x.y.z.dhall2config.project`
- `ghc-x.y.z.dhall2stack.yaml`
- `ghc-x.y.z.dhall2stack.yaml.lock`
- `ghc-x.y.z.sha256map.nix`

## Clean

* [ ] After making `all-possible-projects`, can we clean all of these?

```
$ make -f project-files.mk clean
```

## SHA Map Targets

* [ ] Can we build `ghc-x.y.z.sha256map.nix`?

```
$ make -f project-files.mk project-sha256maps
```

Try again flipping the sense of the variable SHA256MAP_VIA_PYTHON or by setting
it on the command line:

```
$ make -f project-files.mk ghc-x.y.z.sha256map.nix --always-make SHA256MAP_VIA_PYTHON=true
```

* [ ] Is it generating maps using another script, the Python one instead of the Haskell one?
* [ ] Are the entries in the generated map sorted in both cases?

This should make two of `ghc-x.y.z.sha256map.nix`[^version_only]. You might like to use those
file names as targets explicitly.

[^version_only]: For now we only provide the option to pick which script to use
  to generate `ghc-$(GHC_VERSION).sha256map.nix`. The
  `ghc-$(GHC_UPGRADE).sha256map.nix` is only ever generated using the Haskell
  script.

## Sorting Packages Works

We need to run a Haskell script to generate `.updo/pkgs-sorted.dhall`.  After a
cabal clean, this script will need to download dependencies.

* [ ] Can it do that without polluting the generated file with status updates from cabal saying
      that it has downloaded this or that dependency[^silent-script]?
      
[^silent-script]: The Haskell script must run silently for this to work.

```
$ cabal clean
$ make -f project-files.mk pkgs-sorted --always-make
updo/project-dhall/pkgs-sorted.hs > .updo/pkgs-sorted.dhall
```

## Upgrade Packages Done Works

Generated files, not for source control are written to  `.updo`.

* [ ] Can we generate `.updo/pkgs-upgrade-done.dhall`?
* [ ] Does adding or removing packages from
      `project-dhall/pkgs-upgrade-todo.dhall` change the list of packages in
      `.updo/pkgs-upgrade-done.dhall`?

```
$ make -f project-files.mk pkgs-upgrade-done --always-make
updo/project-dhall/pkgs-sorted.hs > .updo/pkgs-sorted.dhall
./updo/project-dhall/pkgs-upgrade-done.hs \
  ./.updo/pkgs-sorted.dhall \
  ./project-dhall/pkgs-upgrade-todo.dhall \
  > .updo/pkgs-upgrade-done.dhall
```

## Alternative Targets

* [ ] Can we build these targets?

- `ghc-x.y.z.dhall2yaml2stack.yaml`
- `ghc-x.y.z.stack2cabal.project`
- `ghc-x.y.z.cabal2stack.yaml`