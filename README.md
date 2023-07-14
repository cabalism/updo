# 🫸 Updo 🫷

> A hairstyle and a way of maintaining and upgrading Haskell projects.

### What is a Project?

Updo is good for project packages, constraints, source repository packages and
stackage resolver[^download-stackage-config]. More than this can be added into a
project template.

[^download-stackage-config]: For cabal, the cabal.config will have to be
  downloaded to `project-stackage/resolver.config` and then edited to comment
  out constraints that would otherwise lead to impossible to solve conflicting
  constraints because cabal constraints are additive and cannot be overridden.

### Goals

1. Map a single set of packages, constraints and source repository packages to
   projects for cabal and stack.
2. Minimize the effort of upgrading constraints, source repository packages and
   GHC versions.

We do this with configuration in `.dhall` files, makefiles and `dhall text`
templates. We also have some simple Haskell scripts for operations not possible
in Dhall like sorting a `List Text`[^dhall-sort-list-nat].

[^dhall-sort-list-nat]: Dhall can only sort `List Natural`.

# Template Outputs

We generate projects from the configured inputs using templates.  With the
`dhall text` command we're able to mirror to both stack and cabal projects using
text templates. See [templates](./TEMPLATES.md) for more.

We provide one stack template `dhall2stack`. The `dhall2cabal` template for
cabal is so similar that the generated outputs can be compared using file diff
tooling. However, we prefer the `dhall2config` template because it retains
package groups that can bring order to an [upgrade](#upgrading-a-project) if the
cabal solving order is roughly captured by package group ordering.

We provide one level of nesting for grouping packages together as inputs. Groups
are preserved or flattened depending on the template.

* `dhall2stack` - Stack projects cannot import so necessarily must lose the
  grouping and contain the list of packages.
* `dhall2cabal` - This template lists packages within the cabal project.
* `dhall2config` - When generating the cabal project we keep [package
  groups](#package-groups), generating the same tree structure but replacing the
  leaves. From each `group-name.dhall` we generate a `group-name.config` file.

It is nice to be able to look at packages both grouped and as one sorted list.
Find the flat sorted lists of packages used by some templates in the `.updo`
folder.

```
.updo
├── pkgs-sorted.dhall        ▨ List Text
└── pkgs-upgrade-done.dhall  ▨ List Text
```

If you don't want groups then please put all packages in the one group.

# Maintaining a Project

Maintaining a project involves configuring or reconfiguring inputs and then
using make to generate the projects. What do we want to configure? We don't need
much:

1. A stackage resolver[^download-stackage-config].
2. A list of constraints (package dependencies with their versions).
3. A list of source repository packages, for unpublished[^unpublished-packages]
  packages or for unpublished versions of packages.
4. A list of packages.

[^unpublished-packages]: A package not published to hackage or stackage.


All configuration goes into `./project-dhall` (where `.` is the root folder for
your Haskell project) except for the `cabal.config` that we'll need to download
from stackage. Save this file as `project-stackage/lts-m.n.config` using the
exact resolver name.

```
project-stackage
└── lts-m.n.config
```

For each `ghc-x.y.z` compiler version, create this set of inputs and templates:

```
project-dhall
└── ghc-x.y.z
    ├── constraints.dhall      ▨ List { dep : Text, ver : Text }
    ├── deps-external.dhall    ▨ List { loc : Text, tag : Text, sub : List Text }
    ├── deps-internal.dhall    ▨ List { loc : Text, tag : Text, sub : List Text }
    ├── forks-external.dhall   ▨ List { loc : Text, tag : Text, sub : List Text }
    ├── forks-internal.dhall   ▨ List { loc : Text, tag : Text, sub : List Text }
    └── text-templates
        ├── dhall2cabal.dhall  ▨ template for ghc-x.y.z.dhall2cabal.project
        ├── dhall2config.dhall ▨ template for ghc-x.y.z.dhall2config.project
        ├── dhall2stack.dhall  ▨ template for ghc-x.y.z.dhall2stack.yaml
        └── stacksnippet.dhall ▨ anything for ghc-x.y.z.dhall2stack.yaml
```

Anything in `stacksnippet.dhall` gets added to the top of the generated stack
project[^base.yaml].  The rest of the files are inputs.

[^base.yaml]: Anything in `stacksnippet.dhall` will be used by
  [dhall2yaml2stack](alternatives/yaml2stack#readme) too and is put into a
  `base.yaml`, the topmost fragment when stitching together fragments.

### 1. Stackage Resolver and Constraints

The stackage resolver is able to provide us with a list of packages that work
together for both stack and cabal. Any other dependencies as constraints or
source packages are either not found on stackage or are different versions.

By specifying a resolver with stack we get a list of exact `==` versions for
packages published to stackage. Stackage also publishes a `cabal.config` file
that has these package versions as cabal constraints. As cabal constraints are
additive, in practice importing directly from stackage often leads to conflicts
between stackage's constraints and the project's own constraints and for that
reason we'll usually have to download the `cabal.config` from
stackage[^stackage-config-url-path] and comment out lines that are conflicted.

[^stackage-config-url-path]: The URL path to download this file from is
  `/resolver/cabal.config`. The latest nightly is always at
  https://www.stackage.org/nightly/cabal.config.

We don't expect any packages to impose constraints on their dependencies at the
package level, in their `.cabal` files, but if they do then these constraints
must fit with constraints at the project level.

### 2. Constraints

In constraints[^constraints] put published packages that you want to use that are
not on stackage or if they are on stackage where you want to use a different
version.

[^constraints]: To stack, constraints and source repository packages are
    both `extra-deps`. We use cabal-like nomenclature but, be warned, cabal
    constraints cover more than just `package ==version`, such as flags:
    ```cabal
    constraints:
        bson -_old-network
      , HaXml +splitbase
    ```

```dhall
[ { dep = "diagrams-postscript", ver = "1.5" }
, { dep = "diagrams-svg", ver = "1.4.3" }
]
```

The type of constraints is a list of records with dependency and version fields:

```dhall
List { dep : Text, ver : Text }
```

### 3. Source Repository Packages

There are various reasons to depend on source packages and forks of source
packages; to keep a mirror within your organization, to add fixes that haven't been
upstreamed yet, or to pick up a fix someone else has made but hasn't yet been merged
upstream, released and published.

```
project-dhall
└── ghc-x.y.z
    ├── deps-external.dhall    ▨ List { loc : Text, tag : Text, sub : List Text }
    ├── deps-internal.dhall    ▨ List { loc : Text, tag : Text, sub : List Text }
    ├── forks-external.dhall   ▨ List { loc : Text, tag : Text, sub : List Text }
    └── forks-internal.dhall   ▨ List { loc : Text, tag : Text, sub : List Text }
```

The type of these files are all the same. They contain a list of records with
fields for location, tag (commit SHA) and subdirectories:

```dhall
List { loc : Text, tag : Text, sub : List Text }
```

Any empty `sub` field must be type annotated like this `[] : List Text`. Each
source repository package record will bring in one dependency or as many as
there are `sub` fields.

These source repository `*.dhall` files are for original work from your
organisation (internal) and from third parties (external) and for internal and
external forks. If using the empty list in any one of these files then use an
explicit type annotation:

```dhall
[] : List { loc : Text, tag : Text, sub : List Text }
```

### 4. Package Groups
In the `pkgs` folder, create one or more groups for related packages.

```
project-dhall
├── ghc-x.y.z
├── pkgs
│   ├── db.dhall      ▨ List Text
│   ├── server.dhall  ▨ List Text
│   └── tools.dhall   ▨ List Text
└── pkg-groups.dhall  ▨ List Text
```

The contents of each group is a `List Text` of relative paths to folders
containing package `.cabal` files.

```dhall
-- ./project-dhall/pkgs/tools.dhall
[ "tool/linter/"
, "tool/formatter/"
]
```

List the package groups in `pkg-groups.dhall`[^pkg-groups]. A good order to use
is the order of cabal constraint solving that you may witness when upgrading a
project and seeing solving failures.

[^pkg-groups]: Dhall can't do arbitrary IO like reading files from a folder.

```dhall
-- ./project-dhall/pkg-groups.dhall
[ "tools"
, "db"
, "server"
]
```

This gives you control of the imports in the generated
`./project-cabal/pkgs.config`, itself imported into
`ghc-x.y.z.dhall2config.project`:

* Use `CABAL_RELATIVITY=ImportRelative` to be import-compatible with `cabal-3.10`.
  ```cabal
  -- ./project-cabal/pkgs.config
  import: ./pkgs/tools.config
  import: ./pkgs/db.config
  import: ./pkgs/server.config
  ```
* The default of `CABAL_RELATIVITY=ProjectRelative` is import-compatible with `cabal-3.8`.
  ```cabal
  -- ./project-cabal/pkgs.config
  import: ./project-cabal/pkgs/tools.config
  import: ./project-cabal/pkgs/db.config
  import: ./project-cabal/pkgs/server.config
  ```


```cabal
-- ./ghc-x.y.z.dhall2config.project
import: ./project-stackage/lts-m.n.config

import: ./project-cabal/pkgs.config

import: ./project-cabal/ghc-x.y.z/constraints.config
import: ./project-cabal/ghc-x.y.z/deps-external.config
import: ./project-cabal/ghc-x.y.z/deps-internal.config
import: ./project-cabal/ghc-x.y.z/forks-external.config
import: ./project-cabal/ghc-x.y.z/forks-internal.config
```

# Upgrading a Project

### GHC Upgrade
For a GHC compiler upgrade, add another folder to `project-dhall` for the
upgrade from `ghc-u.v.w` to `ghc-x.y.z`:

```
project-dhall
├── ghc-u.v.w
├── ghc-x.y.z
├── pkgs
├── pkg-groups.dhall        ▨ List Text
└── pkgs-upgrade-todo.dhall ▨ List Text
```

At the start of a GHC upgrade put all packages into
`project-dhall/pkgs-upgrade-todo.dhall`. As the upgrade progresses remove
packages from this list as you work on them.

### Constraint Upgrade
For a dependency version upgrade, add or change an entry in `constraints.dhall`
and if that dependency is on stackage for the resolver we're using and if the
version there differs then comment out that entry in the downloaded
`project-stackage/resolver-name.config` file to avoid impossible constraint
solving conflicts with cabal.

### Source Repository Upgrade
For a source repository upgrade, bump the `tag` field if picking up a newer
version.

If the `tag` you were on has been superseded by a published version then delete
the entry and add a constraint unless the published version is already a match
in the downloaded `cabal.config` file from stackage.

### Adding a Fork
If you are forking then you'll want to add a record to `fork-internal.dhall` and
then:
- If forking a stackage package: comment out the entry in the downloaded `cabal.config`
- If forking a hackage package: delete the matching entry in `constraints.dhall`

Using someone else's fork is the same except add the record to
`fork-external.dhall`.

Unforking goes the other way, remove the `fork-*.dhall` entry and either
fallback to the stackage version or another version for which you'll add an
entry in `constraints.dhall`.

# Getting Started

Dhall is a prerequisite.

As well as other make targets, updo can bootstrap itself by adding one of two
recipes to `project-files.mk`:

```make
UPDO_VERSION ?= 1.0.0
HACKAGE := http://hackage.haskell.org/package
UPDO_URL := ${HACKAGE}/updo-${UPDO_VERSION}/updo-${UPDO_VERSION}.tar.gz
```

1. When opting for the scripts:

```make
updo/Makefile:
	rm -rf updo
	curl -sSL ${UPDO_URL} | tar -xz
	chmod +x ./updo-${UPDO_VERSION}/project-nix/sha256map.hs
	chmod +x ./updo-${UPDO_VERSION}/project-nix/sha256map.py
	chmod +x ./updo-${UPDO_VERSION}/project-dhall/pkgs-sorted.hs
	chmod +x ./updo-${UPDO_VERSION}/project-dhall/pkgs-upgrade-done.hs
	chmod +x ./updo-${UPDO_VERSION}/project-dhall/pkgs-upgrade-partition.hs
	chmod +x ./updo-${UPDO_VERSION}/project-dhall2config/pkg-groups.hs
	mv updo-* updo
```

2. When opting to install the included executables:

```make
updo/Makefile:
	rm -rf updo
	curl -sSL ${UPDO_URL} | tar -xz
	cd updo-${UPDO_VERSION}
	stack install
	cd -
	mv updo-* updo
```

Instead of curl and tar, `cabal get` or `stack unpack` can unpack of package sources.

# Make Targets

In the root of your project, add two files.

```
.
├── project-files.mk
└── project-versions.mk
```

In `project-versions.mk` set variables for GHC and STACKAGE.

```make
GHC_VERSION := u.v.w
GHC_UPGRADE := x.y.z
STACKAGE_VERSION := lts-j.k
STACKAGE_UPGRADE := lts-m.n
```

Here is a basic set up for `project-files.mk`:

```make
include project-versions.mk
include updo/Makefile
```

### Canonical Targets

We consider the following as canonical project names and no target is required
to build them (they're built by the default `all` target):

```
$ make -f project-files.mk
```

- `stack.yaml`
- `cabal.project`
- `stack.upgrade.yaml`
- `cabal.upgrade.project`

We use CABAL_VIA and STACK_VIA makefile variables to decide which `ghc-x.y.z`
prefixed projects will be copied to the default project names (`cabal.project`
and `stack.yaml`). This way, Updo will create a single pair of projects for one
version of GHC.

### GHC-Prefixed Targets

We can build `ghc-x.y.z` projects explicitly:

```
$ make -f project-files.mk ghc-x.y.z.dhall2config.project
$ make -f project-files.mk ghc-x.y.z.dhall2cabal.project
$ make -f project-files.mk ghc-x.y.z.dhall2stack.yaml
```

### Modified Time Travel

Be careful with modified time that make relies on. If you hand-modify a target
so that it is newer than a prerequisite then make will not use its recipe to
build the target. This is easy to do inadvertently with a git GUI tool when
discarding changes or by doing a `git restore <project-name>`. As git advances
the modified time, this is effectively the same as `touch <project-name>`.  To
recover from this situation, either;
- Run make with its `--always-make` flag.
- Remove the target with `rm <project-name>`.

### Lock Files

For stack, the project and its lock are separate targets (`.yaml` and
`.yaml.lock`).

```
$ make -f project-files.mk ghc-x.y.z.dhall2stack.yaml
dhall text --file project-dhall/ghc-x.y.z/project-stack.dhall > ghc-x.y.z.dhall2stack.yaml
```

```
$ make -f project-files.mk ghc-x.y.z.dhall2stack.yaml.lock
stack build --dry-run --stack-yaml ghc-x.y.z.dhall2stack.yaml
```

We don't provide a make target for cabal `.project.freeze` files, relying
instead on a combination of constraints, those in the `cabal.config` downloaded
from stackage and those we add ourselves to `constraints.dhall`. If you do want
to freeze anyway there's a cabal command to generate a freeze file.

```
$ make -f project-files.mk ghc-x.y.z.dhall2cabal.project
dhall text --file project-dhall/ghc-x.y.z/project-cabal.dhall > ghc-x.y.z.dhall2cabal.project
```

# Clean GHC-Prefixed Projects

There's a make target to remove all `ghc-x.y.z` prefixed projects:

```
$ make -f project-files.mk clean
rm -f ghc-*.stack.* ghc-*.dhall2config.* ghc-*.dhall2cabal.* ghc-*.dhall2stack.*
ghc-*.stack2cabal.* ghc-*.cabal2stack.* ghc-*.dhall2yaml2stack.* ghc-*.sha256map.nix
```

# GHC-Prefixed Projects as Temporary

To treat `ghc-x.y.z` prefixed files as temporary, add lines like these to
`project-files.mk`:

```make
.INTERMEDIATE: ghc-$(GHC_VERSION).dhall2config.project
.INTERMEDIATE: ghc-$(GHC_VERSION).dhall2cabal.project
.INTERMEDIATE: ghc-$(GHC_VERSION).dhall2stack.yaml
.INTERMEDIATE: ghc-$(GHC_VERSION).sha256map.nix
```

# SHA256 Map Generation Method

With this snippet from `project-files.mk`, we can switch between two methods of
generating `ghc-x.y.z.sha256map.nix`.

```make
# How to generate project-nix/ghc-$(GHC_VERSION)/sha256map.nix?
# This is copied from ghc-$(GHC_VERSION).sha256map.nix.
#  - false to generate from *.dhall inputs via sha256map.hs.
#  - true to generate from stack.yaml via sha256map.py.
SHA256MAP_VIA_PYTHON ?= false

# If true, generate the sha256map from the stack.yaml with python,
# overriding the recipe for this target.
ifeq ($(SHA256MAP_VIA_PYTHON), true)
ghc-$(GHC_VERSION).sha256map.nix: stack.yaml
	updo/project-nix/sha256map.py <$^ >$@
endif
```

The default is to use the `sha256map.hs` script to generate
it[^replacing_versions]:

[^replacing_versions]: Using ghc-x.y.z and lts-m.n in the example output, not
  the actual GHC version or resolver.

```
$ make -f project-files.mk ghc-x.y.z.sha256map.nix --always-make
echo \
  '[./project-dhall/ghc-x.y.z/deps-external.dhall
  , ./project-dhall/ghc-x.y.z/deps-internal.dhall
  , ./project-dhall/ghc-x.y.z/forks-external.dhall
  , ./project-dhall/ghc-x.y.z/forks-internal.dhall
  , ([] : List {loc : Text, tag : Text, sub : List Text})
  ]' \
  | ./updo/project-nix/sha256map.hs > ghc-x.y.z.sha256map.nix
```

To use the `sha256map.py` script instead that is much slower:

```
$ SHA256MAP_VIA_PYTHON=true \
  make -f project-files.mk project-nix/ghc-x.y.z/sha256map.nix
updo/project-nix/sha256map.py <stack.yaml >ghc-x.y.z.sha256map.nix
mkdir -p project-nix/ghc-x.y.z && cp ghc-x.y.z.sha256map.nix project-nix/ghc-x.y.z/sha256map.nix
rm ghc-x.y.z.sha256map.nix

```

You can [read more about Updo Nix](project-nix#readme) and its use with
haskell.nix.

# Installed Exes or Scripts

To use the executables of Updo's own package, by default, instead of its
scripts, add these lines to `project-files.mk`:

```make
SHA256MAP_HS_EXE ?= true
PKG_GROUPS_HS_EXE ?= true
PKGS_SORTED_HS_EXE ?= true
PKGS_UPGRADE_DONE_HS_EXE ?= true
```

### Why Dhall for Configuration?

Dhall has excellent imports and is an intentionally limited typed programming
language. We use dhall's `text` command to write cabal and stack projects using
[dhall text templating][dhall-text-templating].

The `dhall` executable comes with a `format` command[^format-lsp]. This helps in
the maintenance of the various `.dhall` files.

An example of the limits of the language we encountered in Updo is not being
able to compare anything but `Natural` values. Sorting something like `List
Text` is not possible but there is great interop with Haskell so this can be
done over there as we do with scripts.

## Acknowledgements

I already knew Dhall well from maintaining hpack-dhall. Thanks Simon Hengel for
suggesting I take over its maintenance.

FP Complete is the corporate sponsor of this tooling. Thanks to my collegues;
Daniel Herrera Rendón for first seeing this could work, Neil Mayhew for makefile
tips and tricks, Abel Sen for careful reviews and thoughtful suggestions and
Eduardo Lemos Rocha for advocacy and enthusiasm.

Please contact me for any questions, problems, suggestions or the like.
Maintenance of Updo will be coming from me and not FP Complete.

Updo grew to fill a need after every other way I could find to maintain projects
was unsatisfactory for the specific problem I had, herding hundreds of packages
and their dependencies into buildable projects over two compiler versions for
both cabal and stack.  The more I use it, the more I love it as a fast, simple
and appropriate solution for maintaining Haskell projects.

Thanks FP Complete.

[^format-lsp]: Formatting is also available with the [Dhall LSP Server][LSP].

[dhall-text-templating]: https://www.haskellforall.com/2017/06/dhall-is-now-template-engine.html
[LSP]: https://github.com/PanAeon/vscode-dhall-lsp-server
