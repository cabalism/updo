# Updo

> A hairstyle and a way of maintaining and upgrading Haskell projects.

## What is a Project?

Updo is good for project packages, constraints, source repository packages and
stackage resolver[^download-stackage-config]. More than this can be added into a
project template.

[^download-stackage-config]: For cabal, the cabal.config will have to be
  downloaded to `project-stackage/resolver.config` and then edited to comment
  out constraints that would otherwise lead to impossible to solve conflicting
  constraints because cabal constraints are additive and cannot be overridden.

## Goals

1. Map a single set of packages, constraints and source repository packages to
   projects for cabal and stack.
2. Minimize the effort of upgrading constraints, source repository packages and
   GHC versions.

We do this with configuration in `.dhall` files, makefiles and `dhall text`
templates. We also have some simple Haskell scripts for operations not possible
in Dhall like sorting a `List Text`[^dhall-sort-list-nat].

[^dhall-sort-list-nat]: Dhall can only sort `List Natural`.

## Configure

What do we want to configure? We don't need much:

1. A list of packages.
2. A stackage resolver[^download-stackage-config].
3. A list of constraints (package dependencies with their versions).
4. A list of source packages, for unpublished[^unpublished-packages] packages or
  for unpublished versions of packages.

[^unpublished-packages]: A package not published to hackage or stackage.

### Lists of Packages

It is nice to be able to look at packages both grouped and as one sorted list.
We provide one level of nesting for groups for the list of internal packages. If
you don't want groups then please put all packages in the one group.

For the cabal project we keep the grouping with each group in its own
`group-name.config` file and then in the project there is one `import
group-name.config` for each group. If you don't want to use cabal imports then
the list of packages can be put directly into the `cabal.project`.  Stack
projects cannot import so the all the packages there are in one list.

### Stackage Resolver and Constraints

The stackage resolver is able to provide us with a list of packages that work
together for both stack and cabal. Any other dependencies as constraints or
source packages are either not found on stackage or are different versions.

By specifying a resolver with stack we get a list of exact `==` versions for
packages published to stackage. Stackage also publishes a `cabal.config` file
that has these package versions as cabal constraints. As cabal constraints are
additive, in practice importing directly from stackage often leads to conflicts
between stackage's constraints and the project's own constraints and for that
reason we'll usually have to download the `cabal.config` from stackage and
comment out lines that are conflicted.

We don't expect any packages to impose constraints on their dependencies at the
package level, in their `.cabal` files, but if they do then these constraints
must fit with constraints at the project level.

### List of Source Packages

There are various reasons to depend on source packages and forks of source
packages; to keep a mirror within your organization, to add fixes that haven't been
upstreamed yet, or to pick up a fix someone else has made but hasn't yet been merged
upstream, released and published.

### Why Dhall for Configuration?

Dhall has excellent imports. It is an intentionally limited programming
language. You won't be able to compare anything but `Natural` values so sorting
something like `List Text` is not possible within it but there is great interop
with Haskell so this can be done there.

The `dhall` executable comes with a `format` command[^format-lsp]. This helps in
the maintenance of the various `.dhall` files.

We use dhall's `text` command to write cabal and stack projects using [dhall
text templating][dhall-text-templating].

[^format-lsp]: Formatting is also available with the [Dhall LSP Server][LSP].

## Maintaining a Project

For a GHC compiler version, say `ghc-x.y.z`, all configuration goes into the
`project-dhall/ghc-x.y.z` folder within your project except for the
`cabal.config` that we'll need to download from stackage. Save this file as
`project-stackage/lts-m.n` using the exact resolver name.

```bash
.
├── ghc-x.y.z
└── pkgs
```

In the `pkgs` folder, create one or more groups for related packages.

```
.
├── db.dhall
├── server.dhall
└── tools.dhall
```

The contents of each group is a `List Text` of relative paths to folders
containing package `.cabal` files.

```dhall
[ "./tool/linter/"
, "./tool/formatter/"
]
```

For each compiler version, such as `ghc-x.y.z`, create this set of files:

```
.
├── ghc-x.y.z
│   ├── constraints.dhall
│   ├── deps-external.dhall
│   ├── deps-internal.dhall
│   ├── forks-external.dhall
│   ├── forks-internal.dhall
│   ├── project-dhall2cabal.dhall
│   └── project-dhall2stack.dhall
```

By default, Updo will create a single pair of projects (`cabal.project` and
`stack.yaml`) for one version of GHC. Specify which version to use by setting 
make variables in `Make-project`.

The `project-dhall2cabal.dhall` file is the template for the
`ghc-x.y.z.dhall2cabal.project` file we'll produce.  Likewise
`project-dhall2stack.dhall` is the template for producing
`ghc-x.y.z.dhall2stack.yaml`. The rest of the files are inputs. In constraints
put published packages that you want to use that are not on stackage or if they
are on stackage where you want to use a different version.

```
[ { dep = "diagrams-postscript", ver = "1.5" }
, { dep = "diagrams-svg", ver = "1.4.3" }
]
```

The type of constraints is a list of records with dependency and version fields:

```dhall
List { dep : Text, ver : Text }
```
  
The type of the other files are all the same. They contain a list of records
with fields for location, tag (commit SHA) and subdirectories:

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

With the `dhall text` command we're able to mirror to both stack and cabal
projects using string templates. See [templates](./TEMPLATES.md) for more.

## Upgrading a Project

For a GHC compiler upgrade, add another folder to `project-dhall` for the
upgrade from `ghc-u.v.w` to `ghc-x.y.z`:

```bash
.
├── ghc-u.v.w
├── ghc-x.y.z
└── pkgs
```

At the start of a GHC upgrade put all packages into
`project-dhall/pkgs-upgrade-todo.dhall`. As the upgrade progresses remove
packages from this list as you work on them.

For a dependency version upgrade, add or change an entry in `constraints.dhall`
and if that dependency is on stackage for the resolver we're using and if the
version there differs then comment out that entry in the downloaded
`project-stackage/resolver-name.config` file to avoid impossible constraint
solving conflicts with cabal.

For a source repository upgrade, bump the `tag` field if picking up a newer
version.

If the `tag` you were on has been superceded by a published version then delete
the entry and add a constraint unless the published version is already a match
in the downloaded `cabal.config` file from stackage.

If you are forking then you'll want to add a record to `fork-internal.dhall` and
then:
- If forking a stackage package: comment out the entry in the downloaded `cabal.config`
- If forking a hackage package: delete the matching entry in `constraints.dhall`

Using someone else's fork is the same except add the record to
`fork-external.dhall`.

Unforking goes the other way, remove the `fork-*.dhall` entry and either
fallback to the stackage version or another version for which you'll add an
entry in `constraints.dhall`.

## Make Targets

In the root of your project, add `Make-project` and include that from the root
`Makefile`.

```make
include Make-project
```

For the contents of `Make-project` in this order, set variables for GHC and
STACKAGE and include another makefile.

```make
GHC_VERSION := u.v.w
GHC_UPGRADE := x.y.z
STACKAGE_VERSION := lts-j.k
STACKAGE_UPGRADE := lts-m.n

include project-dhall/Makefile
```

With this we can build certain explicitly named projects GHC_VERSION and
GHC_UPGRADE `dhall2cabal` and `dhall2stack` projects:

```
make ghc-x.y.z.dhall2cabal.project
make ghc-x.y.z.dhall2stack.yaml
```

The stack `.yaml` and `.yaml.lock` are separate targets.

```
$ make ghc-x.y.z.dhall2stack.yaml
dhall text --file project-dhall/ghc-x.y.z/project-stack.dhall > ghc-x.y.z.dhall2stack.yaml
```

```
$ make ghc-x.y.z.dhall2stack.yaml.lock
stack build --dry-run --stack-yaml ghc-x.y.z.dhall2stack.yaml
```

We don't provide a make target for cabal `*.project.freeze` files, relying
instead on a combination of constraints, those in the `cabal.config` downloaded
from stackage and those we add ourselves to `constraints.dhall`. If you do want
to freeze anyway there's a cabal command to generate a freeze file.

```
$ make ghc-x.y.z.dhall2cabal.project
dhall text --file project-dhall/ghc-x.y.z/project-cabal.dhall > ghc-x.y.z.dhall2cabal.project
```

There are rules for copying the GHC_VERSION explicitly named projects to the
default project names (`cabal.project` and `stack.yaml`) and by default the copy
source is deleted by make because it is an `.INTERMEDIATE` target.

[dhall-text-templating]: https://www.haskellforall.com/2017/06/dhall-is-now-template-engine.html
[LSP]: https://github.com/PanAeon/vscode-dhall-lsp-server