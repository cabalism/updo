### 2.0.0
* Add make recipes in the readme that bootstrap updo from source.
* Allow all inputs to be omitted and default to empty lists for the omissions.
* Add a choice of stackage import location for `dhall2cabal` and `dhall2config`
  text templates:
    - `StackageWeb`: "import: https://stackage.org/resolver/cabal.config"
    - `StackageLocal`: "import: ./project-stackage/resolver.config"
* For version equality constraints, strip `@sha256:...` revision suffixes for
  Cabal as we already do for `@rev:...` revisions as the [revision
  feature](https://github.com/haskell/cabal/issues/7833) is not yet implemented
  for Cabal.
* The grammar of the comment describing source package dependencies and forks is
  better, no longer saying 1 packages. Same goes for the comment describing
  local packages.
* With `dhall2cabal` or `dhall2stack` generated projects, source dependencies
  and their commentary are only included if there are dependencies in a
  category; internal or external, dependency or fork.
* Show comments with `Verbosity.Info` or not with `Verbosity.Quiet`.
* Add a skeleton at `updo/project-skeleton` with `Makefile` recipes that will
  put configuration into `project-dhall/ghc-x.y.z`. This will help get started
  with something like:

  ```
  $ tree project-dhall
  project-dhall
  ├── ghc-x.y.z
  │   └── text-templates
  │       ├── cabal-snippet.dhall
  │       ├── dhall2cabal.dhall
  │       ├── dhall2config.dhall
  │       ├── dhall2stack.dhall
  │       └── stack-snippet.dhall
  ├── pkg-groups.dhall
  ├── pkgs
  │   └── all-in-one.dhall
  └── pkgs-upgrade-todo.dhall

  3 directories, 8 files

  $ cat project-dhall/pkgs/all-in-one.dhall
  -- NOTE: Using an empty list should trigger warnings like the following:
  --
  -- $ stack build
  -- Error: [S-8506]
  -- Stack failed to parse the target(s).
  --
  -- While parsing, Stack encountered the error:
  --
  -- The project contains no local packages (packages not marked with 'extra-dep').
  --
  -- Stack expects a target to be a package name (e.g. my-package), a package
  -- identifier (e.g.  my-package-0.1.2.3), a package component (e.g.
  -- my-package:test:my-test-suite), or, failing that, a relative path to a directory
  -- that is a local package directory or a parent directory of one or more local
  -- package directories.
  --
  -- $ cabal build
  -- Warning: There are no packages or optional-packages in the project
  -- Error: [Cabal-7136]
  -- There is no <pkgname>.cabal package file or cabal.project file. To build
  -- packages locally you need at minimum a <pkgname>.cabal file. You can use 'cabal
  -- init' to create one.
  --
  -- For non-trivial projects you will also want a cabal.project file in the root
  -- directory of your project. This file lists the packages in your project and all
  -- other build configuration. See the Cabal user guide for full details.
  --
  -- TODO: Add paths to directories of local packages here.  Use "folder-name" and
  -- not "./folder-name" or "folder-name/".
  [] : List Text
  ```

### 1.0.0
* First release
