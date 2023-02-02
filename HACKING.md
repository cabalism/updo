# Updo Hacking

## Package Groups

During the cabal solving step, cabal would fail to solve for dependencies for
packages in a particular order. The grouping of packages preserves this.

To mirror package groups from `project-dhall/pkgs/*.dhall` to
`project-cabal/pkgs/*.config` use make, either for `pkgs.config` that imports
all `pkgs/*.config` or for a single package group.

```
$ make project-cabal/pkgs.config
$ cat project-cabal/pkgs.config
import: project-cabal/pkgs/shims.config
...
import: project-cabal/pkgs/code-tools.config
```

When `project-dhall/pkgs-upgrade-todo.dhall` is:

```dhall
[ "./periodic-jobs/runner/" ]
```

Then the single `periodic-jobs.config` package group as generated is:

```
$ make project-cabal/pkgs/periodic-jobs.config
$ cat project-cabal/pkgs/periodic-jobs.config
if impl(ghc <= 8.10.7)
  packages:
      ./periodic-jobs/core/
    , ./periodic-jobs/runner/
else
  packages:
      ./periodic-jobs/core/

-- TODO for impl(ghc >= 9.2.5)
-- ./periodic-jobs/runner/
```

To split a group of packages into a dhall record of `{pkgs, done, todo}`:

```
$ ./updo/project-dhall/pkgs-upgrade-partition.hs ./project-dhall/pkgs/periodic-jobs.dhall
{ pkgs = [ "./periodic-jobs/core/", "./periodic-jobs/runner/" ]
, done = [ "./periodic-jobs/core/" ]
, todo = [ "./periodic-jobs/runner/" ]
}
```

## GHC Upgrade Hacking

During a GHC version uprade, start by putting all packages into
`pkgs-upgrade-todo.dhall` and progressively take one or a few packages out at at
time and rerun the make targets to build the project you want to use.

To sort all the packages.

```
$ cd project-dhall
$ ./pkgs-sorted.hs
[ "./administration-client/"
, "./aeson-shim/"
...
, "./warden/shared/haskell/orphans/servant-docs/"
, "./warden/shared/haskell/testlib/"
]
```