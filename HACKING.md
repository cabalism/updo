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
import: project-cabal/pkgs/tools.config
```

When `project-dhall/pkgs-upgrade-todo.dhall` is:

```dhall
[ "./jobs/runner/" ]
```

Then the single `jobs.config` package group as generated is:

```
$ make project-cabal/pkgs/jobs.config
$ cat project-cabal/pkgs/jobs.config
if impl(ghc <= 8.10.7)
  packages:
      ./jobs/core/
    , ./jobs/runner/
else
  packages:
      ./jobs/core/

-- TODO for impl(ghc >= 9.2.5)
-- ./jobs/runner/
```

To split a group of packages into a dhall record of `{pkgs, done, todo}`:

```
$ ./updo/project-dhall/pkgs-upgrade-partition.hs ./project-dhall/pkgs/jobs.dhall
{ pkgs = [ "./jobs/core/", "./jobs/runner/" ]
, done = [ "./jobs/core/" ]
, todo = [ "./jobs/runner/" ]
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
[ "./admin-client/"
, "./aeson-shim/"
...
, "./docs/"
, "./testing/"
]
```