# Updo Nix

The `Makefile` has targets for `ghc-x.y.z.sha256map.nix` which has the form:

```nix
{
  "repo-url"."commit-rev" = "sha256";
}
```

This can be used with [haskell.nix][haskell-nix].  We grab the sha256 hash with
`nix-prefetch-git` from the shell so you'll need that installed.

[haskell-nix]: https://input-output-hk.github.io/haskell.nix/tutorials/source-repository-hashes.html#avoiding-modifying-cabalproject-and-stackyaml

```
$ nix-prefetch-git --help
syntax: nix-prefetch-git [options] [URL [REVISION [EXPECTED-HASH]]]

Options:
      --out path      Path where the output would be stored.
      --url url       Any url understood by 'git clone'.
      --rev ref       Any sha1 or references (such as refs/heads/master)
      --hash h        Expected hash.
      --branch-name   Branch name to check out into
      --sparse-checkout Only fetch and checkout part of the repository.
      --deepClone     Clone the entire repository.
      --no-deepClone  Make a shallow clone of just the required ref.
      --leave-dotGit  Keep the .git directories.
      --fetch-lfs     Fetch git Large File Storage (LFS) files.
      --fetch-submodules Fetch submodules.
      --builder       Clone as fetchgit does, but url, rev, and out option are mandatory.
      --quiet         Only print the final json summary.
```

```
$ nix-prefetch-git https://github.com/input-output-hk/haskell.nix.git bc01ebc05a8105035c9449943046b46c8364b932
...
{
  "url": "https://github.com/input-output-hk/haskell.nix.git",
  "rev": "bc01ebc05a8105035c9449943046b46c8364b932",
  "date": "2019-05-30T13:13:18+08:00",
  "sha256": "003lm3pm024vhbfmii7xcdd9v2rczpflxf7gdl2pyxia7p014i8z",
  "fetchSubmodules": false
}
```

We also include a `sha256map.py` script from haskell.nix[^sorted] but this is 4
times slower.

The `Makefile` also has `project-versions.nix` as a target.

```nix
{ current-version =
    { ghc-xyz-project-stack = "stack.yaml";
      ghc-xyz-project-cabal = "cabal.project";
      ghc-xyz-sha256map = import ./project-nix/ghc-u.v.w/sha256map.nix;
    };
  upgrade-version =
    { ghc-xyz-project-stack = "stack.upgrade.yaml";
      ghc-xyz-project-cabal = "cabal.upgrade.project";
      ghc-xyz-sha256map = import ./project-nix/ghc-x.y.z/sha256map.nix;
    };
}
```

[^sorted]: Renamed from `sha256map-regenerate.py` and modified to sort the map by keys.
