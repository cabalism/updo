# Alternatives to Updo

Updo uses `.dhall` configuration but you may want to configure your project in
stack and mirror it to cabal or vica-versa with a project-to-project conversion.

Updo can be of help with mirroring if you chose stack-first or cabal-first
project maintenance but then you're own your own as far as managing GHC upgrades
goes.

## Project to Project Conversion

We provide makefiles for converting a stack project to a cabal project with
[`stack2cabal`](https://github.com/hasufell/stack2cabal) and for going the other
way with [`cabal2stack`](https://github.com/iconnect/cabal2stack).

### Configure with Stack

A stack project is YAML, `stack.yaml` by default. Without conditionals or
imports everything has to be in the same file except for stack's own [snapshot
indirection](https://docs.haskellstack.org/en/stable/custom_snapshot/) to
another file.

There are a bunch of YAML formatting tools such as
[vscode-yaml](https://www.npmjs.com/package/vscode-yaml).

### Configure with Cabal

A cabal project, `cabal.project` by default, is its own format. It has
conditionals and imports.

There is not a tool for formatting it. The `cabal format` command and the
[cabal-fmt](https://github.com/phadej/cabal-fmt) tool both format packages, not
projects.

### Mirroring with `stack2cabal`

This tool inspects the stack project and downloads source repositories to check
for version constraints and generates a `cabal.project` and
`cabal.project.freeze`.

This tool is slow as it clones whole and sometimes large source repositories
locally. This can be skipped with `--no-inspect-remotes` but the inspection
ensures correct versions in the freeze file.

```
> stack2cabal --help
Usage: stack2cabal [-f|--file STACK_YAML] [-o|--output-file CABAL_PROJECT] 
                   [--no-inspect-remotes] [--no-pin-ghc] [--no-sort-repos] 
                   [--no-run-hpack] [-p|--pin-hackage-index FUZZY_DATE]

Available options:
  --no-inspect-remotes     Don't check package names from remote git sources
                           (this is faster, but may leave incorrect versions in
                           cabal.project.freeze if remote packages overwrite
                           stack resolver versions)
```

It generates a freeze file but these lock are problematic as they don't function
as people expect and they are platform specific [freeze
problems](https://github.com/haskell/cabal/issues/8059).

### Mirroring with `cabal2stack`

This tool inspects cabal's plan so is more tied to the machine architecture
where the plan was created by cabal. The generated stack project will differ if
created on mac or linux but by adding a `--resolver` flag the generated project
is buildable on both mac and linux.

Like `stack2cabal`, this tool also needs to download remote source repositories
and is about as slow because the download takes most of the time.

The stack project generated will [vary by
platform](https://github.com/iconnect/cabal2stack/issues/1) it will [repeat
dependencies](https://github.com/iconnect/cabal2stack/issues/4) already in the
resolver.
