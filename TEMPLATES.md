# Dhall Text Templates

The `ghc-x.y.z.dhall2cabal.project` and `ghc-x.y.z.dhall2stack.yaml` projects
are generated from Dhall text templates in `project-dhall/ghc-x.y.z`, from
`project-cabal.dhall` and `project-stack.dhall` respectively.

Dhall text templating was
[announced](https://www.haskellforall.com/2017/06/dhall-is-now-template-engine.html)
in June 2017 and this is now a command of the dhall executable:

```
$ dhall text --help`
Usage: dhall text [--file FILE] [--output FILE]
  Render a Dhall expression that evaluates to a Text literal

Available options:
  --file FILE              Read expression from a file instead of standard input
  --output FILE            Write result to a file instead of standard output
  -h,--help                Show this help text
```