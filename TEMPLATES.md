# Dhall Text Templates

We use Dhall text templates in `project-dhall/ghc-x.y.z/text-templates`:

- From template `dhall2cabal.dhall`, we generate `ghc-x.y.z.dhall2cabal.project`
- From template `dhall2stack.dhall`, we generate `ghc-x.y.z.dhall2stack.yaml`

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
