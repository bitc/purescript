# psc-compile

TODO Document the tool.

For an example of how to use this, check out the included Shake sample
buildfile `Shakefile.hs`

```
$ psc-compile --help
psc-compile - Compiles a single PureScript module to Javascript

Usage: psc-compile FILE [-o|--output ARG] [--no-tco] [--no-magic-do] [--no-opts]
                   [-v|--verbose-errors] [-c|--comments] [-r|--require-path ARG]
                   [-d|--deps] [-p|--no-prefix]

Available options:
  --version                Show the version number
  -h,--help                Show this help text
  FILE                     The input .purs file
  -o,--output ARG          The output directory. Note that this is also the
                           directory that will be looked in for `externs.purs'
                           files belonging to previously compiled
                           modules. (default: "output")
  --no-tco                 Disable tail call optimizations
  --no-magic-do            Disable the optimization that overloads the do
                           keyword to generate efficient code specifically for
                           the Eff monad
  --no-opts                Skip the optimization phase
  -v,--verbose-errors      Display verbose error messages
  -c,--comments            Include comments in the generated code
  -r,--require-path ARG    The path prefix to use for require() calls in the
                           generated JavaScript
  -d,--deps                Do not compile. Instead, print list of files that the
                           module depends on (to stdout)
  -p,--no-prefix           Do not include comment header
```
