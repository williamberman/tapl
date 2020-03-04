A set of programming languages from https://en.wikipedia.org/wiki/Types_and_Programming_Languages implemented in haskell.

Requires stack, https://docs.haskellstack.org/en/stable/README/.

All languages are run from the same binary. Reads source from a file and supports a REPL.

## Examples
```
stack run -- -l Arith -f examples/test.arith
stack run -- -l Untyped -f examples/test.untyped
stack run -- --help
```
