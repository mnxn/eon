# The Eon Programming Language

```
func main(): string = "Hello, World!"
```

## Usage

Run a program:

- `eon <filename>`
- `eon run <filename>`

Evaluate an expression:

- `eon eval <filename>`

Parse a program/expression:

- `eon parse <filename>`
- `eon parse-expression <filename`

Typecheck a program/expression:

- `eon check <filename>`
- `eon check-expression <filename>`

## Instructions

Install dependencies with [opam](https://opam.ocaml.org/):

- `opam install . --deps-only`

Build with [dune](https://dune.build/):

- `dune build bin/eon.exe`

Run an example from the [examples](./examples/) directory:

- `dune exec bin/eon.exe examples/hello.eon`
