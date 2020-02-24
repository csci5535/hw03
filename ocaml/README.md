# hw02/ocaml

This repository contains some minimal OCaml starter files for Homework Assignment 2.

```
.
├── hw03.ml       a template for your submission (if you wish)
├── hw03.sh       a minimal script to run tests
└── test_hw03.ml  a template for your tests (if you wish)
```

1 directory, 6 files

## Dependencies

First, make sure you have `ocaml` and `opam` installed from your system's package manager. Then, you can install the build and library dependencies as follows:

```
opam install dune base ounit2
```

## Build and Run Tests

You can build and run tests as follows:

```
dune runtest
```

Or, the simple top-level script

```
./hw03.sh
```

simply calls the above.
