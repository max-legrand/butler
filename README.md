# butler (OCaml)

OCaml port of butler. Same config format as the Rust version.

## Requirements

- dune >= 3.22. Dependencies (including the OCaml compiler) are fetched by dune package management from the committed `dune.lock/`. No opam needed.
- [`watchexec`](https://github.com/watchexec/watchexec) on `PATH` at run time (only needed when a service has a `watchlist`)

## Build and run

    dune build
    dune test
    dune exec butler -- -f archive/test/butler.service

The first build compiles the OCaml compiler, which takes a few minutes.

Change dependencies in `dune-project`, then run `dune pkg lock` and commit `dune.lock/`.

## Notes

- Config is read by a small built-in YAML subset parser (block lists and maps, `[a, b]` lists, quoted and plain scalars, `#` comments). Anchors, tags, and multi-line scalars are not supported.
- Watch patterns use `Re.Perl`, so lookahead and backreferences are not available.
