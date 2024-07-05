# IEEE P3109 Floating-point arithmetic

A very incomplete sketch of a verified implementation of P3109 floats.

## Verification

```
imandrax-cli check src/theorems.iml src/checks.iml
```

## Dev

```
opam switch create . 5.2.0
eval $(opam env)
opam install . --deps-only
dune build
```

### Tests

There's a trivial test app in `test/test_app.ml` that uses the library. After `dune build`, you should be able to run

```
_build/default/test/test_app.exe
```