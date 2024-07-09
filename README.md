# IEEE P3109 Floating-point arithmetic

A very incomplete sketch of a verified implementation of P3109 floats.

## Verification

```
imandrax-cli check specification/theorems.iml specification/checks.iml implementation/theorems.iml
```

## Dev

```
opam switch create . 5.2.0
eval $(opam env)
opam install . --deps-only
dune build
```

### Tests

There's a trivial test app in `implementation/test/test_app.ml` that uses the library. After `dune build`, you should be able to run

```
dune exec -- implementation/test/test_app.exe
```

Similarly, the specification is also executable:

```
dune exec -- specification/test/test_app_spec.exe
```