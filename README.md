# How to Build

#### 1. Install ppx deriving

```bash
opam install ppx_deriving
```

#### 2. Build

```bash
ocamlbuild -use-ocamlfind 'calc.native'
```

You may encounter an error for the first time because of residual file
from our previous manual build (run.sh). Just follow the instruction to
resolve this: `./_build/sanitize.sh`.

#### 3. Run the compiled program

```bash
./calc.native
```
