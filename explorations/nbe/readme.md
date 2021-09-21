# What is it
This library implements *normalization by evaluation* for Gödel's *System T*

# How to run it
Run `dune utop`

Open the library

``` ocaml
open Nbe;;
```

Now try out an example on your favorite lambda term:
``` ocaml
nbe [("y", Nat); ("z", Nat)] Nat (App (Lam("x", Nat, Var("y")), Var ("z")));;
```

On this example, we get back:
``` ocaml
        - : syn = Var "y"
```
as expected.
