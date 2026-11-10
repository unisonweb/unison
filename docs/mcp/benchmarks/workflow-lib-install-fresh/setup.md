Starting codebase: `scratch/main` with `@unison/base`, and the two data types the JSON is read into. Nothing else; the worker has to find a JSON library on Unison Share (so it needs the network).

``` unison
unique type LineItem = { sku : Text, quantity : Nat, priceCents : Nat }

unique type Order = { customer : Text, items : [LineItem], note : Optional Text }
```

``` ucm
scratch/main> add
```
