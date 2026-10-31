When a file shadows a name in the codebase, a type error can still mention the codebase's definition. It's shown by its
name, hash-qualified to tell it apart from the file's definition. It used to be shown as a bare hash.

``` ucm :hide
> builtins.merge lib.builtin
```

``` unison
type Shape = Circle Nat

area : Shape -> Nat
area = cases Circle n -> n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Shape

  + area : Shape -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison :error
type Shape = Circle Nat | Square Nat

> area (Square 1)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  The 1st argument to `area`

            has type:  Shape
      but I expected:  Shape#oebc8v8v9l

      1 | type Shape = Circle Nat | Square Nat
      2 | 
      3 | > area (Square 1)
```
