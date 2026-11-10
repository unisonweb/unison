When a file redefines a type and uses a definition of the codebase that uses the old one (`area`, here), that definition
is made part of the file, so that there is one `Shape` in play, and it is typechecked against the new type: here that
shows `area` no longer covers every constructor. (Before this, the error was a mismatch between `Shape` and
`Shape#oebc8v8v9l`, the codebase's, hash-qualified to tell it apart; a definition that still mentions the codebase's
version is shown that way.)

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

  Also typechecking, from the codebase: area. They use what this
  file redefines, so they are part of the file now, and `update`
  saves them rebuilt on the new definition.

  Pattern match doesn't cover all possible cases:
       10 | area = cases Circle n -> n
    

  Patterns not matched:
   * Square _

  ☝️

  Lines 5 to 10 are not in your file: they are area, pulled in
  from the codebase because they use what the file redefines. An
  error there means the new definition doesn't fit the way they
  use it: add the definition to your file and change it too, or
  change the file so that it fits.
```
