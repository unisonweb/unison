``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
unique type Foo
  = Bar Nat
  | Baz Nat Nat

foo : Foo -> Nat
foo = cases
  Bar n -> n
  Baz n m -> n + m
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Foo

  + foo : Foo -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
unique type Foo
  = Bar Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Foo

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm :error
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Some definitions don't typecheck with your changes. I've
  update the file scratch.u with the definitions that need
  fixing. Once the file is compiling, try `update` again.

  I've also switched you to a new branch update-main for this
  work. On `update`, it will be merged back into main.
```

``` unison :added-by-ucm scratch.u
type Foo = Bar Nat

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

foo : Foo -> Nat
foo = cases
  Bar n   -> n
  Baz n m -> n Nat.+ m

```
