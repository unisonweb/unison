``` ucm :hide
myproject/main> builtins.merge lib.builtin
```

``` unison
a.x.x.x.x = 100
b.x.x.x.x = 100
foo = 25
c.y.y.y.y = foo + 10
d.y.y.y.y = foo + 10
bar = a.x.x.x.x + c.y.y.y.y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ New definitions:
    
      a.x.x.x.x : Nat
      b.x.x.x.x : Nat
      bar       : Nat
      c.y.y.y.y : Nat
      d.y.y.y.y : Nat
      foo       : Nat
```

``` ucm
myproject/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
foo = +30
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Int
```

``` ucm :error
myproject/main> update

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
foo = +30

-- The definitions below no longer typecheck with the changes above.
-- Please fix the errors and try `update` again.

bar : Nat
bar =
  use Nat +
  x + c.y.y.y.y

c.y.y.y.y : Nat
c.y.y.y.y =
  use Nat +
  foo + 10

d.y.y.y.y : Nat
d.y.y.y.y =
  use Nat +
  foo + 10

```
