# Duplicate names in scratch file.

``` ucm :hide
scratch/main> builtins.merge
```

Term and ability constructor collisions should cause a parse error.

``` unison :error
structural ability Stream where
  send : a -> ()

Stream.send : a -> ()
Stream.send _ = ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ❗️

  I found multiple bindings with the name Stream.send:
      2 |   send : a -> ()
      3 | 
      4 | Stream.send : a -> ()
```

Term and type constructor collisions should cause a parse error.

``` unison :error
structural type X = x

X.x : a -> ()
X.x _ = ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ❗️

  I found multiple bindings with the name X.x:
      1 | structural type X = x
      2 | 
      3 | X.x : a -> ()
```

Ability and type constructor collisions should cause a parse error.

``` unison :error
structural type X = x
structural ability X where
  x : ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found multiple types with the name X:

      1 | structural type X = x
      2 | structural ability X where
      3 |   x : ()
```

Field accessors and terms with the same name should cause a parse error.

``` unison :error
structural type X = {x : ()}
X.x.modify = ()
X.x.set = ()
X.x = ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ❗️

  I found multiple bindings with the name X.x:
      1 | structural type X = {x : ()}
      2 | X.x.modify = ()
      3 | X.x.set = ()
      4 | X.x = ()


  I found multiple bindings with the name X.x.modify:
      1 | structural type X = {x : ()}
      2 | X.x.modify = ()


  I found multiple bindings with the name X.x.set:
      1 | structural type X = {x : ()}
      2 | X.x.modify = ()
      3 | X.x.set = ()
```

Types and terms with the same name are allowed.

``` unison
structural type X = Z

X = ()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `update`, here's how your codebase would change:

    ⍟ These new definitions are ok to `add`:
    
      structural type X
        (also named builtin.Unit)
      X : ()
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view X

  structural type X = Z

  X : ()
  X = ()
```
