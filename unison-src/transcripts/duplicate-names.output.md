# Duplicate names in scratch file.

Term and ability constructor collisions should cause a parse error.

```unison
structural ability Stream where
  send : a -> ()

Stream.send : a -> ()
Stream.send _ = ()
```

```ucm

  ❗️
  
  I found multiple bindings with the name Stream.send:
      2 |   send : a -> ()
      3 | 
      4 | Stream.send : a -> ()
      5 | Stream.send _ = ()
  

```
Term and type constructor collisions should cause a parse error.

```unison
structural type X = x 

X.x : a -> ()
X.x _ = ()
```

```ucm

  ❗️
  
  I found multiple bindings with the name X.x:
      1 | structural type X = x 
      2 | 
      3 | X.x : a -> ()
      4 | X.x _ = ()
  

```
Ability and type constructor collisions should cause a parse error.

```unison
structural type X = x 
structural ability X where
  x : ()
```

```ucm

  I found two types called X:
  
      1 | structural type X = x 
      2 | structural ability X where
      3 |   x : ()
  

```
Field accessors and terms with the same name should cause a parse error.

```unison
structural type X = {x : ()}
X.x.modify = ()
X.x.set = ()
X.x = ()
```

```ucm

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

```unison
structural type X = Z

X = ()
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type X
        (also named builtin.Unit)
      X : ()

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type X
      (also named builtin.Unit)
    X : ()

.> view X

  structural type X = Z
  
  X : ()
  X = ()

```
