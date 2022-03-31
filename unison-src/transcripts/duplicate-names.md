# Duplicate names in scratch file.

```ucm:hide
.> builtins.merge
```

Term and ability constructor collisions should cause a parse error.

```unison:error
structural ability Stream where
  send : a -> ()

Stream.send : a -> ()
Stream.send _ = ()
```

Term and type constructor collisions should cause a parse error.

```unison:error
structural type X = x 

X.x : a -> ()
X.x _ = ()
```

Ability and type constructor collisions should cause a parse error.

```unison:error
structural type X = x 
structural ability X where
  x : ()
```

Field accessors and terms with the same name should cause a parse error.

```unison:error
structural type X = {x : ()}
X.x.modify = ()
X.x.set = ()
X.x = ()
```

Types and terms with the same name are allowed.

```unison
structural type X = Z

X = ()
```

```ucm
.> add
.> view X
```
