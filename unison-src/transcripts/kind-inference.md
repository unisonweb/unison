
```ucm:hide
.> builtins.merge
```

## A type param cannot have conflicting kind constraints within a single decl

conflicting constraints on the kind of `a` in a product
```unison:error
unique type T a = T a (a Nat)
```

conflicting constraints on the kind of `a` in a sum
```unison:error
unique type T a 
  = Star a 
  | StarStar (a Nat)
```

## Kinds are inferred by decl component

Successfully infer `a` in `Ping a` to be of kind `* -> *` by
inspecting its component-mate `Pong`.
```unison
unique type Ping a = Ping Pong
unique type Pong = Pong (Ping Optional)
```

Catch the conflict on the kind of `a` in `Ping a`. `Ping` restricts
`a` to `*`, whereas `Pong` restricts `a` to `* -> *`.
```unison:error
unique type Ping a = Ping a Pong
unique type Pong = Pong (Ping Optional)
```

Successful example between mutually recursive type and ability
```unison
unique type Ping a = Ping (a Nat -> {Pong Nat} ())
unique ability Pong a where
  pong : Ping Optional -> ()
```

Catch conflict between mutually recursive type and ability
```unison:error
unique type Ping a = Ping (a -> {Pong Nat} ())
unique ability Pong a where
  pong : Ping Optional -> ()
```

Consistent instantiation of `T`'s `a` parameter in `S`
```unison
unique type T a = T a

unique type S = S (T Nat)
```

Demonstrate kind defaulting by component
```unison:error
unique type T a = T

unique type S = S (T Optional)
```

Catch invalid instantiation of `T`'s `a` parameter in `S`
```unison:error
unique type T a = T a

unique type S = S (T Optional)
```

## Checking annotations

Catch kind error in type annotation
```unison:error
test : Nat Nat
test = 0
```

Catch kind error in annotation example 2
```unison:error
test : Optional -> ()
test _ = ()
```

Catch kind error in annotation example 3
```unison:error
unique type T a = T (a Nat)

test : T Nat -> ()
test _ = ()
```

Catch kind error in scoped type variable annotation
```unison:error
unique type StarStar a = StarStar (a Nat)
unique type Star a = Star a

test : StarStar a -> ()
test _ = 
  buggo : Star a
  buggo = bug ""
  ()
```

## Effect/type mismatch

Effects appearing where types are expected
```unison:error
unique ability Foo where
  foo : ()

test : Foo -> ()
test _ = ()
```

Types appearing where effects are expected
```unison:error
test : {Nat} ()
test _ = ()
```

## Cyclic kinds

```unison:error
unique type T a = T (a a)
```

```unison:error
unique type T a b = T (a b) (b a)
```

```unison:error
unique type Ping a = Ping (a Pong)
unique type Pong a = Pong (a Ping)
```
