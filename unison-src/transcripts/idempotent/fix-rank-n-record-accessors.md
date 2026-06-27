``` ucm :hide
> builtins.merge lib.builtin
```

Record types generate getter, setter, and modifier functions for each
field. This transcript checks two things about those generated
accessors when the record has higher-rank fields and/or type
parameters.

# Higher-rank fields

A field may have a higher-rank type — one containing a nested
`forall`. The generated getter must hand that polymorphic value back
out without losing the `forall`, so it can still be used at multiple
types.

``` unison
structural type Wrapper = { transform : forall a. a -> a, name : Text }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type Wrapper

  + Wrapper.name             : Wrapper -> Text
  + Wrapper.name.modify      : (Text ->{g} Text)
                               -> Wrapper
                               ->{g} Wrapper
  + Wrapper.name.set         : Text -> Wrapper -> Wrapper
  + Wrapper.transform        : Wrapper -> (∀ a. a -> a)
  + Wrapper.transform.modify : ((∀ a. a -> a)
                                ->{g} (∀ a. a -> a))
                               -> Wrapper
                               ->{g} Wrapper
  + Wrapper.transform.set    : (∀ a. a -> a)
                               -> Wrapper
                               -> Wrapper

  Run `update` to apply these changes to your codebase.
```

The getter's result type keeps the nested `forall`, and the setter and
modifier accept the higher-rank field:

``` ucm :hide
> add
```

``` unison
useTransform : (Nat, Text)
useTransform =
  id = Wrapper.transform (Wrapper (x -> x) "the identity")
  (id 1, id "hi")

renamed : Wrapper
renamed = Wrapper.name.set "still the identity" (Wrapper (x -> x) "id")
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + renamed      : Wrapper
  + useTransform : (Nat, Text)

  Run `update` to apply these changes to your codebase.
```

# Changing the type of a field

When a field is the *only* one to mention a type variable, updating
that field can change the variable to a different type. The setter and
modifier are therefore given their *fully general* types, freshening
exactly those variables in the result.

Here `here` is the sole field mentioning `a`, and `there` the sole
field mentioning `b`:

``` unison
structural type These a b = { here : a, there : b }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type These a b

  + These.here         : These a b -> a
  + These.here.modify  : (a1 ->{g} a)
                         -> These a1 b
                         ->{g} These a b
  + These.here.set     : a -> These a1 b -> These a b
  + These.there        : These a b -> b
  + These.there.modify : (b1 ->{g} b)
                         -> These a b1
                         ->{g} These a b
  + These.there.set    : b -> These a b1 -> These a b

  Run `update` to apply these changes to your codebase.
```

Notice the setters and modifiers are type-changing — e.g.
`These.here.modify : (a1 ->{g} a) -> These a1 b ->{g} These a b`
changes `a` while leaving `b` alone:

``` ucm :hide
> add
```

So we can turn a `These Nat Text` into a `These Boolean Text` by
modifying (or setting) only the `here` field:

``` unison
start : These Nat Text
start = These 0 "hi"

modified : These Boolean Text
modified = These.here.modify (n -> n == 0) start

replaced : These Boolean Text
replaced = These.here.set true start
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + modified : These Boolean Text
  + replaced : These Boolean Text
  + start    : These Nat Text

  Run `update` to apply these changes to your codebase.
```

# Higher-rank and type-changing together

A record whose single field is a higher-rank function — a `Functor`.
The field is both higher-rank and the sole mention of `f`, so its
modifier is higher-rank *and* type-changing in `f`:

``` unison
structural type Functor f = { map : forall a b. (a -> b) -> f a -> f b }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type Functor f

  + Functor.map        : Functor f
                         -> (∀ a b. (a -> b) -> f a -> f b)
  + Functor.map.modify : ((∀ a b. (a -> b) -> f1 a -> f1 b)
                          ->{g} (∀ a b. (a -> b) -> f a -> f b))
                         -> Functor f1
                         ->{g} Functor f
  + Functor.map.set    : (∀ a b. (a -> b) -> f a -> f b)
                         -> Functor f1
                         -> Functor f

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
> add
```

``` ucm
> view Functor.map.modify

  Functor.map.modify :
    ((∀ a b. (a -> b) -> f1 a -> f1 b)
     ->{g} (∀ a b. (a -> b) -> f a -> f b))
    -> Functor f1
    ->{g} Functor f
  Functor.map.modify f = cases Functor map -> Functor (f map)
```

# A variable shared by several fields is left fixed

If more than one field mentions a type variable, changing it via one
field's accessor would leave the other fields inconsistent, so that
variable is *not* freshened. The accessors for `Pair` (whose two
fields share `a`) are the ordinary non-type-changing ones:

``` unison
structural type Pair a = { first : a, second : a }
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + structural type Pair a

  + Pair.first         : Pair a -> a
  + Pair.first.modify  : (a ->{g} a) -> Pair a ->{g} Pair a
  + Pair.first.set     : a -> Pair a -> Pair a
  + Pair.second        : Pair a -> a
  + Pair.second.modify : (a ->{g} a) -> Pair a ->{g} Pair a
  + Pair.second.set    : a -> Pair a -> Pair a

  Run `update` to apply these changes to your codebase.
```
