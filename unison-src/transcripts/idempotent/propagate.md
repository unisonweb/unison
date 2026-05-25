# Propagating type edits

``` ucm :hide
> builtins.merge lib.builtins
```

We introduce a type `Foo` with a function dependent `fooToInt`.

``` unison
unique type Foo = Foo

fooToInt : Foo -> Int
fooToInt _ = +42
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Foo

  + fooToInt : Foo -> Int

  Run `update` to apply these changes to your codebase.
```

And then we add it.

``` ucm
> add

  Done.

> find.verbose

  1. -- #j743idicb1sf7udts85812agaml4rkfi3iss6lstvmvgufibd40blq5qtmoh9ndrtkvkaqkurn7npgc61ob8j2louj04j8slkppsl90
     type Foo
     
  2. -- #j743idicb1sf7udts85812agaml4rkfi3iss6lstvmvgufibd40blq5qtmoh9ndrtkvkaqkurn7npgc61ob8j2louj04j8slkppsl90#0
     Foo.Foo : Foo
     
  3. -- #sd7apvqbpk3vl2aassq4gcckovohqrs05ne1g9ol0fb6gd227bp388osj7bg40kttt2o9f1kit9avlb94ep8q1ho3g284ursrplb4l0
     fooToInt : Foo -> Int
     

> view fooToInt

  fooToInt : Foo -> Int
  fooToInt _ = +42
```

Then if we change the type `Foo`...

``` unison
unique type Foo = Foo | Bar
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Foo

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

and update the codebase to use the new type `Foo`...

``` ucm
> update

  Done.
```

... it should automatically propagate the type to `fooToInt`.

``` ucm
> view fooToInt

  fooToInt : Foo -> Int
  fooToInt _ = +42
```

### Preserving user type variables

We make a term that has a dependency on another term and also a non-redundant
user-provided type signature.

``` unison
preserve.someTerm : Optional foo -> Optional foo
preserve.someTerm x = x

preserve.otherTerm : Optional baz -> Optional baz
preserve.otherTerm y = someTerm y
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + preserve.otherTerm : Optional baz -> Optional baz
  + preserve.someTerm  : Optional foo -> Optional foo

  Run `update` to apply these changes to your codebase.
```

Add that to the codebase:

``` ucm
> add

  Done.
```

Let's now edit the dependency:

``` unison
preserve.someTerm : Optional x -> Optional x
preserve.someTerm _ = None
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ preserve.someTerm : Optional x -> Optional x

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

Update...

``` ucm
> update

  Done.
```

Now the type of `someTerm` should be `Optional x -> Optional x` and the
type of `otherTerm` should remain the same.

``` ucm
> view preserve.someTerm

  preserve.someTerm : Optional x -> Optional x
  preserve.someTerm _ = None

> view preserve.otherTerm

  preserve.otherTerm : Optional baz -> Optional baz
  preserve.otherTerm y = someTerm y
```
