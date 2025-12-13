# Type-based search

``` ucm :hide
> alias.type ##Text builtin.Text
```

## Basic type-based search

``` unison :hide
unique type A = A Text

foo : A
foo = A "foo!"

bar : Text -> A
bar = A

baz : A -> Text
baz = cases
  A t -> t
```

``` ucm :hide
> add
```

``` ucm
> find : Text -> A

  1. bar : Text -> A
  2. A.A : Text -> A

> find : A -> Text

  1. baz : A -> Text

> find : A

  1. foo : A
```

``` ucm :error
> find : Text

  ☝️

  I couldn't find exact type matches, resorting to fuzzy
  matching...

  1. baz : A -> Text
  2. bar : Text -> A
  3. A.A : Text -> A
```

## Type-based search should not search transitive dependencies

https://github.com/unisonweb/unison/issues/6052

When using type-based search (`find : <type>`), the search should only return
results from the local project and direct dependencies, not transitive dependencies.

Set up a scenario with transitive dependencies:

  - `myFn` in the project root
  - `directDep.fn` in a direct dependency
  - `directDep.lib.transitive.fn` in a transitive dependency (should NOT appear in `find` results)

``` unison :hide
myFn : A -> A
myFn a = a

lib.directDep.fn : A -> A
lib.directDep.fn a = a

lib.directDep.lib.transitive.fn : A -> A
lib.directDep.lib.transitive.fn a = a
```

``` ucm :hide
> add
```

Now, `find : A -> A` should only show `myFn` (local) - not things in lib:

``` ucm
> find : A -> A

  1. myFn : A -> A
```

Even when there are local matches, `find-in lib` can be used to explicitly search dependencies:

``` ucm
> find-in lib : A -> A

  1. directDep.fn : A -> A
```

And `find.all : A -> A` should show `myFn` and `directDep.fn` but NOT `transitive.fn`:

``` ucm
> find.all : A -> A

  1. lib.directDep.fn : A -> A
  2. myFn : A -> A
```

Now test the fallback case: when `find` has no local matches, it falls back to searching
direct dependencies (but still not transitive deps).

``` unison :hide
unique type B = B

lib.directDep.libOnlyFn : B -> B
lib.directDep.libOnlyFn b = b

lib.directDep.lib.transitive.libOnlyFn : B -> B
lib.directDep.lib.transitive.libOnlyFn b = b
```

``` ucm :hide
> add
```

`find : B -> B` should fall back to lib but still exclude transitive deps:

``` ucm :error
> find : B -> B

  ☝️

  I couldn't find exact type matches, resorting to fuzzy
  matching...

  ☝️

  I couldn't find matches in this namespace, searching in
  'lib'...

  1. lib.directDep.libOnlyFn : B -> B
```

To explicitly search lib including transitive deps, use `find-in.all lib`:

``` ucm
> find-in.all lib : A -> A

  1. directDep.fn : A -> A
  2. directDep.lib.transitive.fn : A -> A
```
