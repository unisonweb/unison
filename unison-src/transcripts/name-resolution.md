# Example 1

We have a namespace type named `Namespace.Foo` and a file type named `File.Foo`. A reference to the type `Foo` is
ambiguous. A reference to `Namespace.Foo` or `File.Foo` work fine.

``` ucm
scratch/main> builtins.mergeio lib.builtins
```

``` unison
type Namespace.Foo = Bar
```

``` ucm
scratch/main> add
```

``` unison :error
type File.Foo = Baz
type UsesFoo = UsesFoo Foo
```

``` unison
type File.Foo = Baz
type UsesFoo = UsesFoo Namespace.Foo File.Foo
```

``` ucm
scratch/main> project.delete scratch
```

# Example 2

We have a namespace type named `Foo` and a file type named `File.Foo`. A reference to the type `Foo` is not ambiguous:
it refers to the namespace type (because it is an exact match).

``` ucm
scratch/main> builtins.mergeio lib.builtins
```

``` unison
type Foo = Bar
```

``` ucm
scratch/main> add
```

``` unison
type File.Foo = Baz
type UsesFoo = UsesFoo Foo
```

``` ucm
scratch/main> add
scratch/main> view UsesFoo
```

``` ucm
scratch/main> project.delete scratch
```

# Example 3

We have a namespace type named `Namespace.Foo` and a file type named `Foo`. A reference to the type `Foo` is not ambiguous:
it refers to the file type (because it is an exact match).

``` ucm
scratch/main> builtins.mergeio lib.builtins
```

``` unison
type Namespace.Foo = Bar
```

``` ucm
scratch/main> add
```

``` unison
type Foo = Baz
type UsesFoo = UsesFoo Foo
```

``` ucm
scratch/main> add
scratch/main> view UsesFoo
```

``` ucm
scratch/main> project.delete scratch
```

# Example 4

We have a namespace term `ns.foo : Nat` and a file term `file.foo : Text`. A reference to the term `foo` is ambiguous,
but resolves to `ns.foo` via TDNR.

``` ucm
scratch/main> builtins.mergeio lib.builtins
```

``` unison
ns.foo : Nat
ns.foo = 42
```

``` ucm
scratch/main> add
```

``` unison
file.foo : Text
file.foo = "foo"

bar : Text
bar = foo ++ "bar"
```

``` ucm
scratch/main> project.delete scratch
```

# Example 4

We have a namespace term `ns.foo : Nat` and a file term `file.foo : Text`. A reference to the term `foo` is ambiguous,
but resolves to `file.foo` via TDNR.

``` ucm
scratch/main> builtins.mergeio lib.builtins
```

``` unison
ns.foo : Nat
ns.foo = 42
```

``` ucm
scratch/main> add
```

``` unison
file.foo : Text
file.foo = "foo"

bar : Nat
bar = foo + 42
```

``` ucm
scratch/main> project.delete scratch
```

# Example 4

We have a namespace term `ns.foo : Nat` and a file term `file.foo : Nat`. A reference to the term `foo` is ambiguous.
A reference to `ns.foo` or `file.foo` work fine.

``` ucm
scratch/main> builtins.mergeio lib.builtins
```

``` unison
ns.foo : Nat
ns.foo = 42
```

``` ucm
scratch/main> add
```

``` unison :error
file.foo : Nat
file.foo = 43

bar : Nat
bar = foo + 10
```

``` unison
file.foo : Nat
file.foo = 43

bar : Nat
bar = file.foo + ns.foo
```

``` ucm
scratch/main> add
scratch/main> view bar
```

``` ucm
scratch/main> project.delete scratch
```
