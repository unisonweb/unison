# Example 1

We have a namespace type named `Namespace.Foo` and a file type named `File.Foo`. A reference to the type `Foo` is
ambiguous. A reference to `Namespace.Foo` or `File.Foo` work fine.

```ucm
scratch/main> builtins.mergeio lib.builtins
```

```unison
type Namespace.Foo = Bar
```

```ucm
scratch/main> add
```

```unison:error
type File.Foo = Baz
type UsesFoo = UsesFoo Foo
```

```unison
type File.Foo = Baz
type UsesFoo = UsesFoo Namespace.Foo File.Foo
```

```ucm
scratch/main> project.delete scratch
```

# Example 2

We have a namespace type named `Foo` and a file type named `File.Foo`. A reference to the type `Foo` is not ambiguous:
it refers to the namespace type (because it is an exact match).

```ucm
scratch/main> builtins.mergeio lib.builtins
```

```unison
type Foo = Bar
```

```ucm
scratch/main> add
```

```unison
type File.Foo = Baz
type UsesFoo = UsesFoo Foo
```

```ucm
scratch/main> add
scratch/main> view UsesFoo
```

```ucm
scratch/main> project.delete scratch
```

# Example 3

We have a namespace type named `Namespace.Foo` and a file type named `Foo`. A reference to the type `Foo` is not ambiguous:
it refers to the file type (because it is an exact match).

```ucm
scratch/main> builtins.mergeio lib.builtins
```

```unison
type Namespace.Foo = Bar
```

```ucm
scratch/main> add
```

```unison
type Foo = Baz
type UsesFoo = UsesFoo Foo
```

```ucm
scratch/main> add
scratch/main> view UsesFoo
```

```ucm
scratch/main> project.delete scratch
```

# Example 4

We have a namespace term `Woot.state : Nat` and a file term `Something.state : Text -> Something`. A reference to the
term `state : Text` resolves to `Something.state`, which shadows `Woot.state`. (This behavior will change).

```ucm
scratch/main> builtins.mergeio lib.builtins
```

```unison
Woot.state : Nat
Woot.state = 42
```

```ucm
scratch/main> add
```

```unison
type Something = { state : Text }

ex = do
  s = Something "hello"
  state s ++ " world!"
```

```ucm
scratch/main> project.delete scratch
```
