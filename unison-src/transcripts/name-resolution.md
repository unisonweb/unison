# Example 1

We have a codebase type named `Codebase.Foo` and a file type named `File.Foo`. A reference to the type `Foo` is
ambiguous. A reference to `Codebase.Foo` or `File.Foo` work fine.

```ucm
scratch/main> builtins.mergeio lib.builtins
```

```unison
type Codebase.Foo = Bar
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
type UsesFoo = UsesFoo Codebase.Foo File.Foo
```

```ucm
scratch/main> project.delete scratch
```

# Example 2

We have a codebase term `Woot.state : Nat` and a file term `Something.state : Text -> Something`. A reference to the
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
