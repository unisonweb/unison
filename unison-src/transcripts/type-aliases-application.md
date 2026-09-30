# Calling functions with alias types

```ucm:hide
> builtins.mergeio
```

```unison
type alias Endo a = a -> a

increment : Endo Nat
increment n = n + 1

apply : Endo Nat -> Nat
apply f = f 41

> increment 41
> apply increment
> apply (n -> n + 2)
```

```ucm
> add
> view increment
```

Calls through a stored alias must work too, while the stored signature
continues to use the alias.

```unison
> increment 43
> apply (n -> n + 3)
```

Calling a stored alias-typed function from an entry point must not evaluate
it ahead of time as though it were a constant.

```unison
main : '{IO} ()
main = do
  if increment 41 == 42 then () else bug "incorrect alias function result"
```

```ucm
> add
> run main
```
