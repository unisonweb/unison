##### Find by type support

`find : a -> b` should find the `bug` and `todo` functions.

```ucm
.> find : a -> b
```

`find : b -> a` should also find the `bug` and `todo` functions.

```ucm
.> find : b -> a
```

```unison:hide:all
foldl : (b ->{𝕖} a ->{𝕖} b) -> b -> [a] ->{𝕖} b
foldl f b as =
  go b i =
    case List.at i as of
      None -> b
      Some a ->
        use Nat +
        go (f b a) (i + 1)
  go b 0
```

```ucm:hide
.> add
```

`foldl` should be found if `a` and `b` are swapped.

```ucm
.> find : (b -> a -> b) -> b -> [a] -> b
```

```ucm
.> find : (a -> b -> a) -> a -> [b] -> a
```

```unison:hide:all
flip: (a -> b -> c) -> b -> a -> c
flip f b a = f a b
```

```ucm:hide
.> add
```

The `flip` function should be found regardless of the naming of the types.

```ucm
.> find : (a -> b -> c) -> (b -> a -> c)
.> find : (b -> a -> c) -> (a -> b -> c)
.> find : (c -> b -> a) -> (b -> c -> a)
.> find : (d -> e -> f) -> (e -> d -> f)
```
