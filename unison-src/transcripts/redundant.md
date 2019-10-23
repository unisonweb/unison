The same kind of thing happens with `map`. Are we saying this is incorrect behaviour?

```unison
map : (a -> b) -> [a] -> [b]
map f xs = case xs of 
  x +: xs -> f x +: map f xs
  [] -> []
```

```ucm
.> add
.> view map
.> find map
```

