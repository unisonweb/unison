
```ucm:hide
.> builtins.merge
```

This should render as `Bytes.fromList [1,2,3,4]`, not `##Bytes.fromSequence [1,2,3,4]`:

```unison
> Bytes.fromList [1,2,3,4]
```

