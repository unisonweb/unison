Structural records should parse.

```unison
jon =
  { name : "Jon Arbuckle"
    , age : 35
  }
```

We should be able to add them to the codebase.

```ucm
scratch/main> update
```

We should be able to evaluate and print them.

```unison
> jon
```

```ucm
scratch/main> view jon
```

Record types can unify with each other:

```unison
jons =
  [ { name : "Jon Arbuckle", age : 35 }
  , { name : "Jon Snow", age : 25 }
  ]
```
