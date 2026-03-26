# Parsing

Structural records should parse.

```unison
jon =
  { name : "Jon Arbuckle"
  , age : 35
  }
```

# Evaluation/Runtime

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

# Codebase saving

We should be able to add them to the codebase.

```unison
jon =
  { name : "Jon Arbuckle"
  , age : 35
  }
```

```ucm
scratch/main> update
```


# Type Errors

We should get custom errors when a record is missing a field:

```unison
jons =
    [ { name : "Jon Arbuckle" }
    , { name : "Jon Snow", age : 25 }
    ]
```

We should get a reasonable error when a record has an extra field:

```unison
jons =
    [ { name : "Jon Snow", age : 25 }
    , { name : "Jon Arbuckle", age : 35, pet : "Garfield" }
    ]
```

We should get a reasonable error when a record field has mismatched types:

```unison
jons =
    [ { name : "Jon Arbuckle", age : 35 }
    , { name : "Jon Snow", age : "25" }
    ]
```
