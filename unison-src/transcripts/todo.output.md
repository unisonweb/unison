# Test the `todo` command

## Simple type-changing update.

```unison
x = 1
useX = x + 10

type MyType = MyType Nat
useMyType = match MyType 1 with
  MyType a -> a + 10
```

Perform a type-changing update so dependents are added to our update frontier.

```unison
x = -1

type MyType = MyType Text
```

```ucm
.simple> update.old

  ⍟ I've updated these names to your new definition:
  
    type MyType
    x : Int

.simple> todo

  ✅
  
  No conflicts or edits in progress.

```

```



🛑

The transcript was expecting an error in the stanza above, but did not encounter one.
