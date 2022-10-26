## Blocks and scoping

```ucm:hide
.> builtins.merge
```

### Names introduced by a block shadow names introduced in outer scopes

For example:

```unison
ex thing =
  thing y = y
  -- refers to `thing` in this block
  -- not the argument to `ex`
  bar x = thing x + 1
  bar 42

> ex "hello"
```

### Whether a block shadows outer names doesn't depend on the order of bindings in the block

The `thing` reference in `bar` refers to the one declared locally in the block that `bar` is part of. This is true even if the declaration which shadows the outer name appears later in the block, for instance:

```unison
ex thing =
  bar x = thing x + 1
  thing y = y
  bar 42

> ex "hello"
```

### Blocks use lexical scoping and can only reference definitions in parent scopes or in the same block

This is just the normal lexical scoping behavior. For example:

```unison
ex thing =
  bar x = thing x + 1 -- references outer `thing`
  baz z =
    thing y = y -- shadows the outer `thing`
    thing z     -- references the inner `thing`
  bar 42

> ex (x -> x * 100)
```

Here's another example, showing that bindings cannot reference bindings declared in blocks nested in the _body_ (the final expression) of a block:

```unison
ex thing =
  bar x = thing x + 1 -- refers to outer thing
  let
    thing y = y
    bar 42

> ex (x -> x * 100)
```

### Blocks can define one or more functions which are recursive or mutually recursive

We call these groups of definitions that reference each other in a block _cycles_. For instance:

```unison
sumTo n =
  -- A recursive function, defined inside a block
  go acc n =
    if n == 0 then acc
    else go (acc + n) (drop n 1)
  go 0 n

ex n =
  -- Two mutually recursive functions, defined in a block
  ping x = pong (x + 1)
  pong x = ping (x + 2)
  ping 42
```

The `go` function is a one-element cycle (it reference itself), and `ping` and `pong` form a two-element cycle.

### Cyclic references or forward reference must be guarded

For instance, this works:

```unison
ex n =
  ping x = pong + 1 + x
  pong = 42
  ping 0
```

Since the forward reference to `pong` appears inside `ping`.

This, however, will not compile:

```unison:error
ex n =
  pong = ping + 1
  ping = 42
  pong
```

This also won't compile; it's a cyclic reference that isn't guarded:

```unison:error
ex n =
  loop = loop
  loop
```

This, however, will compile. This also shows that `'expr` is another way of guarding a definition.

```unison
ex n =
  loop = '(!loop)
  !loop
```

Just don't try to run it as it's an infinite loop!

### Cyclic definitions in a block don't have access to any abilities

The reason is it's unclear what the order should be of any requests that are made. It can also be viewed of a special case of the restriction that elements of a cycle must all be guarded. Here's an example:

```unison:error
structural ability SpaceAttack where
  launchMissiles : Text -> Nat

ex n =
  zap1 = launchMissiles "neptune" + zap2
  zap2 = launchMissiles "pluto" + zap1
  zap1
```

### The _body_ of recursive functions can certainly access abilities

For instance, this works fine:

```unison
structural ability SpaceAttack where
  launchMissiles : Text -> Nat

ex n =
  zap1 planet = launchMissiles planet + zap2 planet
  zap2 planet = launchMissiles planet + zap1 planet
  zap1 "pluto"
```

### Unrelated definitions not part of a cycle and are moved after the cycle

For instance, `zap` here isn't considered part of the cycle (it doesn't reference `ping` or `pong`), so this typechecks fine:

```unison
structural ability SpaceAttack where
  launchMissiles : Text -> Nat

ex n =
  ping x = pong (x + 1)
  zap = launchMissiles "neptune"
  pong x = ping (x + 2)
  ping 42
```

This is actually parsed as if you moved `zap` after the cycle it find itself a part of:

```unison
structural ability SpaceAttack where
  launchMissiles : Text -> Nat

ex n =
  ping x = pong (x + 1)
  pong x = ping (x + 2)
  zap = launchMissiles "neptune"
  ping 42
```
