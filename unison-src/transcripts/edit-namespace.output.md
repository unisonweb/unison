```unison
{{ ping doc }}
nested.cycle.ping n = n Nat.+ pong n

{{ pong doc }}
nested.cycle.pong n = n Nat.+ ping n

toplevel = "hi"

simple.x = 10
simple.y = 20

-- Shouldn't edit things in lib
lib.project.ignoreMe = 30
```

Edit current namespace

```ucm
.simple> edit.namespace

  ☝️
  
  I added 2 definitions to the top of scratch.u
  
  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.

```
```unison:added-by-ucm scratch.u
x : ##Nat
x = 10

y : ##Nat
y = 20
```

Edit should hit things recursively

```ucm
.> edit.namespace

  ☝️
  
  I added 7 definitions to the top of scratch.u
  
  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.

```
```unison:added-by-ucm scratch.u
nested.cycle.ping : Nat -> Nat
nested.cycle.ping n =
  use Nat +
  n + nested.cycle.pong n

nested.cycle.ping.doc : Doc2
nested.cycle.ping.doc = {{ ping doc }}

nested.cycle.pong : Nat -> Nat
nested.cycle.pong n =
  use Nat +
  n + nested.cycle.ping n

nested.cycle.pong.doc : Doc2
nested.cycle.pong.doc = {{ pong doc }}

simple.x : Nat
simple.x = 10

simple.y : Nat
simple.y = 20

toplevel : Text
toplevel = "hi"
```

Edit should handle multiple explicit paths at once.

```ucm
.> edit.namespace nested.cycle simple

  ☝️
  
  I added 6 definitions to the top of scratch.u
  
  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.

```
```unison:added-by-ucm scratch.u
nested.cycle.ping : Nat -> Nat
nested.cycle.ping n =
  use Nat +
  n + nested.cycle.pong n

nested.cycle.ping.doc : Doc2
nested.cycle.ping.doc = {{ ping doc }}

nested.cycle.pong : Nat -> Nat
nested.cycle.pong n =
  use Nat +
  n + nested.cycle.ping n

nested.cycle.pong.doc : Doc2
nested.cycle.pong.doc = {{ pong doc }}

simple.x : Nat
simple.x = 10

simple.y : Nat
simple.y = 20
```

