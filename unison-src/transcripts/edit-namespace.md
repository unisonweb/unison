```ucm:hide
.lib> builtins.mergeio
```

```unison:hide
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

```ucm:hide
.> add
```

Edit current namespace

```ucm
.simple> edit.namespace
```

Edit should hit things recursively

```ucm
.> edit.namespace
```

Edit should handle multiple explicit paths at once.

```ucm
.> edit.namespace nested.cycle simple
```
