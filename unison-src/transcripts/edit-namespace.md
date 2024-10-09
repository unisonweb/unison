``` ucm :hide
project/main> builtins.mergeio lib.builtin
```

``` unison
{{ ping doc }}
nested.cycle.ping n = n Nat.+ pong n

{{ pong doc }}
nested.cycle.pong n = n Nat.+ ping n

toplevel = "hi"

simple.x = 10
simple.y = 20

-- Shouldn't edit things in lib
lib.project.ignoreMe = 30

-- Shouldn't render record accessors
unique type Foo = { bar : Nat, baz : Nat }
```

``` ucm
project/main> add
```

`edit.namespace` edits the whole namespace (minus the top-level `lib`).

``` ucm
project/main> edit.namespace
```

`edit.namespace` can also accept explicit paths

``` ucm
project/main> edit.namespace nested simple
```
