```ucm:hide
.> builtins.merge
.> move.namespace builtin lib.builtin
```

```unison
unique type Foo = Bar Nat
```

```ucm
.> add
.> move.term Foo.Bar Stray.Bar
```

```unison
unique type Foo = Bar Nat Nat
```

Bug: this update crashes ucm. Oops we can't even capture that in a transcript.

# .> update
