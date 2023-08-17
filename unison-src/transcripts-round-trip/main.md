This transcript verifies that the pretty-printer produces code that can be successfully parsed, for a variety of examples. Terms or types that fail to round-trip can be added  to either `reparses-with-same-hash.u` or `reparses.u` as regression tests.

```ucm:hide
.> builtins.mergeio
```

```ucm:hide
.> copy.namespace builtin a1.lib.builtin
.> copy.namespace builtin a2.lib.builtin
.> load unison-src/transcripts-round-trip/reparses-with-same-hash.u
.a1> add
```

```unison /private/tmp/roundtrip.u
x = ()
```

```ucm:hide
.a1> find
```

So we can see the pretty-printed output:

```ucm
.a1> edit 1-1000
```

```ucm:hide
.a1> delete.namespace.force lib.builtin
```

```ucm:hide
.a2> load /private/tmp/roundtrip.u
```

```ucm:hide
.a2> add
.a2> delete.namespace.force lib.builtin
```

This diff should be empty if the two namespaces are equivalent. If it's nonempty, the diff will show us the hashes that differ.

```ucm:error
.> diff.namespace a1 a2
```

```ucm:hide
.> undo
.> undo
```

Now check that definitions in 'reparses.u' at least parse on round trip:

```ucm:hide
.a3> copy.namespace .builtin lib.builtin
.a3> load unison-src/transcripts-round-trip/reparses.u
.a3> add
```

This just makes 'roundtrip.u' the latest scratch file.

```unison:hide /private/tmp/roundtrip.u
x = ()
```

```ucm:hide
.a3> find
```

```ucm
.a3> edit 1-5000
```

```ucm:hide
.> move.namespace a3 a3_old
.a3> copy.namespace .builtin lib.builtin
.a3> load /private/tmp/roundtrip.u
.a3> add
.a3> delete.namespace.force lib.builtin
.a3_old> delete.namespace.force lib.builtin
```

These are currently all expected to have different hashes on round trip, though we'd prefer if they round tripped with the same hash.

NOTE, since we don't currently have anything that round trips with a different hash, this fails. If you find an example that reparses with a different hash, add it to `reparses.u` and change this stanza to `ucm` rather than `ucm:error`.

```ucm:error
.> diff.namespace a3 a3_old
```

## Other regression tests not covered by above

### Comment out builtins in the edit command

Regression test for https://github.com/unisonweb/unison/pull/3548

```ucm:hide
.> alias.term ##Nat.+ plus
.> edit plus
.> load /private/tmp/roundtrip.u
.> undo
```
