
```ucm:hide
.> builtins.mergeio
.> load unison-src/transcripts-using-base/doc.md.files/syntax.u
```

We can display the guide before and after adding it to the codebase:

```ucm
.> display doc.guide
.> add
.> display doc.guide
```

But we can't display this due to a decompilation problem.

```unison
rendered = Pretty.get (docFormatConsole doc.guide)
```

```ucm
.> display rendered
.> add
.> display rendered
.> undo
```

And then this sometimes generates a GHC crash "strange closure error" but doesn't seem deterministic.

```unison
rendered = Pretty.get (docFormatConsole doc.guide)

> rendered
```
