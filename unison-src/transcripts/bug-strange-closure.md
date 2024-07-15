
```ucm:hide
scratch/main> builtins.mergeio lib.builtins
scratch/main> load unison-src/transcripts-using-base/doc.md.files/syntax.u
```

We can display the guide before and after adding it to the codebase:

```ucm
scratch/main> display doc.guide
scratch/main> add
scratch/main> display doc.guide
```

But we can't display this due to a decompilation problem.

```unison
rendered = Pretty.get (docFormatConsole doc.guide)
```

```ucm
scratch/main> display rendered
scratch/main> add
scratch/main> display rendered
scratch/main> undo
```

And then this sometimes generates a GHC crash "strange closure error" but doesn't seem deterministic.

```unison
rendered = Pretty.get (docFormatConsole doc.guide)

> rendered
```
