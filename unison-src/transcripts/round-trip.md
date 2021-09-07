This transcript verifies that the pretty-printer produces code that can be successfully parsed, for a variety of examples. Terms or types that fail to round-trip can be added here as regression tests. Add tests at the bottom of this

```ucm:hide
.> builtins.mergeio
.> load unison-src/transcripts-using-base/base.u
```

## How to use this transcript: checking round-trip for inline definitions

```unison:hide
x = 1 + 1
```

```ucm
.> add
.> edit x
.> reflog
.> reset-root 2
```

Resetting the namespace after each example ensures they don't interact at all, which is probably what you want.

The `load` command which does parsing and typechecking of the `edit`'d definitions needs to be in a separate stanza from the `edit` command.

```ucm
.> load scratch.u
```

## How to use this transcript: checking round-trip for definitions from a file

Examples can also be loaded from `.u` files:

```ucm
.> load unison-src/transcripts/round-trip/ex2.u
.> add
```

When loading definitions from a file, an empty stanza like this will ensure that this empty file is where the definitions being `edit`'d will get dumped.

```unison:hide
-- empty scratch file, `edit` will target this
```

Without the above stanza, the `edit` will send the definition to the most recently loaded file, which would be `ex2.u`, making the transcript not idempotent.

```ucm
.> edit b
.> reflog
.> reset-root 2
```

```ucm
.> load scratch.u
```

No reason you can't load a bunch of definitions from a single `.u` file in one go, the only thing that's annoying is you'll have to `find` and then `edit 1-11` in the transcript to load all the definitions into the file.

## Example 1

Add tests here
