This transcript shows how the pretty-printer picks names for a hash when multiple are available. The algorithm is:

1. Names that are "name-only" come before names that are hash qualified. So `List.map` comes before `List.map#2384a` and also `aaaa#xyz`.
2. Shorter names (in terms of segment count) come before longer ones, for instance `base.List.map` comes before `somelibrary.external.base.List.map`.
3. Otherwise if there are multiple names with a minimal number of segments, compare the names alphabetically.

```ucm:hide
scratch/main> builtins.merge lib.builtins
scratch/biasing> builtins.merge lib.builtins
```

```unison:hide
a.a = a.b + 1
a.b = 0 + 1
a.aaa.but.more.segments = 0 + 1
```

Will add `a` and `b` to the codebase and give `b` a longer (in terms of segment length alias), and show that it isn't used when viewing `a`:

```ucm
scratch/main> add
scratch/main> view a.a
```

Next let's introduce a conflicting symbol and show that its hash qualified name isn't used when it has an unconflicted name:

```unison:hide
a2.a = a2.b + 1
a2.b = 0 + 1
a2.aaa.but.more.segments = 0 + 1
a2.c = 1
a2.d = a2.c + 10
a2.long.name.but.shortest.suffixification = 1

a3.a = a3.b + 1
a3.b = 0 + 1
a3.aaa.but.more.segments = 0 + 1
a3.c = 2
a3.d = a3.c + 10
a3.long.name.but.shortest.suffixification = 1
```

```ucm
scratch/main> add
scratch/main> debug.alias.term.force a2.c a3.c
scratch/main> debug.alias.term.force a2.d a3.d
```

At this point, `a3` is conflicted for symbols `c` and `d`, so those are deprioritized.
The original `a2` namespace has an unconflicted definition for `c` and `d`, but since there are multiple 'c's in scope,
`a2.c` is chosen because although the suffixified version has fewer segments, its fully-qualified name has the fewest segments.

```ucm
scratch/main> view a b c d
```

## Name biasing

```unison
deeply.nested.term =
  a + 1

deeply.nested.num = 10

a = 10
```

```ucm
scratch/biasing> add
-- Despite being saved with name `a`,
-- the pretty printer should prefer the suffixified 'deeply.nested.num name' over the shallow 'a'.
-- It's closer to the term being printed.
scratch/biasing> view deeply.nested.term
```

Add another term with `num` suffix to force longer suffixification of `deeply.nested.num`

```unison
other.num = 20
```

```ucm
scratch/biasing> add
-- nested.num should be preferred over the shorter name `a` due to biasing
-- because `deeply.nested.num` is nearby to the term being viewed.
scratch/biasing> view deeply.nested.term
```
