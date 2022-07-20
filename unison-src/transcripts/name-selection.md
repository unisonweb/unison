This transcript shows how the pretty-printer picks names for a hash when multiple are available. The algorithm is:

1. Names that are "name-only" come before names that are hash qualified. So `List.map` comes before `List.map#2384a` and also `aaaa#xyz`.
2. Shorter names (in terms of segment count) come before longer ones, for instance `base.List.map` comes before `somelibrary.external.base.List.map`.
3. Otherwise if there are multiple names with a minimal number of segments, compare the names alphabetically.

```ucm:hide
.> alias.type ##Nat Nat
.> alias.term ##Nat.+ Nat.+
```

```unison:hide
a = b + 1
b = 0 + 1
```

Will add `a` and `b` to the codebase and give `b` a longer (in terms of segment length alias), and show that it isn't used when viewing `a`:

```ucm
.a> add
.a> alias.term b aaa.but.more.segments
.a> view a
.> cd .
```

Next let's introduce a conflicting symbol and show that its hash qualified name isn't used when it has an unconflicted name:

```
.> fork a a2
.> fork a a3
```

```unison:hide
c = 1
d = c + 10
```

```ucm:hide
.a2> builtins.merge
```
```ucm
.a2> add
.a2> alias.term c long.name.but.shortest.suffixification
```

```unison:hide
c = 2
d = c + 10
```

```ucm
.a3> add
.a3> merge .a2 .a3
```

At this point, `a3` is conflicted for symbols `c` and `d`, so those are deprioritized. 
The original `a2` namespace has an unconflicted definition for `c` and `d`, but since there are multiple 'c's in scope, 
`long.name.but.shortest.suffixification` is chosen because its suffixified version has the fewest segments.

```ucm
.> view a b c d
```
