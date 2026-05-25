# ADR-016 scenario (a): updating a given changes the dependent term's hash

When a definition is annotated as a `given` in the namespace, the
elaborator can resolve constraint goals against it (chunk D4 wires
the namespace ambient pool; for D3 we exercise the parser/typechecker
plumbing only).

The exit criterion from `docs/implicits-plan.md` §5.6: *"working update
through a given changes the hash."* This transcript captures the hash
of an elaborated term, updates the given it elaborates against, and
confirms the dependent term acquires a new hash on re-elaboration.

ADR-014 spells out *why*: an elaborated term hashes against the
resolved dictionary's hash via the existing `App` machinery. So when
the dictionary's hash changes, so does the elaborated term's hash.

``` ucm :hide
> builtins.merge
```

Define a tiny `Show` ability stand-in (a record-style data type
holding the rendering function), a given `Show.nat` that conforms to
it, and a consumer `foo` that depends on it. Until D4 wires the
namespace ambient pool we make the dependency explicit by calling
`Show.nat` by name; D4's resolver substitutes that call automatically
from a constraint goal.

``` unison
unique type Show a = Show (a -> Text)

given Show.nat : Show Nat = Show.Show Nat.toText

foo : Nat -> Text
foo n = match Show.nat with Show.Show f -> f n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Show a

  + foo      : Nat -> Text
  + Show.nat : Show Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Done.
```

Capture the original hash of `foo` (call it H1).

``` ucm
> names foo

  'foo':
  Hash          Kind   Names
  #9qoq16jvg3   Term   foo
```

Now update `Show.nat` to a different (but type-compatible)
implementation. The body changes, so `Show.nat`'s term hash must
change; everything that references `Show.nat` re-elaborates and
re-hashes.

``` unison
Show.nat : Show Nat
Show.nat = Show.Show (n -> "n=" ++ Nat.toText n)
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ Show.nat : Show Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
> update

  Done.
```

Re-record `foo`'s hash (call it H2). Per ADR-016 + ADR-014: the new
hash differs from H1; the old hash H1 remains valid for any code
that already references it (content addressing — ADR-002 principle 2).

``` ucm
> names foo

  'foo':
  Hash          Kind   Names
  #058ts2h1fa   Term   foo

> names Show.nat

  'Show.nat':
  Hash          Kind   Names
  #igvikkkjns   Term   Show.nat
```
