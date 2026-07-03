# Scenario: end-to-end constraint resolution and `view` round-trip

This transcript exercises the user-visible surface of implicit
resolution: a `=>` constraint on a `class` accessor is discharged
automatically at the call site, a *parametric* given
(`Show.list : Show a => Show [a]`) is chained onto a base given
(`Show.nat`), and `view` renders the result back to exactly what the
user wrote — with no leaked dictionary arguments and no spurious
`give` keyword.

Why: the elaborator inserts the chosen dictionaries into the term's
hash, but `view` elides them by deriving the implicit slots from the
declared type. A parametric dictionary is a nested application
(`Show.list Show.nat`), so the elision must recognise that shape as an
auto-resolved dictionary rather than mistaking it for a user-supplied
`give` argument.

``` ucm :hide
scratch/main> builtins.merge
```

Declare a `Show` class, a base given for `Nat`, and a parametric given
for lists whose premise `Show a` is itself discharged by resolution.
`foo` consumes the constraint by calling the `Show.show` accessor,
which carries `Show a =>` implicitly.

``` unison
class Show a = { show : a -> Text }

given Show.nat : Show Nat = Show Nat.toText

given Show.list : Show a => Show [a] =
  Show (xs -> match xs with
    []     -> "[]"
    x +: _ -> "[" ++ Show.show x ++ ", ..]")

foo : Text
foo = Show.show [1, 2, 3]

> foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + class Show a

  + foo       : Text
  + Show.list : Show a => Show [a]
  + Show.nat  : Show Nat
  + Show.show : Show a => a -> Text

  Run `update` to apply these changes to your codebase.

    13 | > foo
           ⧩
           "[1, ..]"
```

``` ucm :hide
scratch/main> add
```

`dependencies foo` lists both givens the resolver chained
(`Show.list` and, through it, `Show.nat`) — the chosen dictionaries
are baked into `foo`'s hash.

``` ucm
scratch/main> dependencies foo

  Dependencies of: foo

    Types:

    1. builtin.Text

    Terms:

    2. Show.list
    3. Show.nat
    4. Show.show

  Tip: Try `view 4` to see the source of any numbered item in
       the above list.
```

`view foo` must round-trip to exactly `Show.show [1, 2, 3]` — the
resolved `Show.list Show.nat` dictionary is elided and no `give`
keyword is introduced.

``` ucm
scratch/main> view foo

  foo : Text
  foo = show [1, 2, 3]
```

`view Show.list` re-renders the parametric given with the `given`
keyword and its `Show a =>` premise intact, and `view Show` renders
the declaration with the `class` keyword and record-field syntax.

``` ucm
scratch/main> view Show.list

  given Show.list : Show a => Show [a] =
    Show cases
      []     -> "[]"
      x +: _ -> "[" Text.++ show x Text.++ ", ..]"

scratch/main> view Show

  class Show a = { show : a -> Text }
```
