# Scenario: breaking a given leaves dependent terms valid

Breaking a given leaves dependent terms valid (hashed against the
old) but breaks new code.

Why: an elaborated term hashes against the resolved dictionary's
hash via the existing `App` machinery, so the dependent term retains
its old resolution after the given is removed (content addressing).
New code that *would* re-elaborate against the missing given fails
fresh.

``` ucm :hide
> builtins.merge
```

Define `Show`, the given `Show.nat`, and a consumer `foo`. The
consumer references the given by name.

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

Record `foo`'s and `Show.nat`'s hashes; `foo`'s body references
`Show.nat`'s hash via the elaborated `App`.

``` ucm
> names foo

  'foo':
  Hash          Kind   Names
  #9qoq16jvg3   Term   foo

> names Show.nat

  'Show.nat':
  Hash          Kind   Names
  #06rjmokfst   Term   Show.nat
```

Delete `Show.nat`. The name binding goes away; the underlying term
referent remains in storage, so any term that already hashed against
it is unaffected (this is the entire point of content addressing).

``` ucm
> delete.term.force Show.nat

  I deleted these terms:

    1. Show.nat

  Tip: You can use `undo` or use a hash from `reflog` to undo
       this change.
```

`foo` is still valid: it was hashed against the old `Show.nat`'s
referent, which the codebase still keeps. `view` and `names` still
work.

``` ucm
> names foo

  'foo':
  Hash          Kind   Names
  #9qoq16jvg3   Term   foo

> view foo

  foo : Nat -> Text
  foo n =
    (Show f) = #06rjmokfst
    f n
```

Now try to add a new definition `bar` that re-elaborates against the
(now missing) `Show.nat`. The reference is unbound; typechecking
fails.

``` unison :error
bar : Nat -> Text
bar n = match Show.nat with Show.Show f -> f n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I couldn't figure out what Show.nat refers to here:

      2 | bar n = match Show.nat with Show.Show f -> f n

  I think its type should be:

      Show Nat

  Some common causes of this error include:
    * Your current namespace is too deep to contain the
      definition in its subtree
    * The definition is part of a library which hasn't been
      added to this project
    * You have a typo in the name
```
