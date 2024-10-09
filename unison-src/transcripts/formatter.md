``` ucm :hide
scratch/main> builtins.mergeio
```

``` unison :hide
{{ # Doc
This is a *doc*!

term link {x}

type link {type   Optional}

}}
x :
  Nat
  -> Nat
x y =
    x   =     1 + 1
    x + y
-- Should keep comments after

-- symbolyDefinition
(<|>) : Nat -> Nat -> (Nat, Nat)
(<|>) a b = (a, b)

symbolyEndOfBlock =
  x = 1
  (+:)


-- Test for a previous regression that added extra brackets.
oneLiner = {{ one liner }}
-- After

-- Before
explicit.doc = {{
# Here's a top-level doc

With a paragraph

Or two
}}
-- After

{{ A doc before an ability }}
ability Thing where
  more  : Nat -> Text -> Nat
  doThing  : Nat -> Int


{{ Ability with single constructor }}
structural ability Ask a where
  ask : {Ask a} a

-- Regression test for: https://github.com/unisonweb/unison/issues/4666
provide : a -> '{Ask a} r -> r
provide a action =
  h = cases
        {ask -> resume} -> handle resume a with h
        {r} -> r
  handle !action with h

{{
A Doc before a type
}}
structural type Optional   a = More Text
  | Some
  | Other   a
  | None Nat

{{ A doc before a type with no type-vars }}
type Two = One Nat | Two Text

-- Regression for https://github.com/unisonweb/unison/issues/4669

multilineBold = {{

**This paragraph is really really really really really long and spans multiple lines
with a strike-through block**

_This paragraph is really really really really really long and spans multiple lines
with a strike-through block_

~This paragraph is really really really really really long and spans multiple lines
with a strike-through block~

}}
```

``` ucm
scratch/main> debug.format
```

Formatter should leave things alone if the file doesn't typecheck.

``` unison :error
brokenDoc = {{ hello }} + 1
```

``` ucm
scratch/main> debug.format
```
