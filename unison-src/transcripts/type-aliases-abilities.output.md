# Ability-row aliases

``` ucm :hide
> builtins.mergeio
```

``` unison
structural ability Read where read : Text
structural ability Count where count : Nat

type alias Effects = {Read, Count}
type alias MoreEffects = Effects

action : '{MoreEffects, Read, Count} Text
action = do
  _ = Count.count
  Read.read

handler : Request {Read, Count} Text -> Text
handler = cases
  { Read.read -> k } -> handle k "alias" with handler
  { Count.count -> k } -> handle k 1 with handler
  { result } -> result

> handle action() with handler
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type alias Effects = {Read, Count}
  + type alias MoreEffects = Effects
  + structural ability Count
  + structural ability Read

  + action  : '{MoreEffects, Read, Count} Text
  + handler : Request {Read, Count} Text -> Text

  Run `update` to apply these changes to your codebase.

    18 | > handle action() with handler
           ⧩
           "alias"
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view action

  action : '{MoreEffects, Read, Count} Text
  action = do
    _ = count
    Read.read
```

Stored rows must expand as well as file-local rows. Repeated abilities
have set semantics.

``` unison
action2 : '{Effects} Text
action2 = do action()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + action2 : '{Effects} Text

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```
