# Ability-row aliases

```ucm:hide
> builtins.mergeio
```

```unison
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

```ucm
> add
> view action
```

Stored rows must expand as well as file-local rows. Repeated abilities
have set semantics.

```unison
action2 : '{Effects} Text
action2 = do action()
```

```ucm
> add
```
