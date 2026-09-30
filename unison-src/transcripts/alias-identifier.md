# `alias` as an identifier

`alias` is contextual — it's only treated specially after `type`. Outside
that position it remains a normal identifier.

```ucm :hide
> builtins.mergeio
```

```unison
alias : Nat -> Nat
alias n = n + 1
```

```ucm
> add
> view alias
```
