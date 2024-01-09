# Type modifiers are optional, `unique` is the default.

```ucm:hide
.> builtins.merge
```

Types and abilities may be prefixed with either `unique` or `structural`. When left unspecified, `unique` is assumed.

```unison
type Abc = Abc
unique type Def = Def
structural type Ghi = Ghi

ability MyAbility where const : a
unique ability MyAbilityU where const : a
structural ability MyAbilityS where const : a
```
