# Type modifiers are optional, `unique` shoud be used as default

```ucm:hide
.> builtins.merge
```

Types do not need to be prefixed with either `unique` or `structural`:

```unison
type Abc = Abc 
```

Abilities do not need to be prefixed with either `unique` or `structural`:

```unison
ability MyAbility where const : a 
```

There should be no errors when `unique` or `structural` is provided:

```unison
structural type AbcS = AbcS
unique type AbcU = AbcU
structural ability MyAbilityS where const : a 
unique ability MyAbilityU where const : a 
```