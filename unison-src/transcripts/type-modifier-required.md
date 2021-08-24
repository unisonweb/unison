# Type modifiers are required

```ucm:hide
.> builtins.merge
```

Types needs to be prefixed with either `unique` or `structural`:

```unison:error
type Abc = Abc 
```

Abilities needs to be prefixed with either `unique` or `structural`:

```unison:error
ability MyAbility where const : a 
```

There should be no errors when `unique` or `structural` is provided:

```unison
structural type AbcS = AbcSg
unique type AbcU = AbcU
structural ability MyAbilityS where const : a 
unique ability MyAbilityU where const : a 
```