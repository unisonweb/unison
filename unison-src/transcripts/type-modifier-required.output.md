# Type modifiers are required

Types needs to be prefixed with either `unique` or `structural`:

```unison
type Abc = Abc 
```

```ucm

  I expected to see `structural` or `unique` at the start of
  this line:
  
      1 | type Abc = Abc 
  
  Learn more about when to use `structural` vs `unique` in the
  Unison Docs:
  https://www.unison-lang.org/learn/language-reference/unique-types/

```
Abilities needs to be prefixed with either `unique` or `structural`:

```unison
ability MyAbility where const : a 
```

```ucm

  I expected to see `structural` or `unique` at the start of
  this line:
  
      1 | ability MyAbility where const : a 
  
  Learn more about when to use `structural` vs `unique` in the
  Unison Docs:
  https://www.unison-lang.org/learn/language-reference/unique-types/

```
There should be no errors when `unique` or `structural` is provided:

```unison
structural type AbcS = AbcS
unique type AbcU = AbcU
structural ability MyAbilityS where const : a 
unique ability MyAbilityU where const : a 
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type AbcS
        (also named builtin.Unit)
      unique type AbcU
      structural ability MyAbilityS
      unique ability MyAbilityU

```
