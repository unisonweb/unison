# Type modifiers are optional, `unique` shoud be used as default

Types do not need to be prefixed with either `unique` or `structural`:

```unison
type Abc = Abc 
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Abc

```
Abilities do not need to be prefixed with either `unique` or `structural`:

```unison
ability MyAbility where const : a 
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      ability MyAbility

```
There should be no errors when `unique` or `structural` is provided:

```unison
structural type AbcS = AbcS
unique type AbcU = AbcU
structural ability MyAbilityS where const : a 
unique ability MyAbilityU where const : a 
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type AbcS
        (also named builtin.Unit)
      type AbcU
      structural ability MyAbilityS
      ability MyAbilityU

```
