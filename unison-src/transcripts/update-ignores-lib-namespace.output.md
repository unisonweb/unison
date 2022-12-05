`update` / `patch` (anything that a patch) ignores the namespace named "lib" at the location it's applied. This follows
the project organization convention that dependencies are put in "lib"; it's much easier to apply a patch to all of
one's own code if the "lib" namespace is simply ignored.

```unison
foo = 100
lib.foo = 100
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo     : Nat
      lib.foo : Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    foo     : Nat
    lib.foo : Nat

```
```unison
foo = 200
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat
        (The old definition is also named lib.foo. I'll update
        this name too.)

```
```ucm
.> update

  ⍟ I've updated these names to your new definition:
  
    foo : Nat
      (The old definition was also named lib.foo. I updated this
      name too.)

.> names foo

  Term
  Hash:   #9ntnotdp87
  Names:  foo
  
  Tip: Use `names.global` to see more results.

```
