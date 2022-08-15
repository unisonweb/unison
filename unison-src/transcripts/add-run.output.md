# add.run

## happy path
```unison
even : Nat -> Boolean
even x = if x == 0 then true else odd (drop x 1)

odd : Nat -> Boolean
odd x = if x == 0 then false else even (drop x 1)

is2even : 'Boolean
is2even = '(even 2)
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      even    : Nat -> Boolean
      is2even : 'Boolean
      odd     : Nat -> Boolean

```
```ucm
.> add.run foo

  ⚠️
  
  There is no previous evaluation to save. Use `run` to evaluate
  something before attempting to save it.

```
```ucm
.> run is2even

  true

```
```ucm
.> add.run is2even

  ⚠️
  
  Cannot save the last run result into `is2even` because that
  name conflicts with a name in the scratch file.

```
```ucm
.> add.run foo.bar.baz

```
```ucm
.> view foo.bar.baz

  foo.bar.baz : Boolean
  foo.bar.baz = true

```
## It continues to work if a dependency is changed

```unison
unique type Foo = Foo | Bar

foo : 'Foo
foo = 'Foo
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo
      foo : 'Foo

```
```ucm
.> run foo

  Foo

```
```unison
unique type Foo = Foo | Bar | Baz
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo

```
```ucm
.> add.run result-foo

.> view Foo

  unique type Foo = Foo | Bar

```
