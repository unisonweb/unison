```unison
foo = 5
foo.bar = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo     : ##Nat
      foo.bar : ##Nat

```
```ucm
p0/main> add

  ⍟ I've added these definitions:
  
    foo     : ##Nat
    foo.bar : ##Nat

```
```unison
bonk = 5
donk.bonk = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bonk      : ##Nat
        (also named foo)
      donk.bonk : ##Nat
        (also named foo.bar)

```
```ucm
p1/main> add

  ⍟ I've added these definitions:
  
    bonk      : ##Nat
    donk.bonk : ##Nat

p1/main> fork p0/main: zzz

  Done.

p1/main> find zzz

  1. zzz.foo : ##Nat
  2. zzz.foo.bar : ##Nat
  

p1/main> fork p0/main:foo yyy

```

```ucm
p1/main> addp1/main> fork p0/main: zzzp1/main> find zzzp1/main> fork p0/main:foo yyyp1/main> find yyyp0/main> fork p1/main: p0/main:p1p0/main> ls p1p0/main> ls p1.zzzp0/main> ls p1.yyy
```


🛑

The transcript failed due to an error in the stanza above. The error is:

<none>:1:9:
  |
1 | p0/main:foo
  |         ^
Expected an absolute path but found a relative path. Try adding a leading '.' to your path

