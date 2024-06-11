```unison
unique type Foo = Foo
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo

```
```ucm
  ☝️  The namespace .a.b is empty.

.a.b> add

  ⍟ I've added these definitions:
  
    type Foo

scratch/main> fork .a.b .c.d.f

  Done.

  ☝️  The namespace .c.g.f is empty.

```
```unison
unique type Foo = Foo
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo

```
```ucm
.c.g.f> add

  ⍟ I've added these definitions:
  
    type Foo

```
```unison
foo = .d.f.Foo.Foo
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : d.f.Foo

```
```ucm
.c> add

  ⍟ I've added these definitions:
  
    foo : d.f.Foo

```
At this point we have:
`.a.b.Foo`
`.c.d.f.Foo` which is equal to `.a.b.Foo`
`.c.g.f.Foo` which is distinct from the other `Foo` types

```ucm
scratch/main> delete .c.d.f.Foo

  Done.

```
Once `.c.d.f.Foo` is deleted `.c.foo` should have the type `.a.b.Foo`
when viewed from `scratch/main>`, but an unnamed type when viewed from `.c>`,
since referencing `.a.b.Foo` would reference names outside of the
namespace rooted at `.c`.

```ucm
scratch/main> ls c

  nothing to show

```



🛑

The transcript failed due to an error in the stanza above. The error is:


  nothing to show

