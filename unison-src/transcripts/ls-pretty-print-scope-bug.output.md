```unison
unique type Foo = Foo
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo

```
```ucm
  ☝️  The namespace .a.b is empty.

.a.b> add

  ⍟ I've added these definitions:
  
    unique type Foo

.> fork .a.b .c.d.f

  Done.

  ☝️  The namespace .c.g.f is empty.

```
```unison
unique type Foo = Foo
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo

```
```ucm
.c.g.f> add

  ⍟ I've added these definitions:
  
    unique type Foo

```
```unison
foo = .d.f.Foo.Foo
```

```ucm

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
.> delete .c.d.f.Foo

  Done.

```
Once `.c.d.f.Foo` is deleted `.c.foo` should have the type `.a.b.Foo`
when viewed from `.>`, but an unnamed type when viewed from `.c>`,
since referencing `.a.b.Foo` would reference names outside of the
namespace rooted at `.c`.

```ucm
.> ls c

  1. d/  (1 term)
  2. foo (b.Foo)
  3. g/  (1 term, 1 type)

.c> ls

  1. d/  (1 term)
  2. foo (b.Foo)
  3. g/  (1 term, 1 type)

```
