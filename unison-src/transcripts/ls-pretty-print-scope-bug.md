```unison
unique type Foo = Foo
```

```ucm
scratch/main a.b> add
scratch/main> fork .a.b .c.d.f
.c.g.f>
```

```unison
unique type Foo = Foo
```

```ucm
scratch/main c.g.f> add
.c>
```

```unison
foo = .d.f.Foo.Foo
```

```ucm
scratch/main c> add
```

At this point we have:
`.a.b.Foo`
`.c.d.f.Foo` which is equal to `.a.b.Foo`
`.c.g.f.Foo` which is distinct from the other `Foo` types

```ucm
scratch/main> delete .c.d.f.Foo
```
Once `.c.d.f.Foo` is deleted `.c.foo` should have the type `.a.b.Foo`
when viewed from `scratch/main>`, but an unnamed type when viewed from `.c>`,
since referencing `.a.b.Foo` would reference names outside of the
namespace rooted at `.c`.

```ucm
scratch/main> ls c
scratch/main c> ls
```
