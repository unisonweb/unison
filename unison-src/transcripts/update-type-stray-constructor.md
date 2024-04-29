```ucm:hide
.> builtins.merge lib.builtin
```

```unison
unique type Foo = Bar Nat
```

```ucm
.> add
.> move.term Foo.Bar Stray.Bar
```

Now we've set up a situation where the constructor is not where it's supposed to be; it's somewhere else.

```unison
unique type Foo = Bar Nat Nat
```

Note that the constructor name shown here (implied to be called `Foo.Stray.Bar`) doesn't really exist, it's just showing up due to a pretty-printer bug.

```ucm:error
.> view Foo
.> update
```
