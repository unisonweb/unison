```ucm:hide
.> builtins.merge lib.builtin
```

```unison
unique type Foo = Bar Nat
```

```ucm
.> add
.> delete.term Foo.Bar
```

Now we've set up a situation where the original constructor missing.

```unison
unique type Foo = Bar Nat Nat
```

```ucm:error
.> view Foo
.> update
```
