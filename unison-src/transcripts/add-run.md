# add.run

```ucm:hide
.> builtins.merge
```

## happy path
```unison
even : Nat -> Boolean
even x = if x == 0 then true else odd (drop x 1)

odd : Nat -> Boolean
odd x = if x == 0 then false else even (drop x 1)

is2even : 'Boolean
is2even = '(even 2)
```

```ucm:error
.> add.run foo
```

```ucm
.> run is2even
```

```ucm:error
.> add.run is2even
```

```ucm
.> add.run foo.bar.baz
```

```ucm
.> view foo.bar.baz
```

## It continues to work if a dependency is changed

```unison
unique type Foo = Foo | Bar

foo : 'Foo
foo = 'Foo
```

```ucm
.> run foo
```

```unison
unique type Foo = Foo | Bar | Baz
```

```ucm
.> add.run result-foo
.> view Foo
```
