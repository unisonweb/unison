# add.run

## Basic usage

```ucm:hide
.> builtins.merge
```

```unison
even : Nat -> Boolean
even x = if x == 0 then true else odd (drop x 1)

odd : Nat -> Boolean
odd x = if x == 0 then false else even (drop x 1)

is2even : 'Boolean
is2even = '(even 2)
```

it errors if there isn't a previous run

```ucm:error
.> add.run foo
```

```ucm
.> run is2even
```

it errors if the desired result name conflicts with a name in the
unison file
```ucm:error
.> add.run is2even
```

otherwise, the result is successfully persisted
```ucm
.> add.run foo.bar.baz
```

```ucm
.> view foo.bar.baz
```

## It resolves references within the unison file

```unison
z b = b Nat.+ 12
y a b = a Nat.+ b Nat.+ z 10




main : '{IO, Exception} (Nat -> Nat -> Nat)
main _ = y
```

```ucm
.> run main
.> add.run result
```

## It resolves references within the codebase

```unison
inc : Nat -> Nat
inc x = x + 1
```

```ucm
.> add inc
```

```unison
main : '(Nat -> Nat)
main _ x = inc x
```

```ucm
.> run main
.> add.run natfoo
.> view natfoo
```

## It captures scratch file dependencies at run time

```unison
x = 1
y = x + x
main = 'y
```

```ucm
.> run main
```


```unison
x = 50
```

this saves 2 to xres, rather than 100
```ucm
.> add.run xres
.> view xres
```

## It fails with a message if add cannot complete cleanly

```unison
main = '5
```

```ucm:error
.> run main
.> add.run xres
```
