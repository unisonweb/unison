# Tests for `move`

```ucm:hide
scratch/main> builtins.merge
```

## Happy Path - namespace, term, and type

Create a term, type, and namespace with history

```unison
Foo = 2
unique type Foo = Foo
Foo.termInA = 1
unique type Foo.T = T
```

```ucm
scratch/main> add
```

```unison
Foo.termInA = 2
unique type Foo.T = T1 | T2
```

```ucm
scratch/main> update
```

Should be able to move the term, type, and namespace, including its types, terms, and sub-namespaces.

```ucm
scratch/main> move Foo Bar
scratch/main> ls
scratch/main> ls Bar
scratch/main> history Bar
```

## Happy Path - Just term

```unison
bonk = 5
```

```ucm
.z> builtins.merge
.z> add
.z> move bonk zonk
.z> ls
```

## Happy Path - Just namespace

```unison
bonk.zonk = 5
```

```ucm
.a> builtins.merge
.a> add
.a> move bonk zonk
.a> ls
.a> view zonk.zonk
```

## Sad Path - No term, type, or namespace named src

```ucm:error
scratch/main> move doesntexist foo
```
