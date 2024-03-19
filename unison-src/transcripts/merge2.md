# tests for the new merge command

## Basic fast-forward merge

```ucm:hide
.> project.create-empty proj
proj/main> builtins.mergeio
```

```unison
foo : Nat
foo = 1
```

```ucm
proj/main> add
proj/main> branch topic
proj/topic> 
```

```unison
bar : Nat
bar = foo + 1
```

```ucm
proj/topic> add
proj/main> merge2 /topic
proj/main> view bar
```

```ucm:hide
.> project.delete proj
```

## Add/Add agree

```ucm:hide
.> project.create-empty proj
proj/main> builtins.mergeio
proj/main> branch topic
proj/topic>
```

```unison
foo : Nat
foo = 1
```

```ucm
proj/topic> add
proj/main> 
```

```unison
foo : Nat
foo = 1

bar : Nat
bar = 2
```

```ucm
proj/main> add
proj/main> merge2 /topic
```

```ucm:hide
.> project.delete proj
```

## Add/Add conflict

```ucm:hide
.> project.create-empty proj
proj/main> builtins.mergeio
proj/main> branch topic
proj/topic>
```

```unison
foo : Nat
foo = 1
```

```ucm
proj/topic> add
proj/main> 
```

```unison
foo : Nat
foo = 4
```

```ucm:error
proj/main> add
proj/main> merge2 /topic
```

```ucm:hide
.> project.delete proj
```

## Update/Update conflict

```ucm:hide
.> project.create-empty proj
proj/main> builtins.mergeio
```

```unison
foo : Nat
foo = 1
```

```ucm
proj/main> add
proj/main> branch topic
```

```unison
foo : Nat
foo = 2
```

```ucm
proj/topic> update
proj/main>
```

```unison
foo : Nat
foo = 3
```

```ucm
proj/main> update
```

```ucm:error
proj/main> merge2 /topic
```

```ucm:hide
.> project.delete proj
```

## Update/Update agree

```ucm:hide
.> project.create-empty proj
proj/main> builtins.mergeio
```

```unison
foo : Nat
foo = 1
```

```ucm
proj/main> add
proj/main> branch topic
```

```unison
foo : Nat
foo = 2
```

```ucm
proj/topic> update
proj/main>
```

```unison
foo : Nat
foo = 2

bar : Nat
bar = 3
```

```ucm
proj/main> update
```

```ucm
proj/main> merge2 /topic
```

```ucm:hide
.> project.delete proj
```

## Update/Delete conflict

We don't consider these, so this transcript is capturing our
ignorance.

```ucm:hide
.> project.create-empty proj
proj/main> builtins.mergeio
```

```unison
foo : Nat
foo = 1
```

```ucm
proj/main> add
proj/main> branch topic
proj/topic> delete.term foo
```

```unison
foo : Nat
foo = 2
```

```ucm
proj/main> update
```

We silently ignore the delete

```ucm
proj/main> merge2 /topic
proj/main> view foo
```

```ucm:hide
.> project.delete proj
```

## Alice deletes x bob adds y

```ucm:hide
.> project.create-empty proj
proj/main> builtins.mergeio
```

```unison
foo : Nat
foo = 1
```

```ucm
proj/main> add
proj/main> branch topic
proj/main> delete.term foo
proj/topic>
```

```unison
bar : ()
bar = ()
```

```ucm
proj/topic> add
```

```ucm
proj/main> merge2 /topic
proj/main> ls
```

```ucm:hide
.> project.delete proj
```

## Alice adds x bob deletes y

```ucm:hide
.> project.create-empty proj
proj/main> builtins.mergeio
```

```unison
foo : Nat
foo = 1
```

```ucm
proj/main> add
proj/main> branch topic
proj/topic> delete.term foo
proj/main>
```

```unison
bar : ()
bar = ()
```

```ucm
proj/main> add
```

```ucm
proj/main> merge2 /topic
proj/main> ls
```

```ucm:hide
.> project.delete proj
```

## Alice deletes x bob deletes x

```ucm:hide
.> project.create-empty proj
proj/main> builtins.mergeio
```

```unison
foo : Nat
foo = 1
```

```ucm
proj/main> add
proj/main> branch topic
proj/topic> delete.term foo
proj/main> delete.term foo
```

```ucm
proj/main> merge2 /topic
proj/main> ls
```

```ucm:hide
.> project.delete proj
```

## Altered dependent

```ucm:hide
.> project.create-empty proj
proj/main> builtins.mergeio
```

`foo : Nat` is in the ancestor of `main` and `topic`

```unison
foo : Nat
foo = 1
```

```ucm
proj/main> add
proj/main> branch topic
proj/topic> 
```

`topic` adds a dependent of `foo`

```unison
bar : Nat
bar = foo + 1
```

```ucm
proj/topic> add
proj/main>
```

`main` changes the type of `foo`

```unison
foo : Int
foo = +1
```

```ucm
proj/main> update
```

attempt to merge `topic` into `main`

```ucm:error
proj/main> merge2 /topic
```

```ucm:hide
.> project.delete proj
```

## Precondition violations

### term in lib


```ucm:hide
.> project.create-empty proj
proj/main> builtins.mergeio
```

```unison
lib.foo : Nat
lib.foo = 1
```

```ucm
proj/main> add
proj/main> branch topic
```

```unison
bonk : Nat
bonk = 5
```

```ucm
proj/topic> add
```

```ucm:error
proj/main> merge2 /topic
```

```ucm:hide
.> project.delete proj
```
