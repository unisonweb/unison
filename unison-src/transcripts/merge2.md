# The `merge` command

The `merge` command merges together two branches in the same project: the current branch (unspecificed), and the target
branch. For example, to merge `topic` into `main`, switch to `main` and run `merge topic`. Let's see a simple,
unconflicted merge in action, wherein Alice (us) and Bob (them) have added different terms.

## Basic merge

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm
project/main> branch alice
```

```unison
foo : Text
foo = "alices foo"
```

```ucm
project/alice> add
project/main> branch bob
```

```unison
bar : Text
bar = "bobs bar"
```

```ucm
project/bob> add
project/alice> merge2 /bob
project/alice> view foo bar
```

```ucm:hide
.> project.delete project
```

## Update propagation

Updates are propagated. In this example, Alice updates `foo`, and Bob adds a new dependent `bar` of (the old) `foo`.
When Bob's branch is merged into Alice's, her update to `foo` is propagated to `bar`.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```unison
foo : Text
foo = "old foo"
```

```ucm
project/main> add
project/main> branch alice
```

```unison
foo : Text
foo = "new foo"
```

```ucm
project/alice> update
project/main> branch bob
```

```unison
bar : Text
bar = foo ++ foo
```

```ucm
project/bob> add
project/alice> merge2 /bob
project/alice> view foo bar
```

```ucm:hide
.> project.delete project
```

## Update propagation with common dependent

Different hashes don't necessarily imply an update. In this example, Alice and Bob both update different dependencies
`bar` and `baz` of a common dependent `foo`, so their `foo`s have different hashes. However, we can merge these changes
together just fine, resulting in a `foo` that incorporates both updates.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```unison
foo : Text
foo = "foo" ++ bar ++ baz

bar : Text
bar = "old bar"

baz : Text
baz = "old baz"
```

```ucm
project/main> add
project/main> branch alice
```

```unison
bar : Text
bar = "alices bar"
```

```ucm
project/alice> update
project/main> branch bob
```

```unison
baz : Text
baz = "bobs baz"
```

```ucm
project/bob> update
project/alice> merge2 /bob
project/alice> view foo bar baz
```

```ucm:hide
.> project.delete project
```

## Typechecking failure

Alice's update may fail to typecheck when propagating to Bob's dependents.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```unison
foo : Text
foo = "foo"
```

```ucm
project/main> add
project/main> branch alice
```

```unison
foo : Nat
foo = 100
```

```ucm
project/alice> update
project/main> branch bob
```

```unison
bar : Text
bar = foo ++ foo
```

```ucm:error
project/bob> update
project/alice> merge2 /bob
```

```ucm:hide
.> project.delete project
```

## Simple term conflict

Alice and Bob may disagree about the definition of a term. In this case, the conflicted term and all of its dependents
are given to the user to resolve. The unconflicted parts of a merge (and any merge with conflicts in general) are put
into the namespace.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```unison
foo : Text
foo = "old foo"

bar : Text
bar = "old bar"
```

```ucm
project/main> add
project/main> branch alice
```

```unison
foo : Text
foo = "alices foo"

bar : Text
bar = "alices bar"
```

```ucm
project/alice> update
project/main> branch bob
```

```unison
foo : Text
foo = "bobs foo"

baz : Text
baz = "bobs baz"
```

```ucm:error
project/bob> update
project/alice> merge2 /bob
```

```ucm
project/merge-bob-into-alice> view bar baz
```

```ucm:hide
.> project.delete project
```

## Simple type conflict

Ditto for types; if the hashes don't match, it's a conflict. In this example, Alice and Bob do different things to the
same constructor. However, any explicit changes to the same type will result in a conflict, including changes that could
concievably be merged (e.g. Alice and Bob both add a new constructor, or edit different constructors).

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```unison
unique type Foo = MkFoo Nat
```

```ucm
project/main> add
project/main> branch alice
```

```unison
unique type Foo = MkFoo Nat Nat
```

```ucm
project/alice> update
project/main> branch bob
```

```unison
unique type Foo = MkFoo Nat Text
```

```ucm:error
project/bob> update
project/alice> merge2 /bob
```

```ucm:hide
.> project.delete project
```

## Term conflict with a constructor

In this example, Alice updates a type, while Bob "updates" one of the constructors (by changing it to a term), and adds
back a name for the constructor somewhere else. Bob didn't actually update the type itself, but there is nonetheless
a conflict between Alice's type (due to one of its constructors) and Bob's term.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```unison
unique type Foo
  = MkFooOne Nat
  | MkFooTwo Nat Nat
```

```ucm
project/main> add
project/main> branch alice
```

```unison
unique type Foo
  = MkFooOne Nat Text
  | MkFooTwo Nat Nat
```

```ucm
project/alice> update
project/main> branch bob
```

```unison
unique type Foo
  = MkFooOne Nat
  | MkFooTwoRenamed Nat Nat

Foo.MkFooTwo : Text
Foo.MkFooTwo = "hello"
```

```ucm:error
project/bob> update
project/alice> merge2 /bob
```

```ucm:hide
.> project.delete project
```


## Precondition violations

Let's see a number of merge precondition violations. These are conditions under which we can't perform a merge, and the
user will have to fix up the namespace(s) manually before attempting to merge again.

### Conflicted aliases

If `foo` and `bar` are aliases in the nearest common ancestor, but not in Alice's branch, then we don't know whether to
update Bob's dependents to Alice's `foo` or Alice's `bar` (and vice-versa).

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```unison
foo : Nat
foo = 100

bar : Nat
bar = 100
```

```ucm
project/main> add
project/main> branch alice
```

```unison
foo : Nat
foo = 200

bar : Nat
bar = 300
```

```ucm
project/alice> update
project/main> branch bob
```

```unison
baz : Text
baz = "baz"
```

```ucm:error
project/bob> add
project/alice> merge2 /bob
```

```ucm:hide
.> project.delete project
```

### Conflict involving builtin

We don't have a way of rendering a builtin in a scratch file, where users resolve merge conflicts. Thus, if there is a
conflict involving a builtin, we can't perform a merge.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm
project/main> branch topic
project/main> alias.type builtin.Nat MyNat
project/topic>
```

```unison
unique type MyNat = MyNat Nat
```

```ucm:error
project/topic> add
project/main> merge2 /topic
```

```ucm:hide
.> project.delete project
```

# fast-forward

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

### Constructor alias

```ucm:hide
.> project.create-empty proj
proj/main> builtins.mergeio
```

```unison
unique type Foo = Bar
```

```ucm
proj/main> add
proj/main> branch topic
proj/topic> alias.term Foo.Bar Foo.Alias
```

```ucm:error
proj/main> merge2 /topic
```

```ucm:hide
.> project.delete proj
```

### Missing constructor

```ucm:hide
.> project.create-empty proj
proj/main> builtins.mergeio
```

```unison
unique type Foo = Bar | Baz
```

```ucm
proj/main> add
proj/main> branch topic
proj/topic> delete.term Foo.Bar
```

```ucm:error
proj/main> merge2 /topic
```

```ucm:hide
.> project.delete proj
```

### Nested decl alias

```ucm:hide
.> project.create-empty proj
proj/main> builtins.mergeio
```

```unison
structural type Foo = FooCon
```

```ucm
proj/main> add
proj/main> branch topic
```

```unison
structural type Foo.Bar = BarCon
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

### Stray constructor alias

```ucm:hide
.> project.create-empty proj
proj/main> builtins.mergeio
```

```unison
unique type Foo = Bar
```

```ucm
proj/main> add
proj/main> branch topic
proj/topic> alias.term Foo.Bar Stray
```

```ucm:error
proj/main> merge2 /topic
```

```ucm:hide
.> project.delete proj
```
