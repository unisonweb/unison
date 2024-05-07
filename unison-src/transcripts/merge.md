# The `merge` command

The `merge` command merges together two branches in the same project: the current branch (unspecificed), and the target
branch. For example, to merge `topic` into `main`, switch to `main` and run `merge topic`.

Let's see a simple unconflicted merge in action: Alice (us) and Bob (them) add different terms. The merged result
contains both additions.

## Basic merge: two unconflicted adds

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
project/alice> merge /bob
project/alice> view foo bar
```

```ucm:hide
.> project.delete project
```

## Basic merge: two equal adds

If Alice and Bob also happen to add the same thing, that's not a conflict.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm
project/main> branch alice
```

```unison
foo : Text
foo = "alice and bobs foo"
```

```ucm
project/alice> add
project/main> branch bob
```

```unison
foo : Text
foo = "alice and bobs foo"

bar : Text
bar = "bobs bar"
```

```ucm
project/bob> add
project/alice> merge /bob
project/alice> view foo bar
```

```ucm:hide
.> project.delete project
```

## Simple update propagation

Updates that occur in one branch are propagated to the other. In this example, Alice updates `foo`, while Bob adds a new
dependent `bar` of (the old) `foo`. When Bob's branch is merged into Alice's, her update to `foo` is propagated to his
`bar`.

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
project/alice> merge /bob
project/alice> view foo bar
```

```ucm:hide
.> project.delete project
```

## Update propagation with common dependent

We classify something as an update if its "syntactic hash" - not its normal Unison hash - differs. This allows us to
cleanly merge unconflicted updates that were individually propagated to a common dependent.

Let's see an example. We have `foo`, which depends on `bar` and `baz`. Alice updates `bar` (propagating to `foo`),
and Bob updates `baz` (propagating to `foo`). When we merge their updates, both updates will be reflected in the final
`foo`.

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
project/alice> merge /bob
project/alice> view foo bar baz
```

```ucm:hide
.> project.delete project
```

## Propagating an update to an update

It's also (of course) possible for Alice's update to propagate to one of Bob's updates. In this example, `foo` depends
on `bar` which depends on `baz`. Alice updates `baz`, propagating to `bar` and `foo`, while Bob updates `bar` (to
something that still depends on `foo`), propagating to `baz`. The merged result will have Alice's update to `foo`
incorporated into Bob's updated `bar`, and both updates will propagate to `baz`.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```unison
foo : Text
foo = "old foo" ++ bar

bar : Text
bar = "old bar" ++ baz

baz : Text
baz = "old baz"
```

```ucm
project/main> add
project/main> branch alice
```

```unison
baz : Text
baz = "alices baz"
```

```ucm
project/alice> update
project/main> branch bob
```

```unison
bar : Text
bar = "bobs bar" ++ baz
```

```ucm
project/bob> update
project/alice> merge /bob
project/alice> view foo bar baz
```

```ucm:hide
.> project.delete project
```

## Update + delete isn't (currently) a conflict

We don't (yet?) consider update+delete a conflict; in this case, the delete is just ignored.

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
foo = "alices foo"
```

```ucm
project/alice> update
project/main> branch bob
project/bob> delete.term foo
project/alice> merge /bob
project/alice> view foo
```

```ucm:hide
.> project.delete project
```

## Library dependencies always merge cleanly

Library dependencies can't cause conflicts – they are just unioned together. If two library dependencies have the same
name but different hashes, then the merge algorithm makes up two fresh names.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm
project/main> branch alice
```

```unison
lib.alice.foo : Nat
lib.alice.foo = 17

lib.bothSame.bar : Nat
lib.bothSame.bar = 18

lib.bothDifferent.baz : Nat
lib.bothDifferent.baz = 19
```

```ucm
project/alice> add
project/main> branch bob
```

```unison
lib.bob.foo : Nat
lib.bob.foo = 20

lib.bothSame.bar : Nat
lib.bothSame.bar = 18

lib.bothDifferent.baz : Nat
lib.bothDifferent.baz = 21
```

```ucm
project/bob> add
project/alice> merge bob
project/alice> view foo bar baz
```

```ucm:hide
.> project.delete project
```

## No-op merge (Bob = Alice)

If Bob is equals Alice, then merging Bob into Alice looks like this.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm
project/main> branch alice
project/main> branch bob
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

## No-op merge (Bob < Alice)

If Bob is behind Alice, then merging Bob into Alice looks like this.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm
project/main> branch alice
project/main> branch bob
```

```unison
foo : Text
foo = "foo"
```

```ucm
project/alice> add
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

## Fast-forward merge (Bob > Alice)

If Bob is ahead of Alice, then merging Bob into Alice looks like this.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm
project/main> branch alice
project/main> branch bob
```

```unison
foo : Text
foo = "foo"
```

```ucm
project/bob> add
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

## Merge failure: someone deleted something

If either Alice or Bob delete something, so long as the other person didn't update it (in which case we ignore the
delete, as explained above), then the delete goes through. This can cause merge failures due to out-of-scope
identifiers. The user may have to do some digging around to find what the deleted name used to refer to.

In this example, Alice deletes `foo`, while Bob adds a new dependent of `foo`.

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
project/alice> delete.term foo
project/main> branch bob
```

```unison
bar : Text
bar = foo ++ foo
```

```ucm:error
project/bob> add
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

## Merge failure: type error

It may be possible to cleanly merge Alice's and Bob's changes together, yet the resulting namespace doesn't typecheck.

In this example, Alice updates a `Text` to a `Nat`, while Bob adds a new dependent of the `Text`. Upon merging,
propagating Alice's update to Bob's dependent fails to typecheck.

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
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

## Merge failure: simple term conflict

Alice and Bob may disagree about the definition of a term. In this case, the conflicted term and all of its dependents
are presented to the user to resolve.

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

qux : Text
qux = "alices qux depends on alices foo" ++ foo
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
project/alice> merge /bob
```

```ucm
project/merge-bob-into-alice> view bar baz
```

```ucm:hide
.> project.delete project
```

## Merge failure: simple type conflict

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
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

## Merge failure: type-update + constructor-rename conflict

Renaming a constructor is modeled as an update, so if Alice updates a type and Bob renames one of its constructors but
doesn't change its hash, that's still a conflict.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```unison
unique type Foo = Baz Nat | Qux Text
```

```ucm
project/main> add
project/main> branch alice
```

```unison
unique type Foo = Baz Nat Nat | Qux Text
```

```ucm
project/alice> update
project/main> branch bob
```

```unison
unique type Foo = Baz Nat | BobQux Text
```

```ucm:error
project/bob> update
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

## Merge failure: constructor-rename conflict

Another example demonstrating that constructor "renames" (add + delete) are modeled as updates.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```unison
unique type Foo = Baz Nat | Qux Text
```

```ucm
project/main> add
project/main> branch alice
project/alice> move.term Foo.Baz Foo.Alice
project/main> branch bob
project/bob> move.term Foo.Qux Foo.Bob
```

```ucm:error
project/alice> merge bob
```

```ucm:hide
.> project.delete project
```

## Merge failure: non-constsructor/constructor conflict

It's possible for a term conflict to involve a constructor on one side and a not-a-constructor on the other.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm
project/main> branch alice
```

```unison
my.cool.thing : Nat
my.cool.thing = 17
```

```ucm
project/alice> add
project/main> branch bob
```

```unison
unique ability my.cool where
  thing : Nat -> Nat
```

```ucm
project/bob> add
```

```ucm:error
project/alice> merge bob
```

```ucm:hide
.> project.delete project
```

## Merge algorithm quirk: the "not-conflict conflict"

Since a conflicted type declaration must bring into the scratch file (for conflict resolution) all of its constructors,
it's possible that an unconflicted thing gets ultimately presented as a conflict.

In this example, Alice and Bob have a disagreement about what the type "Foo" refers to, so their constructors
("Foo.Alice" and "Foo.Bob") are brought into the scratch file.

But Bob updated "Foo.Bob", and Alice didn't touch it! Nonetheless, her untouched "Foo.Bar" term is considered in
conflict with Bob's.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```unison
Foo.Bar : Nat
Foo.Bar = 17
```

```ucm
project/main> add
project/main> branch alice
```

```unison
unique type Foo = Alice Nat
```

```ucm
project/alice> add
project/main> branch bob
project/bob> delete.term Foo.Bar
```

```unison
unique type Foo = Bar Nat Nat
```

```ucm
project/bob> add
```

```ucm:error
project/alice> merge bob
```

```ucm:hide
.> project.delete project
```

Here's a more complicated example that demonstrates the same idea.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

In the LCA, we have a type with two constructors, and some term.

```unison
unique type Foo
  = Bar.Baz Nat
  | Bar.Qux Nat Nat

Foo.Bar.Hello : Nat
Foo.Bar.Hello = 17
```

```ucm
project/main> add
```

Alice deletes this type entirely, and repurposes its constructor names for other terms. She also updates the term.

```ucm
project/main> branch alice
project/alice> delete.type Foo
project/alice> delete.term Foo.Bar.Baz
project/alice> delete.term Foo.Bar.Qux
```

```unison
Foo.Bar.Baz : Nat
Foo.Bar.Baz = 100

Foo.Bar.Qux : Nat
Foo.Bar.Qux = 200

Foo.Bar.Hello : Nat
Foo.Bar.Hello = 18
```

```ucm
project/alice> update
```

Bob, meanwhile, first deletes the term, then sort of deletes the type and re-adds it under another name, but one
constructor's name doesn't actually change. The other constructor takes the name of the deleted term.

```ucm
project/main> branch bob
project/bob> delete.term Foo.Bar.Hello
project/bob> move.type Foo Foo.Bar
project/bob> move.term Foo.Bar.Qux Foo.Bar.Hello
```

At this point, Bob and alice have both updated the name "Foo.Bar.Hello" in different ways, so that's a conflict.
Therefore, Bob's entire type ("Foo.Bar" with constructors "Foo.Bar.Baz" and "Foo.Bar.Hello") gets rendered into the
scratch file.

Notably, Alice's "unconflicted" update on the name "Foo.Bar.Baz" (because she changed its hash and Bob didn't touch it)
is nonetheless considered conflicted with Bob's "Foo.Bar.Baz".

```ucm:error
project/alice> merge bob
```

```ucm:hide
.> project.delete project
```

## Merge algorithm quirk: add/add unique types

Currently, two unique types created by Alice and Bob will be considered in conflict, even if they "look the same".
The result may be confusing to a user – a file containing two identical-looking copies of a unique type is rendered,
which is a parse error.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm
project/main> branch alice
```

```unison
unique type Foo = Bar

alice : Foo -> Nat
alice _ = 18
```

```ucm
project/alice> add
project/main> branch bob
```

```unison
unique type Foo = Bar

bob : Foo -> Nat
bob _ = 19
```

```ucm
project/bob> add
```

```ucm:error
project/alice> merge bob
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
project/alice> merge /bob
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
project/main> branch alice
project/alice> alias.type builtin.Nat MyNat
project/main> branch bob
```

```unison
unique type MyNat = MyNat Nat
```

```ucm
project/bob> add
```

```ucm:error
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

### Constructor alias

Each naming of a decl may not have more than one name for each constructor underneath the decl's namespace.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm
project/main> branch alice
```

```unison
unique type Foo = Bar
```

```ucm
project/alice> add
project/alice> alias.term Foo.Bar Foo.some.other.Alias
project/main> branch bob
```

```unison
bob : Nat
bob = 100
```

```ucm
project/bob> add
```

```ucm:error
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

### Missing constructor name

Each naming of a decl may not have zero names for a constructor underneath the decl's namespace.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm
project/main> branch alice
```

```unison
unique type Foo = Bar
```

```ucm
project/alice> add
project/alice> delete.term Foo.Bar
project/main> branch /bob
```

```unison
bob : Nat
bob = 100
```

```ucm
project/bob> add
```

```ucm:error
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

### Nested decl alias

Decl aliases must be disjoint in a namespace: one cannot contain another.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm
project/main> branch alice
```

```unison
structural type A = B Nat | C Nat Nat
structural type A.inner.X = Y Nat | Z Nat Nat
```

```ucm
project/alice> add
project/main> branch bob
```

```unison
bob : Nat
bob = 100
```

```ucm
project/bob> add
```

```ucm:error
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

### Stray constructor alias

Each naming of a constructor must be underneath its decl's namespace.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm
project/main> branch alice
```

```unison
unique type Foo = Bar
```

```ucm
project/alice> add
project/alice> alias.term Foo.Bar AliasOutsideFooNamespace
project/main> branch bob
```

```unison
bob : Nat
bob = 101
```

```ucm
project/bob> add
```

```ucm:error
project/alice> merge bob
```

```ucm:hide
.> project.delete project
```

### Term or type in `lib`

A bit of an odd one, but we have a convention that `lib` contains only namespaces, which are dependencies. Thus, the
`lib` namespace can always merge cleanly, so long as there aren't stray terms or types in it.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm
project/main> branch alice
```

```unison
lib.foo : Nat
lib.foo = 1
```

```ucm
project/alice> add
project/main> branch bob
```

```unison
bob : Nat
bob = 100
```

```ucm
project/bob> add
```

```ucm:error
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```
