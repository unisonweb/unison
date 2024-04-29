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
project/main> branch topic
project/main> alias.type builtin.Nat MyNat
project/topic>
```

```unison
unique type MyNat = MyNat Nat
```

```ucm:error
project/topic> add
project/main> merge /topic
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
project/main> branch topic
```

```unison
unique type Foo = Bar
```

```ucm:error
project/topic> add
project/topic> alias.term Foo.Bar Foo.some.other.Alias
project/main> merge /topic
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
project/main> branch topic
```

```unison
unique type Foo = Bar
```

```ucm:error
project/topic> add
project/topic> delete.term Foo.Bar
project/main> merge /topic
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
project/main> branch topic
```

```unison
structural type A = B Nat | C Nat Nat
structural type A.inner.X = Y Nat | Z Nat Nat
```

```ucm:error
project/topic> add
project/main> merge /topic
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
project/main> branch topic
```

```unison
unique type Foo = Bar
```

```ucm:error
project/topic> add
project/topic> alias.term Foo.Bar AliasOutsideFooNamespace
project/main> merge /topic
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
project/main> branch topic
```

```unison
lib.foo : Nat
lib.foo = 1
```

```ucm:error
project/topic> add
project/main> merge /topic
```

```ucm:hide
.> project.delete project
```
