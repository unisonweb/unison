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

```ucm:hide
project/main> branch alice
```

Alice's adds:
```unison:hide
foo : Text
foo = "alices foo"
```

```ucm:hide
project/alice> add
project/main> branch bob
```

Bob's adds:
```unison:hide
bar : Text
bar = "bobs bar"
```

```ucm:hide
project/bob> add
```
Merge result:
```ucm
project/alice> merge /bob
project/alice> view foo bar
```

```ucm:hide
.> project.delete project
```

## Basic merge: two identical adds

If Alice and Bob also happen to add the same definition, that's not a conflict.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
project/main> branch alice
```

Alice's adds:
```unison:hide
foo : Text
foo = "alice and bobs foo"
```

```ucm:hide
project/alice> add
project/main> branch bob
```

Bob's adds:
```unison:hide
foo : Text
foo = "alice and bobs foo"

bar : Text
bar = "bobs bar"
```
```ucm:hide
project/bob> add
```
Merge result:
```ucm
project/alice> merge /bob
project/alice> view foo bar
```

```ucm:hide
.> project.delete project
```

## Simple update propagation

Updates that occur in one branch are propagated to the other. In this example, Alice updates `foo`, while Bob adds a new dependent `bar` of the original `foo`. When Bob's branch is merged into Alice's, her update to `foo` is propagated to his `bar`.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

Original branch:
```unison:hide
foo : Text
foo = "old foo"
```

```ucm:hide
project/main> add
project/main> branch alice
```

Alice's updates:
```unison:hide
foo : Text
foo = "new foo"
```

```ucm:hide
project/alice> update
project/main> branch bob
```

Bob's adds:
```unison:hide
bar : Text
bar = foo ++ " - " ++ foo
```
```ucm
project/bob> display bar
```

```ucm:hide
project/bob> add
```
Merge result:
```ucm
project/alice> merge /bob
project/alice> view foo bar
project/alice> display bar
```

```ucm:hide
.> project.delete project
```

## Update propagation with common dependent

We classify something as an update if its "syntactic hash"—not its normal Unison hash—differs from the original definition. This allows us to cleanly merge unconflicted updates that were individually propagated to a common dependent.

Let's see an example. We have `foo`, which depends on `bar` and `baz`. Alice updates `bar` (propagating to `foo`), and Bob updates `baz` (propagating to `foo`). When we merge their updates, both updates will be reflected in the final `foo`.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

Original branch:
```unison:hide
foo : Text
foo = "foo" ++ " - " ++ bar ++ " - " ++ baz

bar : Text
bar = "old bar"

baz : Text
baz = "old baz"
```

```ucm:hide
project/main> add
project/main> branch alice
```

Alice's updates:
```unison:hide
bar : Text
bar = "alices bar"
```

```ucm:hide
project/alice> update
```
```ucm
project/alice> display foo
```
```ucm:hide
project/main> branch bob
```

Bob's updates:
```unison:hide
baz : Text
baz = "bobs baz"
```

```ucm:hide
project/bob> update
```
```ucm
project/bob> display foo
```
Merge result:
```ucm
project/alice> merge /bob
project/alice> view foo bar baz
project/alice> display foo
```

```ucm:hide
.> project.delete project
```

## Propagating an update to an update

Of course, it's also possible for Alice's update to propagate to one of Bob's updates. In this example, `foo` depends on `bar` which depends on `baz`. Alice updates `baz`, propagating to `bar` and `foo`, while Bob updates `bar` (to something that still depends on `foo`), propagating to `baz`. The merged result will have Alice's update to `foo` incorporated into Bob's updated `bar`, and both updates will propagate to `baz`.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

Original branch:
```unison:hide
foo : Text
foo = "old foo" ++ " - " ++ bar

bar : Text
bar = "old bar" ++ " - " ++ baz

baz : Text
baz = "old baz"
```

```ucm:hide
project/main> add
```
```ucm
project/main> display foo
```
```ucm:hide
project/main> branch alice
```

Alice's updates:
```unison:hide
baz : Text
baz = "alices baz"
```

```ucm:hide
project/alice> update
```
```ucm
project/alice> display foo
```
```ucm:hide
project/main> branch bob
```

Bob's updates:
```unison:hide
bar : Text
bar = "bobs bar" ++ " - " ++ baz
```

```ucm:hide
project/bob> update
```
```ucm
project/bob> display foo
```

Merge result:
```ucm
project/alice> merge /bob
project/alice> view foo bar baz
project/alice> display foo
```

```ucm:hide
.> project.delete project
```

## Update + delete isn't (currently) a conflict

We don't currently consider "update + delete" a conflict like Git does. In this situation, the delete is just ignored, allowing the update to proceed.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

Original branch:
```unison:hide
foo : Text
foo = "old foo"
```

```ucm:hide
project/main> add
project/main> branch alice
```

Alice's updates:
```unison:hide
foo : Text
foo = "alices foo"
```

```ucm:hide
project/alice> update
project/main> branch bob
```

Bob's changes:
```ucm
project/bob> delete.term foo
```

Merge result:
```ucm
project/alice> merge /bob
project/alice> view foo
```

```ucm:hide
.> project.delete project
```

In a future version, we'd like to give the user a warning at least.

## Library dependencies don't create merge conflicts

Library dependencies don't cause merge conflicts, the library dependencies are just unioned together. If two library dependencies have the same name but different namespace hashes, then the merge algorithm makes up two fresh names.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

Alice's adds:
```ucm:hide
project/main> branch alice
```

```unison:hide
lib.alice.foo : Nat
lib.alice.foo = 17

lib.bothSame.bar : Nat
lib.bothSame.bar = 18

lib.bothDifferent.baz : Nat
lib.bothDifferent.baz = 19
```

```ucm:hide
project/alice> add
project/main> branch bob
```

Bob's adds:
```unison:hide
lib.bob.foo : Nat
lib.bob.foo = 20

lib.bothSame.bar : Nat
lib.bothSame.bar = 18

lib.bothDifferent.baz : Nat
lib.bothDifferent.baz = 21
```

```ucm:hide
project/bob> add
```
Merge result:
```ucm
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

Alice's addition:
```unison:hide
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

Bob's addition:
```unison:hide
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

If either Alice or Bob delete something, so long as the other person didn't update it (in which case we ignore the delete, as explained above), then the delete goes through.

This can cause merge failures due to out-of-scope identifiers, and the user may have to do some digging around to find what the deleted name used to refer to. In a future version, we would emit a \[better\] warning at least.

In this example, Alice deletes `foo`, while Bob adds a new dependent of `foo`.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

Original branch:
```unison:hide
foo : Text
foo = "foo"
```

```ucm:hide
project/main> add
project/main> branch alice
```
Alice's delete:
```ucm
project/alice> delete.term foo
```
```ucm:hide
project/main> branch bob
```

Bob's new code that depends on `foo`:
```unison:hide
bar : Text
bar = foo ++ " - " ++ foo
```

```ucm:error
project/bob> add
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

## Merge failure: type error

It may be Alice's and Bob's changes merge together cleanly in the sense that there's no textual conflicts, yet the resulting namespace doesn't typecheck.

In this example, Alice updates a `Text` to a `Nat`, while Bob adds a new dependent of the `Text`. Upon merging, propagating Alice's update to Bob's dependent causes a typechecking failure.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

Original branch:
```unison:hide
foo : Text
foo = "foo"
```

```ucm:hide
project/main> add
project/main> branch alice
```

Alice's update:
```unison:hide
foo : Nat
foo = 100
```

```ucm:hide
project/alice> update
project/main> branch bob
```

Bob's new definition:
```unison:hide
bar : Text
bar = foo ++ " - " ++ foo
```

```ucm:hide
project/bob> update
```

```ucm:error
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

Original branch:
```unison:hide
foo : Text
foo = "old foo"

bar : Text
bar = "old bar"
```

```ucm:hide
project/main> add
project/main> branch alice
```

Alice's changes:
```unison:hide
foo : Text
foo = "alices foo"

bar : Text
bar = "alices bar"

qux : Text
qux = "alices qux depends on alices foo" ++ foo
```

```ucm:hide
project/alice> update
project/main> branch bob
```

Bob's changes:

```unison:hide
foo : Text
foo = "bobs foo"

baz : Text
baz = "bobs baz"
```

```ucm:hide
project/bob> update
```
```ucm:error
project/alice> merge /bob
```

```ucm
project/merge-bob-into-alice> view bar baz
```

```ucm:hide
.> project.delete project
```

## Merge failure: simple type conflict

Ditto for types; if the hashes don't match, it's a conflict. In this example, Alice and Bob do different things to the same constructor. However, any explicit changes to the same type will result in a conflict, including changes that could concievably be merged (e.g. Alice and Bob both add a new constructor, or edit different constructors).

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

Original branch:
```unison:hide
unique type Foo = MkFoo Nat
```

```ucm:hide
project/main> add
project/main> branch alice
```

Alice's changes:
```unison:hide
unique type Foo = MkFoo Nat Nat
```

```ucm:hide
project/alice> update
project/main> branch bob
```

Bob's changes:
```unison:hide
unique type Foo = MkFoo Nat Text
```
```ucm:hide
project/bob> update
```
```ucm:error
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

## Merge failure: type-update + constructor-rename conflict

We model the renaming of a type's constructor as an update, so if Alice updates a type and Bob renames one of its constructors (even without changing its structure), we consider it a conflict.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

Original branch:
```unison:hide
unique type Foo = Baz Nat | Qux Text
```

```ucm:hide
project/main> add
project/main> branch alice
```

Alice's changes `Baz Nat` to `Baz Nat Nat`
```unison:hide
unique type Foo = Baz Nat Nat | Qux Text
```

```ucm:hide
project/alice> update
project/main> branch bob
```

Bob's renames `Qux` to `BobQux`:
```unison:hide
unique type Foo = Baz Nat | BobQux Text
```
```ucm:hide
project/bob> update
```
```ucm:error
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

## Merge failure: constructor-rename conflict

Here is another example demonstrating that constructor renames are modeled as updates.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

Original branch:
```unison:hide
unique type Foo = Baz Nat | Qux Text
```

```ucm:hide
project/main> add
project/main> branch alice
```

Alice's rename:
```ucm
project/alice> move.term Foo.Baz Foo.Alice
```
```ucm:hide
project/main> branch bob
```
Bob's rename:
```ucm
project/bob> move.term Foo.Qux Foo.Bob
```

```ucm:error
project/alice> merge bob
```

```ucm:hide
.> project.delete project
```

## Merge failure: non-constructor/constructor conflict

A constructor on one side can conflict with a regular term definition on the other.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm:hide
project/main> branch alice
```

Alice's additions:
```unison:hide
my.cool.thing : Nat
my.cool.thing = 17
```

```ucm:hide
project/alice> add
project/main> branch bob
```

Bob's additions:
```unison:hide
unique ability my.cool where
  thing : Nat -> Nat
```

```ucm:hide
project/bob> add
```

```ucm:error
project/alice> merge bob
```

```ucm:hide
.> project.delete project
```

## Merge failure: type/type conflict with term/constructor conflict

Here's a subtle situation where a new type is added on each side of the merge, and an existing term is replaced with a constructor of one of the types.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

Original branch:
```unison:hide
Foo.Bar : Nat
Foo.Bar = 17
```

```ucm:hide
project/main> add
project/main> branch alice
```

Alice adds this type `Foo` with constructor `Foo.Alice`:
```unison:hide
unique type Foo = Alice Nat
```

```ucm:hide
project/alice> add
project/main> branch bob
```

Bob adds the type `Foo` with constructor `Foo.Bar`, replacing the original `Foo.Bar` term:
```ucm
project/bob> delete.term Foo.Bar
```

```unison:hide
unique type Foo = Bar Nat Nat
```

```ucm:hide
project/bob> add
```

These won't cleanly merge.
```ucm:error
project/alice> merge bob
```

```ucm:hide
.> project.delete project
```

Here's a more involved example that demonstrates the same idea.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

In the LCA, we have a type with two constructors, and some term.

```unison:hide
unique type Foo
  = Bar.Baz Nat
  | Bar.Qux Nat Nat

Foo.Bar.Hello : Nat
Foo.Bar.Hello = 17
```

```ucm:hide
project/main> add
project/main> branch alice
```

Alice deletes this type entirely, and repurposes its constructor names for other terms. She also updates the term.

```ucm:hide
project/alice> delete.type Foo
project/alice> delete.term Foo.Bar.Baz
project/alice> delete.term Foo.Bar.Qux
```

```unison:hide:all
Foo.Bar.Baz : Nat
Foo.Bar.Baz = 100

Foo.Bar.Qux : Nat
Foo.Bar.Qux = 200

Foo.Bar.Hello : Nat
Foo.Bar.Hello = 18
```

```ucm:hide
project/alice> update
```
```ucm
project/alice> view Foo.Bar.Baz Foo.Bar.Qux Foo.Bar.Hello
```

Bob, meanwhile, first deletes the term, then sort of deletes the type and re-adds it under another name, but one constructor's fully qualified names doesn't actually change. The other constructor reuses the name of the deleted term.

```ucm:hide
project/main> branch bob
project/bob> delete.term Foo.Bar.Hello
project/bob> move.type Foo Foo.Bar
project/bob> move.term Foo.Bar.Qux Foo.Bar.Hello
```

```ucm
project/bob> view Foo.Bar
```

At this point, Bob and alice have both updated the name `Foo.Bar.Hello` in different ways, so that's a conflict. Therefore, Bob's entire type (`Foo.Bar` with constructors `Foo.Bar.Baz` and `Foo.Bar.Hello`) gets rendered into the scratch file.

Notably, Alice's "unconflicted" update on the name "Foo.Bar.Baz" (because she changed its hash and Bob didn't touch it) is nonetheless considered conflicted with Bob's "Foo.Bar.Baz".

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

We will resolve this situation automatically in a future version.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm:hide
project/main> branch alice
```

Alice's additions:
```unison:hide
unique type Foo = Bar

alice : Foo -> Nat
alice _ = 18
```

```ucm:hide
project/alice> add
project/main> branch bob
```

Bob's additions:
```unison:hide
unique type Foo = Bar

bob : Foo -> Nat
bob _ = 19
```

```ucm:hide
project/bob> add
```

```ucm:error
project/alice> merge bob
```

```ucm:hide
.> project.delete project
```

## Precondition violations

There are a number of conditions under which we can't perform a merge, and the user will have to fix up the namespace(s) manually before attempting to merge again.

### Conflicted aliases

If `foo` and `bar` are aliases in the nearest common ancestor, but not in Alice's branch, then we don't know whether to update Bob's dependents to Alice's `foo` or Alice's `bar` (and vice-versa).

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

Original branch:
```unison:hide
foo : Nat
foo = 100

bar : Nat
bar = 100
```

```ucm:hide
project/main> add
project/main> branch alice
```

Alice's updates:
```unison:hide
foo : Nat
foo = 200

bar : Nat
bar = 300
```

```ucm:hide
project/alice> update
project/main> branch bob
```

Bob's addition:
```unison:hide
baz : Text
baz = "baz"
```

```ucm:hide
project/bob> add
```

```ucm:error
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

### Conflict involving builtin

We don't have a way of rendering a builtin in a scratch file, where users resolve merge conflicts. Thus, if there is a
conflict involving a builtin, we can't perform a merge.

One way to fix this in the future would be to introduce a syntax for defining aliases in the scratch file.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm:hide
project/main> branch alice
```

Alice's branch:
```ucm
project/alice> alias.type builtin.Nat MyNat
```

Bob's branch:
```ucm:hide
project/main> branch bob
```

```unison:hide
unique type MyNat = MyNat Nat
```

```ucm:hide
project/bob> add
```

```ucm:error
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

### Constructor alias

Each naming of a decl may not have more than one name for each constructor, within the decl's namespace.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```ucm:hide
project/main> branch alice
```

Alice's branch:
```unison:hide
unique type Foo = Bar
```

```ucm:hide
project/alice> add
```
```ucm
project/alice> alias.term Foo.Bar Foo.some.other.Alias
```

Bob's branch:
```ucm:hide
project/main> branch bob
```

```unison:hide
bob : Nat
bob = 100
```

```ucm:hide
project/bob> add
```

```ucm:error
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

### Missing constructor name

Each naming of a decl must have a name for each constructor, within the decl's namespace.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

Alice's branch:
```ucm:hide
project/main> branch alice
```

```unison:hide
unique type Foo = Bar
```

```ucm:hide
project/alice> add
```

```ucm
project/alice> delete.term Foo.Bar
```

Bob's branch:
```ucm:hide
project/main> branch /bob
```

```unison:hide
bob : Nat
bob = 100
```

```ucm:hide
project/bob> add
```

```ucm:error
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

### Nested decl alias

A decl cannot be aliased within the namespace of another of its aliased.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

Alice's branch:
```ucm:hide
project/main> branch alice
```

```unison:hide
structural type A = B Nat | C Nat Nat
structural type A.inner.X = Y Nat | Z Nat Nat
```

```ucm:hide
project/alice> add
```

```ucm
project/alice> names A
```

Bob's branch:
```ucm:hide
project/main> branch bob
```

```unison:hide
bob : Nat
bob = 100
```

```ucm:hide
project/bob> add
```

```ucm:error
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

### Stray constructor alias

Constructors may only exist within the corresponding decl's namespace.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

Alice's branch:
```ucm:hide
project/main> branch alice
```

```unison:hide:all
unique type Foo = Bar
```

```ucm
project/alice> add
project/alice> alias.term Foo.Bar AliasOutsideFooNamespace
```

Bob's branch:
```ucm:hide
project/main> branch bob
```

```unison:hide:all
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

By convention, `lib` can only namespaces; each of these represents a library dependencies. Individual terms and types are not allowed at the top level of `lib`.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

Alice's branch:
```ucm:hide
project/main> branch alice
```

```unison:hide
lib.foo : Nat
lib.foo = 1
```

```ucm:hide
project/alice> add
project/main> branch bob
```

Bob's branch:
```unison:hide
bob : Nat
bob = 100
```

```ucm:hide
project/bob> add
```

```ucm:error
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

## LCA precondition violations

The LCA is not subject to most precondition violations, which is good, because the user can't easily manipulate it!

Here's an example. We'll delete a constructor name from the LCA and still be able to merge Alice and Bob's stuff
together.

```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

LCA:

```unison
structural type Foo = Bar Nat | Baz Nat Nat
```

```ucm
project/main> add
project/main> delete.term Foo.Baz
```

Alice's branch:

```ucm
project/main> branch alice
project/alice> delete.type Foo
project/alice> delete.term Foo.Bar
```

```unison
alice : Nat
alice = 100
```

```ucm
project/alice> add
```

Bob's branch:

```ucm
project/main> branch bob
project/bob> delete.type Foo
project/bob> delete.term Foo.Bar
```

```unison
bob : Nat
bob = 101
```

```ucm
project/bob> add
```

Now we merge:

```ucm
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```

## Regression tests

### Delete one alias and update the other


```ucm:hide
.> project.create-empty project
project/main> builtins.mergeio
```

```unison
foo = 17
bar = 17
```

```ucm
project/main> add
project/main> branch alice
project/alice> delete.term bar
```

```unison
foo = 18
```

```ucm
project/alice> update
project/main> branch bob
```

```unison
bob = 101
```

```ucm
project/bob> add
```

```ucm
project/alice> merge /bob
```

```ucm:hide
.> project.delete project
```
