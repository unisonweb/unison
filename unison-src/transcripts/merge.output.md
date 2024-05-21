# The `merge` command

The `merge` command merges together two branches in the same project: the current branch (unspecificed), and the target
branch. For example, to merge `topic` into `main`, switch to `main` and run `merge topic`.

Let's see a simple unconflicted merge in action: Alice (us) and Bob (them) add different terms. The merged result
contains both additions.

## Basic merge: two unconflicted adds

Alice's adds:
```unison
foo : Text
foo = "alices foo"
```

Bob's adds:
```unison
bar : Text
bar = "bobs bar"
```

Merge result:
```ucm
project/alice> merge /bob

  I merged project/bob into project/alice.

project/alice> view foo bar

  bar : Text
  bar = "bobs bar"
  
  foo : Text
  foo = "alices foo"

```
## Basic merge: two identical adds

If Alice and Bob also happen to add the same definition, that's not a conflict.

Alice's adds:
```unison
foo : Text
foo = "alice and bobs foo"
```

Bob's adds:
```unison
foo : Text
foo = "alice and bobs foo"

bar : Text
bar = "bobs bar"
```

Merge result:
```ucm
project/alice> merge /bob

  I merged project/bob into project/alice.

project/alice> view foo bar

  bar : Text
  bar = "bobs bar"
  
  foo : Text
  foo = "alice and bobs foo"

```
## Simple update propagation

Updates that occur in one branch are propagated to the other. In this example, Alice updates `foo`, while Bob adds a new dependent `bar` of the original `foo`. When Bob's branch is merged into Alice's, her update to `foo` is propagated to his `bar`.

Original branch:
```unison
foo : Text
foo = "old foo"
```

Alice's updates:
```unison
foo : Text
foo = "new foo"
```

Bob's adds:
```unison
bar : Text
bar = foo ++ " - " ++ foo
```

```ucm
project/bob> display bar

  "old foo - old foo"

```
Merge result:
```ucm
project/alice> merge /bob

  I merged project/bob into project/alice.

project/alice> view foo bar

  bar : Text
  bar =
    use Text ++
    foo ++ " - " ++ foo
  
  foo : Text
  foo = "new foo"

project/alice> display bar

  "old foo - old foo"

```
## Update propagation with common dependent

We classify something as an update if its "syntactic hash"—not its normal Unison hash—differs from the original definition. This allows us to cleanly merge unconflicted updates that were individually propagated to a common dependent.

Let's see an example. We have `foo`, which depends on `bar` and `baz`. Alice updates `bar` (propagating to `foo`), and Bob updates `baz` (propagating to `foo`). When we merge their updates, both updates will be reflected in the final `foo`.

Original branch:
```unison
foo : Text
foo = "foo" ++ " - " ++ bar ++ " - " ++ baz

bar : Text
bar = "old bar"

baz : Text
baz = "old baz"
```

Alice's updates:
```unison
bar : Text
bar = "alices bar"
```

```ucm
project/alice> display foo

  "foo - alices bar - old baz"

```
Bob's updates:
```unison
baz : Text
baz = "bobs baz"
```

```ucm
project/bob> display foo

  "foo - old bar - bobs baz"

```
Merge result:
```ucm
project/alice> merge /bob

  I merged project/bob into project/alice.

project/alice> view foo bar baz

  bar : Text
  bar = "alices bar"
  
  baz : Text
  baz = "bobs baz"
  
  foo : Text
  foo =
    use Text ++
    "foo" ++ " - " ++ bar ++ " - " ++ baz

project/alice> display foo

  "foo - alices bar - bobs baz"

```
## Propagating an update to an update

Of course, it's also possible for Alice's update to propagate to one of Bob's updates. In this example, `foo` depends on `bar` which depends on `baz`. Alice updates `baz`, propagating to `bar` and `foo`, while Bob updates `bar` (to something that still depends on `foo`), propagating to `baz`. The merged result will have Alice's update to `foo` incorporated into Bob's updated `bar`, and both updates will propagate to `baz`.

Original branch:
```unison
foo : Text
foo = "old foo" ++ " - " ++ bar

bar : Text
bar = "old bar" ++ " - " ++ baz

baz : Text
baz = "old baz"
```

```ucm
project/main> display foo

  "old foo - old bar - old baz"

```
Alice's updates:
```unison
baz : Text
baz = "alices baz"
```

```ucm
project/alice> display foo

  "old foo - old bar - alices baz"

```
Bob's updates:
```unison
bar : Text
bar = "bobs bar" ++ " - " ++ baz
```

```ucm
project/bob> display foo

  "old foo - bobs bar - old baz"

```
Merge result:
```ucm
project/alice> merge /bob

  I merged project/bob into project/alice.

project/alice> view foo bar baz

  bar : Text
  bar =
    use Text ++
    "bobs bar" ++ " - " ++ baz
  
  baz : Text
  baz = "alices baz"
  
  foo : Text
  foo =
    use Text ++
    "old foo" ++ " - " ++ bar

project/alice> display foo

  "old foo - bobs bar - alices baz"

```
## Update + delete isn't (currently) a conflict

We don't currently consider "update + delete" a conflict like Git does. In this situation, the delete is just ignored, allowing the update to proceed.

Original branch:
```unison
foo : Text
foo = "old foo"
```

Alice's updates:
```unison
foo : Text
foo = "alices foo"
```

Bob's changes:
```ucm
project/bob> delete.term foo

  Done.

```
Merge result:
```ucm
project/alice> merge /bob

  I merged project/bob into project/alice.

project/alice> view foo

  foo : Text
  foo = "alices foo"

```
In a future version, we'd like to give the user a warning at least.

## Library dependencies don't create merge conflicts

Library dependencies don't cause merge conflicts, the library dependencies are just unioned together. If two library dependencies have the same name but different namespace hashes, then the merge algorithm makes up two fresh names.

Alice's adds:
```unison
lib.alice.foo : Nat
lib.alice.foo = 17

lib.bothSame.bar : Nat
lib.bothSame.bar = 18

lib.bothDifferent.baz : Nat
lib.bothDifferent.baz = 19
```

Bob's adds:
```unison
lib.bob.foo : Nat
lib.bob.foo = 20

lib.bothSame.bar : Nat
lib.bothSame.bar = 18

lib.bothDifferent.baz : Nat
lib.bothDifferent.baz = 21
```

Merge result:
```ucm
project/alice> merge bob

  I merged project/bob into project/alice.

project/alice> view foo bar baz

  lib.alice.foo : Nat
  lib.alice.foo = 17
  
  lib.bob.foo : Nat
  lib.bob.foo = 20
  
  lib.bothDifferent__0.baz : Nat
  lib.bothDifferent__0.baz = 19
  
  lib.bothDifferent__1.baz : Nat
  lib.bothDifferent__1.baz = 21
  
  lib.bothSame.bar : Nat
  lib.bothSame.bar = 18

```
## No-op merge (Bob = Alice)

If Bob is equals Alice, then merging Bob into Alice looks like this.

```ucm
project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

project/alice> merge /bob

  😶
  
  project/alice was already up-to-date with project/bob.

```
## No-op merge (Bob < Alice)

If Bob is behind Alice, then merging Bob into Alice looks like this.

```ucm
project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

```
Alice's addition:
```unison
foo : Text
foo = "foo"
```

```ucm
project/alice> add

  ⍟ I've added these definitions:
  
    foo : Text

project/alice> merge /bob

  😶
  
  project/alice was already up-to-date with project/bob.

```
## Fast-forward merge (Bob > Alice)

If Bob is ahead of Alice, then merging Bob into Alice looks like this.

```ucm
project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

```
Bob's addition:
```unison
foo : Text
foo = "foo"
```

```ucm
project/bob> add

  ⍟ I've added these definitions:
  
    foo : Text

project/alice> merge /bob

  I fast-forward merged project/bob into project/alice.

```
## Merge failure: someone deleted something

If either Alice or Bob delete something, so long as the other person didn't update it (in which case we ignore the delete, as explained above), then the delete goes through.

This can cause merge failures due to out-of-scope identifiers, and the user may have to do some digging around to find what the deleted name used to refer to. In a future version, we would emit a \[better\] warning at least.

In this example, Alice deletes `foo`, while Bob adds a new dependent of `foo`.

Original branch:
```unison
foo : Text
foo = "foo"
```

Alice's delete:
```ucm
project/alice> delete.term foo

  Done.

```
Bob's new code that depends on `foo`:
```unison
bar : Text
bar = foo ++ " - " ++ foo
```

```ucm
project/bob> add

  ⍟ I've added these definitions:
  
    bar : Text

project/alice> merge /bob

  I couldn't automatically merge project/bob into project/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

```
```unison:added-by-ucm scratch.u
bar : Text
bar =
  use Text ++
  foo ++ " - " ++ foo


```

## Merge failure: type error

It may be Alice's and Bob's changes merge together cleanly in the sense that there's no textual conflicts, yet the resulting namespace doesn't typecheck.

In this example, Alice updates a `Text` to a `Nat`, while Bob adds a new dependent of the `Text`. Upon merging, propagating Alice's update to Bob's dependent causes a typechecking failure.

Original branch:
```unison
foo : Text
foo = "foo"
```

Alice's update:
```unison
foo : Nat
foo = 100
```

Bob's new definition:
```unison
bar : Text
bar = foo ++ " - " ++ foo
```

```ucm
project/alice> merge /bob

  I couldn't automatically merge project/bob into project/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

```
```unison:added-by-ucm scratch.u
bar : Text
bar =
  use Text ++
  foo ++ " - " ++ foo


```

## Merge failure: simple term conflict

Alice and Bob may disagree about the definition of a term. In this case, the conflicted term and all of its dependents
are presented to the user to resolve.

Original branch:
```unison
foo : Text
foo = "old foo"

bar : Text
bar = "old bar"
```

Alice's changes:
```unison
foo : Text
foo = "alices foo"

bar : Text
bar = "alices bar"

qux : Text
qux = "alices qux depends on alices foo" ++ foo
```

Bob's changes:

```unison
foo : Text
foo = "bobs foo"

baz : Text
baz = "bobs baz"
```

```ucm
project/alice> merge /bob

  I couldn't automatically merge project/bob into project/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

```
```unison:added-by-ucm scratch.u
-- project/alice
foo : Text
foo = "alices foo"

-- project/bob
foo : Text
foo = "bobs foo"

-- The definitions below are not conflicted, but they each depend on one or more
-- conflicted definitions above.

qux : Text
qux =
  use Text ++
  "alices qux depends on alices foo" ++ foo


```

```ucm
project/merge-bob-into-alice> view bar baz

  bar : Text
  bar = "alices bar"
  
  baz : Text
  baz = "bobs baz"

```
## Merge failure: simple type conflict

Ditto for types; if the hashes don't match, it's a conflict. In this example, Alice and Bob do different things to the same constructor. However, any explicit changes to the same type will result in a conflict, including changes that could concievably be merged (e.g. Alice and Bob both add a new constructor, or edit different constructors).

Original branch:
```unison
unique type Foo = MkFoo Nat
```

Alice's changes:
```unison
unique type Foo = MkFoo Nat Nat
```

Bob's changes:
```unison
unique type Foo = MkFoo Nat Text
```

```ucm
project/alice> merge /bob

  I couldn't automatically merge project/bob into project/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

```
```unison:added-by-ucm scratch.u
-- project/alice
type Foo = MkFoo Nat Nat

-- project/bob
type Foo = MkFoo Nat Text


```

## Merge failure: type-update + constructor-rename conflict

We model the renaming of a type's constructor as an update, so if Alice updates a type and Bob renames one of its constructors (even without changing its structure), we consider it a conflict.

Original branch:
```unison
unique type Foo = Baz Nat | Qux Text
```

Alice's changes `Baz Nat` to `Baz Nat Nat`
```unison
unique type Foo = Baz Nat Nat | Qux Text
```

Bob's renames `Qux` to `BobQux`:
```unison
unique type Foo = Baz Nat | BobQux Text
```

```ucm
project/alice> merge /bob

  I couldn't automatically merge project/bob into project/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

```
```unison:added-by-ucm scratch.u
-- project/alice
type Foo = Baz Nat Nat | Qux Text

-- project/bob
type Foo = Baz Nat | BobQux Text


```

## Merge failure: constructor-rename conflict

Here is another example demonstrating that constructor renames are modeled as updates.

Original branch:
```unison
unique type Foo = Baz Nat | Qux Text
```

Alice's rename:
```ucm
project/alice> move.term Foo.Baz Foo.Alice

  Done.

```
Bob's rename:
```ucm
project/bob> move.term Foo.Qux Foo.Bob

  Done.

```
```ucm
project/alice> merge bob

  I couldn't automatically merge project/bob into project/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

```
```unison:added-by-ucm scratch.u
-- project/alice
type Foo = Qux Text | Alice Nat

-- project/bob
type Foo = Bob Text | Baz Nat


```

## Merge failure: non-constructor/constructor conflict

A constructor on one side can conflict with a regular term definition on the other.

Alice's additions:
```unison
my.cool.thing : Nat
my.cool.thing = 17
```

Bob's additions:
```unison
unique ability my.cool where
  thing : Nat -> Nat
```

```ucm
project/alice> merge bob

  I couldn't automatically merge project/bob into project/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

```
```unison:added-by-ucm scratch.u
-- project/alice
my.cool.thing : Nat
my.cool.thing = 17

-- project/bob
ability my.cool where thing : Nat ->{cool} Nat


```

## Merge failure: type/type conflict with term/constructor conflict

Here's a subtle situation where a new type is added on each side of the merge, and an existing term is replaced with a constructor of one of the types.

Original branch:
```unison
Foo.Bar : Nat
Foo.Bar = 17
```

Alice adds this type `Foo` with constructor `Foo.Alice`:
```unison
unique type Foo = Alice Nat
```

Bob adds the type `Foo` with constructor `Foo.Bar`, replacing the original `Foo.Bar` term:
```ucm
project/bob> delete.term Foo.Bar

  Done.

```
```unison
unique type Foo = Bar Nat Nat
```

These won't cleanly merge.
```ucm
project/alice> merge bob

  I couldn't automatically merge project/bob into project/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

```
```unison:added-by-ucm scratch.u
-- project/alice
Foo.Bar : Nat
Foo.Bar = 17

-- project/alice
type Foo = Alice Nat

-- project/bob
type Foo = Bar Nat Nat


```

Here's a more involved example that demonstrates the same idea.

In the LCA, we have a type with two constructors, and some term.

```unison
unique type Foo
  = Bar.Baz Nat
  | Bar.Qux Nat Nat

Foo.Bar.Hello : Nat
Foo.Bar.Hello = 17
```

Alice deletes this type entirely, and repurposes its constructor names for other terms. She also updates the term.

```ucm
project/alice> view Foo.Bar.Baz Foo.Bar.Qux Foo.Bar.Hello

  Foo.Bar.Baz : Nat
  Foo.Bar.Baz = 100
  
  Foo.Bar.Hello : Nat
  Foo.Bar.Hello = 18
  
  Foo.Bar.Qux : Nat
  Foo.Bar.Qux = 200

```
Bob, meanwhile, first deletes the term, then sort of deletes the type and re-adds it under another name, but one constructor's fully qualified names doesn't actually change. The other constructor reuses the name of the deleted term.

```ucm
project/bob> view Foo.Bar

  type Foo.Bar = Baz Nat | Hello Nat Nat

```
At this point, Bob and alice have both updated the name `Foo.Bar.Hello` in different ways, so that's a conflict. Therefore, Bob's entire type (`Foo.Bar` with constructors `Foo.Bar.Baz` and `Foo.Bar.Hello`) gets rendered into the scratch file.

Notably, Alice's "unconflicted" update on the name "Foo.Bar.Baz" (because she changed its hash and Bob didn't touch it) is nonetheless considered conflicted with Bob's "Foo.Bar.Baz".

```ucm
project/alice> merge bob

  I couldn't automatically merge project/bob into project/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

```
```unison:added-by-ucm scratch.u
-- project/alice
Foo.Bar.Baz : Nat
Foo.Bar.Baz = 100

-- project/alice
Foo.Bar.Hello : Nat
Foo.Bar.Hello = 18

-- project/bob
type Foo.Bar = Baz Nat | Hello Nat Nat


```

## Merge algorithm quirk: add/add unique types

Currently, two unique types created by Alice and Bob will be considered in conflict, even if they "look the same".
The result may be confusing to a user – a file containing two identical-looking copies of a unique type is rendered,
which is a parse error.

We will resolve this situation automatically in a future version.

Alice's additions:
```unison
unique type Foo = Bar

alice : Foo -> Nat
alice _ = 18
```

Bob's additions:
```unison
unique type Foo = Bar

bob : Foo -> Nat
bob _ = 19
```

```ucm
project/alice> merge bob

  I couldn't automatically merge project/bob into project/alice.
  However, I've added the definitions that need attention to the
  top of scratch.u.

```
```unison:added-by-ucm scratch.u
-- project/alice
type Foo
  = Bar

-- project/bob
type Foo
  = Bar

-- The definitions below are not conflicted, but they each depend on one or more
-- conflicted definitions above.

alice : Foo -> Nat
alice _ = 18

bob : Foo -> Nat
bob _ = 19


```

## Precondition violations

There are a number of conditions under which we can't perform a merge, and the user will have to fix up the namespace(s) manually before attempting to merge again.

### Conflicted aliases

If `foo` and `bar` are aliases in the nearest common ancestor, but not in Alice's branch, then we don't know whether to update Bob's dependents to Alice's `foo` or Alice's `bar` (and vice-versa).

Original branch:
```unison
foo : Nat
foo = 100

bar : Nat
bar = 100
```

Alice's updates:
```unison
foo : Nat
foo = 200

bar : Nat
bar = 300
```

Bob's addition:
```unison
baz : Text
baz = "baz"
```

```ucm
project/alice> merge /bob

  On project/alice, bar and foo are not aliases, but they used
  to be.

```
### Conflict involving builtin

We don't have a way of rendering a builtin in a scratch file, where users resolve merge conflicts. Thus, if there is a
conflict involving a builtin, we can't perform a merge.

One way to fix this in the future would be to introduce a syntax for defining aliases in the scratch file.

Alice's branch:
```ucm
project/alice> alias.type builtin.Nat MyNat

  Done.

```
Bob's branch:
```unison
unique type MyNat = MyNat Nat
```

```ucm
project/alice> merge /bob

  There's a merge conflict on MyNat, but it's a builtin on one
  or both branches. We can't yet handle merge conflicts on
  builtins.

```
### Constructor alias

Each naming of a decl may not have more than one name for each constructor, within the decl's namespace.

Alice's branch:
```unison
unique type Foo = Bar
```

```ucm
project/alice> alias.term Foo.Bar Foo.some.other.Alias

  Done.

```
Bob's branch:
```unison
bob : Nat
bob = 100
```

```ucm
project/alice> merge /bob

  On project/alice, Foo.Bar and Foo.some.other.Alias are
  aliases. Every type declaration must have exactly one name for
  each constructor.

```
### Missing constructor name

Each naming of a decl must have a name for each constructor, within the decl's namespace.

Alice's branch:
```unison
unique type Foo = Bar
```

```ucm
project/alice> delete.term Foo.Bar

  Done.

```
Bob's branch:
```unison
bob : Nat
bob = 100
```

```ucm
project/alice> merge /bob

  On project/alice, the type Foo is missing a name for one of
  its constructors. Please add one before merging.

```
### Nested decl alias

A decl cannot be aliased within the namespace of another of its aliased.

Alice's branch:
```unison
structural type A = B Nat | C Nat Nat
structural type A.inner.X = Y Nat | Z Nat Nat
```

```ucm
project/alice> names A

  Type
  Hash:  #65mdg7015r
  Names: A A.inner.X
  
  Tip: Use `names.global` to see more results.

```
Bob's branch:
```unison
bob : Nat
bob = 100
```

```ucm
project/alice> merge /bob

  On project/alice, the type A.inner.X is an alias of A. Type
  aliases cannot be nested. Please make them disjoint before
  merging.

```
### Stray constructor alias

Constructors may only exist within the corresponding decl's namespace.

Alice's branch:
```ucm
project/alice> add

  ⍟ I've added these definitions:
  
    type Foo

project/alice> alias.term Foo.Bar AliasOutsideFooNamespace

  Done.

```
Bob's branch:
```ucm
project/bob> add

  ⍟ I've added these definitions:
  
    bob : Nat

```
```ucm
project/alice> merge bob

  On project/alice, the constructor AliasOutsideFooNamespace is
  not in a subnamespace of a name of its type. Please either
  delete it or rename it before merging.

```
### Term or type in `lib`

By convention, `lib` can only namespaces; each of these represents a library dependencies. Individual terms and types are not allowed at the top level of `lib`.

Alice's branch:
```unison
lib.foo : Nat
lib.foo = 1
```

Bob's branch:
```unison
bob : Nat
bob = 100
```

```ucm
project/alice> merge /bob

  On project/alice, there's a type or term directly in the `lib`
  namespace, but I expected only library dependencies to be in
  there. Please remove it before merging.

```
