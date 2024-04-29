# The `merge` command

The `merge` command merges together two branches in the same project: the current branch (unspecificed), and the target
branch. For example, to merge `topic` into `main`, switch to `main` and run `merge topic`.

Let's see a simple unconflicted merge in action: Alice (us) and Bob (them) add different terms. The merged result
contains both additions.

## Basic merge: two unconflicted adds

```ucm
project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

```
```unison
foo : Text
foo = "alices foo"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Text

```
```ucm
project/alice> add

  ⍟ I've added these definitions:
  
    foo : Text

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

```
```unison
bar : Text
bar = "bobs bar"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Text

```
```ucm
project/bob> add

  ⍟ I've added these definitions:
  
    bar : Text

project/alice> merge /bob

  I merged bob into alice.

project/alice> view foo bar

  bar : Text
  bar = "bobs bar"
  
  foo : Text
  foo = "alices foo"

```
## Basic merge: two equal adds

If Alice and Bob also happen to add the same thing, that's not a conflict.

```ucm
project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

```
```unison
foo : Text
foo = "alice and bobs foo"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Text

```
```ucm
project/alice> add

  ⍟ I've added these definitions:
  
    foo : Text

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

```
```unison
foo : Text
foo = "alice and bobs foo"

bar : Text
bar = "bobs bar"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Text
      foo : Text

```
```ucm
project/bob> add

  ⍟ I've added these definitions:
  
    bar : Text
    foo : Text

project/alice> merge /bob

  I merged bob into alice.

project/alice> view foo bar

  bar : Text
  bar = "bobs bar"
  
  foo : Text
  foo = "alice and bobs foo"

```
## Simple update propagation

Updates that occur in one branch are propagated to the other. In this example, Alice updates `foo`, while Bob adds a new
dependent `bar` of (the old) `foo`. When Bob's branch is merged into Alice's, her update to `foo` is propagated to his
`bar`.

```unison
foo : Text
foo = "old foo"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Text

```
```ucm
project/main> add

  ⍟ I've added these definitions:
  
    foo : Text

project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

```
```unison
foo : Text
foo = "new foo"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Text

```
```ucm
project/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

```
```unison
bar : Text
bar = foo ++ foo
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Text

```
```ucm
project/bob> add

  ⍟ I've added these definitions:
  
    bar : Text

project/alice> merge /bob

  I merged bob into alice.

project/alice> view foo bar

  bar : Text
  bar =
    use Text ++
    foo ++ foo
  
  foo : Text
  foo = "new foo"

```
## Update propagation with common dependent

We classify something as an update if its "syntactic hash" - not its normal Unison hash - differs. This allows us to
cleanly merge unconflicted updates that were individually propagated to a common dependent.

Let's see an example. We have `foo`, which depends on `bar` and `baz`. Alice updates `bar` (propagating to `foo`),
and Bob updates `baz` (propagating to `foo`). When we merge their updates, both updates will be reflected in the final
`foo`.

```unison
foo : Text
foo = "foo" ++ bar ++ baz

bar : Text
bar = "old bar"

baz : Text
baz = "old baz"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Text
      baz : Text
      foo : Text

```
```ucm
project/main> add

  ⍟ I've added these definitions:
  
    bar : Text
    baz : Text
    foo : Text

project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

```
```unison
bar : Text
bar = "alices bar"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      bar : Text

```
```ucm
project/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

```
```unison
baz : Text
baz = "bobs baz"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      baz : Text

```
```ucm
project/bob> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

project/alice> merge /bob

  I merged bob into alice.

project/alice> view foo bar baz

  bar : Text
  bar = "alices bar"
  
  baz : Text
  baz = "bobs baz"
  
  foo : Text
  foo =
    use Text ++
    "foo" ++ bar ++ baz

```
## Propagating an update to an update

It's also (of course) possible for Alice's update to propagate to one of Bob's updates. In this example, `foo` depends
on `bar` which depends on `baz`. Alice updates `baz`, propagating to `bar` and `foo`, while Bob updates `bar` (to
something that still depends on `foo`), propagating to `baz`. The merged result will have Alice's update to `foo`
incorporated into Bob's updated `bar`, and both updates will propagate to `baz`.

```unison
foo : Text
foo = "old foo" ++ bar

bar : Text
bar = "old bar" ++ baz

baz : Text
baz = "old baz"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Text
      baz : Text
      foo : Text

```
```ucm
project/main> add

  ⍟ I've added these definitions:
  
    bar : Text
    baz : Text
    foo : Text

project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

```
```unison
baz : Text
baz = "alices baz"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      baz : Text

```
```ucm
project/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

```
```unison
bar : Text
bar = "bobs bar" ++ baz
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      bar : Text

```
```ucm
project/bob> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

project/alice> merge /bob

  I merged bob into alice.

project/alice> view foo bar baz

  bar : Text
  bar =
    use Text ++
    "bobs bar" ++ baz
  
  baz : Text
  baz = "alices baz"
  
  foo : Text
  foo =
    use Text ++
    "old foo" ++ bar

```
## Update + delete isn't (currently) a conflict

We don't (yet?) consider update+delete a conflict; in this case, the delete is just ignored.

```unison
foo : Text
foo = "old foo"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Text

```
```ucm
project/main> add

  ⍟ I've added these definitions:
  
    foo : Text

project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

```
```unison
foo : Text
foo = "alices foo"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Text

```
```ucm
project/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

project/bob> delete.term foo

  Done.

project/alice> merge /bob

  I merged bob into alice.

project/alice> view foo

  foo : Text
  foo = "alices foo"

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
```unison
foo : Text
foo = "foo"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Text

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
```unison
foo : Text
foo = "foo"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Text

```
```ucm
project/bob> add

  ⍟ I've added these definitions:
  
    foo : Text

project/alice> merge /bob

  I merged bob into alice.

```
## Merge failure: someone deleted something

If either Alice or Bob delete something, so long as the other person didn't update it (in which case we ignore the
delete, as explained above), then the delete goes through. This can cause merge failures due to out-of-scope
identifiers. The user may have to do some digging around to find what the deleted name used to refer to.

In this example, Alice deletes `foo`, while Bob adds a new dependent of `foo`.

```unison
foo : Text
foo = "foo"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Text

```
```ucm
project/main> add

  ⍟ I've added these definitions:
  
    foo : Text

project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

project/alice> delete.term foo

  Done.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

```
```unison
bar : Text
bar = foo ++ foo
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Text

```
```ucm
project/bob> add

  ⍟ I've added these definitions:
  
    bar : Text

project/alice> merge /bob

  I couldn't automatically merge bob into alice. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
bar : Text
bar =
  use Text ++
  foo ++ foo
```

## Merge failure: type error

It may be possible to cleanly merge Alice's and Bob's changes together, yet the resulting namespace doesn't typecheck.

In this example, Alice updates a `Text` to a `Nat`, while Bob adds a new dependent of the `Text`. Upon merging,
propagating Alice's update to Bob's dependent fails to typecheck.

```unison
foo : Text
foo = "foo"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Text

```
```ucm
project/main> add

  ⍟ I've added these definitions:
  
    foo : Text

project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

```
```unison
foo : Nat
foo = 100
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat

```
```ucm
project/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

```
```unison
bar : Text
bar = foo ++ foo
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Text

```
```ucm
project/bob> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

project/alice> merge /bob

  I couldn't automatically merge bob into alice. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
bar : Text
bar =
  use Text ++
  foo ++ foo
```

## Merge failure: simple term conflict

Alice and Bob may disagree about the definition of a term. In this case, the conflicted term and all of its dependents
are presented to the user to resolve.

```unison
foo : Text
foo = "old foo"

bar : Text
bar = "old bar"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Text
      foo : Text

```
```ucm
project/main> add

  ⍟ I've added these definitions:
  
    bar : Text
    foo : Text

project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

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

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      qux : Text
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      bar : Text
      foo : Text

```
```ucm
project/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

```
```unison
foo : Text
foo = "bobs foo"

baz : Text
baz = "bobs baz"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      baz : Text
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Text

```
```ucm
project/bob> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

project/alice> merge /bob

  I couldn't automatically merge bob into alice. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
foo : Text
foo = "alices foo"

foo : Text
foo = "bobs foo"

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

Ditto for types; if the hashes don't match, it's a conflict. In this example, Alice and Bob do different things to the
same constructor. However, any explicit changes to the same type will result in a conflict, including changes that could
concievably be merged (e.g. Alice and Bob both add a new constructor, or edit different constructors).

```unison
unique type Foo = MkFoo Nat
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo

```
```ucm
project/main> add

  ⍟ I've added these definitions:
  
    type Foo

project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

```
```unison
unique type Foo = MkFoo Nat Nat
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Foo

```
```ucm
project/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

```
```unison
unique type Foo = MkFoo Nat Text
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Foo

```
```ucm
project/bob> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

project/alice> merge /bob

  I couldn't automatically merge bob into alice. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
type Foo = MkFoo Nat Nat

type Foo = MkFoo Nat Text
```

## Merge failure: type-update + constructor-rename conflict

Renaming a constructor is modeled as an update, so if Alice updates a type and Bob renames one of its constructors but
doesn't change its hash, that's still a conflict.

```unison
unique type Foo = Baz Nat | Qux Text
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo

```
```ucm
project/main> add

  ⍟ I've added these definitions:
  
    type Foo

project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

```
```unison
unique type Foo = Baz Nat Nat | Qux Text
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Foo

```
```ucm
project/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

```
```unison
unique type Foo = Baz Nat | BobQux Text
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked the definitions in scratch.u. This
  file has been previously added to the codebase.

```
```ucm
project/bob> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

project/alice> merge /bob

  I couldn't automatically merge bob into alice. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
type Foo = Qux Text | Baz Nat Nat

type Foo = Baz Nat | BobQux Text
```

## Merge failure: constructor-rename conflict

Another example demonstrating that constructor "renames" (add + delete) are modeled as updates.

```unison
unique type Foo = Baz Nat | Qux Text
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo

```
```ucm
project/main> add

  ⍟ I've added these definitions:
  
    type Foo

project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

project/alice> move.term Foo.Baz Foo.Alice

  Done.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

project/bob> move.term Foo.Qux Foo.Bob

  Done.

```
```ucm
project/alice> merge bob

  I couldn't automatically merge bob into alice. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
type Foo = Bob Text | Alice Nat

type Foo = Bob Text | Alice Nat
```

## Precondition violations

Let's see a number of merge precondition violations. These are conditions under which we can't perform a merge, and the
user will have to fix up the namespace(s) manually before attempting to merge again.

### Conflicted aliases

If `foo` and `bar` are aliases in the nearest common ancestor, but not in Alice's branch, then we don't know whether to
update Bob's dependents to Alice's `foo` or Alice's `bar` (and vice-versa).

```unison
foo : Nat
foo = 100

bar : Nat
bar = 100
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Nat
      foo : Nat

```
```ucm
project/main> add

  ⍟ I've added these definitions:
  
    bar : Nat
    foo : Nat

project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /alice`.

```
```unison
foo : Nat
foo = 200

bar : Nat
bar = 300
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      bar : Nat
        (The old definition is also named foo.)
      foo : Nat
        (The old definition is also named bar.)

```
```ucm
project/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /bob`.

```
```unison
baz : Text
baz = "baz"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      baz : Text

```
```ucm
project/bob> add

  ⍟ I've added these definitions:
  
    baz : Text

project/alice> merge /bob

  On alice, bar and foo are not aliases, but they used to be.

```
### Conflict involving builtin

We don't have a way of rendering a builtin in a scratch file, where users resolve merge conflicts. Thus, if there is a
conflict involving a builtin, we can't perform a merge.

```ucm
project/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.

project/main> alias.type builtin.Nat MyNat

  Done.

```
```unison
unique type MyNat = MyNat Nat
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type MyNat

```
```ucm
project/topic> add

  ⍟ I've added these definitions:
  
    type MyNat

project/main> merge /topic

  There's a merge conflict on MyNat, but it's a builtin on one
  or both branches. We can't yet handle merge conflicts on
  builtins.

```
### Constructor alias

Each naming of a decl may not have more than one name for each constructor underneath the decl's namespace.

```ucm
project/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.

```
```unison
unique type Foo = Bar
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo

```
```ucm
project/topic> add

  ⍟ I've added these definitions:
  
    type Foo

project/topic> alias.term Foo.Bar Foo.some.other.Alias

  Done.

project/main> merge /topic

  On topic, Foo.Bar and Foo.some.other.Alias are aliases. Every
  type declaration must have exactly one name for each
  constructor.

```
### Missing constructor name

Each naming of a decl may not have zero names for a constructor underneath the decl's namespace.

```ucm
project/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.

```
```unison
unique type Foo = Bar
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo

```
```ucm
project/topic> add

  ⍟ I've added these definitions:
  
    type Foo

project/topic> delete.term Foo.Bar

  Done.

project/main> merge /topic

  Missing constructor name.

```
### Nested decl alias

Decl aliases must be disjoint in a namespace: one cannot contain another.

```ucm
project/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.

```
```unison
structural type A = B Nat | C Nat Nat
structural type A.inner.X = Y Nat | Z Nat Nat
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type A
      structural type A.inner.X

```
```ucm
project/topic> add

  ⍟ I've added these definitions:
  
    structural type A
    structural type A.inner.X

project/main> merge /topic

  Nested decl alias.

```
### Stray constructor alias

Each naming of a constructor must be underneath its decl's namespace.

```ucm
project/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.

```
```unison
unique type Foo = Bar
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo

```
```ucm
project/topic> add

  ⍟ I've added these definitions:
  
    type Foo

project/topic> alias.term Foo.Bar AliasOutsideFooNamespace

  Done.

project/main> merge /topic

  Stray constructor.

```
### Term or type in `lib`

A bit of an odd one, but we have a convention that `lib` contains only namespaces, which are dependencies. Thus, the
`lib` namespace can always merge cleanly, so long as there aren't stray terms or types in it.

```ucm
project/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /topic`.

```
```unison
lib.foo : Nat
lib.foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      lib.foo : Nat

```
```ucm
project/topic> add

  ⍟ I've added these definitions:
  
    lib.foo : Nat

project/main> merge /topic

  Defns in lib

```
