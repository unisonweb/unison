# The `merge` command

The `merge` command merges together two branches in the same project: the current branch (unspecificed), and the target
branch. For example, to merge `topic` into `main`, switch to `main` and run `merge topic`. Let's see a simple,
unconflicted merge in action, wherein Alice (us) and Bob (them) have added different terms.

## Basic merge

```ucm
project/main> branch alice

  Done. I've created the alice branch based off of main.
  
  Tip: Use `merge /alice /main` to merge your work back into the
       main branch.

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
  
  Tip: Use `merge /bob /main` to merge your work back into the
       main branch.

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

project/alice> merge2 /bob

  I merged bob into alice.

project/alice> view foo bar

  bar : Text
  bar = "bobs bar"
  
  foo : Text
  foo = "alices foo"

```
## Update propagation

Updates are propagated. In this example, Alice updates `foo`, and Bob adds a new dependent `bar` of (the old) `foo`.
When Bob's branch is merged into Alice's, her update to `foo` is propagated to `bar`.

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
  
  Tip: Use `merge /alice /main` to merge your work back into the
       main branch.

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
  
  Tip: Use `merge /bob /main` to merge your work back into the
       main branch.

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

project/alice> merge2 /bob

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

Different hashes don't necessarily imply an update. In this example, Alice and Bob both update different dependencies
`bar` and `baz` of a common dependent `foo`, so their `foo`s have different hashes. However, we can merge these changes
together just fine, resulting in a `foo` that incorporates both updates.

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
  
  Tip: Use `merge /alice /main` to merge your work back into the
       main branch.

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
  
  Tip: Use `merge /bob /main` to merge your work back into the
       main branch.

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

project/alice> merge2 /bob

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
## Typechecking failure

Alice's update may fail to typecheck when propagating to Bob's dependents.

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
  
  Tip: Use `merge /alice /main` to merge your work back into the
       main branch.

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
  
  Tip: Use `merge /bob /main` to merge your work back into the
       main branch.

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

project/alice> merge2 /bob

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

## Simple term conflict

Alice and Bob may disagree about the definition of a term. In this case, the conflicted term and all of its dependents
are given to the user to resolve. The unconflicted parts of a merge (and any merge with conflicts in general) are put
into the namespace.

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
  
  Tip: Use `merge /alice /main` to merge your work back into the
       main branch.

```
```unison
foo : Text
foo = "alices foo"

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
      foo : Text

```
```ucm
project/alice> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

project/main> branch bob

  Done. I've created the bob branch based off of main.
  
  Tip: Use `merge /bob /main` to merge your work back into the
       main branch.

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

project/alice> merge2 /bob

  I couldn't automatically merge bob into alice. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
foo : Text
foo = "alices foo"

foo : Text
foo = "bobs foo"
```

```ucm
project/merge-bob-into-alice> view bar baz

  bar : Text
  bar = "alices bar"
  
  baz : Text
  baz = "bobs baz"

```
## Simple type conflict

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
  
  Tip: Use `merge /alice /main` to merge your work back into the
       main branch.

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
  
  Tip: Use `merge /bob /main` to merge your work back into the
       main branch.

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

project/alice> merge2 /bob

  I couldn't automatically merge bob into alice. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
type Foo = MkFoo Nat Nat

type Foo = MkFoo Nat Text
```

## Term conflict with a constructor

In this example, Alice updates a type, while Bob "updates" one of the constructors (by changing it to a term), and adds
back a name for the constructor somewhere else. Bob didn't actually update the type itself, but there is nonetheless
a conflict between Alice's type (due to one of its constructors) and Bob's term.

```unison
unique type Foo
  = MkFooOne Nat
  | MkFooTwo Nat Nat
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
  
  Tip: Use `merge /alice /main` to merge your work back into the
       main branch.

```
```unison
unique type Foo
  = MkFooOne Nat Text
  | MkFooTwo Nat Nat
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
  
  Tip: Use `merge /bob /main` to merge your work back into the
       main branch.

```
```unison
unique type Foo
  = MkFooOne Nat
  | MkFooTwoRenamed Nat Nat

Foo.MkFooTwo : Text
Foo.MkFooTwo = "hello"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⊡ Previously added definitions will be ignored: Foo
    
    ⍟ These new definitions are ok to `add`:
    
      Foo.MkFooTwo : Text

```
```ucm
project/bob> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

project/alice> merge2 /bob

  I couldn't automatically merge bob into alice. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
type Foo = MkFooTwo Nat Nat | MkFooOne Nat Text

type Foo = MkFooTwoRenamed Nat Nat | MkFooOne Nat

Foo.MkFooTwo : Text
Foo.MkFooTwo = "hello"
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
  
  Tip: Use `merge /alice /main` to merge your work back into the
       main branch.

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
  
  Tip: Use `merge /bob /main` to merge your work back into the
       main branch.

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

project/alice> merge2 /bob

  On alice, bar and foo are not aliases, but they used to be.

```
### Conflict involving builtin

We don't have a way of rendering a builtin in a scratch file, where users resolve merge conflicts. Thus, if there is a
conflict involving a builtin, we can't perform a merge.

```ucm
project/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

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

project/main> merge2 /topic

  There's a merge conflict on MyNat, but it's a builtin on one
  or both branches. We can't yet handle merge conflicts on
  builtins.

```
# fast-forward

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
```unison
bar : Nat
bar = foo + 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Nat

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    bar : Nat

proj/main> merge2 /topic

  I merged topic into main.

proj/main> view bar

  bar : Nat
  bar =
    use Nat +
    foo + 1

```
## Add/Add agree

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    foo : Nat

```
```unison
foo : Nat
foo = 1

bar : Nat
bar = 2
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
proj/main> add

  ⍟ I've added these definitions:
  
    bar : Nat
    foo : Nat

proj/main> merge2 /topic

  I merged topic into main.

```
## Add/Add conflict

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    foo : Nat

```
```unison
foo : Nat
foo = 4
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> merge2 /topic

  I couldn't automatically merge topic into main. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
foo : Nat
foo = 4

foo : Nat
foo = 1
```

## Update/Update conflict

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
```unison
foo : Nat
foo = 2
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
proj/topic> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
```unison
foo : Nat
foo = 3
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
proj/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
```ucm
proj/main> merge2 /topic

  I couldn't automatically merge topic into main. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
foo : Nat
foo = 3

foo : Nat
foo = 2
```

## Update/Update agree

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
```unison
foo : Nat
foo = 2
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
proj/topic> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
```unison
foo : Nat
foo = 2

bar : Nat
bar = 3
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Nat
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Nat

```
```ucm
proj/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
```ucm
proj/main> merge2 /topic

  I merged topic into main.

```
## Update/Delete conflict

We don't consider these, so this transcript is capturing our
ignorance.

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

proj/topic> delete.term foo

  Done.

```
```unison
foo : Nat
foo = 2
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
We silently ignore the delete

```ucm
proj/main> merge2 /topic

  I merged topic into main.

proj/main> view foo

  foo : Nat
  foo = 2

```
## Alice deletes x bob adds y

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

proj/main> delete.term foo

  Done.

```
```unison
bar : ()
bar = ()
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : ()

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    bar : ()

```
```ucm
proj/main> merge2 /topic

  I merged topic into main.

proj/main> ls

  1. bar      (())
  2. builtin/ (628 terms, 89 types)

```
## Alice adds x bob deletes y

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

proj/topic> delete.term foo

  Done.

```
```unison
bar : ()
bar = ()
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : ()

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    bar : ()

```
```ucm
proj/main> merge2 /topic

  I merged topic into main.

proj/main> ls

  1. bar      (())
  2. builtin/ (628 terms, 89 types)

```
## Alice deletes x bob deletes x

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

proj/topic> delete.term foo

  Done.

proj/main> delete.term foo

  Done.

```
```ucm
proj/main> merge2 /topic

  😶
  
  proj/main was already up-to-date with proj/topic.

proj/main> ls

  1. builtin/ (628 terms, 89 types)

```
## Altered dependent

`foo : Nat` is in the ancestor of `main` and `topic`

```unison
foo : Nat
foo = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      foo : Nat

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
`topic` adds a dependent of `foo`

```unison
bar : Nat
bar = foo + 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bar : Nat

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    bar : Nat

```
`main` changes the type of `foo`

```unison
foo : Int
foo = +1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : Int

```
```ucm
proj/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
attempt to merge `topic` into `main`

```ucm
proj/main> merge2 /topic

  I couldn't automatically merge topic into main. However, I've
  added the definitions that need attention to the top of
  scratch.u.

```
```unison:added-by-ucm scratch.u
bar : Nat
bar =
  use Nat +
  foo + 1
```

## Precondition violations

### term in lib


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
proj/main> add

  ⍟ I've added these definitions:
  
    lib.foo : Nat

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
```unison
bonk : Nat
bonk = 5
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      bonk : Nat

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    bonk : Nat

```
```ucm
proj/main> merge2 /topic

  Defns in lib

```
### Constructor alias

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
proj/main> add

  ⍟ I've added these definitions:
  
    type Foo

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

proj/topic> alias.term Foo.Bar Foo.Alias

  Done.

```
```ucm
proj/main> merge2 /topic

  On topic, Foo.Alias and Foo.Bar are aliases. Every type
  declaration must have exactly one name for each constructor.

```
### Missing constructor

```unison
unique type Foo = Bar | Baz
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
proj/main> add

  ⍟ I've added these definitions:
  
    type Foo

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

proj/topic> delete.term Foo.Bar

  Done.

```
```ucm
proj/main> merge2 /topic

  Missing constructor name.

```
### Nested decl alias

```unison
structural type Foo = FooCon
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type Foo
        (also named builtin.Unit)

```
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    structural type Foo
      (also named builtin.Unit)

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

```
```unison
structural type Foo.Bar = BarCon
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type Foo.Bar
        (also named Foo and builtin.Unit)

```
```ucm
proj/topic> add

  ⍟ I've added these definitions:
  
    structural type Foo.Bar
      (also named Foo and builtin.Unit)

```
```ucm
proj/main> merge2 /topic

  Nested decl alias.

```
### Stray constructor alias

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
proj/main> add

  ⍟ I've added these definitions:
  
    type Foo

proj/main> branch topic

  Done. I've created the topic branch based off of main.
  
  Tip: Use `merge /topic /main` to merge your work back into the
       main branch.

proj/topic> alias.term Foo.Bar Stray

  Done.

```
```ucm
proj/main> merge2 /topic

  Stray constructor.

```
