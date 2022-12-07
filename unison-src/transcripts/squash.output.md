
# Squash merges

`squash src dest` merges can be used to merge from `src` to `dest`, discarding the history of `src`. It's useful when the source namespace history is irrelevant or has a bunch of churn you wish to discard. Often when merging small pull requests, you'll use a squash merge.

Let's look at some examples. We'll start with a namespace with just the builtins. Let's take a look at the hash of this namespace:

```ucm
.> history builtin

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ 1. #jgei2u1mk0 (start of history)

.> fork builtin builtin2

  Done.

```
(We make a copy of `builtin` for use later in this transcript.)

Now suppose we `fork` a copy of builtin, then rename `Nat.+` to `frobnicate`, then rename it back. Notice this produces multiple entries in the history:

```ucm
.> fork builtin mybuiltin

  Done.

.mybuiltin> rename.term Nat.+ Nat.frobnicate

  Done.

.mybuiltin> rename.term Nat.frobnicate Nat.+

  Done.

.mybuiltin> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #qf2na62l7i
  
    > Moves:
    
      Original name  New name
      Nat.frobnicate Nat.+
  
  ⊙ 2. #b3docj1m3t
  
    > Moves:
    
      Original name New name
      Nat.+         Nat.frobnicate
  
  □ 3. #jgei2u1mk0 (start of history)

```
If we merge that back into `builtin`, we get that same chain of history:

```ucm
.> merge mybuiltin builtin

  Nothing changed as a result of the merge.

.> history builtin

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #qf2na62l7i
  
    > Moves:
    
      Original name  New name
      Nat.frobnicate Nat.+
  
  ⊙ 2. #b3docj1m3t
  
    > Moves:
    
      Original name New name
      Nat.+         Nat.frobnicate
  
  □ 3. #jgei2u1mk0 (start of history)

```
Let's try again, but using a `merge.squash` (or just `squash`) instead. The history will be unchanged:

```ucm
.> merge.squash mybuiltin builtin2

  Nothing changed as a result of the merge.

  😶
  
  builtin2 was already up-to-date with mybuiltin.

.> history builtin2

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ 1. #jgei2u1mk0 (start of history)

```
The churn that happened in `mybuiltin` namespace ended up back in the same spot, so the squash merge of that namespace with our original namespace had no effect.

## Another example

Let's look at a more interesting example, where the two namespaces have diverged a bit. Here's our starting namespace:

```unison
x = 1
```

```ucm
  ☝️  The namespace .trunk is empty.

.trunk> add

  ⍟ I've added these definitions:
  
    x : Nat

.> fork trunk alice

  Done.

.> fork trunk bob

  Done.

```
Alice now does some hacking:

```unison
radNumber = 348
bodaciousNumero = 2394
neatoFun x = x
```

```ucm
.alice> add

  ⍟ I've added these definitions:
  
    bodaciousNumero : Nat
    neatoFun        : x -> x
    radNumber       : Nat

.alice> rename.term radNumber superRadNumber

  Done.

.alice> rename.term neatoFun productionReadyId

  Done.

```
Meanwhile, Bob does his own hacking:

```unison
whatIsLove = "?"
babyDon'tHurtMe = ".. Don't hurt me..."
no more = no more
```

```ucm
.bob> add

  ⍟ I've added these definitions:
  
    babyDon'tHurtMe : Text
    no              : more -> r
    whatIsLove      : Text

```
At this point, Alice and Bob both have some history beyond what's in trunk:

```ucm
.> history trunk

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ 1. #i52j9fd57b (start of history)

.> history alice

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #e9jd55555o
  
    > Moves:
    
      Original name New name
      neatoFun      productionReadyId
  
  ⊙ 2. #l5ocoo2eac
  
    > Moves:
    
      Original name New name
      radNumber     superRadNumber
  
  ⊙ 3. #i1vq05628n
  
    + Adds / updates:
    
      bodaciousNumero neatoFun radNumber
  
  □ 4. #i52j9fd57b (start of history)

.> history bob

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #brr4400742
  
    + Adds / updates:
    
      babyDon'tHurtMe no whatIsLove
  
  □ 2. #i52j9fd57b (start of history)

```
Alice then squash merges into `trunk`, as does Bob. It's as if Alice and Bob both made their changes in one single commit.

```ucm
.> merge.squash alice trunk

  Here's what's changed in trunk after the merge:
  
  Added definitions:
  
    1. bodaciousNumero   : Nat
    2. productionReadyId : x -> x
    3. superRadNumber    : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> history trunk

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #f9lvm9gd2k
  
    + Adds / updates:
    
      bodaciousNumero productionReadyId superRadNumber
  
  □ 2. #i52j9fd57b (start of history)

.> merge.squash bob trunk

  Here's what's changed in trunk after the merge:
  
  Added definitions:
  
    1. babyDon'tHurtMe : Text
    2. no              : more -> r
    3. whatIsLove      : Text
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> history trunk

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #dbp78ts6q3
  
    + Adds / updates:
    
      babyDon'tHurtMe no whatIsLove
  
  ⊙ 2. #f9lvm9gd2k
  
    + Adds / updates:
    
      bodaciousNumero productionReadyId superRadNumber
  
  □ 3. #i52j9fd57b (start of history)

```
Since squash merges don't produce any merge nodes, we can `undo` a couple times to get back to our starting state:

```ucm
.> undo

  Here are the changes I undid
  
  Name changes:
  
    Original                  Changes
    1. bob.babyDon'tHurtMe    2. trunk.babyDon'tHurtMe (added)
    
    3. bob.no                 4. trunk.no (added)
    
    5. bob.whatIsLove         6. trunk.whatIsLove (added)

.> undo

  Here are the changes I undid
  
  Name changes:
  
    Original                      Changes
    1. alice.bodaciousNumero      2. trunk.bodaciousNumero (added)
    
    3. alice.productionReadyId    4. trunk.productionReadyId (added)
    
    5. alice.superRadNumber       6. trunk.superRadNumber (added)

.> history trunk

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ 1. #i52j9fd57b (start of history)

```
This time, we'll first squash Alice and Bob's changes together before squashing their combined changes into `trunk`. The resulting `trunk` will have just a single entry in it, combining both Alice and Bob's changes:

```ucm
.> squash alice bob

  Here's what's changed in bob after the merge:
  
  Added definitions:
  
    1. bodaciousNumero   : Nat
    2. productionReadyId : x -> x
    3. superRadNumber    : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> squash bob trunk

  Here's what's changed in trunk after the merge:
  
  Added definitions:
  
    1. babyDon'tHurtMe   : Text
    2. bodaciousNumero   : Nat
    3. no                : more -> r
    4. productionReadyId : x -> x
    5. superRadNumber    : Nat
    6. whatIsLove        : Text
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> history trunk

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #qtotqgds4i
  
    + Adds / updates:
    
      babyDon'tHurtMe bodaciousNumero no productionReadyId
      superRadNumber whatIsLove
  
  □ 2. #i52j9fd57b (start of history)

```
So, there you have it. With squashing, you can control the granularity of your history.

## Throwing out all history

Another thing we can do is `squash` into an empty namespace. This effectively makes a copy of the namespace, but without any of its history:

```ucm
.> squash alice nohistoryalice

  Here's what's changed in nohistoryalice after the merge:
  
  Added definitions:
  
    1. bodaciousNumero   : Nat
    2. productionReadyId : x -> x
    3. superRadNumber    : Nat
    4. x                 : Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> history nohistoryalice

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ 1. #1d9haupn3d (start of history)

```
There's nothing really special here, `squash src dest` discards `src` history that comes after the LCA of `src` and `dest`, it's just that in the case of an empty namespace, that LCA is the beginning of time (the empty namespace), so all the history of `src` is discarded.

## Checking for handling of deletes

This checks to see that squashing correctly preserves deletions:

```ucm
  ☝️  The namespace .delete is empty.

.delete> builtins.merge

  Done.

.delete> fork builtin builtin2

  Done.

.delete> delete.term.verbose builtin2.Nat.+

  Name changes:
  
    Original                    Changes
    1. builtin.Nat.+         ┐  2. delete.builtin2.Nat.+ (removed)
    3. builtin2.Nat.+        │  
    4. delete.builtin.Nat.+  │  
    5. delete.builtin2.Nat.+ │  
    6. mybuiltin.Nat.+       ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

.delete> delete.term.verbose builtin2.Nat.*

  Name changes:
  
    Original                    Changes
    1. builtin.Nat.*         ┐  2. delete.builtin2.Nat.* (removed)
    3. builtin2.Nat.*        │  
    4. delete.builtin.Nat.*  │  
    5. delete.builtin2.Nat.* │  
    6. mybuiltin.Nat.*       ┘  
  
  Tip: You can use `undo` or `reflog` to undo this change.

.delete> squash builtin2 builtin

  Here's what's changed in builtin after the merge:
  
  Removed definitions:
  
    1. Nat.* : Nat -> Nat -> Nat
    2. Nat.+ : Nat -> Nat -> Nat
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.delete> history builtin

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #vinfl9l054
  
    - Deletes:
    
      Nat.* Nat.+
  
  □ 2. #jgei2u1mk0 (start of history)

```
Notice that `Nat.+` and `Nat.*` are deleted by the squash, and we see them deleted in one atomic step in the history.

Just confirming that those two definitions are in fact removed:

```ucm
.delete> view .delete.builtin.Nat.+

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    .delete.builtin.Nat.+

```
```ucm
.delete> view .delete.builtin.Nat.*

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    .delete.builtin.Nat.*

```
## Caveats

If you `squash mystuff trunk`, you're discarding any history of `mystuff` and just cons'ing onto the history of `trunk`. Thus, don't expect to be able to `merge trunk mystuff later and get great results. Squashing should only be used when you don't care about the history (and you know others haven't pulled and built on your line of history being discarded, so they don't care about the history either).
