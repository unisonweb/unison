WIP

The Unison codebase
===================

The Unison codebase is not just a mutable bag of text files, it's a structured object that undergoes a series of well-typed transformations over the course of development. Each well-typed transformation is called a `Changeset`. Applying a `Changeset` to a `Codebase` yields another well-typed codebase, a `Codebase` is never in a "broken" state, yet we can still make arbitrary edits to a codebase.

This document explains what a `Codebase` and a `Changeset` are and how the programmer interacts with them. The benefits of the Unison approach which we'll see are:

* Incremental compilation is perfectly precise and comes for free, regardless of what editor you use. You'll almost never spend time [waiting for Unison code to compile](https://xkcd.com/303/), _no matter how large your codebase_.
* Refactoring is a controlled experience where the refactoring always typechecks and you can precisely measure your progress, meaning that arbitrary changes to a codebase can be completed without ever dealing with a depressingly long list of (often misleading) compile errors or broken tests.
* Codebase changes can be worked on concurrently by multiple developers, and many situations that traditionally result in incidental merge conflicts or build issues can no longer occur. (e.g., Alice swapped the order of two definitions in a file, conflicting with Bob's adding an unrelated definition.)
* Renames, even bulk renames of whole packages of definitions, are 100% accurate and fast. When it's this easy to rename things, there's less anxiety about picking names and less need to pick the perfect name at the moment you start writing something.
* We can assign multiple names to the same definitions, and you can choose which naming you prefer and publish your naming schemes for others to use if they wish. [Bikeshedding](http://bikeshed.com/) over names can be a thing of the past (or at least vastly reduced 😀).
* Dependency hell is also vastly reduced: many situations that contribute to dependency hell simply cannot arise with the Unison codebase model.
* As an added bonus, it's no problem to use different versions of some library in different parts of your application when convenient, just as you might use two unrelated libraries in your application.
* It's easy to mix and match parts of different libraries into a custom bundle, which others can use, all while retaining full compatibility with the existing libraries that the bundle draws from.
* Publishing code is trivial; it won't require any additional steps beyond pushing to a git repository or shared filesystem. (Other filesystem-like services can be supported in the future.)
* Import statements are first-class values which can be shared and aggregated and published for consumption by others. No more project-wide import boilerplate at the top of every file!
* And this is all done in a backwards compatible way using existing tools: you can still use your favorite text editor, can still version your code with Git, use GitHub, etc.

Warning: once you experience this mode of editing a codebase and the control, safety, and ease of it, the "mutable bag of text files" model of a codebase may start to seem barbaric in comparison. 😱

## The big idea  🧠

Here it is: _Unison definitions are identified by content._ In Unison, it's not possible to change a definition, only to introduce new definitions. What can change is the mapping between definition and human-friendly names. e.g. `x -> x + 1` (a definition) vs `Integer.increment` (a name we might associate with that definition for the purposes of writing and reading other code that references it). An analogy: Unison definitions are like stars in the sky. We can assign different names to the stars in the sky, we can discover new stars, but the stars themselves exist independently of their names.

From this simple idea, we can build a better development experience around codebase editing with all of the above benefits.

## The model

This section gives the model of what a `Codebase` is, what a `Changeset` is, what their API is. Later we'll cover what the actual user experience is for interacting with the model, along with various concrete usage scenarios.

A `Codebase` denotes two things, a `Set Code` (a set of definitions), and a `Map Code (Set Metadata)`. `Code` could be a function or value definition (a `Term`) or a `TypeDeclaration`. `Metadata` includes things like:

* Human-friendly name
* Link to documentation, which is also just a `Term`
* Link to LICENSE, which is also just a `Term`
* Link to author, date created, previous version, etc ..

Each `Term` in the `Codebase` also includes its `Type`. A Unison codebase contains no ill-typed terms.

At a high level, a `Changeset` denotes a function from `Codebase` to `Codebase`, but using the representation `Codebase -> Codebase` is limited. We want a representation that comes with a commutative merge operation that allows multiple developers to collaborate on building a `Changeset`:

```haskell
data Codebase = Codebase { code : Set Code, names : Map Code (Set Metadata) }

Code.dependencies : Code -> Set Code
Code.dependencies c = ...

data Changeset = Changeset
  { added      : Set Code
  , edited     : Map Code Edit
  , deprecated : Set Code
  , names      : Map Code NameEdit }

data NameEdit = NameEdit { adds : Set Name, removes : Set Name }

-- names that have been added AND removed
NameEdit.conflicts : NameEdit -> Set Name

data Edit
  = Replace Code Typing
  | SwapArguments Permutation
  ..
  | Conflict (Set Edit)

-- Indicates whether the replacement is the same type, a subtype, or a different type
-- which is useful information for knowing how far we can automatically propagate a `Changeset`.
data Typing = Same | Subtype | Different
```

The commutative monoid for `Changeset` combines corresponding elements of the `Changeset`:

```haskell
instance Monoid Changeset where
  mempty = Changeset mempty mempty mempty mempty

  c1 `mappend` c2 =
    Changeset (added c1 `Set.union` added c2)
              (Map.unionWith mappend (edited c1) (edited c2))
              (deprecated c1 `Set.union` deprecated c2)
              (Map.unionWith mappend (names c1) (names c2))

instance Monoid Edit where ...
instance Monoid NameEdit where ...
```

To apply a `Changeset` to a `Codebase`, we interpret the `Changeset` as a `Codebase -> Codebase`. There are some interesting decisions about how to do this, but here's one implementation:

```haskell
apply : Changeset -> Codebase -> Codebase
apply c cb = Codebase (added c `Set.union` code cb) ... todo
```

This is it for the model. The rest of this document focuses on how to expose this nice model for use by the Unison programmer.

## The developer experience

TODO
