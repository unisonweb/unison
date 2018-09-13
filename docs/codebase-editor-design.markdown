WIP

# Editing a Unison codebase

The Unison codebase is not just a mutable bag of text files, it's a structured object that undergoes a series of well-typed transformations over the course of development. Each well-typed transformation is called a `Changeset`. Applying a `Changeset` to a `Codebase` yields another well-typed codebase, a `Codebase` is never in a "broken" state, yet we can still make arbitrary edits to a codebase.

This document explains what a `Codebase` and a `Changeset` are and how the programmer interacts with them. The benefits of the Unison approach which we'll see are:

* Incremental compilation is perfectly precise and comes for free, regardless of what editor you use. You'll almost never spend time [waiting for Unison code to compile](https://xkcd.com/303/), _no matter how large your codebase_.
* Refactoring is a controlled experience where the refactoring always typechecks and you can precisely measure your progress, so arbitrary changes to a codebase can be completed without ever dealing with a depressingly long list of (often misleading) compile errors or broken tests!
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
  | SwapArguments Permutation -- optional idea for more semantic edits
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

The `Edit` type produces a `Conflict` when merged, though with more structured edits (note the `SwapArguments` data constructor), even more could be done here.

A `Changeset` is _complete_ when it either covers the entire codebase or when it can be expanded to cover the whole codebase because its "frontier" consists of only type preserving edits. We can make this idea more precise. A `Changeset`, `c`, has a _frontier_ of `Code` drawn from `altered c` which is not a dependency of any other `Code` in `altered c`. `altered` just includes everything that has been edited or deprecated:

```haskell
altered c = deprecated c `Set.union` Map.keySet (edited c)
```

A `Changeset`, `c`, is complete with respect to a `Codebase`, `cb`, when each `Code` in the frontier of `c` is either a type-preserving edit (a `Replace new Same` or `Replace new Subtype`) OR a type-changing edit (this includes deprecation) which has _no transitive dependents_ in `cb`.

If we want to measure how much work is remaining to complete a `Changeset`, we can count all the transitive dependents of type-changing edits in the frontier. This number will decrease monotonically as the `Changeset` is worked on.

_Related:_ There are some useful computations we can do to suggest which dependents of the frontier to upgrade next, based on what will make maximal progress in decreasing the remaining work. The idea is that it's useful to focus first on the "trunk" of a refactoring, which lots of code depend on, rather than the branches and leaves. Programmers sometimes try to do something like this when refactoring, but it can be difficult to know what's what when the main feedback you get from the compiler is just a big list of compile errors.

To apply a `Changeset` to a `Codebase`, we interpret the `Changeset` as a `Codebase -> Codebase`. There are some interesting decisions about how to do this, but here's one implementation:

```haskell
apply : Changeset -> Codebase -> Codebase
apply c cb = Codebase (added c `Set.union` code cb) ... todo
```

Notice that with this implementation a `Changeset` can talk about upgrades and edits to functions, without having to know what they are called! This makes changesets more portable as they can still be shared with people who might have different local names for things.

There is one important invariant about `Changeset` that the public operations we expose for it will all preserve:

> If two definitions, `a`, and `b` are both updated by the `Changeset`, and `a` is a transitive dependency of `b`, then all transitive dependents on the path connecting `a` to `b` must also have updates.

Put another way, the `Changeset` can't "skip over" updates. It can be of limited scope (unable to cover the whole codebase yet), or it can start near the frontiers of the codebase (if it's an update of a `main` function with lots of transitive dependencies), but it can't skip over dependents: if the changeset starts an upgrade at some node in the dependency graph, it needs to expand to immediate dependents, recursively, until reaching some type-preserving frontier. If we didn't have this property, we can't really view the changeset as performing a replacement of some old definitions (as some dependents of those old definitions are still referencing the old definitions in the new codebase) and it would be unclear what we should do with the names.

This is it for the model. The rest of this document focuses on how to expose this nice model for use by the Unison programmer.

## The developer experience

TODO

### Repository format

Design goal - a Unison repository can be versioned using Git (or Hg, or whatever), and there should never be merge conflicts when merging two Unison repositories. That is, Git merge conflicts are a bad UX for surfacing concurrent edits that the user may wish to reconcile.

```
terms/
  jAjGDJnsdfL/
    compiled.ub  -- compiled form of the term
    type.ub    -- binary representation of the type of the term
    index.html -- pretty, hyperlinked source code of the term
    Runar.factorial.name -- just an empty file
    math.factorial.name
    reference-english-JasVXOEBBV8.hash -- link to docs, in English
    reference-spanish-9JasdfjHNBdjj.hash -- link to docs, in Spanish
    doc-english-OD03VvvsjK.hash -- other docs
    license-8JSJdkVvvow92.hash -- reference to the license for this term
    author-38281234jf.hash -- link to
types/ -- directory of all type declarations
  8sdfA1baBw/
    compiled.ub -- compiled form of the type declaration
    index.html  -- pretty, hyperlinked source code of the type decl
    reference-english-KgLfAIBw312.hash -- reference docs
    doc-english-8AfjKBCXdkw.hash -- other docs
    license-8JSJdkVvvow92.hash -- reference to the license for this term
    author-38281234jf.hash -- link to
    0/ -- constructor id, has a set of names
      Nil.name -- empty file
      Empty.name -- empty file
    1/ -- constructor id, has a set of names
      Cons.name
      Prepend.name
changesets/
  wfjs09df823jfasdlkfjasd9/ -- a guid
    myAwesomeChanges.name -- the name of this changeset (can be changed)
    description.markdown  -- docs about this changeset
    added/
    deprecated/
    edited/
    names/
  bv2kfaslu72jsdf823jjfas/
    coolFeature.name
    ..
```

Sets are represented by directories of immutable empty files whose file names represent the elements of the set - the sets are union'd as a result of a Git merge. Deletions are handled without conflicts as well.

Likewise, maps are represented by directories with a subdirectory named by each key in the map. The content of each subdirectory represents the value for that key in the map.

Observation: we'll probably want some additional indexing structure (which won't be versioned) which can be cached on disk and derived from the primary repo format. This is useful for answering different queries on the codebase more efficiently.

## Notes and ideas

You can have first-class imports with a type like:

```haskell
type Namespace = Map Name (Set Code) -> Map Code [NameEdit]
```

There's a nice little combinator library you can write to build up `Namespace` values in various ways, and we can imagine the Unison `use` syntax to be sugar for this library.
