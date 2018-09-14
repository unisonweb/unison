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

At a high level, a `Changeset` denotes a function from `Codebase` to `Codebase`, but we don't literally use the representation `Codebase -> Codebase`. We want a representation that can be converted to a `Codebase -> Codebase` but which also comes equipped with a commutative merge operation that allows multiple developers to collaborate on building a `Changeset`. Here's a model for that.

As mentioned, a codebase is a `Set Code` and a `Map Code (Set Metadata)`. Note that `Code` has a function, `dependencies`, which is the set of `Code` it uses in its definition:

```haskell
data Codebase = Codebase { code : Set Code, metadata : Map Code (Set Metadata) }

data Metadata = Metadata { names : Set Name, links : Map Name Code }

Code.dependencies : Code -> Set Code
Code.dependencies c = ...
```

A `Changeset` denotes `Codebase -> Codebase`, but represented as a mergeable data structure that tells us how to modify the input `Codebase`:

```haskell
data Changeset = Changeset
  { added      : Set Code
  , edited     : Map Code Edit
  , deprecated : Set Code
  , editedMetadata : Map Code MetadataEdit }
```

Will say what `Edit` and `MetadataEdit` are momentarily, but the commutative monoid for `Changeset` combines corresponding elements of the `Changeset`:

```haskell
instance Monoid Changeset where
  mempty = Changeset mempty mempty mempty mempty

  c1 `mappend` c2 =
    Changeset (added c1 `Set.union` added c2)
              (Map.unionWith mappend (edited c1) (edited c2))
              (deprecated c1 `Set.union` deprecated c2)
              (Map.unionWith mappend (names c1) (names c2))

instance Monoid Edit where ...
instance Monoid MetadataEdit where ...
```

A `MetadataEdit` denotes a function from `Metadata -> Metadata`.

```haskell
data MetadataEdit =
  MetadataEdit { nameAdds    : Set Name
               , nameRemoves : Set Name
               , linkAdds    : Map Name (Set Code)
               , linkRemoves : Map Name (Set Code) }

-- names that have been added AND removed
MetadataEdit.conflicts : MetadataEdit -> Set Name

instance Monoid MetadataEdit where
  mempty = MetadataEdit mempty mempty mempty mempty
  me1 `mappend` me2 =
    MetadataEdit (nameAdds me1 `Set.union` nameAdds me2)
                 (nameRemoves me1 `Set.union` nameRemoves me2)
                 (Map.unionWith Set.union (linkAdds me1) (linkAdds me2))
                 (Map.unionWith Set.union (linkRemoves me1) (linkRemoves me2))
```

The monoid just unions the `Set Name` and for colliding keys in `linkAdds` and `linkRemoves`, unions the `Set Code` from the two maps.

The `Edit` type denotes a function `Code -> Code`, though we represent it in a way that can be merged:

```
data Edit
  = Replace Code Typing
  | SwapArguments Permutation -- optional idea for more semantic edits
  ..
  | Conflict (Set Edit)

data Typing = Same | Subtype | Different
```

The `Typing` indicates whether the replacement `Code` is the same type as the old `Code`, a subtype of it, or a different type. This is useful for knowing how far we can automatically propagate a `Changeset`.

The `Edit` type produces a `Conflict` when merged, though with more structured edits (note the `SwapArguments` data constructor), even more could be done here.

```haskell
instance Monoid Edit where
  mempty = Conflict mempty
  Conflict e `mappend` e2 | Set.null e = e2
  e `mappend` Conflict e2 | Set.null e2 = e
  Replace c t `mappend` Replace c2 _ | c == c2 = Replace c t

  SwapArguments p1 `mappend` SwapArguments p2 | Permutation.commutes p1 p2 =
    SwapArguments (Permutation.compose p1 p2)

  e `mappend` e2 = Conflict (Set.union (flat e) (flat e2)) where
    flat (Conflict e) = flat e
    flat e = Set.singleton e
```

A `Changeset` is _complete_ when it either covers the entire codebase or when it can be expanded to cover the whole codebase because its "frontier" which the codebase depends on consists of only type preserving edits. More precisely, the a `Changeset`, `c`, is complete with respect to a `Codebase`, `cb`, when all dependents (from `cb`) of type-changing edits (this includes deprecation) also have a non-conflicted edit in `c`.

If we want to measure how much work is remaining to complete a `c : Changeset` with respect to `cb : Codebase`, we can count the transitive dependents of all _escaped dependents_ of type-changing edits in the `Changeset`. An _escaped dependent_ is in `cb` but not `c`. This number will decrease monotonically as the `Changeset` is worked on.

_Related:_ There are some useful computations we can do to suggest which dependents of the frontier to upgrade next, based on what will make maximal progress in decreasing the remaining work. The idea is that it's useful to focus first on the "trunk" of a refactoring, which lots of code depend on, rather than the branches and leaves. Programmers sometimes try to do something like this when refactoring, but it can be difficult to know what's what when the main feedback you get from the compiler is just a big list of compile errors.

We also typically want to encourage the user to work on updates by expanding outward from initial changes, such that the set of edits form a connected dependency graph. If the user "skips over" nodes in the graph, there's a chance they may need to redo their work, and we should notify the user about this. It's not something we need to prevent but we don't want the user to be unaware that it's happening.

To apply a complete `Changeset` to a `Codebase`, we interpret the `Changeset` as a `Codebase -> Codebase`. There are some interesting decisions about how to do this, but here's one implementation:

```haskell
apply : Changeset -> Codebase -> Codebase
apply c cb = Codebase (added c `Set.union` code cb) todo
```

Notice that with this implementation a `Changeset` can talk about upgrades and edits to functions, without having to know what they are called! This makes changesets more portable as they can still be shared with people who might have different local names for things.

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
