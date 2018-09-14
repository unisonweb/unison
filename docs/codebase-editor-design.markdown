Note: initial draft, probably a lot of rough edges. Comments/questions/ideas are welcome!

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

Here it is: _Unison definitions are identified by content._ Therefore, there's no such thing as changing a definition, there's only introducing new definitions. What can change is the mapping between definition and human-friendly names. e.g. `x -> x + 1` (a definition) vs `Integer.increment` (a name we associate with it for the purposes of writing and reading other code that references it).

> Unison definitions are like stars in the sky. You can discover new ones, but the ones that are there don't change.  Names are what you write on your map of the sky. The stars themselves don't know or care :)
> — @atacratic

From this simple idea, we can build a better development experience around codebase editing with all of the above benefits.

## The model

This section gives the model of what a `Codebase` is, what a `Changeset` is, what their API is. Later we'll cover what the actual user experience is for interacting with the model, along with various concrete usage scenarios.

A `Codebase` denotes two things, a `Set Code` (a set of definitions), and a `Map Code (Set Metadata)`. `Code` could be a function or value definition (a `Term`) or a `TypeDeclaration`. `Metadata` includes things like:

* Human-friendly name
* Link to documentation, which is also just a `Term`
* Link to LICENSE, which is also just a `Term`
* Link to author, date created, previous version, etc ..

Each `Term` in the `Codebase` also includes its `Type`. A Unison codebase contains no ill-typed terms.

At a high level, a `Changeset` denotes a function from `Codebase` to `Codebase`, but we don't literally use the representation `Codebase -> Codebase`. We want a representation that can be converted to a `Codebase -> Codebase` but which also comes equipped with a commutative merge operation that allows multiple developers to collaborate on building a `Changeset`. This section gives a model for that.

As mentioned, a codebase is a `Set Code` and a `Map Code (Set Metadata)`. We'll have access to a the function `Code.dependencies`, which given a `c : Code` returns the set of other `Code` that appear in the definition of `c`.

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

We'll say what `Edit` and `MetadataEdit` are momentarily, but the commutative monoid for `Changeset` combines corresponding elements of the `Changeset`:

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
MetadataEdit.nameConflicts : MetadataEdit -> Set Name
MetadataEdit.linkConflicts : MetadataEdit -> Set Name

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

```haskell
data Edit
  = Replace Code Typing
  | SwapArguments Permutation -- optional idea for more semantic edits
  ..
  | Conflict (Set Edit)

data Typing = Same | Subtype | Different
```

The `Typing` indicates whether the replacement `Code` is the same type as the old `Code`, a subtype of it, or a different type. This is useful for knowing how far we can automatically propagate a `Changeset`.

The `Edit` type produces a `Conflict` when merged, though with more structured edits (*e.g.*, in the case of the `SwapArguments` data constructor), even more could be done here.

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

A Changeset is complete when it either covers the entire codebase or when it has been developed to the point that the remaining updates are type-preserving and can thus be applied automatically. More precisely, a Changeset `c` is complete with respect to a Codebase `cb`, when all dependents in `cb` of type-changing edits in `c` (including deprecations) also have an edit in `c`, and none of the edits are in a conflicted state.

If we want to measure how much work is remaining to complete a Changeset `c` with respect to Codebase `cb`, we can count the transitive dependents of all _escaped dependents_ of type-changing edits in `c`. An _escaped dependent_ is in `cb` but not `c`. This number will decrease monotonically as the Changeset is developed.

_Related:_ There are some useful computations we can do to suggest which dependents of the frontier to upgrade next, based on what will make maximal progress in decreasing the remaining work. The idea is that it's useful to focus first on the "trunk" of a refactoring, which lots of code depend on, rather than the branches and leaves. Programmers sometimes try to do something like this when refactoring, but it can be difficult to know what's what when the main feedback you get from the compiler is just a big list of compile errors.

We also typically want to encourage the user to work on updates by expanding outward from initial changes, such that the set of edits form a connected dependency graph. If the user "skips over" nodes in the graph, there's a chance they'll need to redo their work, and we should notify the user about this. It's not something we need to prevent but we want the user to be aware that it's happening.

To apply a complete `Changeset` to a `Codebase`, we interpret the `Changeset` as a `Codebase -> Codebase`. There are some interesting decisions about how to do this, but here's one implementation, which changes the name for old versions of the code to include a new prefix, based on a hash of the `Changeset` itself.

```haskell
Changeset.hash : Changeset -> Name
Changeset.hash c = ...

Changeset.isComplete : Changeset -> Codebase -> Bool
Changeset.isComplete c cb = -- as discussed

Changeset.apply : Changeset -> Codebase -> Maybe Codebase
Changeset.apply c cb | isComplete c cb =
  substitutions = [(k, v) | (k, Replace v _) <- Map.toList (edited c) ]
  nameOf k = Map.lookup k (metadata cb)
  Codebase (added c `Set.union` code cb) todo
Changeset.apply _ _ = Nothing
```

Notice that with this implementation a `Changeset` can talk about upgrades and edits to functions, without having to know what they are called! This makes changesets more portable as they can still be shared with people who might have different local names for things.

This is it for the model. The rest of this document focuses on how to expose this nice model for use by the Unison programmer.

## The developer experience

Alice is an employee of Acme, Inc. She comes into work with a brilliant idea for a new function. She's not sure what to call this function yet and she's terrible at naming things, so she puts the function, initially called `wrangle` in her "sandbox" namespace, `Alice.scratch`. (`Alice` may be her GitHub username or anything else that's pretty unique) The function starts its life with a fully qualified name (FQN) of `Alice.scratch.wrangle`. She commits this to her local Unison repository and then pushes to the company's central repo on GitHub.

Bob, meanwhile, is a fellow Acme employee. He happens to notice the commit fly by. He tells unison `> view Alice.sandbox.wrangle`, which pretty-prints the implementation along with any documentation. In awe at its brilliance, he pings Alice over work chat: "Alice, my gosh, you've done it!! You've solved the exact optimization problem I've been struggling with for the past six months! This function is... _awesome_ and I strongly suggest we move it to `Acme.awesome`. Is this cool? I'd like to start using it in the code I'm writing." Alice: "TOTALLY, Bob, go for it". High fives all around. 🙌

Bob tells Unison: `> move Alice.sandbox.wrangle Acme.awesome`. The rename happens instantly and is 100% accurate. Bob commits and pushes, and Alice pulls, which obtains the new name.

Some time passes, the `Acme.awesome` function starts getting used all over the `Acme` codebase, inspiring Alice to come up with an even nicer implementation. Unfortunately, it requires modifying the type signature slightly. She tells Bob, and they collaborate on a changeset which fully updates their repository, using the commutative merge operation discussed above. They don't need to do anything special for this merging to happen, just a regular `git pull`.

They start the changeset with a simple `> edit Acme.awesome` and are guided through propagating it fully via the friendly Unison codebase editor. It's easy, controlled, and they monitor their progress just via the "remaining work" statistic that the codebase editor helpfully displays for any in-progress changeset. When they're done, they `> apply Acme.awesomeUpgrade` to update the codebase. At each point in time, they remain in full control, without a big list of compile errors.

Alice and Bob later decide, after much discussion, that they prefer the name `Acme.rad` for this function and apply this renaming to their repository. Again this happens instantaneously and with 100% accuracy. Doing `> view foo` for a definition, `foo`, which references `Acme.rad` prints the updated name.

__Aside:__ We can think of `apply` as really taking two arguments, the changeset itself, and some prefix for the new definitions. If we have a complete changeset with respect to a codebase and the prefix is empty, we are saying we want the names for the new versions of definitions to clobber the old definitions (perhaps the old definitions get the changeset id prefixed to them). If the changeset is incomplete, perhaps the developer is forced to pick a prefix for the new definitions, so that the new `Acme.awesome` is actually put under `v2.Acme.awesome`. And even if the changeset _is_ complete the developer may choose to still put the new definitions under a namespace like `v2`, so that they can conveniently refer to both versions just with a qualified import.

### Publishing

Feeling pretty good about all this, Alice and Bob decide to publish the `Acme.rad` function for use by other folks outside of `Acme, Inc`. They do so just by sharing a URL that links to their GitHub repository. There's no separate step of creating some artifact like a jar and uploading that to some third-party package repository. That URL is something like `https://acme.github.io/unison/QjdBS8sdbWdj`, where the `QjdBS8sdbWdj` is a Base 58 encoding of a particular Unison hash. The GitHub repository format for Unison doubles as a GitHub pages site so anyone can explore the repository from that point, obtaining pretty-printed and hyperlinked source code, pretty HTML documentation, and so on.

Carol, an individual developer and a fan of Acme's work, tells Unison `> get https://acme.github.io/unison/QjdBS8sdbWdj`, which pulls down that definition and its transitive dependencies to her local repository. Dependencies are fetched at the most fine-grained level possible (individual definitions) rather than at the level of "whole libraries", so the size of the transfer is quite small and happens instantaneously. (Carol most definitely does NOT have to sync the entire Acme codebase of which she is just using 1 function)

__Note:__ In the event of naming conflicts when doing a `get`, Unison might warn Carol or ask if she'd like to attach a prefix to all the names imported via the `get`.

Carol starts using `Acme.rad` in her code, as does a development team of 10 at Dave's Global Megacorp Incorporated (DGMI). The DGMI developers don't like the `Acme.rad` name so much and rename it locally to `DGMI.Imported.Acme.widgetOptimizationRoutine`.

Then the following happens:

* Alice and Bob find an even more amazing implementation of their function, and it's type-preserving. They create a changeset for it and share a link to the changeset. Carol decides not to upgrade as the function was just an internal implementation detail for her library. She ignores the changeset, and perhaps decides to just rename the function to `Carol.Imported.Acme.rad`, effectively "forking" this single function, which she will maintain going forward.
* The 10 person DGMI team decides they want to upgrade to the latest `Acme.rad`, and collaborate on a changeset to complete the upgrade in their repository. They do so by actually _extending_ the changeset published by Acme, Inc, until it is complete with respect to their codebase. Despite the differing names (`Acme.rad` vs `DGMI.Imported.Acme.widgetOptimizationRoutine`), the changeset from Acme still works fine as a starting point, as the target of edits is always based on the content of the definition being edited, _not its name_.
* DGMI publishes this extended changeset, which users of the DGMI codebase can extend further, and so on.

## Repository format

A design goal of the repository format is that it can be versioned using Git (or Hg, or whatever), and there should never be merge conflicts when merging two Unison repositories. That is, Git merge conflicts are a bad UX for surfacing concurrent edits that the user may wish to reconcile.

```text
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

**Arya**: I'm still thinking we'll want something like scopes to be able to apply a changeset to a prefix in a "clone package foo.x to foo.y and apply these changes" sort of wway.
