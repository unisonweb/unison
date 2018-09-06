

###

```
terms/
  reference-english-JasVXOEBBV8.markdown -- the reference docs ref'd below
  license-8JSJdkVvvow92.markdown
  ...
  jAjGDJnsdfL.ub -- binary form of the term
  jAjGDJnsdfL/
    source-98asdfjKjsldfj.markdown -- source code, in markdown form
    Runar.factorial.name -- just an empty file
    math.factorial.name
    reference-english-JasVXOEBBV8.hash -- reference docs, in English
    reference-spanish-9JasdfjHNBdjj.hash -- reference docs
    doc-english-OD03VvvsjK.hash -- other docs
    license-8JSJdkVvvow92.hash -- reference to the license for this term
types/ -- directory of all type declarations
  8sdfA1baBw.ub -- binary form of the type declaration
  8sdfA1baBw/
    0/ -- constructor id, has a set of names
      Nil.name -- empty file
      Empty.name -- empty file
    1/ -- constructor id, has a set of names
      Cons.name
      Prepend.name
    reference-english-KgLfAIBw312.hash -- reference docs
    doc-english-8AfjKBCXdkw.hash -- other docs
```

`Id` is the basic type used to refer to things in a Unison codebase:

```Haskell
type Id
  = One Reference
  | Cycle [Hash] Int -- Int is which Hash in the `[Hash]` is being ref'd
```

`Scope` denotes a `Set Id`. We might build these up symbolically:

```Haskell
Scope.transitiveDependencies : Id -> Scope
Scope.transitiveDependents : Id -> Scope

Scope.union : Scope -> Scope -> Scope
-- maybe other set operations?
-- Scope.intersection : Scope -> Scope -> Scope
```

Here's the codebase API:

```Haskell
effect Codebase where

  -- Parse an expression
  Term.parse : Text ->{Codebase} Term

  -- Parse one or more type declarations
  TypeDeclarations.parse : Text ->{Codebase} TypeDeclarations

  -- Parse one or more term declarations
  TermDeclarations.parse : Text ->{Codebase} TermDeclarations

  -- Get the ids in a type declarations block
  TypeDeclarations.ids : TypeDeclarations -> Map Name Id

  -- Pretty-print some type declarations
  TypeDeclarations.text : TypeDeclarations ->{Codebase} Text

  -- Pretty-print some term declarations
  TermDeclarations.text : TermDeclarations ->{Codebase} Text

  -- optional : TypeDeclarations.Html : TypeDeclarations ->{Codebase} Html

  -- Get the ids in a type declarations block
  TermDeclarations.ids : TermDeclarations -> Map Name Id

  -- Actually add one or more type declarations to the codebase
  TypeDeclarations.add : TypeDeclarations ->{Codebase} ()

  -- Add one or more term declarations to the codebase
  TermDeclarations.add : TermDeclarations ->{Codebase} ()

  -- Resolve a name to zero or more ids with that name
  resolve : Name ->{Codebase} Set Id

  -- idea:
  -- search : Pattern ->{Codebase} Set Id

  -- Look up the names associated with an `Id`
  names : Id ->{Codebase} (Set Name)

  -- Add a name associated with the `Id`
  addName : Id -> Name ->{Codebase} ()

  -- Remove a name from association with the `Id`
  removeName : Id -> Name ->{Codebase} ()

  -- other metadata CRUD operations

  -- `addLicense id licenseId`
  addLicense : Id -> Id ->{Codebase} ()

  -- `addAuthor id authorId`
  addAuthor : Id -> Id ->{Codebase} ()

  getLicense : Id ->{Codebase} License
  getAuthor : Id ->{Codebase} Author

  -- The immediate dependencies of an `Id` (not transitive)
  dependencies : Id ->{Codebase} Set Id

  -- The immediate dependents of an `Id` (not transitive)
  dependents : Id ->{Codebase} Set Id

  -- The immediate ancestor of an `Id`
  ancestor : Id ->{Codebase} Id

  -- Evaluate a term to normal form
  run : Term ->{Codebase} Term
  runIO : Term ->{IO,Codebase} Term
  ...


  -- Create a new empty transaction, with the empty scope
  Transaction.new : Name ->{Codebase} Transaction

  -- Look up a transaction by name
  Transaction.resolve : Name ->{Codebase} Set Transaction

  -- Read the fields of a transaction

  -- The `Id`s that define the scope of the transaction
  Transaction.scope : Transaction ->{Codebase} Set Id

  -- Union the given `Scope` with the existing transaction's `Scope`.
  Transaction.addScope : Transaction -> Scope ->{Codebase} ()

  -- An element of the scope needs to have been updated if one of its
  -- dependencies has been edited or removed as part of the transaction.
  -- `remainder` is just a count of the elements of `scope` needing updating.
  -- Conceptually:
  -- length [ i | i <- Transactions.scope t,
                  Transaction.dependencies i `intersects` (
                    Transaction.editedSet t `union` Transaction.removed t),
                  Set.notSingleton $ Map.lookup i (Transaction.canonical t) ]
  Transaction.remainder : Transaction ->{Codebase} Natural

  -- We never delete from the `edited` set, we only accumulate.
  Transaction.edited : Transaction ->{Codebase} Map Id (Set Id)

  -- Convenience function, just pulls out keys of `edited`.
  Transaction.editedKeySet : Transaction ->{Codebase} Set Id

  -- This is always a submap of `edited` which reflects any conflict resolutions
  Transaction.canonical : Transaction ->{Codebase} Map Id (Set Id)

  -- These are definitions newly introduced as part of this transaction
  Transaction.added : Transaction ->{Codebase} Set Id

  -- These are definitions deleted as part of this transaction
  Transaction.removed : Transaction ->{Codebase} Set Id

  -- Add a definition to the `added` set of the transaction
  Transaction.add : Transaction -> Id ->{Codebase} ()

  -- Add a definition to the `removed` set of the transaction
  Transaction.remove : Transaction -> Id ->{Codebase} ()

  -- `edit t old new` adds `new` to the `edited` map for `old` and
  -- to the `canonical` map for `old`.
  Transaction.edit : Transaction -> Id -> Id ->{Codebase} ()

  -- `resolve id selected` replaces the canonical set with the selected
  -- `Id`, discarding any prior selections
  -- Discarded `Id` can be computed by comparing the `edited` (which has
  -- everything) and `canonical` (which has current selections)
  --
  -- What should this do if the selected `Id` is unrelated to any of the
  -- existing ids?
  Transaction.resolve : Transaction -> Id -> Id ->{Codebase} ()

  -- Actually apply the transaction to the codebase. Idempotent.
  -- What does this actually do???
  -- Presumably something with names...
  -- Arya proposal: If scope includes Id of a namespace / first-class
  --                naming preference object, then update that ... something something something
  -- Observation: Because you can have scopes that are smaller than "the entire
  --              codebase" (which isn't really a thing), git branches aren't
  --              a useful thing for tracking concurrent development (because
  --              git branches only work at the level of the whole git repo).
  -- Paul: seems like we need a `Branch`
  -- `Branch` assigns zero or more names to each Id mapping.
  -- `Transactions` are functions Branch -> Branch.
  -- `Transaction.commit` applies that function to a branch.
  Transaction.commit : Transaction -> Branch ->{Codebase} ()

  -- Branch might just be a name prefix. `<branch-name>.List.sort.name`
  -- You should be able to import some branch

  -- no Transaction.merge for now
```

User preferences wrt to name rendering? Id -> name preferences

