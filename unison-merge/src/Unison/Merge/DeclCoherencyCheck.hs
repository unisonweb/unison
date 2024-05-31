-- | The "decl coherency check": a type declaration in a namespace is "coherent" if it satisfies both of the following
-- criteria.
--
--   1. For each naming of the type decl (say "Foo"#foohash), there exists exactly one name for each of its constructors
--      arbitrarily deep in the corresponding namespace ("Foo" in this example).
--
--      This allows us to render the decl naturally, as in
--
--        structural type Foo
--          = Bar Nat Int
--          | internal.hello.Bonk Nat
--
--      which corresponds to the three names
--
--        "Foo"                     => #foohash
--        "Foo.Bar"                 => #foohash#0
--        "Foo.internal.hello.Bonk" => #foohash#1
--
--      We could not do if there was at least one constructor whose full name does not contain the full name of the type
--      decl itself as a prefix.
--
--      A notable consequence of this requirement is that a second naming of a decl (i.e. an alias) cannot be embedded
--      within the first naming, as in:
--
--        type Foo = ...
--        type Foo.some.inner.namespace = ... -- an alias of Foo
--
--   2. No constructor has a "stray" name that does not have a prefix that equals the type declaration's name. For
--      example, in the namespace
--
--        "Foo"                 => #foohash
--        "Foo.Bar"             => #foohash#0
--        "Deep.What.SomeAlias" => #foohash#0
--
--      the constructor "What.SomeAlias" is "stray", as the type decl #foohash has no name that matches any prefix
--      (i.e. "Deep.What" nor "Deep").
--
-- On to the implementation. We are going to traverse the namespace depth-first. As we go, we have a stateful mapping
-- between decl reference that we *have* seen a name for in one of our parent namespace, and its corresponding set of
-- constructors that we *haven't* yet seen names for, but expect to, before fully searching the corresponding
-- sub-namespace (e.g. the child namespace named "Foo" of the namepace that declares a decl "Foo").
--
-- When processing a namespace, we first process all terms. Each constructor will fall into one of three cases:
--
-- > +----------------------------------------------------------------------------------------------------------------+
-- > | Case         | Mapping before       | Encountered constructor | Mapping after                                  |
-- > +----------------------------------------------------------------------------------------------------------------+
-- > | Happy path   | { #foo : {0, 1, 2} } | #foo#1                  | { #foo : {0, 2} }                              |
-- > | Already seen | { #foo : {0, 1, 2} } | #foo#5                  | Error: duplicate naming for constructor #foo#5 |
-- > | Never seen   | { #foo : {0, 1, 2} } | #bar#2                  | Error: stray constructor #bar#2                |
-- > +----------------------------------------------------------------------------------------------------------------+
--
-- In "happy path", we see a naming of a constructor that we're expecting, and check it off.
-- In "already seen", we see a second naming of a constructor that we're no longer expecting, and fail.
-- In "never seen", we see a naming of a constructor before any naming of its decl, so we fail.
--
-- Next, we process all type decls. Each will again fall into one of three cases:
--
-- > +-----------------------------------------------------------------------------------------------------+
-- > | Case             | Mapping before       | Declaration | Num constructors | New mapping              |
-- > +-----------------------------------------------------------------------------------------------------+
-- > | Uninhabited decl |                      | #foo        | 0                |                          |
-- > | Inhabited decl   |                      | #foo        | 1 or more        | { #foo : {0, ..., n-1} } |
-- > | Already seen     | { foo : {0, 1, 2}  } | #foo        | Irrelevant       | Error: nested decl alias |
-- > +-----------------------------------------------------------------------------------------------------+
--
-- In "uninhabited decl", we find a decl with no constructors, so we don't expect anything new.
-- In "already seen", we find a second naming of a decl, whose constructors will necessarily violate coherency condition
--   (1) above.
--
-- In "inhabited decl", we find a decl with N constructors, and handle it by:
--   1. Adding to our state that we expect a name for each.
--   2. Recursing into the child namespace whose name matches the decl.
--   3. (If we return from the recursion without short-circuiting) remove the mapping added in step (1) and assert that
--      its value is the empty set (meaning we encountered a name for every constructor).
--
-- Note: This check could be moved into SQLite (with sufficient schema support) some day, but for now, we just do this
-- in memory.
--
-- Note: once upon a time, decls could be "incoherent". Then, we decided we want decls to be "coherent". Thus, this
-- machinery was invented.
module Unison.Merge.DeclCoherencyCheck
  ( IncoherentDeclReason (..),
    checkDeclCoherency,
    lenientCheckDeclCoherency,
  )
where

import Control.Lens ((%=), (.=), _2)
import Control.Monad.Except (ExceptT)
import Control.Monad.Except qualified as Except
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Except qualified as Except (except)
import Data.Functor.Compose (Compose (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import U.Codebase.Reference (Reference' (..), TypeReference, TypeReferenceId)
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.Merge.DeclNameLookup (DeclNameLookup (..))
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Map qualified as Map (deleteLookup, deleteLookupJust, upsertF)
import Unison.Util.Nametree (Nametree (..))

data IncoherentDeclReason
  = -- | A second naming of a constructor was discovered underneath a decl's name, e.g.
    --
    --   Foo#Foo
    --   Foo.Bar#Foo#0
    --   Foo.Some.Other.Name.For.Bar#Foo#0
    IncoherentDeclReason'ConstructorAlias !Name !Name
  | IncoherentDeclReason'MissingConstructorName !Name
  | -- | A second naming of a decl was discovered underneath its name, e.g.
    --
    --   Foo#Foo
    --   Foo.Bar#Foo
    IncoherentDeclReason'NestedDeclAlias !Name !Name -- shorter name, longer name
  | IncoherentDeclReason'StrayConstructor !Name

checkDeclCoherency ::
  forall m.
  Monad m =>
  (TypeReferenceId -> m Int) ->
  Nametree (DefnsF (Map NameSegment) Referent TypeReference) ->
  m (Either IncoherentDeclReason DeclNameLookup)
checkDeclCoherency loadDeclNumConstructors =
  Except.runExceptT
    . fmap (view #declNameLookup)
    . (`State.execStateT` DeclCoherencyCheckState Map.empty (DeclNameLookup Map.empty Map.empty))
    . go []
  where
    go ::
      [NameSegment] ->
      (Nametree (DefnsF (Map NameSegment) Referent TypeReference)) ->
      StateT DeclCoherencyCheckState (ExceptT IncoherentDeclReason m) ()
    go prefix (Nametree defns children) = do
      for_ (Map.toList defns.terms) \case
        (_, Referent.Ref _) -> pure ()
        (_, Referent.Con (ConstructorReference (ReferenceBuiltin _) _) _) -> pure ()
        (name, Referent.Con (ConstructorReference (ReferenceDerived typeRef) conId) _) -> do
          DeclCoherencyCheckState {expectedConstructors} <- State.get
          expectedConstructors1 <- lift (Except.except (Map.upsertF f typeRef expectedConstructors))
          #expectedConstructors .= expectedConstructors1
          where
            f ::
              Maybe (Name, ConstructorNames) ->
              Either IncoherentDeclReason (Name, ConstructorNames)
            f = \case
              Nothing -> Left (IncoherentDeclReason'StrayConstructor name1)
              Just (typeName, expected) ->
                case recordConstructorName conId name1 expected of
                  Left existingName -> Left (IncoherentDeclReason'ConstructorAlias existingName name1)
                  Right expected1 -> Right (typeName, expected1)
              where
                name1 = fullName name

      childrenWeWentInto <-
        forMaybe (Map.toList defns.types) \case
          (_, ReferenceBuiltin _) -> pure Nothing
          (name, ReferenceDerived typeRef) -> do
            DeclCoherencyCheckState {expectedConstructors} <- State.get
            whatHappened <- do
              let recordNewDecl ::
                    Maybe (Name, ConstructorNames) ->
                    Compose (ExceptT IncoherentDeclReason m) WhatHappened (Name, ConstructorNames)
                  recordNewDecl =
                    Compose . \case
                      Just (shorterTypeName, _) -> Except.throwError (IncoherentDeclReason'NestedDeclAlias shorterTypeName typeName)
                      Nothing ->
                        lift (loadDeclNumConstructors typeRef) <&> \case
                          0 -> UninhabitedDecl
                          n -> InhabitedDecl (typeName, emptyConstructorNames n)
              lift (getCompose (Map.upsertF recordNewDecl typeRef expectedConstructors))
            case whatHappened of
              UninhabitedDecl -> do
                #declNameLookup . #declToConstructors %= Map.insert typeName []
                pure Nothing
              InhabitedDecl expectedConstructors1 -> do
                child <-
                  Map.lookup name children & onNothing do
                    Except.throwError (IncoherentDeclReason'MissingConstructorName typeName)
                #expectedConstructors .= expectedConstructors1
                go (name : prefix) child
                DeclCoherencyCheckState {expectedConstructors} <- State.get
                -- fromJust is safe here because we upserted `typeRef` key above
                let (fromJust -> (_typeName, maybeConstructorNames), expectedConstructors1) =
                      Map.deleteLookup typeRef expectedConstructors
                constructorNames <-
                  sequence (IntMap.elems maybeConstructorNames) & onNothing do
                    Except.throwError (IncoherentDeclReason'MissingConstructorName typeName)
                #expectedConstructors .= expectedConstructors1
                #declNameLookup . #constructorToDecl %= \constructorToDecl ->
                  List.foldl'
                    (\acc constructorName -> Map.insert constructorName typeName acc)
                    constructorToDecl
                    constructorNames
                #declNameLookup . #declToConstructors %= Map.insert typeName constructorNames
                pure (Just name)
            where
              typeName = fullName name

      let childrenWeHaventGoneInto = children `Map.withoutKeys` Set.fromList childrenWeWentInto
      for_ (Map.toList childrenWeHaventGoneInto) \(name, child) -> go (name : prefix) child
      where
        fullName name =
          Name.fromReverseSegments (name :| prefix)

-- | A lenient variant of 'checkDeclCoherency' - so lenient it can't even fail! It returns a mapping from decl name to
-- constructor names, where constructor names can be missing.
--
-- This function exists merely to extract a best-effort decl-name-to-constructor-name mapping for the LCA of a merge.
-- We require Alice and Bob to have coherent decls, but their LCA is out of the user's control and may have incoherent
-- decls, and whether or not it does, we still need to compute *some* syntactic hash for its decls.
lenientCheckDeclCoherency ::
  forall m.
  Monad m =>
  (TypeReferenceId -> m Int) ->
  Nametree (DefnsF (Map NameSegment) Referent TypeReference) ->
  m (Map Name [Maybe Name])
lenientCheckDeclCoherency loadDeclNumConstructors =
  fmap (view #declToConstructors)
    . (`State.execStateT` LenientDeclCoherencyCheckState Map.empty Map.empty)
    . go []
  where
    go ::
      [NameSegment] ->
      (Nametree (DefnsF (Map NameSegment) Referent TypeReference)) ->
      StateT LenientDeclCoherencyCheckState m ()
    go prefix (Nametree defns children) = do
      for_ (Map.toList defns.terms) \case
        (_, Referent.Ref _) -> pure ()
        (_, Referent.Con (ConstructorReference (ReferenceBuiltin _) _) _) -> pure ()
        (name, Referent.Con (ConstructorReference (ReferenceDerived typeRef) conId) _) -> do
          #expectedConstructors %= Map.adjust (Map.map (lenientRecordConstructorName conId (fullName name))) typeRef

      childrenWeWentInto <-
        forMaybe (Map.toList defns.types) \case
          (_, ReferenceBuiltin _) -> pure Nothing
          (name, ReferenceDerived typeRef) -> do
            whatHappened <- do
              let recordNewDecl :: m (WhatHappened (Map Name ConstructorNames))
                  recordNewDecl =
                    loadDeclNumConstructors typeRef <&> \case
                      0 -> UninhabitedDecl
                      n -> InhabitedDecl (Map.singleton typeName (emptyConstructorNames n))
              state <- State.get
              lift (getCompose (Map.upsertF (\_ -> Compose recordNewDecl) typeRef state.expectedConstructors))
            case whatHappened of
              UninhabitedDecl -> do
                #declToConstructors %= Map.insert typeName []
                pure Nothing
              InhabitedDecl expectedConstructors1 -> do
                let child = Map.findWithDefault (Nametree (Defns Map.empty Map.empty) Map.empty) name children
                #expectedConstructors .= expectedConstructors1
                go (name : prefix) child
                state <- State.get
                let (maybeConstructorNames, expectedConstructors) =
                      Map.alterF f typeRef state.expectedConstructors
                      where
                        f ::
                          Maybe (Map Name ConstructorNames) ->
                          (ConstructorNames, Maybe (Map Name ConstructorNames))
                        f =
                          -- fromJust is safe here because we upserted `typeRef` key above
                          -- deleteLookupJust is safe here because we upserted `typeName` key above
                          fromJust
                            >>> Map.deleteLookupJust typeName
                            >>> over _2 \m -> if Map.null m then Nothing else Just m
                #expectedConstructors .= expectedConstructors
                #declToConstructors %= Map.insert typeName (IntMap.elems maybeConstructorNames)
                pure (Just name)
            where
              typeName = fullName name

      let childrenWeHaventGoneInto = children `Map.withoutKeys` Set.fromList childrenWeWentInto
      for_ (Map.toList childrenWeHaventGoneInto) \(name, child) -> go (name : prefix) child
      where
        fullName name =
          Name.fromReverseSegments (name :| prefix)

data DeclCoherencyCheckState = DeclCoherencyCheckState
  { expectedConstructors :: !(Map TypeReferenceId (Name, ConstructorNames)),
    declNameLookup :: !DeclNameLookup
  }
  deriving stock (Generic)

data LenientDeclCoherencyCheckState = LenientDeclCoherencyCheckState
  { expectedConstructors :: !(Map TypeReferenceId (Map Name ConstructorNames)),
    declToConstructors :: !(Map Name [Maybe Name])
  }
  deriving stock (Generic)

-- A partial mapping from constructor id to name; a collection of constructor names starts out with the correct number
-- of keys (per the number of data constructors) all mapped to Nothing. Then, as names are discovered by walking a
-- name tree, Nothings become Justs.
type ConstructorNames =
  IntMap (Maybe Name)

-- Make an empty set of constructor names given the number of constructors.
emptyConstructorNames :: Int -> ConstructorNames
emptyConstructorNames numConstructors =
  IntMap.fromAscList [(i, Nothing) | i <- [0 .. numConstructors - 1]]

recordConstructorName :: HasCallStack => ConstructorId -> Name -> ConstructorNames -> Either Name ConstructorNames
recordConstructorName conId conName =
  IntMap.alterF f (fromIntegral @Word64 @Int conId)
  where
    f :: Maybe (Maybe Name) -> Either Name (Maybe (Maybe Name))
    f = \case
      Nothing -> error (reportBug "E397219" ("recordConstructorName: didn't expect constructor id " ++ show conId))
      Just Nothing -> Right (Just (Just conName))
      Just (Just existingName) -> Left existingName

lenientRecordConstructorName :: ConstructorId -> Name -> ConstructorNames -> ConstructorNames
lenientRecordConstructorName conId conName =
  IntMap.adjust f (fromIntegral @Word64 @Int conId)
  where
    f :: Maybe Name -> Maybe Name
    f = \case
      Nothing -> Just conName
      -- Ignore constructor alias, just keep first name we found
      Just existingName -> Just existingName

data WhatHappened a
  = UninhabitedDecl
  | InhabitedDecl !a
  deriving stock (Functor, Show)
