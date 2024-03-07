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
module Unison.Codebase.Branch.DeclCoherencyCheck
  ( IncoherentDeclReason (..),
    checkDeclCoherency,
  )
where

import Control.Lens (view, (%=), (.=))
import Control.Monad.Except (ExceptT)
import Control.Monad.Except qualified as Except
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Except qualified as Except (except)
import Data.Functor.Compose (Compose (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import U.Codebase.Reference (Reference' (..), TypeReference, TypeReferenceId)
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Sqlite (Transaction)
import Unison.Util.Defns (Defns (..))
import Unison.Util.Map qualified as Map (deleteLookup, upsertF)
import Unison.Util.Nametree (Nametree (..))

data IncoherentDeclReason
  = -- | A second naming of a constructor was discovered underneath a decl's name, e.g.
    --
    --   Foo#Foo
    --   Foo.Bar#Foo#0
    --   Foo.Some.Other.Name.For.Bar#Foo#0
    IncoherentDeclReason'ConstructorAlias !Name !Name
  | IncoherentDeclReason'MissingConstructorName !Name
  | IncoherentDeclReason'NestedDeclAlias !Name
  | IncoherentDeclReason'NoConstructorNames !Name
  | IncoherentDeclReason'StrayConstructor !Name

checkDeclCoherency ::
  (TypeReferenceId -> Transaction Int) ->
  (Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference))) ->
  Transaction (Either IncoherentDeclReason (Map Name [Name]))
checkDeclCoherency loadDeclNumConstructors =
  Except.runExceptT
    . fmap (view #declNames)
    . (`State.execStateT` DeclCoherencyCheckState Map.empty Map.empty)
    . go []
  where
    go ::
      [NameSegment] ->
      ( Nametree
          (Defns (Map NameSegment Referent) (Map NameSegment TypeReference))
      ) ->
      StateT DeclCoherencyCheckState (ExceptT IncoherentDeclReason Transaction) ()
    go prefix (Nametree Defns {terms, types} children) = do
      for_ (Map.toList terms) \case
        (_, Referent.Ref _) -> pure ()
        (_, Referent.Con (ReferenceBuiltin _) _) -> pure ()
        (name, Referent.Con (ReferenceDerived typeRef) conId) -> do
          DeclCoherencyCheckState {expectedConstructors} <- State.get
          expectedConstructors1 <- lift (Except.except (Map.upsertF f typeRef expectedConstructors))
          #expectedConstructors .= expectedConstructors1
          where
            f :: Maybe (IntMap MaybeConstructorName) -> Either IncoherentDeclReason (IntMap MaybeConstructorName)
            f = \case
              Nothing -> Left (IncoherentDeclReason'StrayConstructor (fullName name))
              Just expected -> IntMap.alterF g (fromIntegral @Word64 @Int conId) expected
                where
                  g :: Maybe MaybeConstructorName -> Either IncoherentDeclReason (Maybe MaybeConstructorName)
                  g = \case
                    Nothing -> error "didnt put expected constructor id"
                    Just NoConstructorNameYet -> Right (Just (YesConstructorName (fullName name)))
                    Just (YesConstructorName firstName) -> Left (IncoherentDeclReason'ConstructorAlias firstName (fullName name))

      childrenWeWentInto <-
        forMaybe (Map.toList types) \case
          (_, ReferenceBuiltin _) -> pure Nothing
          (name, ReferenceDerived typeRef) -> do
            DeclCoherencyCheckState {expectedConstructors} <- State.get
            whatHappened <- do
              let recordNewDecl ::
                    Maybe (IntMap MaybeConstructorName) ->
                    Compose (ExceptT IncoherentDeclReason Transaction) WhatHappened (IntMap MaybeConstructorName)
                  recordNewDecl =
                    Compose . \case
                      Just _ -> Except.throwError (IncoherentDeclReason'NestedDeclAlias typeName)
                      Nothing ->
                        lift (loadDeclNumConstructors typeRef) <&> \case
                          0 -> UninhabitedDecl
                          n -> InhabitedDecl (IntMap.fromAscList [(i, NoConstructorNameYet) | i <- [0 .. n - 1]])
              lift (getCompose (Map.upsertF recordNewDecl typeRef expectedConstructors))
            case whatHappened of
              UninhabitedDecl -> pure Nothing
              InhabitedDecl expectedConstructors1 -> do
                child <-
                  Map.lookup name children & onNothing do
                    Except.throwError (IncoherentDeclReason'NoConstructorNames typeName)
                #expectedConstructors .= expectedConstructors1
                go (name : prefix) child
                DeclCoherencyCheckState {expectedConstructors} <- State.get
                -- fromJust is safe here because we upserted `typeRef` key above
                let (fromJust -> maybeConstructorNames, expectedConstructors1) =
                      Map.deleteLookup typeRef expectedConstructors
                constructorNames <-
                  unMaybeConstructorNames maybeConstructorNames & onNothing do
                    Except.throwError (IncoherentDeclReason'MissingConstructorName typeName)
                #expectedConstructors .= expectedConstructors1
                #declNames %= Map.insert typeName constructorNames
                pure (Just name)
            where
              typeName = fullName name

      let childrenWeHaventGoneInto = children `Map.withoutKeys` Set.fromList childrenWeWentInto
      for_ (Map.toList childrenWeHaventGoneInto) \(name, child) -> go (name : prefix) child
      where
        fullName name =
          Name.fromReverseSegments (name :| prefix)

data DeclCoherencyCheckState = DeclCoherencyCheckState
  { expectedConstructors :: !(Map TypeReferenceId (IntMap MaybeConstructorName)),
    declNames :: !(Map Name [Name])
  }
  deriving stock (Generic)

data MaybeConstructorName
  = NoConstructorNameYet
  | YesConstructorName !Name

unMaybeConstructorNames :: IntMap MaybeConstructorName -> Maybe [Name]
unMaybeConstructorNames =
  traverse f . IntMap.elems
  where
    f :: MaybeConstructorName -> Maybe Name
    f = \case
      NoConstructorNameYet -> Nothing
      YesConstructorName name -> Just name

data WhatHappened a
  = UninhabitedDecl
  | InhabitedDecl !a
  deriving stock (Functor, Show)
