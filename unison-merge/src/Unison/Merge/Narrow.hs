module Unison.Merge.Narrow
  ( narrowDefns,
    narrowDefnsTotal,
  )
where

import Data.Map.Merge.Strict qualified as Map
import Data.Semialign (unalign)
import Data.These (These (..))
import U.Codebase.Reference (TypeReference)
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.DeclNameLookup qualified as DeclNameLookup
import Unison.Merge.ThreeWay (GThreeWay, ThreeWay)
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay (TwoWay)
import Unison.Merge.Updated (GUpdated (..), Updated)
import Unison.Merge.Updated qualified as Updated
import Unison.Name (Name)
import Unison.PartialDeclNameLookup (PartialDeclNameLookup (..))
import Unison.PartialDeclNameLookup qualified as PartialDeclNameLookup
import Unison.Prelude hiding (catMaybes)
import Unison.Reference (Reference' (..), TermReference, TypeReferenceId)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Defns qualified as Defns

-- `narrowDefns` takes and old and new namespace (and their respective decl name lookups), and returns old' and new'
-- namespaces, that contain only definitions that have a chance at having different syntactic hashes.
--
-- (It's not quite as simple as retaining only the definitions with non-equal Unison hashes, as a type declaration's
-- syntactic hash changes if any of its constructors are renamed, but its Unison hash does not).
narrowDefns ::
  (HasCallStack) =>
  GThreeWay PartialDeclNameLookup DeclNameLookup ->
  ThreeWay (DefnsF (Map Name) Referent TypeReference) ->
  TwoWay (Updated (DefnsF (Map Name) Referent TypeReference))
narrowDefns declNameLookups defns =
  narrowDefns1 <$> ThreeWay.gtoUpdated declNameLookups <*> ThreeWay.toUpdated defns

-- | Like 'narrowDefns', but just between the LCA and a branch head, and for when the LCA is known not to have any type
-- declarations with missing constructor names.
narrowDefnsTotal ::
  Updated DeclNameLookup ->
  Updated (DefnsF (Map Name) Referent TypeReference) ->
  Updated (DefnsF (Map Name) Referent TypeReference)
narrowDefnsTotal declNameLookups defns =
  Updated.zipWith
    Defns
    (narrowTermsTotal declNameLookups types)
    (narrowTypesTotal declNameLookups terms)
  where
    (types, terms) =
      Updated.unzipWith Defns.toPair defns

narrowDefns1 ::
  (HasCallStack) =>
  GUpdated PartialDeclNameLookup DeclNameLookup ->
  Updated (DefnsF (Map Name) Referent TypeReference) ->
  Updated (DefnsF (Map Name) Referent TypeReference)
narrowDefns1 declNameLookups defns =
  Updated.zipWith Defns (narrowTerms declNameLookups types) (narrowTypes declNameLookups terms)
  where
    (types, terms) =
      Updated.unzipWith Defns.toPair defns

narrowTerms ::
  (HasCallStack) =>
  GUpdated PartialDeclNameLookup DeclNameLookup ->
  Updated (Map Name Referent) ->
  Updated (Map Name Referent)
narrowTerms declNameLookup =
  filterOutEqualSynhash \name oldRef newRef ->
    case (oldRef, newRef) of
      -- Drop hash-equal terms
      TwoTerms x y -> x == y
      TwoBuiltinConstructors x y -> x == y
      -- Drop equal constructors only if they would have equal synhashes, i.e. their types have the same namings of
      -- constructors
      TwoNonBuiltinConstructors x y -> x == y && sameConstructorNames
        where
          sameConstructorNames = oldConstructorNames == map Just newConstructorNames
          oldConstructorNames = PartialDeclNameLookup.expectConstructorNames declNameLookup.old oldDeclName
          newConstructorNames = DeclNameLookup.expectConstructorNames declNameLookup.new newDeclName
          oldDeclName = PartialDeclNameLookup.expectDeclName declNameLookup.old name
          newDeclName = DeclNameLookup.expectDeclName declNameLookup.new name
      _ -> False

-- | Like 'narrowTerms', but for when the LCA is known not to have any type declarations with missing constructor names.
narrowTermsTotal ::
  (HasCallStack) =>
  Updated DeclNameLookup ->
  Updated (Map Name Referent) ->
  Updated (Map Name Referent)
narrowTermsTotal declNameLookup =
  filterOutEqualSynhash \name oldRef newRef ->
    case (oldRef, newRef) of
      -- Drop hash-equal terms
      TwoTerms x y -> x == y
      TwoBuiltinConstructors x y -> x == y
      -- Drop equal constructors only if they would have equal synhashes, i.e. their types have the same namings of
      -- constructors
      TwoNonBuiltinConstructors x y -> x == y && sameConstructorNames
        where
          sameConstructorNames = oldConstructorNames == newConstructorNames
          oldConstructorNames = DeclNameLookup.expectConstructorNames declNameLookup.old oldDeclName
          newConstructorNames = DeclNameLookup.expectConstructorNames declNameLookup.new newDeclName
          oldDeclName = DeclNameLookup.expectDeclName declNameLookup.old name
          newDeclName = DeclNameLookup.expectDeclName declNameLookup.new name
      _ -> False

pattern TwoTerms :: TermReference -> TermReference -> (Referent, Referent)
pattern TwoTerms x y <- (Referent.Ref x, Referent.Ref y)

pattern TwoBuiltinConstructors :: Text -> Text -> (Referent, Referent)
pattern TwoBuiltinConstructors x y <-
  ( Referent.Con (ConstructorReference (ReferenceBuiltin x) _) _,
    Referent.Con (ConstructorReference (ReferenceBuiltin y) _) _
    )

pattern TwoNonBuiltinConstructors :: TypeReferenceId -> TypeReferenceId -> (Referent, Referent)
pattern TwoNonBuiltinConstructors x y <-
  ( Referent.Con (ConstructorReference (ReferenceDerived x) _) _,
    Referent.Con (ConstructorReference (ReferenceDerived y) _) _
    )

narrowTypes ::
  (HasCallStack) =>
  GUpdated PartialDeclNameLookup DeclNameLookup ->
  Updated (Map Name TypeReference) ->
  Updated (Map Name TypeReference)
narrowTypes declNameLookup =
  filterOutEqualSynhash \name oldRef newRef ->
    case (oldRef, newRef) of
      TwoBuiltinTypes x y -> x == y
      -- Drop equal types only if they would have equal synhashes, i.e. they have the same namings of constructors
      TwoNonBuiltinTypes x y -> x == y && sameConstructorNames
        where
          sameConstructorNames = oldConstructorNames == map Just newConstructorNames
          oldConstructorNames = PartialDeclNameLookup.expectConstructorNames declNameLookup.old name
          newConstructorNames = DeclNameLookup.expectConstructorNames declNameLookup.new name
      _ -> False

-- | Like 'narrowTypes', but for when the LCA is known not to have any type declarations with missing constructor names.
narrowTypesTotal ::
  (HasCallStack) =>
  Updated DeclNameLookup ->
  Updated (Map Name TypeReference) ->
  Updated (Map Name TypeReference)
narrowTypesTotal declNameLookup =
  filterOutEqualSynhash \name oldRef newRef ->
    case (oldRef, newRef) of
      TwoBuiltinTypes x y -> x == y
      -- Drop equal types only if they would have equal synhashes, i.e. they have the same namings of constructors
      TwoNonBuiltinTypes x y -> x == y && sameConstructorNames
        where
          sameConstructorNames = oldConstructorNames == newConstructorNames
          oldConstructorNames = DeclNameLookup.expectConstructorNames declNameLookup.old name
          newConstructorNames = DeclNameLookup.expectConstructorNames declNameLookup.new name
      _ -> False

pattern TwoBuiltinTypes :: Text -> Text -> (TypeReference, TypeReference)
pattern TwoBuiltinTypes x y <-
  (ReferenceBuiltin x, ReferenceBuiltin y)

pattern TwoNonBuiltinTypes :: TypeReferenceId -> TypeReferenceId -> (TypeReference, TypeReference)
pattern TwoNonBuiltinTypes x y <-
  (ReferenceDerived x, ReferenceDerived y)

filterOutEqualSynhash ::
  forall ref.
  (HasCallStack) =>
  (Name -> ref -> ref -> Bool) ->
  Updated (Map Name ref) ->
  Updated (Map Name ref)
filterOutEqualSynhash equal defns =
  Map.merge (Map.mapMissing \_ -> This) (Map.mapMissing \_ -> That) (Map.zipWithMaybeMatched f) defns.old defns.new
    & unalign
    & Updated.fromPair
  where
    f :: Name -> ref -> ref -> Maybe (These ref ref)
    f name oldRef newRef =
      if equal name oldRef newRef
        then Nothing
        else Just (These oldRef newRef)
