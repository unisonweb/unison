module Unison.Merge.Narrow
  ( narrowDefns,
  )
where

import Data.Map.Merge.Strict qualified as Map
import Data.Semialign (unalign)
import Data.These (These (..))
import U.Codebase.Reference (TypeReference)
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
  GThreeWay PartialDeclNameLookup DeclNameLookup ->
  ThreeWay (DefnsF (Map Name) Referent TypeReference) ->
  TwoWay (Updated (DefnsF (Map Name) Referent TypeReference))
narrowDefns declNameLookups defns =
  narrowDefns1 <$> ThreeWay.gtoUpdated declNameLookups <*> ThreeWay.toUpdated defns

narrowDefns1 ::
  GUpdated PartialDeclNameLookup DeclNameLookup ->
  Updated (DefnsF (Map Name) Referent TypeReference) ->
  Updated (DefnsF (Map Name) Referent TypeReference)
narrowDefns1 declNameLookups defns =
  Updated.zipWith Defns (narrowTerms declNameLookups types) (narrowTypes declNameLookups terms)
  where
    (types, terms) =
      Updated.unzipWith Defns.toPair defns

narrowTerms ::
  GUpdated PartialDeclNameLookup DeclNameLookup ->
  Updated (Map Name Referent) ->
  Updated (Map Name Referent)
narrowTerms declNameLookup =
  filterOutEqualSynhash \name oldRef newRef ->
    case (oldRef, newRef) of
      -- Drop hash-equal terms
      (Referent.Ref oldRef1, Referent.Ref newRef1) -> oldRef1 == newRef1
      -- Drop equal constructors only if they would have equal synhashes, i.e. their types have the same
      -- namings of constructors
      (Referent.Con oldRef1 _, Referent.Con newRef1 _) ->
        let oldConstructorNames =
              PartialDeclNameLookup.expectConstructorNames
                declNameLookup.old
                (PartialDeclNameLookup.expectDeclName declNameLookup.old name)
            newConstructorNames =
              DeclNameLookup.expectConstructorNames
                declNameLookup.new
                (DeclNameLookup.expectDeclName declNameLookup.new name)
         in oldRef1 == newRef1 && oldConstructorNames == map Just newConstructorNames
      _ -> False

narrowTypes ::
  GUpdated PartialDeclNameLookup DeclNameLookup ->
  Updated (Map Name TypeReference) ->
  Updated (Map Name TypeReference)
narrowTypes declNameLookup =
  filterOutEqualSynhash \name oldRef newRef ->
    -- Drop equal types only if they would have equal synhashes, i.e. they have the same namings of constructors
    let oldConstructorNames = PartialDeclNameLookup.expectConstructorNames declNameLookup.old name
        newConstructorNames = DeclNameLookup.expectConstructorNames declNameLookup.new name
     in oldRef == newRef && oldConstructorNames == map Just newConstructorNames

filterOutEqualSynhash ::
  forall ref.
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
