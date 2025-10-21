-- | A utility module for unconflicted namespaces and related types/functionality.

module Unison.NamesUtils
  ( byName,
    forgetNames,
    referentsToIds,
    restrictNames,
  )
where

import Data.Set qualified as Set
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Prelude
import Unison.Reference (Reference' (..), TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, zipDefnsWith)
import Unison.Util.Defns qualified as Defns
import Unison.Util.Set qualified as Set

-- | /O(1)/. View unconflicted names by name (throwing away ref->name mapping).
byName :: Defns (BiMultimap terms name) (BiMultimap types name) -> DefnsF (Map name) terms types
byName =
  bimap BiMultimap.range BiMultimap.range

forgetNames :: Defns (BiMultimap terms name) (BiMultimap types name) -> DefnsF Set terms types
forgetNames =
  bimap BiMultimap.dom BiMultimap.dom

restrictNames ::
  (Ord name, Ord terms, Ord types) =>
  DefnsF Set name name ->
  Defns (BiMultimap terms name) (BiMultimap types name) ->
  Defns (BiMultimap terms name) (BiMultimap types name)
restrictNames =
  zipDefnsWith BiMultimap.restrictRan BiMultimap.restrictRan

referentsToIds :: DefnsF Set Referent TypeReference -> DefnsF Set TermReferenceId TypeReferenceId
referentsToIds defns =
  fromTerms <> Defns.fromTypes (Set.mapMaybe Reference.toId defns.types)
  where
    fromTerms =
      Set.foldl'
        ( \acc -> \case
            Referent.Ref (ReferenceDerived ref) ->
              let !terms = Set.insert ref acc.terms in Defns terms acc.types
            Referent.Con (ConstructorReference (ReferenceDerived ref) _) _ ->
              let !types = Set.insert ref acc.types in Defns acc.terms types
            _ -> acc
        )
        (Defns Set.empty Set.empty)
        defns.terms
