-- | Dictionary membership is namespace metadata, separate from term identity.
-- As with other V1 metadata, a mark applies to every alias of the referent
-- within this namespace. Child namespaces have independent metadata.
module Unison.Codebase.Givens
  ( givenSentinel,
    isGiven,
    isGivenAt,
    namesMarkedGiven,
    metadataValuesFor,
    markGivenAt,
    unmarkGivenAt,
  )
where

import Control.Lens (over, view)
import Data.Set qualified as Set
import Unison.Codebase.Branch.Type (Branch0, terms_)
import Unison.Codebase.Metadata qualified as Metadata
import Unison.NameSegment (NameSegment)
import Unison.Reference (TermReference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Star2 qualified as Star2

-- | An opaque metadata tag, not a callable builtin.
givenSentinel :: TermReference
givenSentinel = Reference.Builtin "Builtin.Given"

isGiven :: Referent -> Branch0 m -> Bool
isGiven r = Set.member givenSentinel . metadataValuesFor r

isGivenAt :: Referent -> NameSegment -> Branch0 m -> Bool
isGivenAt r n b = Star2.memberD1 (r, n) (view terms_ b) && isGiven r b

namesMarkedGiven :: Referent -> Branch0 m -> Set.Set NameSegment
namesMarkedGiven r b
  | isGiven r b = Relation.lookupDom r (Star2.d1 (view terms_ b))
  | otherwise = Set.empty

metadataValuesFor :: Referent -> Branch0 m -> Set.Set TermReference
metadataValuesFor r b = Relation.lookupDom r (Star2.d2 (view terms_ b))

-- | Missing (referent, name) pairs are unchanged; unrelated metadata survives.
markGivenAt :: Referent -> NameSegment -> Branch0 m -> Branch0 m
markGivenAt r n b
  | Star2.memberD1 (r, n) (view terms_ b) = over terms_ (Metadata.insert (r, givenSentinel)) b
  | otherwise = b

unmarkGivenAt :: Referent -> NameSegment -> Branch0 m -> Branch0 m
unmarkGivenAt r n b
  | Star2.memberD1 (r, n) (view terms_ b) = over terms_ (Metadata.delete (r, givenSentinel)) b
  | otherwise = b
