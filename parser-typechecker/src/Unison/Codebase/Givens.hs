-- | Helpers for marking definitions as "givens" via the
-- @##Builtin.Given@ sentinel reference stored in namespace metadata.
--
-- A definition is a /given/ if its associated metadata-value set
-- ('Unison.Codebase.Branch.Type.MdValues' in V2, or the @d2@ dimension
-- of 'Unison.Util.Star2.Star2' in V1) contains 'givenSentinel'. The
-- sentinel itself is the builtin reference @##Builtin.Given@; it has no
-- runtime behaviour and is never invoked.
--
-- Per ADR-013, this gives us:
--
--   * No schema change: 'MdValues' is already serialised in the SQLite
--     codebase and included in the namespace (causal) hash via
--     'Unison.Hashing.V2.Branch.Branch'\'s 'Tokenizable' instance.
--   * The term reference itself is unchanged when (un)marking a name,
--     so its content hash and any dependents are unaffected; only the
--     enclosing namespace hash changes.
--
-- Per-referent (not per-name) granularity: V1's 'Star2' metadata is
-- keyed on the referent, so marking one name marks every alias of the
-- same referent within that namespace. See 'isGivenAt' for details.
--
-- Higher-level UCM commands (@given@, @find :given@, etc.) live in
-- chunk B2 and consume the helpers in this module.
module Unison.Codebase.Givens
  ( -- * Sentinel reference
    givenSentinel,

    -- * Querying
    isGivenAt,
    isGiven,
    namesMarkedGiven,
    metadataValuesFor,

    -- * Marking and unmarking
    markGivenAt,
    unmarkGivenAt,
  )
where

import Control.Lens (over, view)
import Data.Set qualified as Set
import Unison.Builtin qualified as Builtin
import Unison.Codebase.Branch.Type (Branch0, terms_)
import Unison.Codebase.Metadata qualified as Metadata
import Unison.NameSegment (NameSegment)
import Unison.Reference (TermReference)
import Unison.Referent (Referent)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Star2 qualified as Star2

-- | The sentinel metadata value whose presence in a definition's
-- 'MdValues' indicates the definition is a /given/. This is exactly
-- 'Unison.Builtin.givenSentinelRef'; re-exported here so callers
-- typically only need to import 'Unison.Codebase.Givens'.
givenSentinel :: TermReference
givenSentinel = Builtin.givenSentinelRef

-- | @True@ if the referent @r@ is present at the given name segment in
-- the branch and carries 'givenSentinel' in its metadata.
--
-- Note: the V1 'Unison.Util.Star2.Star2' schema stores metadata per
-- referent rather than per (referent, name) pair, so adding the
-- sentinel for a referent that is published under multiple names will
-- affect every name it appears at. This matches existing metadata
-- semantics; the V2 schema in 'Unison.Codebase.Branch.Type.MdValues'
-- has the same shape (metadata is keyed on the referent within each
-- name-segment\'s map).
isGivenAt :: Referent -> NameSegment -> Branch0 m -> Bool
isGivenAt r n b =
  Star2.memberD1 (r, n) (view terms_ b) && hasGivenSentinel r b

-- | @True@ if the term referent @r@ has 'givenSentinel' in its
-- metadata anywhere in the supplied branch.
isGiven :: Referent -> Branch0 m -> Bool
isGiven = hasGivenSentinel

-- | All name segments at which the supplied term referent appears and
-- is marked as a given. Useful for @find :given@-style filters
-- (chunk B2).
namesMarkedGiven :: Referent -> Branch0 m -> Set.Set NameSegment
namesMarkedGiven r b
  | hasGivenSentinel r b = Relation.lookupDom r (Star2.d1 (view terms_ b))
  | otherwise = Set.empty

-- | Mark the term referent at the given name as a given by adding
-- 'givenSentinel' to its metadata. If the referent is not present at
-- the name, the branch is returned unchanged.
markGivenAt :: Referent -> NameSegment -> Branch0 m -> Branch0 m
markGivenAt r n b
  | Star2.memberD1 (r, n) (view terms_ b) =
      over terms_ (Metadata.insert (r, givenSentinel)) b
  | otherwise = b

-- | Remove 'givenSentinel' from the term referent\'s metadata,
-- unmarking it as a given. Other metadata values are preserved. The
-- @NameSegment@ argument is accepted for symmetry with 'markGivenAt'
-- and to leave room for a future per-name representation; it is
-- currently ignored because V1 metadata is per-referent.
unmarkGivenAt :: Referent -> NameSegment -> Branch0 m -> Branch0 m
unmarkGivenAt r _n =
  over terms_ (Metadata.delete (r, givenSentinel))

hasGivenSentinel :: Referent -> Branch0 m -> Bool
hasGivenSentinel r b =
  Set.member givenSentinel (metadataValuesFor r b)

-- | The full set of metadata values currently associated with the
-- given term referent in the supplied branch. This is the read-side
-- entry point that 'view' / 'find' query paths can consume; callers
-- testing for givenness should prefer 'isGiven' but may use this
-- function when displaying or filtering on a richer set of tags.
metadataValuesFor :: Referent -> Branch0 m -> Set.Set TermReference
metadataValuesFor r b =
  Relation.lookupDom r (Star2.d2 (view terms_ b))
