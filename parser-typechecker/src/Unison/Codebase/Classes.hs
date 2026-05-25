-- | Helpers for marking type declarations as @class@ via the
-- @##Builtin.Class@ sentinel reference stored in namespace metadata.
-- Mirrors 'Unison.Codebase.Givens' but operates on type references
-- (the 'types_' Star of a 'Branch0' instead of 'terms_').
--
-- A type declaration is a /class/ if its associated metadata-value
-- set contains 'classSentinel'. The sentinel is the builtin reference
-- @##Builtin.Class@; it has no runtime behaviour and is never invoked.
--
-- The marker is purely surface-syntax sugar: the underlying data
-- declaration is unchanged. @view@ inspects this marker to decide
-- whether to render the declaration with the @class@ keyword and
-- record-field syntax, completing the round-trip from source written
-- with @class@ back through @view@.
module Unison.Codebase.Classes
  ( -- * Sentinel reference
    classSentinel,

    -- * Querying
    isClassAt,
    isClass,
    namesMarkedClass,
    metadataValuesForType,

    -- * Marking and unmarking
    markClassAt,
    unmarkClassAt,
  )
where

import Control.Lens (over, view)
import Data.Set qualified as Set
import Unison.Builtin qualified as Builtin
import Unison.Codebase.Branch.Type (Branch0, types_)
import Unison.Codebase.Metadata qualified as Metadata
import Unison.NameSegment (NameSegment)
import Unison.Reference (TermReference, TypeReference)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Star2 qualified as Star2

-- | The sentinel metadata value whose presence on a type's metadata
-- indicates the type was declared with the @class@ keyword.
classSentinel :: TermReference
classSentinel = Builtin.classSentinelRef

-- | @True@ if the type reference @r@ is present at the given name
-- segment in the branch and carries 'classSentinel' in its metadata.
isClassAt :: TypeReference -> NameSegment -> Branch0 m -> Bool
isClassAt r n b =
  Star2.memberD1 (r, n) (view types_ b) && hasClassSentinel r b

-- | @True@ if the type reference @r@ has 'classSentinel' in its
-- metadata anywhere in the supplied branch.
isClass :: TypeReference -> Branch0 m -> Bool
isClass = hasClassSentinel

-- | All name segments at which the supplied type reference appears
-- and is marked as a class.
namesMarkedClass :: TypeReference -> Branch0 m -> Set.Set NameSegment
namesMarkedClass r b
  | hasClassSentinel r b = Relation.lookupDom r (Star2.d1 (view types_ b))
  | otherwise = Set.empty

-- | Mark the type reference at the given name as a class by adding
-- 'classSentinel' to its metadata. If the reference is not present at
-- the name, the branch is returned unchanged.
markClassAt :: TypeReference -> NameSegment -> Branch0 m -> Branch0 m
markClassAt r n b
  | Star2.memberD1 (r, n) (view types_ b) =
      over types_ (Metadata.insert (r, classSentinel)) b
  | otherwise = b

-- | Remove 'classSentinel' from the type reference's metadata,
-- unmarking it as a class. Other metadata values are preserved.
unmarkClassAt :: TypeReference -> NameSegment -> Branch0 m -> Branch0 m
unmarkClassAt r _n =
  over types_ (Metadata.delete (r, classSentinel))

hasClassSentinel :: TypeReference -> Branch0 m -> Bool
hasClassSentinel r b =
  Set.member classSentinel (metadataValuesForType r b)

-- | The full set of metadata values currently associated with the
-- given type reference in the supplied branch.
metadataValuesForType :: TypeReference -> Branch0 m -> Set.Set TermReference
metadataValuesForType r b =
  Relation.lookupDom r (Star2.d2 (view types_ b))
