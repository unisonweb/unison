-- | The constructor reference type.
module Unison.ConstructorReference
  ( GConstructorReference (..),
    ConstructorReference,
    ConstructorReferenceId,
    reference_,
    toShortHash,
  )
where

import Control.Lens
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.Reference (TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as ShortHash
import Unison.Prelude

-- | A reference to a constructor is represented by a reference to its type declaration, plus the ordinal constructor id.
data GConstructorReference r
  = ConstructorReference !r !ConstructorId
  deriving stock (Eq, Functor, Ord, Show, Generic)
  deriving anyclass (NFData)

type ConstructorReference = GConstructorReference TypeReference

type ConstructorReferenceId = GConstructorReference TypeReferenceId

-- | A lens onto the reference part of a constructor reference.
reference_ :: Lens (GConstructorReference r) (GConstructorReference s) r s
reference_ =
  lens (\(ConstructorReference r _) -> r) \(ConstructorReference _ i) r -> ConstructorReference r i

toShortHash :: ConstructorReference -> ShortHash
toShortHash (ConstructorReference r i) =
  case Reference.toShortHash r of
    ShortHash.Builtin b -> ShortHash.Builtin b
    ShortHash.ShortHash prefix cycle _cid -> ShortHash.ShortHash prefix cycle (Just i)
