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
import Unison.Prelude
import Unison.Reference (TypeReference, TypeReferenceId)
import qualified Unison.Reference as Reference
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as ShortHash

-- | A reference to a constructor is represented by a reference to its type declaration, plus the ordinal constructor id.
data GConstructorReference r
  = ConstructorReference !r !ConstructorId
  deriving stock (Eq, Functor, Ord, Show)

type ConstructorReference = GConstructorReference TypeReference

type ConstructorReferenceId = GConstructorReference TypeReferenceId

-- | A lens onto the reference part of a constructor reference.
reference_ :: Lens (GConstructorReference r) (GConstructorReference s) r s
reference_ =
  lens (\(ConstructorReference r _) -> r) \(ConstructorReference _ i) r -> ConstructorReference r i

toShortHash :: ConstructorReference -> ShortHash
toShortHash (ConstructorReference r i) =
  (Reference.toShortHash r) {ShortHash.cid = Just (tShow i)}
