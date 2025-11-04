-- | The constructor reference type.
module Unison.ConstructorReference
  ( GConstructorReference (..),
    ConstructorReference,
    ConstructorReferenceId,
    reference_,
    toId,
    toShortHash,
    toText,
  )
where

import Control.DeepSeq (NFData)
import Control.Lens
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.OrBuiltin qualified as OrBuiltin
import Unison.Prelude
import Unison.Reference (TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.ShortHash (ShortHash)

-- | A reference to a constructor is represented by a reference to its type declaration, plus the ordinal constructor id.
data GConstructorReference r
  = ConstructorReference !r !ConstructorId
  deriving stock (Eq, Functor, Generic, Ord, Show)
  deriving anyclass (NFData)

type ConstructorReference = GConstructorReference TypeReference

type ConstructorReferenceId = GConstructorReference TypeReferenceId

-- | A lens onto the reference part of a constructor reference.
reference_ :: Lens (GConstructorReference r) (GConstructorReference s) r s
reference_ =
  lens (\(ConstructorReference r _) -> r) \(ConstructorReference _ i) r -> ConstructorReference r i

toId :: ConstructorReference -> Maybe ConstructorReferenceId
toId (ConstructorReference typeRef conId) =
  ConstructorReference <$> Reference.toId typeRef <*> pure conId

toShortHash :: ConstructorReference -> ShortHash
toShortHash (ConstructorReference r i) =
  Reference.toShortHash r & set (OrBuiltin.notBuiltin_ . #cid) (Just i)

toText :: ConstructorReference -> Text
toText (ConstructorReference r _) =
  Reference.toText r
