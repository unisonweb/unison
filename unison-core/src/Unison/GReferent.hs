{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.GReferent
  ( GReferent (..),

    -- * Basic queries
    isConstructor,
    fold,

    -- * Lenses
    reference_,

    -- * Conversions
    toReference',
    toTermReference,
    toTypeReference,
  )
where

import Control.Lens (Lens, lens)
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType (ConstructorType)
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.Hashable (Hashable (tokens))
import qualified Unison.Hashable as H
import Unison.Prelude (Word64)

-- | Specifies a term.
--
-- Either a term 'Reference', a data constructor, or an effect constructor.
--
-- Slightly odd naming. This is the "referent of term name in the codebase",
-- rather than the target of a Reference.

-- | When @Ref'@ then @r@ represents a term.
--
-- When @Con'@ then @r@ is a type declaration.
data GReferent r = Ref' r | Con' (GConstructorReference r) ConstructorType
  deriving (Show, Ord, Eq, Functor)

-- | A lens onto the reference in a referent.
reference_ :: Lens (GReferent r) (GReferent r') r r'
reference_ =
  lens toReference' \rt rc ->
    case rt of
      Ref' _ -> Ref' rc
      Con' (ConstructorReference _ cid) ct -> Con' (ConstructorReference rc cid) ct

isConstructor :: GReferent r -> Bool
isConstructor Con' {} = True
isConstructor _ = False

toTermReference :: GReferent r -> Maybe r
toTermReference = \case
  Ref' r -> Just r
  _ -> Nothing

toReference' :: GReferent r -> r
toReference' = \case
  Ref' r -> r
  Con' (ConstructorReference r _i) _t -> r

toTypeReference :: GReferent r -> Maybe r
toTypeReference = \case
  Con' (ConstructorReference r _i) _t -> Just r
  _ -> Nothing

fold :: (r -> a) -> (r -> ConstructorId -> ConstructorType -> a) -> GReferent r -> a
fold fr fc = \case
  Ref' r -> fr r
  Con' (ConstructorReference r i) ct -> fc r i ct

instance Hashable r => Hashable (GReferent r) where
  tokens (Ref' r) = [H.Tag 0] ++ H.tokens r
  tokens (Con' (ConstructorReference r i) dt) = [H.Tag 2] ++ H.tokens r ++ H.tokens (fromIntegral i :: Word64) ++ H.tokens dt
