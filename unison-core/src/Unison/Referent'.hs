{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Referent'
  ( Referent' (..),

    -- * Basic queries
    isConstructor,
    Unison.Referent'.fold,

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
import Unison.Prelude

-- | Specifies a term.
--
-- Either a term 'Reference', a data constructor, or an effect constructor.
--
-- Slightly odd naming. This is the "referent of term name in the codebase",
-- rather than the target of a Reference.

-- | When @Ref'@ then @r@ represents a term.
--
-- When @Con'@ then @r@ is a type declaration.
data Referent' r = Ref' r | Con' (GConstructorReference r) ConstructorType
  deriving (Show, Ord, Eq, Functor, Generic)

-- | A lens onto the reference in a referent.
reference_ :: Lens (Referent' r) (Referent' r') r r'
reference_ =
  lens toReference' \rt rc ->
    case rt of
      Ref' _ -> Ref' rc
      Con' (ConstructorReference _ cid) ct -> Con' (ConstructorReference rc cid) ct

isConstructor :: Referent' r -> Bool
isConstructor Con' {} = True
isConstructor _ = False

toTermReference :: Referent' r -> Maybe r
toTermReference = \case
  Ref' r -> Just r
  _ -> Nothing

toReference' :: Referent' r -> r
toReference' = \case
  Ref' r -> r
  Con' (ConstructorReference r _i) _t -> r

toTypeReference :: Referent' r -> Maybe r
toTypeReference = \case
  Con' (ConstructorReference r _i) _t -> Just r
  _ -> Nothing

fold :: (r -> a) -> (r -> ConstructorId -> ConstructorType -> a) -> Referent' r -> a
fold fr fc = \case
  Ref' r -> fr r
  Con' (ConstructorReference r i) ct -> fc r i ct
