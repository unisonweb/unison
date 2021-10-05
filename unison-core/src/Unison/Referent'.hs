{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Referent' where

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
data Referent' r = Ref' r | Con' r ConstructorId ConstructorType
  deriving (Show, Ord, Eq, Functor)

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
  Con' r _i _t -> r

toTypeReference :: Referent' r -> Maybe r
toTypeReference = \case
  Con' r _i _t -> Just r
  _ -> Nothing

fold :: (r -> a) -> (r -> ConstructorId -> ConstructorType -> a) -> Referent' r -> a
fold fr fc = \case
  Ref' r -> fr r
  Con' r i ct -> fc r i ct

instance Hashable r => Hashable (Referent' r) where
  tokens (Ref' r) = [H.Tag 0] ++ H.tokens r
  tokens (Con' r i dt) = [H.Tag 2] ++ H.tokens r ++ H.tokens (fromIntegral i :: Word64) ++ H.tokens dt