{-# LANGUAGE LambdaCase #-}

module Unison.Referent where

import           Unison.Hashable  (Hashable)
import qualified Unison.Hashable  as H
import           Unison.Reference (Reference)
import           Data.Word        (Word64)

data Referent = Ref Reference | Req Reference Int | Con Reference Int
  deriving (Show, Ord, Eq)

-- referentToTerm moved to Term.fromReferent
-- termToReferent moved to Term.toReferent

toReference :: Referent -> Reference
toReference = \case
  Ref r -> r
  Req r _i -> r
  Con r _i -> r

instance Hashable Referent where
  tokens (Ref r) = [H.Tag 0] ++ H.tokens r
  tokens (Req r i) = [H.Tag 1] ++ H.tokens r ++ H.tokens (fromIntegral i :: Word64)
  tokens (Con r i) = [H.Tag 2] ++ H.tokens r ++ H.tokens (fromIntegral i :: Word64)
