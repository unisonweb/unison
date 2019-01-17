{-# LANGUAGE LambdaCase #-}

module Unison.Referent where

import           Unison.Hashable  (Hashable)
import qualified Unison.Hashable  as H
import           Unison.Reference (Reference)
import qualified Unison.Reference as R
import           Data.Word        (Word64)

data Referent = Ref Reference | Req Reference Int | Con Reference Int
  deriving (Show, Ord, Eq)

-- referentToTerm moved to Term.fromReferent
-- termToReferent moved to Term.toReferent

showShort :: Int -> Referent -> String
showShort numHashChars r = case r of
  Ref r -> R.showShort numHashChars r
  Con r cid -> R.showShort numHashChars r <> "#" <> show cid
  Req r cid -> R.showShort numHashChars r <> "#" <> show cid

isConstructor :: Referent -> Bool
isConstructor (Con _ _) = True
isConstructor (Req _ _) = True
isConstructor _ = False

toReference :: Referent -> Reference
toReference = \case
  Ref r -> r
  Req r _i -> r
  Con r _i -> r

toTypeReference :: Referent -> Maybe Reference
toTypeReference = \case
  Req r _i -> Just r
  Con r _i -> Just r
  _ -> Nothing

instance Hashable Referent where
  tokens (Ref r) = [H.Tag 0] ++ H.tokens r
  tokens (Req r i) = [H.Tag 1] ++ H.tokens r ++ H.tokens (fromIntegral i :: Word64)
  tokens (Con r i) = [H.Tag 2] ++ H.tokens r ++ H.tokens (fromIntegral i :: Word64)
