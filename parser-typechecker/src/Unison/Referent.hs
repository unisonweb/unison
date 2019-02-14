{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}


module Unison.Referent where

import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Data.Word        (Word64)
import           Safe             (readMay)
import           Unison.Hashable  (Hashable)
import qualified Unison.Hashable  as H
import           Unison.Reference (Reference)
import qualified Unison.Reference as R

data Referent = Ref Reference | Con Reference Int
  deriving (Show, Ord, Eq)

-- referentToTerm moved to Term.fromReferent
-- termToReferent moved to Term.toReferent

showShort :: Int -> Referent -> String
showShort numHashChars r = case r of
  Ref r     -> R.showShort numHashChars r
  Con r cid -> R.showShort numHashChars r <> "#" <> show cid

toString :: Referent -> String
toString = \case
  Ref r     -> show r
  Con r cid -> show r <> "#" <> show cid


isConstructor :: Referent -> Bool
isConstructor (Con _ _) = True
isConstructor _         = False

toReference :: Referent -> Reference
toReference = \case
  Ref r -> r
  Con r _i -> r

toTypeReference :: Referent -> Maybe Reference
toTypeReference = \case
  Con r _i -> Just r
  _ -> Nothing

-- Parses Asdf##Foo as Builtin Foo
-- Parses Asdf#abc123-1-2 as Derived 'abc123' 1 2
unsafeFromText :: Text -> Referent
unsafeFromText t = case Text.split (=='#') t of
  [_, "", b]  -> Ref (R.Builtin b)
  [_, h]      -> Ref (R.unsafeFromText h)
  [_, h, (readMay . Text.unpack -> Just cid)] -> Con (R.unsafeFromText h) cid
  _ -> error . Text.unpack $ "couldn't parse a Referent from " <> t


instance Hashable Referent where
  tokens (Ref r) = [H.Tag 0] ++ H.tokens r
  tokens (Con r i) = [H.Tag 2] ++ H.tokens r ++ H.tokens (fromIntegral i :: Word64)
