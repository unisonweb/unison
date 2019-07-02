{-# LANGUAGE OverloadedStrings #-}

module Unison.Referent where

-- import           Data.Maybe             (fromMaybe)
import qualified Data.Char              as Char
import           Data.Maybe             ( fromMaybe )
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Word              (Word64)
import           Unison.Hashable        (Hashable)
import qualified Unison.Hashable        as H
import           Unison.Reference       (Reference)
import qualified Unison.Reference       as R
import           Unison.ShortHash       (ShortHash)
import qualified Unison.ShortHash       as SH
-- import           Safe                   (readMay)

data Referent = Ref Reference | Con Reference Int
  deriving (Show, Ord, Eq)

type Pos = Word64
type Size = Word64

-- referentToTerm moved to Term.fromReferent
-- termToReferent moved to Term.toReferent

-- todo: move these to ShortHash module
toShortHash :: Referent -> ShortHash
toShortHash = \case
  Ref r -> R.toShortHash r
  Con r i -> (R.toShortHash r) { SH.cid = Just $ Text.pack . show $ i }

showShort :: Int -> Referent -> Text
showShort numHashChars = SH.toText . SH.take numHashChars . toShortHash

toText :: Referent -> Text
toText = \case
  Ref r     -> R.toText r
  Con r cid -> R.toText r <> "#" <> Text.pack (show cid)

toString :: Referent -> String
toString = Text.unpack . toText

isConstructor :: Referent -> Bool
isConstructor (Con _ _) = True
isConstructor _         = False

toTermReference :: Referent -> Maybe Reference
toTermReference = \case
  Ref r -> Just r
  _ -> Nothing

toReference :: Referent -> Reference
toReference = \case
  Ref r -> r
  Con r _i -> r

toTypeReference :: Referent -> Maybe Reference
toTypeReference = \case
  Con r _i -> Just r
  _ -> Nothing

unsafeFromText :: Text -> Referent
unsafeFromText = fromMaybe (error "invalid referent") . fromText

fromText :: Text -> Maybe Referent
fromText t = either (const Nothing) Just $
  -- if the string has just one hash at the start, it's just a reference
  if Text.length refPart == 1 then
    Ref <$> R.fromText t
  else if Text.all Char.isDigit cidPart then
    (\r -> Con r (read (Text.unpack cidPart))) <$>
    R.fromText (Text.dropEnd 1 refPart)
  else
    Left ("invalid constructor id: " <> Text.unpack cidPart)
  where
    refPart = Text.dropWhileEnd (/= '#') t
    cidPart = Text.takeWhileEnd (/= '#') t

instance Hashable Referent where
  tokens (Ref r) = [H.Tag 0] ++ H.tokens r
  tokens (Con r i) = [H.Tag 2] ++ H.tokens r ++ H.tokens (fromIntegral i :: Word64)
