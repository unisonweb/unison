{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Referent where

import qualified Data.Char as Char
import qualified Data.Text as Text
import Unison.ConstructorType (ConstructorType)
import qualified Unison.ConstructorType as CT
import Unison.Hashable (Hashable)
import qualified Unison.Hashable as H
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as R
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH

-- Slightly odd naming. This is the "referent of term name in the codebase",
-- rather than the target of a Reference.
type Referent = Referent' Reference

pattern Ref :: Reference -> Referent
pattern Ref r = Ref' r

pattern Con :: Reference -> Int -> ConstructorType -> Referent
pattern Con r i t = Con' r i t

{-# COMPLETE Ref, Con #-}

type Id = Referent' R.Id

data Referent' r = Ref' r | Con' r Int ConstructorType
  deriving (Show, Ord, Eq, Functor)

type Pos = Word64

type Size = Word64

-- referentToTerm moved to Term.fromReferent
-- termToReferent moved to Term.toReferent

-- todo: move these to ShortHash module
toShortHash :: Referent -> ShortHash
toShortHash = \case
  Ref r -> R.toShortHash r
  Con r i _ -> patternShortHash r i

toShortHashId :: Id -> ShortHash
toShortHashId = toShortHash . fromId

-- also used by HashQualified.fromPattern
patternShortHash :: Reference -> Int -> ShortHash
patternShortHash r i = (R.toShortHash r) {SH.cid = Just . Text.pack $ show i}

showShort :: Int -> Referent -> Text
showShort numHashChars = SH.toText . SH.take numHashChars . toShortHash

toText :: Referent -> Text
toText = \case
  Ref r -> R.toText r
  Con r cid ct -> R.toText r <> "#" <> ctorTypeText ct <> Text.pack (show cid)

ctorTypeText :: CT.ConstructorType -> Text
ctorTypeText CT.Effect = EffectCtor
ctorTypeText CT.Data = DataCtor

pattern EffectCtor = "a"

pattern DataCtor = "d"

toString :: Referent -> String
toString = Text.unpack . toText

isConstructor :: Referent -> Bool
isConstructor Con {} = True
isConstructor _ = False

toTermReference :: Referent -> Maybe Reference
toTermReference = \case
  Ref r -> Just r
  _ -> Nothing

toReference :: Referent -> Reference
toReference = toReference'

toReference' :: Referent' r -> r
toReference' = \case
  Ref' r -> r
  Con' r _i _t -> r

fromId :: Id -> Referent
fromId = fmap R.DerivedId

toTypeReference :: Referent -> Maybe Reference
toTypeReference = \case
  Con r _i _t -> Just r
  _ -> Nothing

isPrefixOf :: ShortHash -> Referent -> Bool
isPrefixOf sh r = SH.isPrefixOf sh (toShortHash r)

unsafeFromText :: Text -> Referent
unsafeFromText = fromMaybe (error "invalid referent") . fromText

-- #abc[.xy][#<T>cid]
fromText :: Text -> Maybe Referent
fromText t =
  either (const Nothing) Just $
    -- if the string has just one hash at the start, it's just a reference
    if Text.length refPart == 1
      then Ref <$> R.fromText t
      else
        if Text.all Char.isDigit cidPart
          then do
            r <- R.fromText (Text.dropEnd 1 refPart)
            ctorType <- ctorType
            let cid = read (Text.unpack cidPart)
            pure $ Con r cid ctorType
          else Left ("invalid constructor id: " <> Text.unpack cidPart)
  where
    ctorType = case Text.take 1 cidPart' of
      EffectCtor -> Right CT.Effect
      DataCtor -> Right CT.Data
      _otherwise ->
        Left
          ( "invalid constructor type (expected '"
              <> EffectCtor
              <> "' or '"
              <> DataCtor
              <> "'): "
              <> Text.unpack cidPart'
          )
    refPart = Text.dropWhileEnd (/= '#') t
    cidPart' = Text.takeWhileEnd (/= '#') t
    cidPart = Text.drop 1 cidPart'

instance Hashable Referent where
  tokens (Ref r) = [H.Tag 0] ++ H.tokens r
  tokens (Con r i dt) = [H.Tag 2] ++ H.tokens r ++ H.tokens (fromIntegral i :: Word64) ++ H.tokens dt
