{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Unison.Referent where

import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Word              (Word64)
import           Prelude                hiding (read)
import           Unison.Hashable        (Hashable)
import qualified Unison.Hashable        as H
import           Unison.Reference       (Reference)
import qualified Unison.Reference       as R

import           Data.Bytes.Get
import           Data.Bytes.Put
import           Data.Bytes.Serial      (deserialize, serialize)
import           Data.Bytes.VarInt      (VarInt (..))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base58 as Base58
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Safe                   (readMay)


data Referent = Ref Reference | Req Reference Int | Con Reference Int
  deriving (Show, Ord, Eq)

type Pos = Word64
type Size = Word64
type IsAbility = Bool

-- referentToTerm moved to Term.fromReferent
-- termToReferent moved to Term.toReferent

showShort :: Int -> Referent -> String
showShort numHashChars r = case r of
  Ref r     -> R.showShort numHashChars r
  Con r cid -> R.showShort numHashChars r <> "#" <> show cid
  Req r cid -> R.showShort numHashChars r <> "#" <> show cid

toString :: Referent -> String
toString = \case
  Ref r     -> show r
  Con r cid -> show r <> "#" <> show cid
  Req r cid -> show r <> "#" <> show cid


isConstructor :: Referent -> Bool
isConstructor (Con _ _) = True
isConstructor (Req _ _) = True
isConstructor _         = False

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

-- Parses Asdf##Foo as Builtin Foo
-- Parses Asdf#abc123.2 as Derived 'abc123' 1 2
unsafeFromText :: Text -> Referent
unsafeFromText = either error id . fromText

fromText :: Text -> Either String Referent
fromText t = case Text.split (=='#') t of
  [_, "", b]  -> Right $ Ref (R.Builtin b)
  [_, h]      -> Ref <$> getId h
  [_, h, c]   ->
    Con <$> getId h
        <*> fromMaybe (Left "couldn't parse cid") (readMay (Text.unpack c))

    -- [hash, suffix, con] -> readSuffix suffix >>= \case
    --   (pos, size, True) ->  Req (R.derivedBase58 hash pos size) (read . Text.unpack $ c)
    --   (pos, size, False) -> Con (R.derivedBase58 hash) pos size
  _ -> bail
  where
  getId h = case Text.split (=='.') h of
    [hash]         -> Right $ R.derivedBase58 hash 0 1
    [hash, suffix] -> uncurry (R.derivedBase58 hash) <$> R.readSuffix suffix
    _              -> bail
  bail = Left . Text.unpack $ "couldn't parse a Referent from " <> t

showSuffix :: Pos -> Size -> IsAbility -> String
showSuffix i n isAbility = Text.unpack . encode58 . runPutS $ put where
  encode58 = decodeUtf8 . Base58.encodeBase58 Base58.bitcoinAlphabet
  put = putLength i >> putLength n >> putBoolean isAbility
  putLength = serialize . VarInt
  putBoolean False = putWord8 0
  putBoolean True  = putWord8 1


readSuffix' :: Text -> Either String (Pos, Size, IsAbility)
readSuffix' t =
  runGetS get =<< (tagError . decode58) t where
  tagError = maybe (Left "base58 decoding error") Right
  decode58 :: Text -> Maybe ByteString
  decode58 = Base58.decodeBase58 Base58.bitcoinAlphabet . encodeUtf8
  get = (,,) <$> getLength <*> getLength <*> getBoolean
  getLength = unVarInt <$> deserialize
  getBoolean = go =<< getWord8 where
    go 0 = pure False
    go 1 = pure True
    go t = fail ("unknown tag " <> show t <> " reading Referent suffix")
      -- don't fail here


instance Hashable Referent where
  tokens (Ref r) = [H.Tag 0] ++ H.tokens r
  tokens (Req r i) = [H.Tag 1] ++ H.tokens r ++ H.tokens (fromIntegral i :: Word64)
  tokens (Con r i) = [H.Tag 2] ++ H.tokens r ++ H.tokens (fromIntegral i :: Word64)
