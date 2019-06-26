{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Unison.Reference
  (Reference,
     pattern Builtin,
     pattern Derived,
     pattern DerivedId,
   Id(..),
   derivedBase58,
   Component, members,
   components,
   groupByComponent,
   componentFor,
   unsafeFromText,
   readSuffix,
   showShort,
   toText,
   unsafeId,
   toShortHash) where

import           Control.Monad   (join)
import           Data.Foldable   (toList)
import           Data.List
import qualified Data.Map        as Map
import           Data.Maybe      (fromJust, maybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Data.Word       (Word64)
import           GHC.Generics
import qualified Unison.Hash     as H
import           Unison.Hashable as Hashable
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH
import           Data.Bytes.Get
import           Data.Bytes.Put
import           Data.Bytes.Serial              ( serialize
                                                , deserialize
                                                )
import           Data.Bytes.VarInt              ( VarInt(..) )
import qualified Data.ByteString.Base58 as Base58
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.ByteString (ByteString)

data Reference
  = Builtin Text.Text
  -- `Derived` can be part of a strongly connected component.
  -- The `Pos` refers to a particular element of the component
  -- and the `Size` is the number of elements in the component.
  -- Using an ugly name so no one tempted to use this
  | DerivedId Id deriving (Eq,Ord,Generic)

pattern Derived h i n = DerivedId (Id h i n)

data Id = Id H.Hash Pos Size deriving (Eq,Ord,Generic)

unsafeId :: Reference -> Id
unsafeId (Builtin b) =
  error $ "Tried to get the hash of builtin " <> Text.unpack b <> "."
unsafeId (DerivedId x) = x

-- todo: move these to ShortHash module?
-- but Show Reference currently depends on SH
toShortHash :: Reference -> ShortHash
toShortHash (Builtin b) = SH.Builtin b
toShortHash (Derived h _ 1) = SH.ShortHash (H.base58 h) Nothing Nothing
toShortHash (Derived h i n) = SH.ShortHash (H.base58 h) index Nothing
  where
    -- todo: remove `n` parameter; must also update readSuffix
    index = Just $ showSuffix i n
    showSuffix :: Pos -> Size -> Text
    showSuffix i n = encode58 . runPutS $ put where
      encode58 = decodeUtf8 . Base58.encodeBase58 Base58.bitcoinAlphabet
      put = putLength i >> putLength n
      putLength = serialize . VarInt
toShortHash (DerivedId _) = error "this should be covered above"

toText :: Reference -> Text
toText = SH.toText . toShortHash

showShort :: Int -> Reference -> Text
showShort numHashChars = SH.toText . SH.take numHashChars . toShortHash

-- todo: don't read or return size; must also update showSuffix and fromText
readSuffix :: Text -> Either String (Pos, Size)
readSuffix t =
  runGetS get =<< (tagError . decode58) t where
  tagError = maybe (Left "base58 decoding error") Right
  decode58 :: Text -> Maybe ByteString
  decode58 = Base58.decodeBase58 Base58.bitcoinAlphabet . encodeUtf8
  get = (,) <$> getLength <*> getLength
  getLength = unVarInt <$> deserialize

type Pos = Word64
type Size = Word64

newtype Component = Component { members :: Set Reference }

-- Gives the component (dependency cycle) that the reference is a part of
componentFor :: Reference -> Component
componentFor b@(Builtin        _         ) = Component (Set.singleton b)
componentFor (  DerivedId (Id h _ n)) = Component
  (Set.fromList
    [ DerivedId (Id h i n) | i <- take (fromIntegral n) [0 ..] ]
  )

derivedBase58 :: Text -> Pos -> Size -> Reference
derivedBase58 b58 i n = DerivedId (Id (fromJust h) i n)
  where
  h = H.fromBase58 b58

unsafeFromText :: Text -> Reference
unsafeFromText = either error id . fromText

-- examples:
-- `##Text.take` — builtins don’t have cycles
-- `#2tWjVAuc7` — derived, no cycle
-- `#y9ycWkiC1.y9` — derived, part of cycle
-- todo: take a (Reference -> CycleSize) so that `readSuffix` doesn't have to parse the size from the text.
fromText :: Text -> Either String Reference
fromText t = case Text.split (=='#') t of
  [_, "", b] -> Right (Builtin b)
  [_, h]     -> case Text.split (=='.') h of
    [hash]         -> Right (derivedBase58 hash 0 1)
    [hash, suffix] -> uncurry (derivedBase58 hash) <$> readSuffix suffix
    _ -> bail
  _ -> bail
  where bail = Left $ "couldn't parse a Reference from " <> Text.unpack t

component :: H.Hash -> [k] -> [(k, Reference)]
component h ks = let
  size = fromIntegral (length ks)
  in [ (k, DerivedId (Id h i size)) | (k, i) <- ks `zip` [0..]]

components :: [(H.Hash, [k])] -> [(k, Reference)]
components sccs = join $ uncurry component <$> sccs

groupByComponent :: [(k, Reference)] -> [[(k, Reference)]]
groupByComponent refs = done $ foldl' insert Map.empty refs
  where
    insert m (k, r@(Derived h _ _)) =
      Map.unionWith (<>) m (Map.fromList [(Right h, [(k,r)])])
    insert m (k, r) =
      Map.unionWith (<>) m (Map.fromList [(Left r, [(k,r)])])
    done m = sortOn snd <$> toList m

instance Show Id where show = show . SH.take 5 . toShortHash . DerivedId
instance Show Reference where show = show . SH.take 5 . toShortHash

instance Hashable.Hashable Reference where
  tokens (Builtin txt) = [Hashable.Tag 0, Hashable.Text txt]
  tokens (DerivedId (Id h i n)) = [Hashable.Tag 1, Hashable.Bytes (H.toBytes h), Hashable.Nat i, Hashable.Nat n]
