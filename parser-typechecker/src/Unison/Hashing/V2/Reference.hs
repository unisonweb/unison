{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns   #-}

module Unison.Hashing.V2.Reference
  (Reference,
     pattern Builtin,
     pattern Derived,
     pattern DerivedId,
   Id(..),
   Pos,
   Size,
   derivedBase32Hex,
   Component, members,
   components,
   groupByComponent,
   componentFor,
   unsafeFromText,
   idFromText,
   isPrefixOf,
   fromShortHash,
   fromText,
   readSuffix,
   showShort,
   showSuffix,
   toId,
   toText,
   unsafeId,
   toShortHash,
   idToShortHash) where

import Unison.Prelude

import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified Data.Text       as Text
import qualified Unison.Hash     as H
import           Unison.Hashable as Hashable
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH
import Data.Char (isDigit)

-- | Either a builtin or a user defined (hashed) top-level declaration.
--
-- Used for both terms and types. Doesn't distinguish between them.
--
-- Other used defined things like local variables don't get @Reference@s.
data Reference
  = Builtin Text.Text
  -- `Derived` can be part of a strongly connected component.
  -- The `Pos` refers to a particular element of the component
  -- and the `Size` is the number of elements in the component.
  -- Using an ugly name so no one tempted to use this
  | DerivedId Id deriving (Eq,Ord,Generic)

pattern Derived :: H.Hash -> Pos -> Size -> Reference
pattern Derived h i n = DerivedId (Id h i n)

{-# COMPLETE Builtin, Derived #-}

-- | @Pos@ is a position into a cycle of size @Size@, as cycles are hashed together.
data Id = Id H.Hash Pos Size deriving (Generic)

unsafeId :: Reference -> Id
unsafeId (Builtin b) =
  error $ "Tried to get the hash of builtin " <> Text.unpack b <> "."
unsafeId (DerivedId x) = x

idToShortHash :: Id -> ShortHash
idToShortHash = toShortHash . DerivedId

-- todo: move these to ShortHash module?
-- but Show Reference currently depends on SH
toShortHash :: Reference -> ShortHash
toShortHash (Builtin b) = SH.Builtin b
toShortHash (Derived h _ 1) = SH.ShortHash (H.base32Hex h) Nothing Nothing
toShortHash (Derived h i n) = SH.ShortHash (H.base32Hex h) index Nothing
  where
    -- todo: remove `n` parameter; must also update readSuffix
    index = Just $ showSuffix i n

-- toShortHash . fromJust . fromShortHash == id and
-- fromJust . fromShortHash . toShortHash == id
-- but for arbitrary ShortHashes which may be broken at the wrong boundary, it
-- may not be possible to base32Hex decode them.  These will return Nothing.
-- Also, ShortHashes that include constructor ids will return Nothing;
-- try Referent.fromShortHash
fromShortHash :: ShortHash -> Maybe Reference
fromShortHash (SH.Builtin b) = Just (Builtin b)
fromShortHash (SH.ShortHash prefix cycle Nothing) = do
  h <- H.fromBase32Hex prefix
  case cycle of
    Nothing -> Just (Derived h 0 1)
    Just t -> case Text.splitOn "c" t of
      [i,n] -> Derived h <$> readMay (Text.unpack i) <*> readMay (Text.unpack n)
      _ -> Nothing
fromShortHash (SH.ShortHash _prefix _cycle (Just _cid)) = Nothing

-- (3,10) encoded as "3c10"
-- (0,93) encoded as "0c93"
showSuffix :: Pos -> Size -> Text
showSuffix i n = Text.pack $ show i <> "c" <> show n

-- todo: don't read or return size; must also update showSuffix and fromText
readSuffix :: Text -> Either String (Pos, Size)
readSuffix t = case Text.breakOn "c" t of
  (pos, Text.drop 1 -> size) | Text.all isDigit pos && Text.all isDigit size ->
    Right (read (Text.unpack pos), read (Text.unpack size))
  _ -> Left "suffix decoding error"

isPrefixOf :: ShortHash -> Reference -> Bool
isPrefixOf sh r = SH.isPrefixOf sh (toShortHash r)

toText :: Reference -> Text
toText = SH.toText . toShortHash

showShort :: Int -> Reference -> Text
showShort numHashChars = SH.toText . SH.take numHashChars . toShortHash

type Pos = Word64
type Size = Word64

newtype Component = Component { members :: Set Reference }

-- Gives the component (dependency cycle) that the reference is a part of
componentFor :: Reference -> Component
componentFor b@Builtin {} = Component (Set.singleton b)
componentFor (Derived h _ n) =
  Component $ Set.fromList [Derived h i n | i <- take (fromIntegral n) [0 ..]]

derivedBase32Hex :: Text -> Pos -> Size -> Reference
derivedBase32Hex b32Hex i n = DerivedId (Id (fromMaybe msg h) i n)
  where
  msg = error $ "Reference.derivedBase32Hex " <> show h
  h = H.fromBase32Hex b32Hex

unsafeFromText :: Text -> Reference
unsafeFromText = either error id . fromText

idFromText :: Text -> Maybe Id
idFromText s = case fromText s of
  Left _ -> Nothing
  Right (Builtin _) -> Nothing
  Right (DerivedId id) -> pure id

toId :: Reference -> Maybe Id
toId (DerivedId id) = Just id
toId Builtin{} = Nothing

-- examples:
-- `##Text.take` — builtins don’t have cycles
-- `#2tWjVAuc7` — derived, no cycle
-- `#y9ycWkiC1.y9` — derived, part of cycle
-- todo: take a (Reference -> CycleSize) so that `readSuffix` doesn't have to parse the size from the text.
fromText :: Text -> Either String Reference
fromText t = case Text.split (=='#') t of
  [_, "", b] -> Right (Builtin b)
  [_, h]     -> case Text.split (=='.') h of
    [hash]         -> Right (derivedBase32Hex hash 0 1)
    [hash, suffix] -> uncurry (derivedBase32Hex hash) <$> readSuffix suffix
    _ -> bail
  _ -> bail
  where bail = Left $ "couldn't parse a Reference from " <> Text.unpack t

component :: H.Hash -> [k] -> [(k, Id)]
component h ks = let
  size = fromIntegral (length ks)
  in [ (k, (Id h i size)) | (k, i) <- ks `zip` [0..]]

components :: [(H.Hash, [k])] -> [(k, Id)]
components sccs = uncurry component =<< sccs

groupByComponent :: [(k, Reference)] -> [[(k, Reference)]]
groupByComponent refs = done $ foldl' insert Map.empty refs
  where
    insert m (k, r@(Derived h _ _)) =
      Map.unionWith (<>) m (Map.fromList [(Right h, [(k,r)])])
    insert m (k, r) =
      Map.unionWith (<>) m (Map.fromList [(Left r, [(k,r)])])
    done m = sortOn snd <$> toList m

instance Show Id where show = SH.toString . SH.take 5 . toShortHash . DerivedId
instance Show Reference where show = SH.toString . SH.take 5 . toShortHash

instance Hashable.Hashable Reference where
  tokens (Builtin txt) = [Hashable.Tag 0, Hashable.Text txt]
  tokens (DerivedId (Id h i n)) = [Hashable.Tag 1, Hashable.Bytes (H.toByteString h), Hashable.Nat i, Hashable.Nat n]

-- | Two references mustn't differ in cycle length only.
instance Eq Id where x == y = compare x y == EQ
instance Ord Id where Id h i _ `compare` Id h2 i2 _  = compare h h2 <> compare i i2
