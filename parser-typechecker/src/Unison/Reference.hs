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
   hashComponents,
   groupByComponent,
   componentFor,
   unsafeFromText,
   readSuffix,
   showShort,
   splitSuffix) where

import           Control.Monad   (join)
import           Data.Foldable   (toList)
import           Data.List
import qualified Data.Map        as Map
import           Data.Maybe      (fromJust, fromMaybe, maybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Data.Word       (Word64)
import           GHC.Generics
import qualified Unison.ABT      as ABT
import qualified Unison.Hash     as H
import           Unison.Hashable as Hashable
import qualified Unison.Var      as Var
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

pattern Derived h n i = DerivedId (Id h n i)

data Id = Id H.Hash Pos Size deriving (Eq,Ord,Generic)

instance Show Id where
  show = addDot . splitSuffix where
    addDot (h, s) = show h <> maybe "" ("."<>) s

showShort :: Int -> Reference -> String
showShort numHashChars r =
  let (c, nc) = case r of
        Builtin t -> ("", Just ("#" <> Text.unpack t))
        DerivedId r -> splitSuffix r
  in "#" <> take numHashChars c <> fromMaybe "" nc

splitSuffix :: Id -> (String, Maybe String)
splitSuffix (Id h 0 1) = (show h, Nothing)
splitSuffix (Id h i n) = (show h, Just ("." <> showSuffix i n))
  where
    -- todo: remove `n` parameter; must also update readSuffix
    showSuffix :: Pos -> Size -> String
    showSuffix i n = Text.unpack . encode58 . runPutS $ put where
      encode58 = decodeUtf8 . Base58.encodeBase58 Base58.bitcoinAlphabet
      put = putLength i >> putLength n
      putLength = serialize . VarInt

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

hashComponents ::
     (Functor f, Hashable1 f, Foldable f, Eq v, Var.Var v)
  => (Reference -> ABT.Term f v ())
  -> Map.Map v (ABT.Term f v a)
  -> Map.Map v (Reference, ABT.Term f v a)
hashComponents embedRef tms =
  Map.fromList [ (v, (r,e)) | ((v,e), r) <- cs ]
  where cs = components $ ABT.hashComponents ref tms
        ref h i n = embedRef (DerivedId (Id h i n))

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

instance Show Reference where
  show (Builtin t)         = "##" <> Text.unpack t
  show (DerivedId id) = "#"  <> show id

instance Hashable.Hashable Reference where
  tokens (Builtin txt) = [Hashable.Tag 0, Hashable.Text txt]
  tokens (DerivedId (Id h i n)) = [Hashable.Tag 1, Hashable.Bytes (H.toBytes h), Hashable.Nat i, Hashable.Nat n]
