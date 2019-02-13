{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}


module Unison.Reference
  (Reference(DerivedPrivate_),
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
   showShort) where

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
  = Builtin_ Text.Text
  -- `Derived` can be part of a strongly connected component.
  -- The `Pos` refers to a particular element of the component
  -- and the `Size` is the number of elements in the component.
  -- Using an ugly name so no one tempted to use this
  | DerivedPrivate_ Id deriving (Eq,Ord,Generic)

data Id = Id H.Hash Pos Size deriving (Eq,Ord,Generic)

instance Show Id where
  show (Id h 0 1) = show h
  show (Id h i n) = show h <> "." <> showSuffix i n

showSuffix :: Pos -> Size -> String
showSuffix i n = Text.unpack . encode58 . runPutS $ put where
  encode58 = decodeUtf8 . Base58.encodeBase58 Base58.bitcoinAlphabet
  put = putLength i >> putLength n
  putLength = serialize . VarInt

readSuffix :: Text -> Either String (Pos, Size)
readSuffix t =
  runGetS get =<< (tagError . decode58) t where
  tagError = maybe (Left "base58 decoding error") Right
  decode58 :: Text -> Maybe ByteString
  decode58 = Base58.decodeBase58 Base58.bitcoinAlphabet . encodeUtf8
  get = (,) <$> getLength <*> getLength
  getLength = unVarInt <$> deserialize

pattern Builtin t = Builtin_ t
pattern Derived h n i = DerivedPrivate_ (Id h n i)
pattern DerivedId id = DerivedPrivate_ id

type Pos = Word64
type Size = Word64

newtype Component = Component { members :: Set Reference }

-- Gives the component (dependency cycle) that the reference is a part of
componentFor :: Reference -> Component
componentFor b@(Builtin_        _         ) = Component (Set.singleton b)
componentFor (  DerivedPrivate_ (Id h _ n)) = Component
  (Set.fromList
    [ DerivedPrivate_ (Id h i n) | i <- take (fromIntegral n) [0 ..] ]
  )

derivedBase58 :: Text -> Pos -> Size -> Reference
derivedBase58 b58 i n = DerivedPrivate_ (Id (fromJust h) i n)
  where
  h = H.fromBase58 b58

-- Parses Asdf##Foo as Builtin Foo
-- Parses Asdf#abc123-1-2 as Derived 'abc123' 1 2
unsafeFromText :: Text -> Reference
unsafeFromText = either error id . fromText

-- Parses Asdf##Foo as Builtin Foo
-- Parses Asdf#abc123-1-2 as Derived 'abc123' 1 2
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
        ref h i n = embedRef (DerivedPrivate_ (Id h i n))

component :: H.Hash -> [k] -> [(k, Reference)]
component h ks = let
  size = fromIntegral (length ks)
  in [ (k, DerivedPrivate_ (Id h i size)) | (k, i) <- ks `zip` [0..]]

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

showShort :: Int -> Reference -> String
showShort _ (Builtin_ t) = "##" <> Text.unpack t
showShort numHashChars (DerivedPrivate_ id) = "#" <> take numHashChars (show id)

instance Show Reference where
  show (Builtin_ t)         = "##" <> Text.unpack t
  show (DerivedPrivate_ id) = "#"  <> show id

instance Hashable.Hashable Reference where
  tokens (Builtin_ txt) = [Hashable.Tag 0, Hashable.Text txt]
  tokens (DerivedPrivate_ (Id h i n)) = [Hashable.Tag 1, Hashable.Bytes (H.toBytes h), Hashable.Nat i, Hashable.Nat n]
