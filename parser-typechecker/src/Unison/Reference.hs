{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

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
   showShort) where

import GHC.Generics
import Data.Maybe (fromJust)
import Unison.Hashable as Hashable
import qualified Data.Text as Text
import qualified Unison.Hash as H
import Data.Word (Word64)
import Control.Monad (join)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Unison.ABT as ABT
import qualified Unison.Var as Var

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
  show (Id h i _) = show h <> "-" <> show i

pattern Builtin t = Builtin_ t
pattern Derived h n i <- DerivedPrivate_ (Id h n i)
pattern DerivedId id <- DerivedPrivate_ id

type Pos = Word64
type Size = Word64

newtype Component = Component { members :: Set Reference }

componentFor :: Reference -> Component
componentFor b@(Builtin_ _) = Component (Set.singleton b)
componentFor (DerivedPrivate_ (Id h _ n)) =
  Component (Set.fromList [ DerivedPrivate_ (Id h i n) | i <- take (fromIntegral n) [0..]])

derivedBase58 :: Text -> Pos -> Size -> Reference
derivedBase58 b58 i n = DerivedPrivate_ (Id (fromJust h) i n)
  where
  h = H.fromBase58 b58

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
showShort _ (Builtin_ t) = Text.unpack t
showShort numHashChars (DerivedPrivate_ id) = "#" <> take numHashChars (show id)

instance Show Reference where
  show (Builtin_ t) = Text.unpack t
  show (DerivedPrivate_ id) = "#" <> show id

instance Hashable.Hashable Reference where
  tokens (Builtin_ txt) = [Hashable.Tag 0, Hashable.Text txt]
  tokens (DerivedPrivate_ (Id h i n)) = [Hashable.Tag 1, Hashable.Bytes (H.toBytes h), Hashable.Nat i, Hashable.Nat n]
