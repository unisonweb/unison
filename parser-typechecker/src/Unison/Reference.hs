{-# LANGUAGE DeriveGeneric #-}

module Unison.Reference where

import GHC.Generics
import Unison.Hashable as Hashable
import qualified Data.Text as Text
import qualified Unison.Hash as H
import Data.Word (Word64)
import Control.Monad (join)
import qualified Data.Map as Map
import Data.List
import Data.Foldable (toList)

-- could add a `Word` parameter to `Derived`
-- associated with each hash would actually be a list of terms / type decls
data Reference = Builtin Text.Text | Derived H.Hash deriving (Eq,Ord,Generic)

type Pos = Word64
type Size = Word64

derived :: H.Hash -> Pos -> Size -> Reference
derived _h _pos _size = error "todo"

component :: H.Hash -> [k] -> [(k, Reference)]
component h ks = let
  size = fromIntegral (length ks)
  in [ (k, derived h i size) | (k, i) <- ks `zip` [0..]]

components :: [(H.Hash, [k])] -> [(k, Reference)]
components sccs = join $ uncurry component <$> sccs

groupComponents :: [(k, Reference)] -> [[(k, Reference)]]
groupComponents refs = done $ foldl' insert Map.empty refs
  where
    insert m (k, r@(Derived h)) =
      Map.unionWith (<>) m (Map.fromList [(Right h, [(k,r)])])
    insert m (k, r) =
      Map.unionWith (<>) m (Map.fromList [(Left r, [(k,r)])])
    done m = sortOn snd <$> toList m

instance Show Reference where
  show (Builtin t) = Text.unpack t
  show (Derived h) = "#" <> show h

instance Hashable.Hashable Reference where
  tokens (Builtin txt) = [Hashable.Tag 0, Hashable.Text txt]
  tokens (Derived h) = [Hashable.Tag 0, Hashable.Bytes (H.toBytes h)]
