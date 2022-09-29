{-# LANGUAGE OverloadedStrings #-}

module Unison.PatternMatchCoverage.GrdTree where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import Data.ListLike (ListLike)
import Unison.PatternMatchCoverage.Fix
import Unison.Prelude
import Unison.Util.Pretty

type GrdTree n l = Fix (GrdTreeF n l)

data GrdTreeF n l a
  = LeafF l
  | GrdF n a
  | ForkF (NonEmpty a)
  deriving stock (Functor, Show)

prettyGrdTree :: forall n l s. (ListLike s Char, IsString s) => (n -> Pretty s) -> (l -> Pretty s) -> GrdTree n l -> Pretty s
prettyGrdTree prettyNode prettyLeaf = cata phi
  where
    phi = \case
      LeafF l -> prettyLeaf l
      GrdF n rest -> sep " " [prettyNode n, "──", rest]
      ForkF xs -> "──" <> group (sep "\n" (makeTree $ NEL.toList xs))
    makeTree :: [Pretty s] -> [Pretty s]
    makeTree = \case
      [] -> []
      x : [] -> [sep " " ["──", x]]
      x0 : x1 : xs ->
        sep " " ["┬─", x0] :
        let go y0 = \case
              [] -> [sep " " ["└─", y0]]
              y1 : ys -> "├─ " <> y0 : go y1 ys
         in [indent "  " (sep "\n" (go x1 xs))]

pattern Leaf :: l -> GrdTree n l
pattern Leaf x = Fix (LeafF x)

pattern Grd :: n -> GrdTree n l -> GrdTree n l
pattern Grd x rest = Fix (GrdF x rest)

pattern Fork :: NonEmpty (GrdTree n l) -> GrdTree n l
pattern Fork alts = Fix (ForkF alts)
