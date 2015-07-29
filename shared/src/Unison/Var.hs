module Unison.Var where

import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Set as Set

class (Eq v, Ord v) => Var v where
  named :: Text -> v
  freshIn :: Set v -> v -> v

freshes :: Var v => Set v -> [v] -> [v]
freshes _ [] = []
freshes used (h:t) =
  let h' = freshIn used h
  in h' : freshes (Set.insert h' used) t

freshInBoth :: Var v => Set v -> Set v -> v -> v
freshInBoth vs1 vs2 = freshIn vs1 . freshIn vs2

freshNamed :: Var v => Set v -> Text -> v
freshNamed used n = freshIn used (named n)
