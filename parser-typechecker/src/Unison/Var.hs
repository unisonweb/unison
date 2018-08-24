{-# Language OverloadedStrings #-}

module Unison.Var where

import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Set as Set

-- | A class for variables. Variables may have auxiliary information which
-- may not form part of their identity according to `Eq` / `Ord`. Laws:
--
--   * `name (named n) == n`:
--     `name` returns the name set by `named`.
--   * `Set.notMember (freshIn vs v) vs`:
--     `freshIn` returns a variable not used in the `Set`
--   * `name (freshIn vs v) == name v`:
--     `freshIn` does not alter the name
--   * `Set.notMember (qualifiedName $ freshIn vs v) (Set.map qualifiedName vs)`:
--     `qualifiedName` incorporates all additional id info from freshening into
--     the name of the variable.
--   * `clear (freshIn vs v) === clear (freshIn vs (named (name v)))`:
--     `clear` strips any auxiliary information and returns a variable that behaves
--     as if it has been built solely via calls to `named` and `freshIn`. The `===`
--     is full equality, comparing any auxiliary info as well as qualified name.
--   * `clear v == v`, according to Haskell equality. In other words, no auxiliary
--     info attached to `v` values may participate in the `Eq` or `Ord` instances,
--     it is 'just' metadata.
--
class (Show v, Eq v, Ord v) => Var v where
  named :: Text -> v
  rename :: Text -> v -> v
  name :: v -> Text
  clear :: v -> v
  qualifiedName :: v -> Text
  freshIn :: Set v -> v -> v
  freshenId :: Word -> v -> v

nameds :: Var v => String -> v
nameds s = named (Text.pack s)

joinDot :: Var v => v -> v -> v
joinDot v v2 = named (shortName v `mappend` "." `mappend` shortName v2)

shortName :: Var v => v -> Text
shortName v | named (name v) == v = name v
shortName v = qualifiedName v

freshes :: Var v => Set v -> [v] -> [v]
freshes _ [] = []
freshes used (h:t) =
  let h' = freshIn used h
  in h' : freshes (Set.insert h' used) t

freshInBoth :: Var v => Set v -> Set v -> v -> v
freshInBoth vs1 vs2 = freshIn vs1 . freshIn vs2

freshNamed :: Var v => Set v -> Text -> v
freshNamed used n = freshIn used (named n)
