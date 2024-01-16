module U.Core.ABT.Var where

import Data.Set (Set)

-- | A class for avoiding accidental variable capture
--
--   * `Set.notMember (freshIn vs v) vs`:
--     `freshIn` returns a variable not used in the `Set`
class (Ord v) => Var v where
  freshIn :: Set v -> v -> v
