module U.Codebase.Sqlite.Symbol where

import Data.Word (Word64)
import Data.Text (Text)
import qualified U.Core.ABT.Var as ABT
import qualified Data.Set as Set

data Symbol = Symbol !Word64 !Text deriving (Eq, Ord, Show)

-- |This clever instance relies on Ord to synthesize a new id.
-- If i > i2, then s > vs; otherwise increment the max i2:
--    freshIn [(0,"foo"), (1,"bar")] (0,"cat") = (3, "cat")
instance ABT.Var Symbol where
  freshIn vs s | Set.null vs || Set.notMember s vs = s -- already fresh!
  freshIn vs s@(Symbol i n) = case Set.elemAt (Set.size vs - 1) vs of
    Symbol i2 _ -> if i > i2 then s else Symbol (i2+1) n
