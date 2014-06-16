module Unison.Node.Implementations.State where

import Data.Map as M
import Control.Monad.State.Strict as S
import Unison.Node.Metadata as MD

-- need to add specific term/type Language
-- can add that to the Syntax package, probably should move Layout type there
-- as well
-- basically just
-- nails down some type signatures for term / type ADTs

data NodeState k t e = NodeState {
  terms :: M.Map k e,
  types :: M.Map k t,
  metadata :: M.Map k (MD.Metadata k)
}

empty :: NodeState k t e
empty = NodeState M.empty M.empty M.empty

-- node :: Node (State (NodeState k t e)




