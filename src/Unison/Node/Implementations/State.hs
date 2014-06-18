module Unison.Node.Implementations.State where

import Data.Map as M
import Control.Monad.State.Strict as S
import Unison.Node.Metadata as MD
import Unison.Syntax.Hash as H
import Unison.Syntax.Type as T
import Unison.Syntax.Term as E
import Unison.Node (Node)
import qualified Unison.Node as N

-- need to add specific term/type Language
-- can add that to the Syntax package, probably should move Layout type there
-- as well
-- basically just
-- nails down some type signatures for term / type ADTs

data NodeState = NodeState {
  terms :: M.Map H.Hash Term,
  types :: M.Map H.Hash Type,
  metadata :: M.Map H.Hash (MD.Metadata H.Hash)
}

empty :: NodeState
empty = NodeState M.empty M.empty M.empty

node :: Monad f
     => (H.Hash -> Term -> f ()) -> (H.Hash -> f (Maybe Term))
     -> (H.Hash -> Type -> f ()) -> (H.Hash -> f (Maybe Type))
     -> (H.Hash -> MD.Metadata H.Hash -> f ()) -> (H.Hash -> f (Maybe (MD.Metadata H.Hash)))
     -> Node f H.Hash Type Term
node writeTerm readTerm writeType readType writeMetadata readMetadata =
  let
    createTerm e md = undefined
      {-
      let hash = E.hash e
          updateTerm s = s { terms = insert hash e (terms s) }
          updateMetadata s = s { terms = insert hash e (terms s) }
      in case synthesizeClosed  modify (updateTerm . updateMetadata)

      modify (\s -> s { terms }
-}
    createType = error "todo"
    dependencies = error "todo"
    dependents = error "todo"
    edit = error "todo"
    editType = error "todo"
    metadata = error "todo"
    panel = error "todo"
    search = error "todo"
    searchLocal = error "todo"
    term = error "todo"
    transitiveDependencies = error "todo"
    transitiveDependents = error "todo"
    typ = error "todo"
    typeOf = error "todo"
    typeOfConstructorArg = error "todo"
    updateMetadata = error "todo"
  in N.Node
       createTerm
       createType
       dependencies
       dependents
       edit
       editType
       metadata
       panel
       search
       searchLocal
       term
       transitiveDependencies
       transitiveDependents
       typ
       typeOf
       typeOfConstructorArg
       updateMetadata
