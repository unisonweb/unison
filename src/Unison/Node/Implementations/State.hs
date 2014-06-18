module Unison.Node.Implementations.State where

import Data.Map as M
import Control.Monad.State.Strict as S
import Unison.Node.Metadata as MD
import Unison.Syntax.Hash as H
import Unison.Syntax.Type as T
import Unison.Syntax.Term as E
import Unison.Node (Node)
import qualified Unison.Note as Note
import Unison.Note (Note)

data NodeState = NodeState {
  terms :: M.Map H.Hash Term, -- ^ Maps term hash to source
  typeOf :: M.Map H.Hash Type, -- ^ Maps term hash to type
  types :: M.Map H.Hash Type, -- ^ Maps type hash to source
  metadata :: M.Map H.Hash (MD.Metadata H.Hash) -- ^ Metadata for terms, types
}

empty :: NodeState
empty = NodeState M.empty M.empty M.empty M.empty

data Store f = Store {
  readTerm :: H.Hash -> f (Maybe Term),
  writeTerm :: H.Hash -> Term -> f (),
  readType :: H.Hash -> f (Maybe Type),
  writeType :: H.Hash -> Type -> f (),
  readTypeOf :: H.Hash -> f (Maybe Type),
  writeTypeOf :: H.Hash -> Type -> f (),
  readMetadata :: H.Hash -> f (Maybe (MD.Metadata H.Hash)),
  writeMetadata :: H.Hash -> MD.Metadata H.Hash -> f ()
}

lookup' :: (Show a, Monad f) => (a -> f (Maybe b)) -> a -> f (Either Note b)
lookup' f a = liftM (Note.note' missing) (f a) where
  missing = "Could not find: " ++ show a

{-
node :: Monad f -> Store f -> Node f H.Hash Type Term
node store =
    createTerm e md =
      let hash = E.hash e
          t =
      let hash = E.hash e
          updateTerm s = s { terms = insert hash e (terms s) }
          updateMetadata s = s { terms = insert hash e (terms s) }
      in case synthesizeClosed  modify (updateTerm . updateMetadata)

      modify (\s -> s { terms }
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
-}
