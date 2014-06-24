module Unison.Node where

import Data.Set as S
import Data.Text
import Unison.Node.Panel
import Unison.Node.Metadata as M
import Unison.Edit.Term.Action as A
import Unison.Edit.Term.Path as P
import Unison.Edit.Type.Path as TP
import Unison.Note as N

data Node m k t e = Node {
  -- | Create a new term and provide its metadata
  createTerm :: e -> M.Metadata k -> Noted m k,
  -- | Create a new type and provide its metadata
  createType :: t -> M.Metadata k -> Noted m k,
  -- | Lookup the direct dependencies of @k@, optionally limited to the given set
  dependencies :: Maybe (S.Set k) -> k -> Noted m (S.Set k),
  -- | Lookup the set of terms/types depending directly on the given @k@, optionally limited to the given set
  dependents :: Maybe (S.Set k) -> k -> Noted m (S.Set k),
  -- | Modify the given subterm, which may fail
  edit :: k -> P.Path -> A.Action e -> Noted m (k, e),
  -- | Modify the given type, which may fail
  editType :: k -> P.Path -> A.Action t -> Noted m (k, t),
  -- | Access the metadata for the term or type identified by @k@
  metadata :: k -> Noted m (M.Metadata k),
  -- | Render the term or type identified by @k@ as a panel--
  panel :: k -> Noted m (Panel k t e),
  -- | Search for a term, optionally constrained to be of the given type
  search :: Maybe t -> Query -> Noted m [(k, Metadata k)],
  -- | Search for a term in local scope, optionally constrained to be of the given type
  searchLocal :: Maybe t -> Query -> Noted m [(e, Metadata k)],
  -- | Lookup the source of the term identified by @k@
  term :: k -> Noted m e,
  -- | Lookup the dependencies of @k@, optionally limited to those that intersect the given set
  transitiveDependencies :: Maybe (S.Set k) -> k -> Noted m (S.Set k),
  -- | Lookup the set of terms or types which depend on the given @k@, optionally limited to those that intersect the given set
  transitiveDependents :: Maybe (S.Set k) -> k -> Noted m (S.Set k),
  -- | Lookup the source of the type identified by @k@
  typ :: k -> Noted m t,
  -- | Obtain the type of the given subterm, assuming the path is valid
  typeOf :: k -> P.Path -> Noted m t,
  -- | Obtain the type of a constructor argument of a type
  typeOfConstructorArg :: k -> TP.Path -> Noted m t,
  -- | Update the metadata associated with the given term or type
  updateMetadata :: k -> M.Metadata k -> Noted m ()

  -- possibly later
  -- editConstructor :: k -> -> A.Action -> m (Either N.Note (k, t)), -- ^ Modify the given type, which may fail
  -- examples :: k -> m (Maybe [k]), -- ^
}

data Query = Query Text


