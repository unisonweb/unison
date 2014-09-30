module Unison.Node where

import Data.Set as S
import Data.Map as M
import Unison.Node.Metadata as MD
import Unison.Syntax.Var as V
import Unison.Edit.Term.Action as A
import Unison.Edit.Term.Path as P
import Unison.Edit.Type.Path as TP
import Unison.Note as N

data Node m k t e = Node {
  -- | Obtain the type of the given subterm, assuming the path is valid
  admissibleTypeOf :: e -> P.Path -> Noted m t,
  -- | Create a new term and provide its metadata
  createTerm :: e -> MD.Metadata k -> Noted m k,
  -- | Create a new type and provide its metadata
  createType :: t -> MD.Metadata k -> Noted m k,
  -- | Lookup the direct dependencies of @k@, optionally limited to the given set
  dependencies :: Maybe (S.Set k) -> k -> Noted m (S.Set k),
  -- | Lookup the set of terms/types depending directly on the given @k@, optionally limited to the given set
  dependents :: Maybe (S.Set k) -> k -> Noted m (S.Set k),
  -- | Modify the given subterm, which may fail
  editTerm :: P.Path -> A.Action e -> e -> Noted m e,
  -- | Modify the given type, which may fail
  editType :: P.Path -> A.Action t -> t -> Noted m t,
  -- | Access the metadata for the term and/or types identified by @k@
  metadatas :: [k] -> Noted m (Map k (MD.Metadata k)),
  -- | Search for a term, optionally constrained to be of the given type
  -- and contained in the given set
  search :: Maybe t -> Maybe (S.Set k) -> Query -> Noted m (Map k (Metadata k)),
  -- | Search for a term in local scope of the given hash,
  -- optionally constrained to match some type
  searchLocal :: k -> P.Path -> Maybe t -> Query -> Noted m (Metadata k, [(V.Var, t)]),
  -- | Lookup the source of the term identified by @k@
  terms :: [k] -> Noted m (Map k e),
  -- | Lookup the dependencies of @k@, optionally limited to those that intersect the given set
  transitiveDependencies :: Maybe (S.Set k) -> k -> Noted m (S.Set k),
  -- | Lookup the set of terms or types which depend on the given @k@, optionally limited to those that intersect the given set
  transitiveDependents :: Maybe (S.Set k) -> k -> Noted m (S.Set k),
  -- | Lookup the source of the type identified by @k@
  types :: [k] -> Noted m (Map k t),
  -- | Obtain the type of the given subterm, assuming the path is valid
  typeOf :: e -> P.Path -> Noted m t,
  -- | Obtain the type of a constructor argument of a type
  typeOfConstructorArg :: k -> TP.Path -> Noted m t,
  -- | Update the metadata associated with the given term or type
  updateMetadata :: k -> MD.Metadata k -> Noted m ()

  -- possibly later
  -- editConstructor :: k -> -> A.Action -> m (Either N.Note (k, t)), -- ^ Modify the given type, which may fail
  -- examples :: k -> m (Maybe [k]), -- ^
}


