{-# LANGUAGE TemplateHaskell #-}

module Unison.A_Node where

import Data.Aeson.TH
import Data.Set (Set)
import Data.Map (Map)
import Unison.Note (Noted)
import Unison.Metadata (Metadata)
import Unison.A_TermEdit (Action)
import qualified Unison.A_Term as Term
import qualified Unison.Metadata as Metadata

-- | The results of a search.
-- On client, only need to repeat the query if we modify a character
-- at one of the examined positions OR if we add a character to a search
-- that previously returned incomplete results. Appending characters to a
-- search that returned complete results just filters down the set and
-- can be done client-side, assuming the client has the full result set.
data SearchResults k t e =
  SearchResults
    { query :: Metadata.Query
    , references :: [(k, Metadata k)]
    , matches :: ([e], Int)
    , illTypedMatches :: ([e], Int)
    , positionsExamined :: [Int] }

deriveJSON defaultOptions ''SearchResults

data Node m k t e = Node {
  -- | Obtain the type of the given subterm, assuming the path is valid
  admissibleTypeAt :: e -> Term.Path -> Noted m t,
  -- | Create a new term and provide its metadata
  createTerm :: e -> Metadata k -> Noted m k,
  -- | Create a new type and provide its metadata
  createType :: t -> Metadata k -> Noted m k,
  -- | Lookup the direct dependencies of @k@, optionally limited to the given set
  dependencies :: Maybe (Set k) -> k -> Noted m (Set k),
  -- | Lookup the set of terms/types depending directly on the given @k@, optionally limited to the given set
  dependents :: Maybe (Set k) -> k -> Noted m (Set k),
  -- | Modify the given subterm, which may fail. First argument is the root path.
  -- Second argument is path relative to the root.
  -- Returns (root path, original e, edited e)
  editTerm :: Term.Path -> Term.Path -> Action -> e -> Noted m (Maybe (Term.Path,e,e,Term.Path)),
  -- Evaluate all terms, returning a list of (path, original e, evaluated e)
  evaluateTerms :: [(Term.Path, e)] -> Noted m [(Term.Path,e,e)],
  -- | Returns ( subterm at the given path
  --           , current type
  --           , admissible type
  --           , local vars
  --           , well-typed applications of focus
  --           , well-typed expressions involving local vars )
  -- | Modify the given subterm, which may fail. First argument is the root path.
  localInfo :: e -> Term.Path -> Noted m (e, t, t, [e], [Int], [e]),
  -- | Access the metadata for the term and/or types identified by @k@
  metadatas :: [k] -> Noted m (Map k (Metadata k)),
  -- | Search for a term, optionally constrained to be of the given type
  search :: e -> Term.Path -> Int -> Metadata.Query -> Maybe t -> Noted m (SearchResults k t e),
  -- | Lookup the source of the term identified by @k@
  terms :: [k] -> Noted m (Map k e),
  -- | Lookup the dependencies of @k@, optionally limited to those that intersect the given set
  transitiveDependencies :: Maybe (Set k) -> k -> Noted m (Set k),
  -- | Lookup the set of terms or types which depend on the given @k@, optionally limited to those that intersect the given set
  transitiveDependents :: Maybe (Set k) -> k -> Noted m (Set k),
  -- | Lookup the source of the type identified by @k@
  types :: [k] -> Noted m (Map k t),
  -- | Obtain the type of the given subterm, assuming the path is valid
  typeAt :: e -> Term.Path -> Noted m t,
  -- | Update the metadata associated with the given term or type
  updateMetadata :: k -> Metadata k -> Noted m ()
}



