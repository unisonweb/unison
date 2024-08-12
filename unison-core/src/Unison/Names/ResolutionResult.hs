module Unison.Names.ResolutionResult
  ( ResolutionError (..),
    ResolutionFailure (..),
    ResolutionResult,
    getAnnotation,
    getVar,
  )
where

import Unison.Name (Name)
import Unison.Names (Names)
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)

data ResolutionError ref
  = NotFound
  | -- Contains:
    --
    --   1. The namespace names
    --   2. The refs among those that we could be referring to
    --   3. The local names that we could be referring to
    --
    -- The size of set (2.) + the size of set (3.) is at least 2 (otherwise there wouldn't be any ambiguity).
    Ambiguous Names (Set ref) (Set Name)
  deriving (Eq, Ord, Show)

-- | ResolutionFailure represents the failure to resolve a given variable.
data ResolutionFailure var annotation
  = TypeResolutionFailure var annotation (ResolutionError TypeReference)
  | TermResolutionFailure var annotation (ResolutionError Referent)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

getAnnotation :: ResolutionFailure v a -> a
getAnnotation = \case
  TypeResolutionFailure _ a _ -> a
  TermResolutionFailure _ a _ -> a

getVar :: ResolutionFailure v a -> v
getVar = \case
  TypeResolutionFailure v _ _ -> v
  TermResolutionFailure v _ _ -> v

type ResolutionResult v a r = Either (Seq (ResolutionFailure v a)) r
