module Unison.Names.ResolutionResult where

import Data.Set.NonEmpty
import Unison.Names (Names)
import Unison.Prelude
import Unison.Reference as Reference (Reference)
import Unison.Referent as Referent (Referent)

data ResolutionError ref
  = NotFound
  | -- Contains the names which were in scope and which refs were possible options
    -- The NonEmpty set of refs must contain 2 or more refs (otherwise what is ambiguous?).
    Ambiguous Names (NESet ref)
  deriving (Eq, Ord, Show)

-- | ResolutionFailure represents the failure to resolve a given variable.
data ResolutionFailure var annotation
  = TypeResolutionFailure var annotation (ResolutionError Reference)
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
