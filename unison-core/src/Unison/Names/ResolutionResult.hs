{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Names.ResolutionResult where

import Unison.Prelude
import Unison.Reference as Reference ( Reference )
import Unison.Referent as Referent ( Referent )
import qualified Data.Set as Set
import Unison.Names3 (Names)

-- | ResolutionFailure represents the failure to resolve a given variable.
data ResolutionFailure var annotation
  = TypeResolutionFailure var annotation (Set.Set Reference) Names
  | TermResolutionFailure var annotation (Set.Set Referent) Names
  deriving (Show, Eq, Ord)

getAnnotation :: ResolutionFailure v a -> a
getAnnotation = \case
  TypeResolutionFailure _ a _ _ -> a
  TermResolutionFailure _ a _ _ -> a

getVar :: ResolutionFailure v a -> v
getVar = \case
  TypeResolutionFailure v _ _ _ -> v
  TermResolutionFailure v _ _ _ -> v

type ResolutionResult v a r = Either (Seq (ResolutionFailure v a)) r
