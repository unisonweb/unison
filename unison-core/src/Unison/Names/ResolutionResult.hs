{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Names.ResolutionResult where

import Unison.Prelude
import Unison.Reference as Reference ( Reference )
import Unison.Referent as Referent ( Referent )
import qualified Data.Set as Set

data ResolutionFailure var annotation
  = TermResolutionFailure var annotation (Set Referent)
  | TypeResolutionFailure var annotation (Set Reference)
  deriving (Eq,Ord,Show)

getAnnotation :: ResolutionFailure v a -> a
getAnnotation (TermResolutionFailure _ a _) = a
getAnnotation (TypeResolutionFailure _ a _) = a

getVar :: ResolutionFailure v a -> v
getVar (TermResolutionFailure v _ _) = v
getVar (TypeResolutionFailure v _ _) = v

isAmbiguation :: ResolutionFailure v a -> Bool
isAmbiguation (TermResolutionFailure _ _ s) = Set.size s > 1
isAmbiguation (TypeResolutionFailure _ _ s) = Set.size s > 1

type ResolutionResult v a r = Either (Seq (ResolutionFailure v a)) r
