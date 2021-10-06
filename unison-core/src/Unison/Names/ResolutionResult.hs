{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Names.ResolutionResult where

import Unison.Prelude
import Unison.Reference as Reference ( Reference )
import Unison.Referent as Referent ( Referent )
import Unison.Names3 (Names0)
import Data.Set.NonEmpty

data ErrorDetails
  = TypeNotFound
  | -- Contains the names which were in scope and which References were possible options
    TypeAmbiguous Names0 (NESet Reference)
  | TermNotFound
  | -- Contains the names which were in scope and which Referents were possible options
    TermAmbiguous Names0 (NESet Referent)
  deriving (Eq, Ord, Show)

-- | ResolutionFailure represents the failure to resolve a given variable.
data ResolutionFailure var annotation = ResolutionFailure
  { resolutionVar :: var,
    resolutionAnnotation :: annotation,
    resolutionErrorDetails :: ErrorDetails
  }
  deriving (Eq, Ord, Show)

type ResolutionResult v a r = Either (Seq (ResolutionFailure v a)) r
