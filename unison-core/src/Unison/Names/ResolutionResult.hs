{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Names.ResolutionResult where

import Unison.Prelude
import Unison.Reference as Reference ( Reference )
import Unison.Referent as Referent ( Referent )

data ResolutionFailure v a
  = TermResolutionFailure v a (Set Referent)
  | TypeResolutionFailure v a (Set Reference)
  deriving (Eq,Ord,Show)

type ResolutionResult v a r = Either (Seq (ResolutionFailure v a)) r
