module Unison.Merge.AliceIorBob
  ( AliceIorBob (..),
    whoL,
  )
where

import Control.Lens (Lens')
import Unison.Merge.TwoWayI (TwoWayI)

-- | Alice inclusive-or Bob?
data AliceIorBob
  = OnlyAlice
  | OnlyBob
  | AliceAndBob

whoL :: AliceIorBob -> Lens' (TwoWayI a) a
whoL = \case
  OnlyAlice -> #alice
  OnlyBob -> #bob
  AliceAndBob -> #both
