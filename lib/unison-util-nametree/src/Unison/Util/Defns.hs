module Unison.Util.Defns
  ( Defns (..),
    mapDefns,
    bimapDefns,
    zipDefns,
  )
where

import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Unison.Prelude

-- | Definitions (terms and types) in a namespace.
data Defns terms types = Defns
  { terms :: !terms,
    types :: !types
  }
  deriving stock (Generic, Show)
  deriving (Monoid, Semigroup) via GenericSemigroupMonoid (Defns terms types)

mapDefns :: (a -> b) -> Defns a a -> Defns b b
mapDefns f =
  bimapDefns f f

bimapDefns :: (terms -> terms') -> (types -> types') -> Defns terms types -> Defns terms' types'
bimapDefns f g (Defns terms types) =
  Defns (f terms) (g types)

zipDefns :: (tm1 -> tm2 -> tm3) -> (ty1 -> ty2 -> ty3) -> Defns tm1 ty1 -> Defns tm2 ty2 -> Defns tm3 ty3
zipDefns f g (Defns terms1 types1) (Defns terms2 types2) =
  Defns (f terms1 terms2) (g types1 types2)
