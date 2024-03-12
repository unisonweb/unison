module Unison.Util.Defns
  ( Defns (..),
    mapDefns,
    bimapDefns,
    bifoldMapDefns,
    unzipDefns,
    unzipDefnsWith,
    zipDefns,
    zipDefnsWith,
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

bifoldMapDefns :: Monoid m => (a -> m) -> (b -> m) -> Defns a b -> m
bifoldMapDefns f g (Defns terms types) =
  f terms <> g types

unzipDefns :: Defns (tm1, tm2) (ty1, ty2) -> (Defns tm1 ty1, Defns tm2 ty2)
unzipDefns =
  unzipDefnsWith id id

unzipDefnsWith :: (tm1 -> (tm2, tm3)) -> (ty1 -> (ty2, ty3)) -> Defns tm1 ty1 -> (Defns tm2 ty2, Defns tm3 ty3)
unzipDefnsWith f g (Defns terms1 types1) =
  let (terms2, terms3) = f terms1
      (types2, types3) = g types1
   in (Defns terms2 types2, Defns terms3 types3)

zipDefns :: Defns tm1 ty1 -> Defns tm2 ty2 -> Defns (tm1, tm2) (ty1, ty2)
zipDefns =
  zipDefnsWith (,) (,)

zipDefnsWith :: (tm1 -> tm2 -> tm3) -> (ty1 -> ty2 -> ty3) -> Defns tm1 ty1 -> Defns tm2 ty2 -> Defns tm3 ty3
zipDefnsWith f g (Defns terms1 types1) (Defns terms2 types2) =
  Defns (f terms1 terms2) (g types1 types2)
