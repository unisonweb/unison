{-# LANGUAGE OverloadedRecordDot #-}

module Unison.Util.Defns
  ( Defns (..),
    DefnsF,
    alignDefnsWith,
    bimapDefns,
    bifoldMapDefns,
    bitraverseDefns,
    mapDefns,
    unzipDefns,
    unzipDefnsWith,
    zipDefns,
    zipDefnsWith,
  )
where

import Data.Align (Semialign, alignWith)
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.These (These)
import Unison.Prelude

-- | Definitions (terms and types) in a namespace.
data Defns terms types = Defns
  { terms :: !terms,
    types :: !types
  }
  deriving stock (Generic, Show)
  deriving (Monoid, Semigroup) via GenericSemigroupMonoid (Defns terms types)

-- | A common shape of definitions - terms and types are stored in the same structure.
type DefnsF f terms types =
  Defns (f terms) (f types)

alignDefnsWith :: Semialign f => (These a b -> c) -> Defns (f a) (f b) -> f c
alignDefnsWith f defns =
  alignWith f defns.terms defns.types

bimapDefns :: (terms -> terms') -> (types -> types') -> Defns terms types -> Defns terms' types'
bimapDefns f g (Defns terms types) =
  Defns (f terms) (g types)

bifoldMapDefns :: Monoid m => (a -> m) -> (b -> m) -> Defns a b -> m
bifoldMapDefns f g (Defns terms types) =
  f terms <> g types

bitraverseDefns :: Applicative f => (tm1 -> f tm2) -> (ty1 -> f ty2) -> Defns tm1 ty1 -> f (Defns tm2 ty2)
bitraverseDefns f g (Defns terms types) =
  Defns <$> f terms <*> g types

mapDefns :: (a -> b) -> Defns a a -> Defns b b
mapDefns f =
  bimapDefns f f

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
