{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Unison.Type.Context.Element where

import Control.Lens
import qualified Unison.Syntax.Type as T
import qualified Unison.Syntax.Var as V

-- | Elements of an algorithmic context
data Element l c where
  Universal :: V.Var -> Element l c                -- | ^ `v` is universally quantified
  Existential :: V.Var -> Element l c              -- | ^ `v` existential and unsolved
  Solved :: V.Var -> T.Monotype l c -> Element l c -- | ^ `v` is solved to some monotype
  Ann :: V.Var -> T.Type l c -> Element l c        -- | ^ `v` has type `a`, which may be quantified
  Marker :: V.Var -> Element l c                   -- | ^ used for scoping

(===) :: Element l c -> Element l c -> Bool
Existential v === Existential v2 | v == v2 = True
Universal v   === Universal v2 | v == v2 = True
Marker v      === Marker v2 | v == v2 = True
_ === _ = False

(!==) :: Element l c -> Element l c -> Bool
e1 !== e2 = not (e1 === e2)

_Universal :: Simple Prism (Element l c) V.Var
_Universal = prism Universal go where
  go (Universal v) = Right v
  go e = Left e

_Existential :: Simple Prism (Element l c) V.Var
_Existential = prism Existential go where
  go (Existential v) = Right v
  go e = Left e

_Solved :: Simple Prism (Element l c) (V.Var, T.Monotype l c)
_Solved = prism (uncurry Solved) go where
  go (Solved v t) = Right (v, t)
  go e = Left e

_Ann :: Simple Prism (Element l c) (V.Var, T.Type l c)
_Ann = prism (uncurry Ann) go where
  go (Ann v t) = Right (v, t)
  go e = Left e

_Marker :: Simple Prism (Element l c) V.Var
_Marker = prism Marker go where
  go (Marker v) = Right v
  go e = Left e

deriving instance (Show l, Show c) => Show (Element l c)
deriving instance (Ord l, Ord c) => Ord (Element l c)
deriving instance (Eq l, Eq c) => Eq (Element l c)
