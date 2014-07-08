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
data Element where
  Universal :: V.Var -> Element                -- | ^ `v` is universally quantified
  Existential :: V.Var -> Element              -- | ^ `v` existential and unsolved
  Solved :: V.Var -> T.Monotype -> Element -- | ^ `v` is solved to some monotype
  Ann :: V.Var -> T.Type -> Element        -- | ^ `v` has type `a`, which may be quantified
  Marker :: V.Var -> Element                   -- | ^ used for scoping

instance Show Element where
  show (Universal v) = show v
  show (Existential v) = "'"++show v
  show (Solved v t) = "'"++show v++" = "++show t
  show (Ann v t) = show v++" : "++show t
  show (Marker v) = "|"++show v++"|"

(===) :: Element -> Element -> Bool
Existential v === Existential v2 | v == v2 = True
Universal v   === Universal v2 | v == v2 = True
Marker v      === Marker v2 | v == v2 = True
_ === _ = False

(!==) :: Element -> Element -> Bool
e1 !== e2 = not (e1 === e2)

_Universal :: Simple Prism Element V.Var
_Universal = prism Universal go where
  go (Universal v) = Right v
  go e = Left e

_Existential :: Simple Prism Element V.Var
_Existential = prism Existential go where
  go (Existential v) = Right v
  go e = Left e

_Solved :: Simple Prism Element (V.Var, T.Monotype)
_Solved = prism (uncurry Solved) go where
  go (Solved v t) = Right (v, t)
  go e = Left e

_Ann :: Simple Prism Element (V.Var, T.Type)
_Ann = prism (uncurry Ann) go where
  go (Ann v t) = Right (v, t)
  go e = Left e

_Marker :: Simple Prism Element V.Var
_Marker = prism Marker go where
  go (Marker v) = Right v
  go e = Left e

deriving instance Ord Element
deriving instance Eq Element
