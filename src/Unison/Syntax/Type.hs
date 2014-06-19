{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}

module Unison.Syntax.Type where

import Control.Applicative
import qualified Data.Set as S
import qualified Unison.Syntax.Hash as H
import qualified Unison.Syntax.Kind as K
import qualified Unison.Syntax.Var as V

-- constructor is private not exported
data Monotype = Monotype { getPolytype :: Type } deriving (Eq,Ord)
instance Show Monotype where
  show (Monotype t) = show t

-- | Type literals
data Literal
  = Number
  | String
  | Vector
  | Hash H.Hash -- ^ A type literal uniquely defined by some nameless Hash
  deriving (Eq,Ord,Show,Read)

-- | Types in the Unison language
data Type
  = Unit Literal
  | Arrow Type Type
  | Universal V.Var
  | Existential V.Var
  | Ann Type K.Kind
  | Constrain Type () -- todo: constraint language
  | Forall V.Var Type -- ^ `DeBruijn 1` is bounded by nearest enclosing `Forall`, `DeBruijn 2` by next enclosing `Forall`, etc
  deriving (Eq,Ord,Show,Read)

monotype :: Type -> Maybe Monotype
monotype t = Monotype <$> go t where
  go (Unit l) = pure (Unit l)
  go (Arrow i o) = Arrow <$> go i <*> go o
  go (Universal v) = pure (Universal v)
  go (Existential v) = pure (Existential v)
  go (Ann t' k) = Ann <$> go t' <*> pure k
  go (Constrain t' c) = Constrain <$> go t' <*> pure c
  go _ = Nothing

abstract :: V.Var -> Type -> Type
abstract v = go V.bound1 where
  go _ u@(Unit _) = u
  go n (Arrow i o) = Arrow (go n i) (go n o)
  go n (Universal v')    | v == v'   = Universal n
  go _ u@(Universal _)   | otherwise = u
  go n (Existential v')  | v == v'   = Existential n
  go _ e@(Existential _) | otherwise = e
  go n (Ann t k) = Ann (go n t) k
  go n (Constrain t c) = Constrain (go n t) c
  go n (Forall v' fn) = Forall v' (go (V.succ n) fn)

-- | Type variable which is bound by the nearest enclosing `Forall`
bound1 :: Type
bound1 = Universal V.bound1

-- | HOAS syntax for `Forall` constructor:
-- `forall1 $ \x -> Arrow x x`
forall1 :: (Type -> Type) -> Type
forall1 f = forallN 1 $ \[x] -> f x

-- | HOAS syntax for `Forall` constructor:
-- `forall2 $ \x y -> Arrow (Arrow x y) (Arrow x y)`
forall2 :: (Type -> Type -> Type) -> Type
forall2 f = forallN 2 $ \[x,y] -> f x y

-- | HOAS syntax for `Forall` constructor:
-- `forall2 $ \x y z -> Arrow (Arrow x y z) (Arrow x y z)`
forall3 :: (Type -> Type -> Type -> Type) -> Type
forall3 f = forallN 3 $ \[x,y,z] -> f x y z

-- | HOAS syntax for `Forall` constructor:
-- `forallN 3 $ \[x,y,z] -> Arrow x (Arrow y z)`
forallN :: Int -> ([Type] -> Type) -> Type
forallN n f | n > 0 =
  let vars = take n (tail $ iterate V.decr V.bound1)
      inner = f (map Universal vars)
  in foldr (\v body -> Forall V.bound1 (abstract v body)) inner vars
forallN n _ | otherwise = error $ "forallN " ++ show n

subst1 :: Type -> Type -> Type
subst1 fn arg = subst fn V.bound1 arg

-- | mnemonic `subst fn var=arg`
subst :: Type -> V.Var -> Type -> Type
subst fn var arg = case fn of
  Unit l -> Unit l
  Arrow i o -> Arrow (subst i var arg) (subst o var arg)
  Universal v | v == var -> arg
              | otherwise -> fn
  Existential v | v == var -> arg
                | otherwise -> fn
  Ann fn' t -> Ann (subst fn' var arg) t
  Constrain fn' t -> Constrain (subst fn' var arg) t
  Forall v fn' -> Forall v (subst fn' (V.succ var) arg)

-- | The set of unbound variables in this type
freeVars :: Type -> S.Set V.Var
freeVars t = case t of
  Unit _ -> S.empty
  Arrow i o -> S.union (freeVars i) (freeVars o)
  Universal v -> S.singleton v
  Existential v -> S.singleton v
  Ann fn _ -> freeVars fn
  Constrain fn _ -> freeVars fn
  Forall v fn -> S.delete v (freeVars fn)

hash :: Type -> H.Digest
hash _ = error "todo: Type.hash"

finalizeHash :: Type -> H.Hash
finalizeHash = H.finalize . hash

hashes :: [Type] -> H.Hash
hashes _ = error "todo: Type.hashes"
