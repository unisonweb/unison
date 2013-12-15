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
import qualified Unison.Syntax.Var as V
import qualified Unison.Syntax.Kind as K

-- constructor is private not exported
data Monotype l c = Monotype { getPolytype :: Type l c }
deriving instance (Eq l, Eq c) => Eq (Monotype l c)
deriving instance (Ord l, Ord c) => Ord (Monotype l c)
instance (Show l, Show c) => Show (Monotype l c) where
  show (Monotype t) = "Monotype (" ++ show t ++ ")"

-- | Types with constraints `c`, free variables in `v` and kind annotations in `k`
data Type l c
  = Unit l
  | Arrow (Type l c) (Type l c)
  | Universal V.Var
  | Existential V.Var
  | Ann (Type l c) K.Kind
  | Constrain (Type l c) c
  | Forall V.Var (Type l c) -- | ^ `DeBruijn 1` is bounded by nearest enclosing `Forall`, `DeBruijn 2` by next enclosing `Forall`, etc

  -- deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)
deriving instance (Eq l, Eq c) => Eq (Type l c)
deriving instance (Ord l, Ord c) => Ord (Type l c)
deriving instance (Show l, Show c) => Show (Type l c)

trav :: Applicative f => (V.Var -> f V.Var) -> Type l c -> f (Type l c)
trav _ (Unit l) = pure (Unit l)
trav f (Arrow i o) = Arrow <$> trav f i <*> trav f o
trav f (Universal v) = Universal <$> f v
trav f (Existential v) = Existential <$> f v
trav f (Ann t k) = Ann <$> trav f t <*> pure k
trav f (Constrain t c) = Constrain <$> trav f t <*> pure c
trav f (Forall v fn) = Forall <$> f v <*> trav f fn

monotype :: Type l c -> Maybe (Monotype l c)
monotype t = Monotype <$> go t where
  go (Unit l) = pure (Unit l)
  go (Arrow i o) = Arrow <$> go i <*> go o
  go (Universal v) = pure (Universal v)
  go (Existential v) = pure (Existential v)
  go (Ann t' k) = Ann <$> go t' <*> pure k
  go (Constrain t' c) = Constrain <$> go t' <*> pure c
  go _ = Nothing

abstract :: V.Var -> Type l c -> Type l c
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
bound1 :: Type l c
bound1 = Universal V.bound1

-- | HOAS syntax for `Forall` constructor:
-- `forall1 $ \x -> Arrow x x`
forall1 :: (Type l c -> Type l c) -> Type l c
forall1 f = forallN 1 $ \[x] -> f x

-- | HOAS syntax for `Forall` constructor:
-- `forall2 $ \x y -> Arrow (Arrow x y) (Arrow x y)`
forall2 :: (Type l c -> Type l c -> Type l c) -> Type l c
forall2 f = forallN 2 $ \[x,y] -> f x y

-- | HOAS syntax for `Forall` constructor:
-- `forall2 $ \x y z -> Arrow (Arrow x y z) (Arrow x y z)`
forall3 :: (Type l c -> Type l c -> Type l c -> Type l c) -> Type l c
forall3 f = forallN 3 $ \[x,y,z] -> f x y z

-- | HOAS syntax for `Forall` constructor:
-- `forallN 3 $ \[x,y,z] -> Arrow x (Arrow y z)`
forallN :: Int -> ([Type l c] -> Type l c) -> Type l c
forallN n f | n > 0 =
  let vars = take n (tail $ iterate V.decr V.bound1)
      inner = f (map Universal vars)
  in foldr (\v body -> Forall V.bound1 (abstract v body)) inner vars
forallN n _ | otherwise = error $ "forallN " ++ show n

subst1 :: Type l c -> Type l c -> Type l c
subst1 fn arg = subst fn V.bound1 arg

-- | mnemonic `subst fn var=arg`
subst :: Type l c -> V.Var -> Type l c -> Type l c
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
freeVars :: Type l c -> S.Set V.Var
freeVars t = case t of
  Unit _ -> S.empty
  Arrow i o -> S.union (freeVars i) (freeVars o)
  Universal v -> S.singleton v
  Existential v -> S.singleton v
  Ann fn _ -> freeVars fn
  Constrain fn _ -> freeVars fn
  Forall v fn -> S.delete v (freeVars fn)
