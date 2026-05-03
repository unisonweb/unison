-- | Robinson's unification over the spike 'Ty' AST.
--
-- Substitutions are 'IntMap's keyed on the inner 'Int' of a 'TyVar' and
-- maintained idempotent (the result of 'unify' satisfies @apply s (apply s
-- t) == apply s t@).
module Implicits.Unify
  ( unify,

    -- * Renaming utilities used by the resolver
    freshen,
    Supply,
    runSupply,
    fresh,
  )
where

import Control.Monad.State.Strict (State, evalState, get, put)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Implicits.Types

-- | Attempt to unify two types. Returns the (idempotent) most general
-- unifier extending the supplied substitution, or 'Nothing' on conflict.
unify :: Subst -> Ty -> Ty -> Maybe Subst
unify s t u = go s (applySubst s t) (applySubst s u)
  where
    go s0 a b = case (a, b) of
      _ | a == b -> Just s0
      (TVar v, _) -> bind s0 v b
      (_, TVar v) -> bind s0 v a
      (TCon c1, TCon c2)
        | c1 == c2 -> Just s0
        | otherwise -> Nothing
      (TApp f1 x1, TApp f2 x2) -> do
        s1 <- go s0 f1 f2
        go s1 (applySubst s1 x1) (applySubst s1 x2)
      _ -> Nothing

    bind s0 (TyVar i) t1
      | TVar (TyVar j) <- t1, i == j = Just s0
      | occurs i t1 = Nothing
      | otherwise =
          -- extend s0 with [i := t1] and propagate into existing image
          let s1 = IntMap.insert i t1 (fmap (applySubst (IntMap.singleton i t1)) s0)
           in Just s1

    occurs i t1 = TyVar i `Set.member` freeTyVars (applySubst s t1)

------------------------------------------------------------------------------
-- Fresh-variable supply
------------------------------------------------------------------------------

-- | A monotonic supply of fresh 'TyVar' integers.
type Supply = State Int

runSupply :: Int -> Supply a -> a
runSupply !start = flip evalState start

fresh :: Supply TyVar
fresh = do
  n <- get
  put (n + 1)
  pure (TyVar n)

-- | Rename every 'TyVar' in 'givenTyVars' of a candidate to a fresh
-- metavariable, returning the renamed premises and conclusion. Variables
-- *not* listed in 'givenTyVars' are left alone (they would only appear if
-- callers reused inner names; we still tolerate it).
freshen :: [TyVar] -> [Ty] -> Ty -> Supply ([Ty], Ty)
freshen vs prems concl = do
  -- Build an alpha-renaming substitution.
  pairs <- mapM (\v -> (v,) <$> fresh) vs
  let ren :: Map.Map TyVar TyVar
      ren = Map.fromList pairs
      rename = renameTy ren
  pure (map rename prems, rename concl)

-- | Rename only those variables found in the supplied map; leave others
-- untouched.
renameTy :: Map.Map TyVar TyVar -> Ty -> Ty
renameTy ren = go
  where
    go = \case
      TVar v -> TVar (Map.findWithDefault v v ren)
      TCon c -> TCon c
      TApp a b -> TApp (go a) (go b)
