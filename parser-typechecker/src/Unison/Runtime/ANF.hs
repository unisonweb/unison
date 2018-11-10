{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ViewPatterns #-}
{-# Language PatternSynonyms #-}

module Unison.Runtime.ANF (int) where

import Prelude hiding (abs)
import Data.Foldable
import Data.List
import Data.Int (Int64)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word64)
import Unison.Reference (Reference)
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import Data.Vector (Vector)
import qualified Data.Set as Set
import qualified Unison.Var as Var
import Unison.Var (Var)
import Unison.Runtime.IR (IR)
import qualified Unison.Runtime.IR as IR
import qualified Unison.ABT as ABT
import Unison.Term

newtype ANF v a = ANF_ { term :: Term.AnnotatedTerm v a }

fromTerm :: (Semigroup a, Var v) => Term.AnnotatedTerm v a -> ANF v a
fromTerm t = go t where
  ann = ABT.annotation
  isVar (Var' _) = True
  isVar _ = False
  isClosedLam t@(LamNamed' _ _) | Set.null (ABT.freeVars t) = True
  isClosedLam _ = False
  fixAp t f args =
    let
      args' = Map.fromList $ toVar =<< (args `zip` [0..])
      toVar (b, i) | isVar b   = []
                   | otherwise = [(i, ABT.fresh t (Var.named . Text.pack $ "arg" ++ show i))]
      argsANF = map toANF (args `zip` [0..])
      toANF (b,i) = maybe b (var (ann b)) $ Map.lookup i args'
      addLet (b,i) body = maybe body (\v -> let1' False [(v,go b)] body) (Map.lookup i args')
    in foldr addLet (apps' f argsANF) (args `zip` [(0::Int)..])
  go :: AnnotatedTerm2 vt at a v a -> AnnotatedTerm2 vt at a v a
  go (Apps' f@(LamsNamed' vs body) args) | isClosedLam f = ap vs body args where
    ap vs body [] = lam' (ann f) vs body
    ap (v:vs) body (arg:args) = let1' False [(v,arg)] $ ap vs body args
    ap [] _body _args = error "type error"
  go t@(Apps' f args)
    | isVar f = fixAp t f args
    | otherwise = let fv' = ABT.fresh t (Var.named "f")
                  in let1' False [(fv', anf f)] (fixAp t (var (ann f) fv') args)
  go e@(Handle' h body)
    | isVar h = handle (ann e) h (go body)
    | otherwise = let h' = ABT.fresh e (Var.named "handler")
                  in let1' False [(h', go h)] (handle (ann e) (var (ann h) h') (go body))
  go e@(If' cond t f)
    | isVar cond = iff (ann e) cond (go t) (go f)
    | otherwise = let cond' = ABT.fresh e (Var.named "cond")
                  in let1' False [(cond', anf cond)] (iff (ann e) (var (ann cond) cond') t f)
  go e@(Match' scrutinee cases)
    | isVar scrutinee = match (ann e) scrutinee (fmap go <$> cases)
    | otherwise = let scrutinee' = ABT.fresh e (Var.named "scrutinee")
                  in let1' False [(scrutinee', go scrutinee)] (match (ann e) (var (ann scrutinee) scrutinee') cases)
  go e@(And' x y)
    | isVar x = and (ann e) x (go y)
    | otherwise =
        let x' = ABT.fresh e (Var.named "argX")
        in let1' False [(x', anf x)] (and (ann e) (var (ann x) x') (go y))
  go e@(Or' x y)
    | isVar x = or (ann e) x (go y)
    | otherwise =
        let x' = ABT.fresh e (Var.named "argX")
        in let1' False [(x', go x)] (or (ann e) (var (ann x) x') (go y))
  go e@(ABT.Tm' f) = ABT.tm' (ann e) (go <$> f)
  go e@(ABT.Var' _) = e
  go e@(ABT.out -> ABT.Cycle body) = ABT.cycle' (ann e) (go body)
  go e@(ABT.out -> ABT.Abs v body) = ABT.abs' (ann e) v (go body)
  go e = e

