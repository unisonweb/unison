{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ViewPatterns #-}
{-# Language PatternSynonyms #-}

module Unison.Runtime.ANF where

import Prelude hiding (abs)
import Data.Foldable
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

data Leaf0 v a r
  = I Int64 | F Double | N Word64 | T Text | B Bool
  | Data Reference Int [r]
  | ClosedLam (Term.AnnotatedTerm v a)
  deriving (Foldable, Traversable, Functor, Eq)

data Leaf1 v a = Var v | Leaf0 (Leaf0 v a (Leaf v a)) deriving Eq

data Leaf v a = Leaf1 { leafVars :: Set v, unLeaf :: Leaf1 v a }

instance (Eq a, Var v) => Eq (Leaf v a) where
  a == b = unLeaf a == unLeaf b

data ANF0 v a r
  = Handle (Leaf v a) r
  | Request Reference Int [r]
  | App (Leaf v a) (Leaf v a)
  | Ann r (Type.AnnotatedType v a)
  | Vector (Vector r)
  | If (Leaf v a) r r
  | And (Leaf v a) r
  | Or (Leaf v a) r
  | Lam r
  | LetRec [r] r
  | Let r r
  | Match (Leaf v a) [Term.MatchCase a r] deriving (Eq, Functor, Foldable, Traversable)

data ANF v a r
  = Abs v r
  | Leaf (Leaf v a)
  | ANF0 (ANF0 v a r) deriving (Eq, Functor, Foldable, Traversable)

data Term v a = Term { freeVars :: Set v, out :: ANF v a (Term v a) }

pattern Term' t <- Term _ t
pattern Leaf' l <- Term' (Leaf l)
pattern ANF0' a <- Term' (ANF0 a)
pattern Abs' v r <- Term' (Abs v r)
pattern Lam' v body <- Leaf1 _ (Leaf0 (ClosedLam (fromTerm -> Term' (Abs v body))))

tm0 :: Var v => ANF0 v a (Term v a) -> Term v a
tm0 f = tm (ANF0 f)

tm :: Var v => ANF v a (Term v a) -> Term v a
tm f = Term (Set.unions $ freeVars <$> toList f) f

leaf' :: Var v => Leaf0 v a (Leaf v a) -> Leaf v a
leaf' l = Leaf1 (Set.unions . toList $ leafVars <$> l) (Leaf0 l) where

var' :: Var v => v -> Leaf v a
var' v = Leaf1 (Set.singleton v) (Var v)

leaf :: Var v => Leaf v a -> Term v a
leaf l = Term (leafVars l) (Leaf l)

var :: Var v => v -> Term v a
var = leaf . var'

abs :: Var v => v -> Term v a -> Term v a
abs v body = Term (Set.delete v (freeVars body)) (Abs v body)

fromTerm :: Term.AnnotatedTerm v a -> Term v a
fromTerm = error "todo"

simplify :: Var v => Term v a -> Term v a
simplify t = case out t of
  Abs v body -> abs v $ simplify body
  Leaf _     -> t
  ANF0 t     -> case simplify <$> t of
    App (Lam' v body) arg -> simplify $ subst v arg body
    t -> tm0 t

-- Alpha equivalence
instance (Eq a, Var v, Eq v) => Eq (Term v a) where
  t1 == t2 = case (out t1, out t2) of
    (Leaf a, Leaf b) -> a == b
    (ANF0 tm1, ANF0 tm2) -> tm1 == tm2
    (Abs v body, Abs v2 body2) ->
      if v == v2 then body == body2
      else rename v v2 body == body2
    _ -> False

-- `subst v e ctx` replaces all unbound instance of `v` with `e` in `ctx`,
-- avoiding capture of any variables already mentioned in `ctx`.
subst :: Var v => v -> Leaf v a -> Term v a -> Term v a
subst v _ ctx | Set.notMember v (freeVars ctx) = ctx
subst v replacement ctx = case out ctx of
  Abs v' body | Set.member v' (leafVars replacement) ->
    abs freshV' (subst v replacement $ rename v' freshV' body)
    where
      freshV' = Var.freshInBoth (freeVars body) (leafVars replacement) $ v'
  Abs v' body -> abs v' (subst v replacement body)
  Leaf l     -> leaf (go l) where
    go a = case unLeaf a of
      Var v' -> if v' == v then replacement else a
      Leaf0 l -> leaf' $ go <$> l
  ANF0 tm    -> tm0 $ subst v replacement <$> tm

-- | renames `old` to `new` in the given term, ignoring subtrees that bind `old`
rename :: Var v => v -> v -> Term v a -> Term v a
rename old new t0 = case out t0 of
  Leaf l -> leaf (go l) where
    go a = case unLeaf a of
      Var v -> if v == old then var' new else a
      Leaf0 l -> leaf' $ go <$> l
  Abs v body | v == old  -> t0
             | otherwise -> abs v (rename old new body)
  ANF0 t -> tm0 (rename old new <$> t)
