{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ViewPatterns #-}
{-# Language PatternSynonyms #-}

module Unison.Runtime.ANF where

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

data Leaf0 v a r
  = I Int64 | F Double | N Word64 | T Text | B Bool
  | Data Reference Int [r]
  | ClosedLam (Term.AnnotatedTerm v a)
  | BuiltinLam IR.Arity Reference IR
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

pattern Var' v <- Leaf1 _ (Var v)
pattern Term' t <- Term _ t
pattern Leaf' l <- Term' (Leaf l)
pattern Leaf0' l <- Leaf1 _ (Leaf0 l)
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

toIR :: Var v => Term v a -> IR
toIR t = go [] (0::Word) t where
  ind v env = maybe (error $ show v ++ " var not found in: " ++ show env) id (elemIndex v env)
  go env n t = case out t of
    Leaf l -> leafToIR env n l
    ANF0 t -> case t of
      Handle h body -> case h of
        (Var' v) -> IR.Handle (ind v env) (go env n body)
        _ -> let
          v = Var.freshenId n (Var.nameds ":anf:")
          in IR.Handle 0 (go (v:env) (n+1) body)
      _ -> error "todo"
    Abs v body -> go (v:env) n body

  leafToIR _env _n e | Set.null (leafVars e) = IR.V (leafToV e)
  leafToIR env _n e = case e of
    Var' v -> IR.Var (ind v env)
    Leaf0' l -> case l of
      I n -> IR.V (IR.I n)
      F n -> IR.V (IR.F n)
      N n -> IR.V (IR.N n)
      B b -> IR.V (IR.B b)
      T txt -> IR.V (IR.T txt)
      Data _r _cid _vs -> error "todo"
      ClosedLam _lam -> error "todo"
      BuiltinLam _arity _ref _ir -> error "todo"
    _ -> error "unpossible"

  leafToV (Leaf0' l) = case l of
    I n -> IR.I n
    F n -> IR.F n
    N n -> IR.N n
    B b -> IR.B b
    T txt -> IR.T txt
    _ -> error "todo"
  leafToV (Var' _) = error "unpossible"
  leafToV _        = error "unpossible"

simplify :: Var v => Term v a -> Term v a
simplify t = case out t of
  Abs v body -> abs v $ simplify body
  Leaf _     -> t
  ANF0 t     -> case simplify <$> t of
    App (Lam' v body) arg -> simplify $ subst v arg body
    -- todo - add more rules here
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
