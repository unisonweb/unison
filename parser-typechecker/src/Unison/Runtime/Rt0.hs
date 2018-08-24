{-# Language OverloadedStrings #-}

module Unison.Runtime.Rt0 where

import Control.Monad.Identity (runIdentity)
import Data.Functor
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word64)
import Data.List
import Unison.Symbol (Symbol)
import Unison.Term (AnnotatedTerm)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Unison.ABT as ABT
import qualified Unison.Reference as R
import qualified Unison.Term as Term
import qualified Unison.Var as Var

type Arity = Int
type ConstructorId = Int
type Pos = Int
type ArgCount = Int

type Term v = AnnotatedTerm v ()

data V e
  = I Int64 | F Double | U Word64 | B Bool | T Text
  | Lam Arity (Term Symbol) (IR e)
  | Data R.Reference ConstructorId (Vector (V e))
  | Ext e deriving (Eq,Show)

data IR e
  = Var Pos
  | Add Pos Pos | Sub Pos Pos | Mult Pos Pos | Div Pos Pos
  | Let (IR e) (IR e)
  | LetRec [IR e] (IR e)
  | V (V e)
  | Apply Pos [Pos]
  | If Pos (IR e) (IR e) deriving (Eq,Show)

run :: IR R.Reference -> [V R.Reference] -> V R.Reference
run ir stack = case ir of
  Var i -> stack !! i
  V v -> v
  Add i j -> case (stack !! i, stack !! j) of
    (I i, I j) -> I (i + j)
    (F i, F j) -> F (i + j)
    (U i, U j) -> U (i + j)
    _ -> error "type error"
  Sub i j -> case (stack !! i, stack !! j) of
    (I i, I j) -> I (i - j)
    (F i, F j) -> F (i - j)
    (U i, U j) -> U (i - j)
    _ -> error "type error"
  Mult i j -> case (stack !! i, stack !! j) of
    (I i, I j) -> I (i * j)
    (F i, F j) -> F (i * j)
    (U i, U j) -> U (i * j)
    _ -> error "type error"
  Div i j -> case (stack !! i, stack !! j) of
    (I i, I j) -> I (i `div` j)
    (F i, F j) -> F (i / j)
    (U i, U j) -> U (i `div` j)
    _ -> error "type error"
  If c t f -> case stack !! c of
    B b -> if b then run t stack else run f stack
  Let b body -> run body (run b stack : stack)
  LetRec bs body ->
    let stack' = bs' ++ stack
        bs' = map (\ir -> run ir stack') bs
    in run body stack'
  Apply fnPos args -> call (stack !! fnPos) args stack

call :: V R.Reference -> [Pos] -> [V R.Reference] -> V R.Reference
call (Lam arity term body) args stack = let nargs = length args in
  case nargs of
    _ | nargs == arity -> run body (map (stack !!) args ++ stack)
    _ | nargs > arity ->
      let fn' = run body (map (stack !!) args ++ stack)
      in call fn' (drop arity args) stack
    _ -> {- nargs < arity -} case term of
      Term.LamsNamed' vs body -> Lam (arity - nargs) lam (compile lam)
        where
        lam = Term.lam'() (drop nargs vs) $
          ABT.substs (vs `zip` (map decompile . reverse . take nargs $ stack)) body

decompile :: V e -> Term Symbol
decompile _ = error "todo: decompile"

-- compile :: AnnotatedTerm Symbol a -> IR R.Reference
compile t = go (ABT.annotateBound' . Term.anf $ void t) where
  -- go :: AnnotatedTerm Symbol [Symbol] -> IR R.Reference
  go t = case t of
    Term.Int64' n -> V (I n)
    Term.UInt64' n -> V (U n)
    Term.Float' n -> V (F n)
    Term.Boolean' b -> V (B b)
    Term.Text' t -> V (T t)
    Term.Ref' r -> V (Ext r)
    Term.Var' v -> case elemIndex v (ABT.annotation t) of
      Nothing -> error $ "free variable during compilation: " ++ show v
      Just i -> Var i
    Term.Let1Named' _ b body -> Let (go b) (go body)
    Term.LetRecNamed' bs body -> LetRec (go . snd <$> bs) (go body)
    Term.Apps' f args -> Apply (ind t f) (map (ind t) args) where
      ind t (Term.Var' v) = case elemIndex v (ABT.annotation t) of
        Nothing -> error $ "free variable during compilation: " ++ show v
        Just i -> i
