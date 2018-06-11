module Unison.Runtime.Rt0 where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word64)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Unison.ABT as ABT
import qualified Unison.Reference as R
import qualified Unison.Term as Term

type Arity = Int
type ConstructorId = Int
type Pos = Int
type ArgCount = Int

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
  | Apply ArgCount Pos
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
  Apply nargs fnPos -> case stack !! fnPos of
    Lam arity term body
      | nargs == arity -> run body stack
      | nargs >  arity ->
        let (extra, stack') = splitAt (nargs - arity) stack
            fn' = run body stack'
        in run (Apply (nargs - arity) 0) (fn' : (extra ++ drop nargs stack))
      | otherwise {- nargs < arity -} -> case term of
        Term.LamsNamed' vs body -> Lam (arity - nargs) lam (compile lam)
          where
          lam = Term.lam'' (drop nargs vs) $
            ABT.substs (vs `zip` (map decompile . reverse . take nargs $ stack)) body

decompile :: V e -> Term Symbol
decompile _ = error "todo: decompile"

--anf :: Term Symbol -> Term Symbol
--anf t = ABT.rewriteUp f $ t where
--  f t@(Term.App' f arg) = case arg of
--    Term.Var' _ -> t
--    _ -> ABT.freshen "arg"

compile :: Term Symbol -> IR R.Reference
compile t = case t of
  Term.Int64' n -> V (I n)
  Term.UInt64' n -> V (U n)
  Term.Float' n -> V (F n)
  Term.Boolean' b -> V (B b)
  Term.Text' t -> V (T t)
  Term.Ref' r -> V (Ext r)
  Term.Apps' f args -> case f of
    Term.Var' v -> error "compile todo"

