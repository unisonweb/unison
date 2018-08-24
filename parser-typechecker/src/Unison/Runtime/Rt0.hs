{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language StrictData #-}
{-# Language UnicodeSyntax #-}

module Unison.Runtime.Rt0 where

import Data.Functor
import Data.Foldable
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word64)
import Data.List
import Unison.Symbol (Symbol)
import Unison.Term (AnnotatedTerm)
import qualified Unison.ABT as ABT
import qualified Unison.Reference as R
import qualified Unison.Term as Term

type Arity = Int
type ConstructorId = Int
type Pos = Int
type ArgCount = Int

type Term v = AnnotatedTerm v ()

data V e
  = I Int64 | F Double | U Word64 | B Bool | T Text
  | Lam Arity (Term Symbol) (IR e)
  | Data R.Reference ConstructorId [V e]
  | Ext e deriving (Eq,Show)

data IR e
  = Var Pos
  | AddI Pos Pos | SubI Pos Pos | MultI Pos Pos | DivI Pos Pos
  | AddU Pos Pos | SubU Pos Pos | MultU Pos Pos | DivU Pos Pos
  | AddF Pos Pos | SubF Pos Pos | MultF Pos Pos | DivF Pos Pos
  | Let (IR e) (IR e)
  | LetRec [IR e] (IR e)
  | V (V e)
  | Apply Pos [Pos]
  | Construct R.Reference Int [Pos]
  | Request R.Reference Int [Pos]
  | Handle Pos (IR e)
  | If Pos (IR e) (IR e) deriving (Eq,Show)

type Rt = Machine -> V R.Reference
type Machine = [V R.Reference] -- a stack of values

-- change `run` Rt to return `Either Req` or `V`

push :: V R.Reference -> Machine -> Machine
push = (:)

pushes :: [V R.Reference] -> Machine -> Machine
pushes s m = s <> m

at :: Int -> Machine -> V R.Reference
at i m = m !! i

ati :: Int -> Machine -> Int64
ati i m = case at i m of
  I i -> i
  _ -> error "type error"

atu :: Int -> Machine -> Word64
atu i m = case at i m of
  U i -> i
  _ -> error "type error"

atf :: Int -> Machine -> Double
atf i m = case at i m of
  F i -> i
  _ -> error "type error"

atb :: Int -> Machine -> Bool
atb i m = case at i m of
  B b -> b
  _ -> error "type error"

att :: Int -> Machine -> Text
att i m = case at i m of
  T t -> t
  _ -> error "type error"

-- Contains the effect ref and ctor id, the args, and the continuation
-- which expects the result at the top of the stack
data Req = Req R.Reference Int [V R.Reference] (IR R.Reference)
  deriving Show

appendCont :: Req -> IR R.Reference -> Req
appendCont (Req r cid args k) k2 = Req r cid args (Let k k2)

type Result = Either Req (V R.Reference)

run :: IR R.Reference -> Machine -> Result
run ir m = case ir of
  If c t f -> if atb c m then run t m else run f m
  Let b body -> case run b m of
    Left req -> Left $ req `appendCont` body
    Right v -> run body (v : m)
  LetRec bs body ->
    let m' = pushes bs' m
        g (Left e) = error ("bindings in a let rec must not have effects " ++ show e)
        g (Right a) = a
        bs' = map (\ir -> g $ run ir m') bs
    in run body m'
  Apply fnPos args -> call (at fnPos m) args m
  Request r cid args -> Left (Req r cid ((`at` m) <$> args) (Var 0))
  ir -> pure $ case ir of
    Var i -> at i m
    V v -> v
    Construct r cid args -> Data r cid ((`at` m) <$> args)
    AddI i j -> I (ati i m + ati j m)
    SubI i j -> I (ati i m - ati j m)
    MultI i j -> I (ati i m * ati j m)
    DivI i j -> I (ati i m `div` ati j m)
    AddF i j -> F (atf i m + atf j m)
    SubF i j -> F (atf i m - atf j m)
    MultF i j -> F (atf i m * atf j m)
    DivF i j -> F (atf i m / atf j m)
    AddU i j -> U (atu i m + atu j m)
    SubU i j -> U (atu i m - atu j m)
    MultU i j -> U (atu i m * atu j m)
    DivU i j -> U (atu i m `div` atu j m)
    _ -> error "should be caught by above cases"

call :: V R.Reference -> [Pos] -> Machine -> Result
call (Lam arity term body) args m = let nargs = length args in
  case nargs of
    _ | nargs == arity -> run body (map (`at` m) args `pushes` m)
    _ | nargs > arity ->
      case run body (map (`at` m) args `pushes` m) of
        Left req -> Left $ req `appendCont` error "todo"
        Right fn' -> call fn' (drop arity args) m
    -- nargs < arity
    _ -> case term of
      Term.LamsNamed' vs body -> pure $ Lam (arity - nargs) lam (compile lam)
        where
        lam = Term.lam'() (drop nargs vs) $
          ABT.substs (vs `zip` (map decompile . reverse . take nargs $ m)) body
      _ -> error "type error"
call _ _ _ = error "type error"

decompile :: V R.Reference -> Term Symbol
decompile v = case v of
  I n -> Term.int64 () n
  U n -> Term.uint64 () n
  F n -> Term.float () n
  B b -> Term.boolean () b
  T t -> Term.text () t
  Lam _ f _ -> f
  Data r cid args -> Term.apps' (Term.constructor() r cid) (toList $ fmap decompile args)
  Ext r -> Term.ref () r

compile :: Term Symbol -> IR R.Reference
compile = compile0 []

compile0 :: [Symbol] -> Term Symbol -> IR R.Reference
compile0 bound t = go ((++ bound) <$> ABT.annotateBound' (Term.anf t)) where
  go t = case t of
    Term.LamsNamed' vs body
      | ABT.isClosed t -> V (Lam (length vs) (void t) (go body))
      | otherwise -> let
        fvs = toList $ ABT.freeVars t
        lifted = Term.lam'() (fvs ++ vs) (void body)
        in compile0 (ABT.annotation t) (Term.apps' lifted (Term.var() <$> fvs))
    Term.Int64' n -> V (I n)
    Term.UInt64' n -> V (U n)
    Term.Float' n -> V (F n)
    Term.Boolean' b -> V (B b)
    Term.Text' t -> V (T t)
    Term.Ref' r -> V (Ext r)
    Term.Var' v -> maybe (unknown v) Var $ elemIndex v (ABT.annotation t)
    Term.Let1Named' _ b body -> Let (go b) (go body)
    Term.LetRecNamed' bs body -> LetRec (go . snd <$> bs) (go body)
    Term.Constructor' r cid -> V (Data r cid mempty)
    Term.Apps' f args -> case f of
      Term.Request' r cid -> Request r cid (ind t <$> args)
      Term.Constructor' r cid -> Construct r cid (ind t <$> args)
      _ -> Apply (ind t f) (map (ind t) args) where
    Term.Handle' _h _body ->
      error "todo - make Machine model a bit richer, push and pop stack o' handlers"
    _ -> error $ "TODO - don't know how to compile " ++ show t
    where
      unknown v = error $ "free variable during compilation: " ++ show v
      ind t (Term.Var' v) = case elemIndex v (ABT.annotation t) of
        Nothing -> error $ "free variable during compilation: " ++ show v
        Just i -> i
      ind _ _ = error "ANF should eliminate any non-var arguments to apply"
