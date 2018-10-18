{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language StrictData #-}
{-# Language TupleSections #-}
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
import Unison.Runtime.IR
import qualified Unison.Builtin as B
import qualified Unison.ABT as ABT
import qualified Unison.Reference as R
import qualified Unison.Term as Term

type Machine = [V] -- a stack of values

push :: V -> Machine -> Machine
push = (:)

pushes :: [V] -> Machine -> Machine
pushes s m = reverse s <> m

at :: Int -> Machine -> V
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

appendCont :: Req -> IR -> Req
appendCont (Req r cid args k) k2 = Req r cid args (Let k k2)

data Result = RRequest Req | RMatchFail | RDone V deriving (Show)

done :: V -> Result
done = RDone

run :: (R.Reference -> V) -> IR -> Machine -> Result
run env = go where
  go ir m = case ir of
    If c t f -> if atb c m then go t m else go f m
    And i j -> case at i m of
      b@(B False) -> done b
      _ -> go j m
    Or i j -> case at i m of
      b@(B True) -> done b
      _ -> go j m
    Let b body -> case go b m of
      RRequest req -> RRequest (req `appendCont` body)
      RDone v -> go body (v : m)
      e -> error $ show e
    LetRec bs body ->
      let m' = pushes bs' m
          g (RDone a) = a
          g e = error ("bindings in a let rec must not have effects " ++ show e)
          bs' = map (\ir -> g $ go ir m') bs
      in go body m'
    -- Apply body args -> go body (map (`at` m) args `pushes` m)
    DynamicApply fnPos args -> call (at fnPos m) args m
    Request r cid args -> RRequest (Req r cid ((`at` m) <$> args) (Var 0))
    Handle handler body -> case go body m of
      RRequest req -> call (at handler m) [0] (Requested req `push` m)
      r -> r
    ir -> done $ case ir of
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

  call :: V -> [Pos] -> Machine -> Result
  call (Lam arity term body) args m = let nargs = length args in
    case nargs of
      _ | nargs == arity -> go body (map (`at` m) args `pushes` m)
      _ | nargs > arity ->
        case go body (map (`at` m) (take arity args) `pushes` m) of
          RRequest req -> RRequest $ req `appendCont` error "todo"
          RDone fn' -> call fn' (drop arity args) m
          e -> error $ "type error, tried to apply: " ++ show e
      -- nargs < arity
      _ -> case term of
        Right (Term.LamsNamed' vs body) -> done $ Lam (arity - nargs) (Right lam) (compile env lam)
          where
          lam = Term.lam'() (drop nargs vs) $
            ABT.substs (vs `zip` (map decompile . reverse . take nargs $ m)) body
        Left _builtin -> error "todo - handle partial application of builtins by forming closure"
        _ -> error "type error"
  call _ _ _ = error "type error"

decompile :: V -> Term Symbol
decompile v = case v of
  I n -> Term.int () n
  U n -> Term.nat () n
  F n -> Term.float () n
  B b -> Term.boolean () b
  T t -> Term.text () t
  Lam _ f _ -> case f of Left r -> Term.ref() r; Right f -> f
  Data r cid args -> Term.apps' (Term.constructor() r cid) (toList $ fmap decompile args)
  Requested (Req r cid args _) ->
    let req = Term.apps (Term.request() r cid) (((),) . decompile <$> args)
    in req

compile :: (R.Reference -> V) -> Term Symbol -> IR
compile env = compile0 env []

compile0 :: (R.Reference -> V) -> [Symbol] -> Term Symbol -> IR
compile0 env bound t = go ((++ bound) <$> ABT.annotateBound' (Term.anf t)) where
  go t = case t of
    Term.LamsNamed' vs body
      | ABT.isClosed t -> V (Lam (length vs) (Right $ void t) (go body))
      | otherwise -> let
        fvs = toList $ ABT.freeVars t
        lifted = Term.lam'() (fvs ++ vs) (void body)
        in compile0 env (ABT.annotation t) (Term.apps' lifted (Term.var() <$> fvs))
    Term.And' x y -> And (ind t x) (go y)
    Term.Or' x y -> Or (ind t x) (go y)
    Term.If' cond ifT ifF -> If (ind t cond) (go ifT) (go ifF)
    Term.Int' n -> V (I n)
    Term.Nat' n -> V (U n)
    Term.Float' n -> V (F n)
    Term.Boolean' b -> V (B b)
    Term.Text' t -> V (T t)
    Term.Ref' r -> V (env r)
    Term.Var' v -> maybe (unknown v) Var $ elemIndex v (ABT.annotation t)
    Term.Let1Named' _ b body -> Let (go b) (go body)
    Term.LetRecNamed' bs body -> LetRec (go . snd <$> bs) (go body)
    Term.Constructor' r cid -> V (Data r cid mempty)
    Term.Apps' f args -> case f of
      Term.Ref' r -> Let (V (env r)) (DynamicApply 0 ((+1) . ind t <$> args))
      Term.Request' r cid -> Request r cid (ind t <$> args)
      Term.Constructor' r cid -> Construct r cid (ind t <$> args)
      _ -> DynamicApply (ind t f) (map (ind t) args) where
    Term.Handle' h body -> Handle (ind t h) (go body)
    Term.Ann' e _ -> go e
    _ -> error $ "TODO - don't know how to compile " ++ show t
    where
      unknown v = error $ "free variable during compilation: " ++ show v
      ind t (Term.Var' v) = case elemIndex v (ABT.annotation t) of
        Nothing -> error $ "free variable during compilation: " ++ show v
        Just i -> i
      ind _ e = error $ "ANF should eliminate any non-var arguments to apply " ++ show e

normalize :: (R.Reference -> V) -> AnnotatedTerm Symbol a -> Term Symbol
normalize env t =
  let v = case run env (compile env $ Term.unannotate t) [] of
        RRequest e -> Requested e
        RDone a -> a
        e -> error $ show e
  in decompile v

parseAndNormalize :: (R.Reference -> V) -> String -> Term Symbol
parseAndNormalize env s = normalize env (Term.unannotate $ B.tm s)

parseANF :: String -> Term Symbol
parseANF s = Term.anf . Term.unannotate $ B.tm s
