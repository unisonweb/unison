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
import qualified Unison.Builtin as B
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
  | Requested (Req e)
  | Ext e
  deriving (Eq,Show)
--
-- Contains the effect ref and ctor id, the args, and the continuation
-- which expects the result at the top of the stack
data Req e = Req e Int [V e] (IR e)
  deriving (Eq,Show)

data IR e
  = Var Pos
  | Let (IR e) (IR e)
  | LetRec [IR e] (IR e)
  | V (V e)
  | ExtApply e [Pos] -- fully saturated builtin call
  | Apply (IR e) [Pos] -- fully saturated function call
  | DynamicApply Pos [Pos] -- call to unknown function
  | Construct R.Reference Int [Pos]
  | Request R.Reference Int [Pos]
  | Handle Pos (IR e)
  | If Pos (IR e) (IR e)
  | And Pos (IR e)
  | Or Pos (IR e)
  deriving (Eq,Show)

type Machine = [V R.Reference] -- a stack of values

push :: V R.Reference -> Machine -> Machine
push = (:)

pushes :: [V R.Reference] -> Machine -> Machine
pushes s m = reverse s <> m

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

appendCont :: Req e -> IR e -> Req e
appendCont (Req r cid args k) k2 = Req r cid args (Let k k2)

type Result = Either (Req R.Reference) (V R.Reference)

run :: (R.Reference -> Machine -> V R.Reference) -> IR R.Reference -> Machine -> Result
run env = go where
  go ir m = case ir of
    Var i -> Right (at i m)
    V v -> Right v
    Apply body args -> go body (map (`at` m) args `pushes` m)
    ExtApply fn args -> Right (env fn (map (`at` m) args `pushes` m))
    DynamicApply fnPos args -> call (at fnPos m) args m
    If c t f -> if atb c m then go t m else go f m
    And i j -> case at i m of b@(B False) -> Right b; _ -> go j m
    Or i j -> case at i m of b@(B True) -> Right b; _ -> go j m
    Construct r cid args -> Right (Data r cid ((`at` m) <$> args))
    Request r cid args -> Left (Req r cid ((`at` m) <$> args) (Var 0))
    Handle handler body -> case go body m of
      Left req -> call (at handler m) [0] (Requested req `push` m)
      r -> r
    Let b body -> case go b m of
      Left req -> Left $ req `appendCont` body
      Right v -> go body (v : m)
    LetRec bs body ->
      let m' = pushes bs' m
          g (Left e) = error ("bindings in a let rec must not have effects " ++ show e)
          g (Right a) = a
          bs' = map (\ir -> g $ go ir m') bs
      in go body m'

  call :: V R.Reference -> [Pos] -> Machine -> Result
  call (Lam arity term body) args m = let nargs = length args in
    case nargs of
      _ | nargs == arity -> go body (map (`at` m) args `pushes` m)
      _ | nargs > arity ->
        case go body (map (`at` m) (take arity args) `pushes` m) of
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
  Requested (Req r cid args _) ->
    let req = Term.apps (Term.request() r cid) (((),) . decompile <$> args)
    in req
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
    Term.And' x y -> And (ind t x) (go y)
    Term.Or' x y -> Or (ind t x) (go y)
    Term.If' cond ifT ifF -> If (ind t cond) (go ifT) (go ifF)
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
      Term.LamsNamed' vs body | ABT.isClosed f && length args == length vs
        -> Apply (go body) (map (ind t) args)
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

normalize :: (R.Reference -> Machine -> V R.Reference) -> AnnotatedTerm Symbol a -> Term Symbol
normalize env t =
  let v = case run env (compile $ Term.unannotate t) [] of
        Left e -> Requested e
        Right a -> a
  in decompile v

parseAndNormalize :: (R.Reference -> Machine -> V R.Reference) -> String -> Term Symbol
parseAndNormalize env s = normalize env (Term.unannotate $ B.tm s)

parseANF :: String -> Term Symbol
parseANF s = Term.anf . Term.unannotate $ B.tm s

--env0 :: R.Reference -> Machine -> Result
--env0 r = case r of
