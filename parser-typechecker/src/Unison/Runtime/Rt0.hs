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
import qualified Data.Vector as Vector
import qualified Unison.Runtime.ANF as ANF

newtype Machine = Machine [V] -- a stack of values

push :: V -> Machine -> Machine
push v (Machine m) = Machine (v : m)

pushes :: [V] -> Machine -> Machine
pushes s (Machine m) = Machine (reverse s <> m)

unpushes :: Int -> Machine -> [V]
unpushes n (Machine m) = reverse . take n $ m

at :: Int -> Machine -> V
at i (Machine m) = m !! i

ati :: Int -> Machine -> Int64
ati i m = case at i m of
  I i -> i
  _ -> error "type error"

atn :: Int -> Machine -> Word64
atn i m = case at i m of
  N i -> i
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
    Match scrutinee cases -> match (at scrutinee m) cases m
    Let b body -> case go b m of
      RRequest req -> RRequest (req `appendCont` body)
      RDone v -> go body (push v m)
      e -> error $ show e
    LetRec bs body ->
      let m' = pushes bs' m
          g (RDone a) = a
          g e = error ("bindings in a let rec must not have effects " ++ show e)
          bs' = map (\ir -> g $ go ir m') bs
      in go body m'
    MakeSequence vs -> done (Sequence (Vector.fromList (map (`at` m) vs)))
    -- Apply body args -> go body (map (`at` m) args `pushes` m)
    DynamicApply fnPos args -> call (at fnPos m) args m
    Resume cont arg -> case at cont m of
      Cont k -> go k (at arg m `push` m)
      v -> error $ "type error : resume expects a `Cont` here, got: " ++ show v
    Request r cid args -> RRequest (Req r cid ((`at` m) <$> args) (Var 0))
    Handle handler body -> case go body m of
      RRequest req -> call (at handler m) [0] (Requested req `push` m)
      r -> r
    Var i -> done (at i m)
    V v -> done v
    Construct r cid args -> done $ Data r cid ((`at` m) <$> args)
    AddI i j -> done $ I (ati i m + ati j m)
    SubI i j -> done $ I (ati i m - ati j m)
    MultI i j -> done $ I (ati i m * ati j m)
    DivI i j -> done $ I (ati i m `div` ati j m)
    AddF i j -> done $ F (atf i m + atf j m)
    SubF i j -> done $ F (atf i m - atf j m)
    MultF i j -> done $ F (atf i m * atf j m)
    DivF i j -> done $ F (atf i m / atf j m)
    AddN i j -> done $ N (atn i m + atn j m)
    SubN i j -> done $ N (atn i m - atn j m)
    MultN i j -> done $ N (atn i m * atn j m)
    DivN i j -> done $ N (atn i m `div` atn j m)

  runPattern :: V -> Pattern -> Machine -> Maybe Machine
  runPattern _ PatternIgnore m = Just m
  runPattern v PatternVar m = Just (push v m)
  runPattern (I n) (PatternI n') m | n == n' = Just m
  runPattern (F n) (PatternF n') m | n == n' = Just m
  runPattern (N n) (PatternN n') m | n == n' = Just m
  runPattern (B b) (PatternB b') m | b == b' = Just m
  runPattern (T t) (PatternT t') m | t == t' = Just m
  runPattern (Data rid cid args) (PatternData rid' cid' args') m | rid == rid' && cid == cid' =
    runPatterns args args' m
  runPattern (Sequence args) (PatternSequence args') m =
    runPatterns (toList args) (toList args') m
  runPattern (Requested (Req rid cid args k)) (PatternBind rid' cid' args' k') m | rid == rid' && cid == cid' =
    case runPatterns args args' m of
      Nothing -> Nothing
      Just m -> runPattern (Cont k) k' m
  runPattern _ _ _ = Nothing

  runPatterns [] [] m = Just m
  runPatterns (h:t) (hp:tp) m = case runPattern h hp m of
    Nothing -> Nothing
    Just m  -> runPatterns t tp m
  runPatterns _ _ _ = Nothing

  match :: V -> [(Pattern, Maybe IR, IR)] -> Machine -> Result
  match _ [] _ = RMatchFail
  match s ((pat,guard,rhs) : cases) m0 = case runPattern s pat m0 of
    Nothing -> match s cases m0 -- try next case
    Just m -> case guard of
      Nothing -> go rhs m -- no guard, commit to this case
      Just guard -> case go guard m of
        RDone (B True) -> go rhs m -- guard passed, commit to this case
        _ -> match s cases m0 -- guard failed, try next case

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
          Just argterms = traverse decompile (unpushes nargs m)
          lam = Term.lam'() (drop nargs vs) $
            ABT.substs (vs `zip` argterms) body
        Left _builtin -> error "todo - handle partial application of builtins by forming closure"
        _ -> error "type error"
  call _ _ _ = error "type error"

compile :: (R.Reference -> V) -> Term Symbol -> IR
compile env t = compile0 env [] t

compile0 :: (R.Reference -> V) -> [Symbol] -> Term Symbol -> IR
compile0 env bound t =
  go ((++ bound) <$> ABT.annotateBound' (ANF.fromTerm' t))
  where
  go t = case t of
    Term.And' x y -> And (ind t x) (go y)
    Term.LamsNamed' vs body -> undefined
      V (Lam (length vs) (Right $ void t) (compile0 env (ABT.annotation body) (void body)))
    Term.Or' x y -> Or (ind t x) (go y)
    Term.If' cond ifT ifF -> If (ind t cond) (go ifT) (go ifF)
    Term.Int' n -> V (I n)
    Term.Nat' n -> V (N n)
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
    -- fill in Term.Request and pattern matching
    _ -> error $ "TODO - don't know how to compile " ++ show t
    where
      unknown v = error $ "free variable during compilation: " ++ show v
      ind t (Term.Var' v) = case elemIndex v (ABT.annotation t) of
        Nothing -> error $ "free variable during compilation: " ++ show v
        Just i -> i
      ind _ e = error $ "ANF should eliminate any non-var arguments to apply " ++ show e

normalize :: (R.Reference -> V) -> AnnotatedTerm Symbol a -> Maybe (Term Symbol)
normalize env t =
  let v = case run env (compile env $ Term.unannotate t) (Machine []) of
        RRequest e -> Requested e
        RDone a -> a
        e -> error $ show e
  in decompile v

parseAndNormalize :: (R.Reference -> V) -> String -> (Maybe (Term Symbol))
parseAndNormalize env s = normalize env (Term.unannotate $ B.tm s)

parseANF :: String -> Term Symbol
parseANF s = ANF.fromTerm' . Term.unannotate $ B.tm s
