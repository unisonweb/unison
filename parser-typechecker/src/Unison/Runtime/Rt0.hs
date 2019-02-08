{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language StrictData #-}
{-# Language BangPatterns #-}
{-# Language TupleSections #-}
{-# Language UnicodeSyntax #-}

module Unison.Runtime.Rt0 where

-- import qualified Data.Text as Text
import Data.Foldable
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word64)
import Unison.Codebase.Runtime (Runtime)
import Unison.Runtime.IR
import Unison.Symbol (Symbol)
import Unison.Term (AnnotatedTerm)
import Unison.Util.Monoid (intercalateMap)
import Unison.Var (Var)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Unison.Builtin as B
import qualified Unison.Codebase.Runtime as Rt
import qualified Unison.PrettyPrintEnv as PrettyPrintEnv
import qualified Unison.Reference as R
import qualified Unison.Runtime.ANF as ANF
import qualified Unison.Term as Term
import qualified Unison.TermPrinter as TermPrinter
import qualified Unison.Var as Var

runtime :: Var v => Runtime v
runtime = Rt.Runtime (pure ()) eval
  where
  missing r = error $ "Missing compiled form for: " ++ show r
  changeVar term = Term.vmap (\s -> Var.named (Var.shortName s)) term
  eval _code term = case normalize missing (changeVar term) of
    Nothing -> fail $ "result could not be decompiled from: " ++ show term
    Just t -> pure (changeVar t)

newtype Machine = Machine [V] -- a stack of values

instance Show Machine where
  show (Machine m) = "[ " ++ intercalateMap "\n  " show m ++ " ]"

push :: V -> Machine -> Machine
push v (Machine m) = Machine (v : m)

pushes :: [V] -> Machine -> Machine
pushes s (Machine m) = Machine (reverse s <> m)

at :: Z -> Machine -> V
at i (Machine m) = case i of
  Val v -> v
  Slot i -> m !! fromIntegral i
  LazySlot i -> m !! fromIntegral i -- todo: should this be Lazy?

ati :: Z -> Machine -> Int64
ati i m = case at i m of
  I i -> i
  _ -> error "type error"

atn :: Z -> Machine -> Word64
atn i m = case at i m of
  N i -> i
  _ -> error "type error"

atf :: Z -> Machine -> Double
atf i m = case at i m of
  F i -> i
  _ -> error "type error"

atb :: Z -> Machine -> Bool
atb i m = case at i m of
  B b -> b
  _ -> error "type error"

att :: Z -> Machine -> Text
att i m = case at i m of
  T t -> t
  _ -> error "type error"

data Result = RRequest Req | RMatchFail | RDone V deriving (Show)

done :: V -> Result
done = RDone

arity :: V -> Int
arity (Lam n _ _) = n
arity _ = 0

run :: (R.Reference -> IR) -> IR -> Machine -> Result
run env ir m = go ir m where
  go ir m = case ir of
    If c t f -> if atb c m then go t m else go f m
    And i j -> case at i m of
      b@(B False) -> done b
      _ -> go j m
    Or i j -> case at i m of
      b@(B True) -> done b
      _ -> go j m
    Not i -> done (B (not (atb i m)))
    Match scrutinee cases -> match (at scrutinee m) cases m
    Let b body -> case go b m of
      RRequest req -> RRequest (req `appendCont` body)
      RDone v -> go body (push v m)
      e -> error $ show e
    LetRec bs body -> let
      m' = pushes bs' m
      toVal (RDone a) = a
      toVal e = error ("bindings in a let rec must not have effects " ++ show e)
      bs' = map (\ir -> toVal $ go ir m') bs
      in go body m'
    MakeSequence vs -> done (Sequence (Vector.fromList (map (`at` m) vs)))
    ApplyZ fnPos args -> call (at fnPos m) args m
    ApplyIR (Leaf (Val fn)) args -> call fn args m
    ApplyIR fn args -> case go fn m of
      RRequest _req -> error "todo"
      RDone fn -> call fn args m
      e -> error $ show e
    Request r cid args -> RRequest (Req r cid ((`at` m) <$> args) (Leaf $ Slot 0))
    Handle handler body -> runHandler (at handler m) body m
    Leaf (Val v) -> done v
    Leaf s -> done (at s m)
    Construct r cid args -> done $ Data r cid ((`at` m) <$> args)
    -- Ints
    AddI i j -> done $ I (ati i m + ati j m)
    SubI i j -> done $ I (ati i m - ati j m)
    MultI i j -> done $ I (ati i m * ati j m)
    DivI i j -> done $ I (ati i m `div` ati j m)
    GtI i j -> done $ B (ati i m > ati j m)
    LtI i j -> done $ B (ati i m < ati j m)
    GtEqI i j -> done $ B (ati i m >= ati j m)
    LtEqI i j -> done $ B (ati i m <= ati j m)
    EqI i j -> done $ B (ati i m == ati j m)

    -- Floats
    AddF i j -> done $ F (atf i m + atf j m)
    SubF i j -> done $ F (atf i m - atf j m)
    MultF i j -> done $ F (atf i m * atf j m)
    DivF i j -> done $ F (atf i m / atf j m)
    GtF i j -> done $ B (atf i m > atf j m)
    LtF i j -> done $ B (atf i m < atf j m)
    GtEqF i j -> done $ B (atf i m >= atf j m)
    LtEqF i j -> done $ B (atf i m <= atf j m)
    EqF i j -> done $ B (atf i m == atf j m)

    -- Nats
    AddN i j -> done $ N (atn i m + atn j m)
    DropN i j -> done $ N (atn i m - atn j m)
    SubN i j -> done $ I (fromIntegral (atn i m) - fromIntegral (atn j m))
    MultN i j -> done $ N (atn i m * atn j m)
    DivN i j -> done $ N (atn i m `div` atn j m)
    GtN i j -> done $ B (atn i m > atn j m)
    LtN i j -> done $ B (atn i m < atn j m)
    GtEqN i j -> done $ B (atn i m >= atn j m)
    LtEqN i j -> done $ B (atn i m <= atn j m)
    EqN i j -> done $ B (atn i m == atn j m)

  -- If the body issues a request, we try passing it to the
  -- handler. If it fails, the request is reraised with the
  -- handler attached to the continuation. If the body
  -- completes without issuing a request, we pass `Pure` to
  -- the handler.
  runHandler :: V -> IR -> Machine -> Result
  runHandler h body m = case go body m of
    RRequest req -> case call h [Slot 0] (Requested req `push` m) of
      RMatchFail -> RRequest (wrapHandler h req)
      r -> r
    RDone v -> call h [Slot 0] (Pure v `push` m)
    r -> r

  runPattern :: V -> Pattern -> Machine -> Maybe Machine
  runPattern _ PatternIgnore m = Just m
  runPattern v PatternVar m = Just (push v m)
  runPattern v (PatternAs p) m = runPattern v p (push v m)
  runPattern (I n) (PatternI n') m = if n == n' then Just m else Nothing
  runPattern (F n) (PatternF n') m = if n == n' then Just m else Nothing
  runPattern (N n) (PatternN n') m = if n == n' then Just m else Nothing
  runPattern (B b) (PatternB b') m = if b == b' then Just m else Nothing
  runPattern (T t) (PatternT t') m = if t == t' then Just m else Nothing
  runPattern (Data rid cid args) (PatternData rid' cid' args') m | rid == rid' && cid == cid' =
    runPatterns args args' m
  runPattern (Sequence args) (PatternSequence args') m =
    runPatterns (toList args) (toList args') m
  runPattern (Pure v) (PatternPure p) m = runPattern v p m
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

  call :: V -> [Z] -> Machine -> Result
  call (Lam arity term body) args m = let nargs = length args in
    case nargs of
      _ | nargs == arity -> go body (map (`at` m) args `pushes` m)
      _ | nargs > arity ->
        case go body (map (`at` m) (take arity args) `pushes` m) of
          RRequest req -> RRequest $ req `appendCont` error "todo - overapplication yielding request"
          RDone fn' -> call fn' (drop arity args) m
          e -> error $ "type error, tried to apply: " ++ show e
      -- nargs < arity
      _ -> case term of
        Right (Term.LamsNamed' vs body) -> done $ Lam (arity - nargs) (Right lam) compiled
          where
          argvs = map (`at` m) args
          Just argterms = traverse decompile argvs
          toBound vs = reverse ((,Nothing) <$> vs)
          bound = toBound (drop nargs vs) ++ reverse (vs `zip` map Just argvs)
          compiled = compile0 env bound body
          lam = Term.let1' False (vs `zip` argterms) $
                Term.lam'() (drop nargs vs) body
        Left _builtin -> error "todo - handle partial application of builtins by forming closure"
        _ -> error "type error"
  call (Cont k) [arg] m = go k (push (at arg m) m)
  call f _ _ = error $ "type error " ++ show f

normalize :: (R.Reference -> IR) -> AnnotatedTerm Symbol a -> Maybe (Term Symbol)
normalize env t =
  let v = case run env (compile env $ Term.unannotate t) (Machine []) of
        RRequest e -> Requested e
        RDone a -> a
        e -> error $ show e
  in Term.vmap underlyingSymbol <$> decompile v

parseAndNormalize' :: String -> String
parseAndNormalize' s = parseAndNormalize env s
  where
  env r = case Map.lookup r builtins of
    Nothing -> error $ "unknown ref " ++ show r
    Just ir -> ir

parseAndNormalize :: (R.Reference -> IR) -> String -> String
parseAndNormalize env s = let
  tm = Term.unannotate $ B.tm s
  r = normalize env tm
  in prettyTerm (fromMaybe tm r)

prettyTerm :: Term Symbol -> String
prettyTerm t = let
  ppEnv = PrettyPrintEnv.fromNames B.names
  in TermPrinter.pretty' (Just 80) ppEnv t

parseANF :: String -> Term Symbol
parseANF s = ANF.fromTerm' id . Term.unannotate $ B.tm s

parseANFPretty :: String -> String
parseANFPretty s = prettyTerm (parseANF s)
