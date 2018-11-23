{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language StrictData #-}
{-# Language TupleSections #-}
{-# Language UnicodeSyntax #-}

module Unison.Runtime.Rt0 where

import Data.Foldable
import Data.Int (Int64)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word64)
import Unison.Runtime.IR
import Unison.Symbol (Symbol)
import Unison.Term (AnnotatedTerm)
-- import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Unison.ABT as ABT
import qualified Unison.Builtin as B
import qualified Unison.Reference as R
import qualified Unison.Runtime.ANF as ANF
import qualified Unison.Term as Term

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

data Result = RRequest Req | RMatchFail | RDone V deriving (Show)

done :: V -> Result
done = RDone

run :: (R.Reference -> IR) -> IR -> Machine -> Result
run env = go where
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
    LetRec bs body ->
      let m' = pushes bs' m
          g (RDone a) = a
          g e = error ("bindings in a let rec must not have effects " ++ show e)
          bs' = map (\ir -> g $ go ir m') bs
      in go body m'
    MakeSequence vs -> done (Sequence (Vector.fromList (map (`at` m) vs)))
    DynamicApply fnPos args -> call (at fnPos m) args m
    Request r cid args -> RRequest (Req r cid ((`at` m) <$> args) (Var 0))
    Handle handler body -> runHandler (at handler m) body m
    HandleV handler body -> runHandler handler body m
    Var i -> done (at i m)
    V v -> done v
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
    RRequest req -> case call h [0] (Requested req `push` m) of
      RMatchFail -> RRequest (wrapHandler h req)
      r -> r
    RDone v -> call h [0] (Pure v `push` m)
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

  call :: V -> [Pos] -> Machine -> Result
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
        Right (Term.LamsNamed' vs body) -> done $ Lam (arity - nargs) (Right lam) (compile env lam)
          where
          Just argterms = traverse decompile (unpushes nargs m)
          -- todo: introduce a `let` inside the lambda body to preserve sharing
          -- for any 'large' argterm with multiple occurrences in the body
          lam = Term.lam'() (drop nargs vs) $
            ABT.substs (vs `zip` argterms) body
        Left _builtin -> error "todo - handle partial application of builtins by forming closure"
        _ -> error "type error"
  call (Cont k) [arg] m = go k (push (at arg m) m)
  call _ _ _ = error "type error"

normalize :: (R.Reference -> IR) -> AnnotatedTerm Symbol a -> Maybe (Term Symbol)
normalize env t =
  let v = case run env (compile env $ Term.unannotate t) (Machine []) of
        RRequest e -> Requested e
        RDone a -> a
        e -> error $ show e
  in decompile v

parseAndNormalize' :: String -> Maybe (Term Symbol)
parseAndNormalize' s = parseAndNormalize env s
  where
  env r = case Map.lookup r builtins of
    Nothing -> error $ "unknown ref " ++ show r
    Just ir -> ir

parseAndNormalize :: (R.Reference -> IR) -> String -> (Maybe (Term Symbol))
parseAndNormalize env s = normalize env (Term.unannotate $ B.tm s)

parseANF :: String -> Term Symbol
parseANF s = ANF.fromTerm' . Term.unannotate $ B.tm s

builtins :: Map R.Reference IR
builtins = Map.fromList $
  [ (R.Builtin name, V (Lam arity (Left (R.Builtin name)) ir)) |
    (name, arity, ir) <-
      [ ("Int.+", 2, AddI 1 0)
      , ("Int.-", 2, SubI 1 0)
      , ("Int.*", 2, MultI 1 0)
      , ("Int./", 2, DivI 1 0)
      , ("Int.<", 2, LtI 1 0)
      , ("Int.>", 2, GtI 1 0)
      , ("Int.<=", 2, LtEqI 1 0)
      , ("Int.>=", 2, GtEqI 1 0)
      , ("Int.==", 2, EqI 1 0)
      --, ("Int.increment", "Int -> Int")
      --, ("Int.is-even", "Int -> Boolean")
      --, ("Int.is-odd", "Int -> Boolean")
      --, ("Int.signum", "Int -> Int")
      --, ("Int.negate", "Int -> Int")

      , ("Nat.+", 2, AddN 1 0)
      , ("Nat.drop", 2, DropN 1 0)
      , ("Nat.sub", 2, SubN 1 0)
      , ("Nat.*", 2, MultN 1 0)
      , ("Nat./", 2, DivN 1 0)
      , ("Nat.<", 2, LtN 1 0)
      , ("Nat.>", 2, GtN 1 0)
      , ("Nat.<=", 2, LtEqN 1 0)
      , ("Nat.>=", 2, GtEqN 1 0)
      , ("Nat.==", 2, EqN 1 0)
      --, ("Nat.increment", "Nat -> Nat")
      --, ("Nat.is-even", "Nat -> Boolean")
      --, ("Nat.is-odd", "Nat -> Boolean")

      , ("Float.+", 2, AddF 1 0)
      , ("Float.-", 2, SubF 1 0)
      , ("Float.*", 2, MultF 1 0)
      , ("Float./", 2, DivF 1 0)
      , ("Float.<", 2, LtF 1 0)
      , ("Float.>", 2, GtF 1 0)
      , ("Float.<=", 2, LtEqF 1 0)
      , ("Float.>=", 2, GtEqF 1 0)
      , ("Float.==", 2, EqF 1 0)

      , ("Boolean.not", 1, Not 0)

      , ("Text.empty", 0, V (T ""))
      --, ("Text.++", "Text -> Text -> Text")
      --, ("Text.take", "Nat -> Text -> Text")
      --, ("Text.drop", "Nat -> Text -> Text")
      --, ("Text.size", "Text -> Nat")
      --, ("Text.==", "Text -> Text -> Boolean")
      --, ("Text.!=", "Text -> Text -> Boolean")
      --, ("Text.<=", "Text -> Text -> Boolean")
      --, ("Text.>=", "Text -> Text -> Boolean")
      --, ("Text.<", "Text -> Text -> Boolean")
      --, ("Text.>", "Text -> Text -> Boolean")

      --, ("Stream.empty", "Stream a")
      --, ("Stream.single", "a -> Stream a")
      --, ("Stream.constant", "a -> Stream a")
      --, ("Stream.from-int", "Int -> Stream Int")
      --, ("Stream.from-nat", "Nat -> Stream Nat")
      --, ("Stream.cons", "a -> Stream a -> Stream a")
      --, ("Stream.take", "Nat -> Stream a -> Stream a")
      --, ("Stream.drop", "Nat -> Stream a -> Stream a")
      --, ("Stream.take-while", "(a ->{} Boolean) -> Stream a -> Stream a")
      --, ("Stream.drop-while", "(a ->{} Boolean) -> Stream a -> Stream a")
      --, ("Stream.map", "(a ->{} b) -> Stream a -> Stream b")
      --, ("Stream.flat-map", "(a ->{} Stream b) -> Stream a -> Stream b")
      --, ("Stream.fold-left", "b -> (b ->{} a ->{} b) -> Stream a -> b")
      --, ("Stream.iterate", "a -> (a -> a) -> Stream a")
      --, ("Stream.reduce", "a -> (a ->{} a ->{} a) -> Stream a -> a")
      --, ("Stream.toSequence", "Stream a -> Sequence a")
      --, ("Stream.filter", "(a ->{} Boolean) -> Stream a -> Stream a")
      --, ("Stream.scan-left", "b -> (b ->{} a ->{} b) -> Stream a -> Stream b")
      --, ("Stream.sum-int", "Stream Int -> Int")
      --, ("Stream.sum-nat", "Stream Nat -> Nat")
      --, ("Stream.sum-float", "Stream Float -> Float")
      --, ("Stream.append", "Stream a -> Stream a -> Stream a")
      --, ("Stream.zip-with", "(a ->{} b ->{} c) -> Stream a -> Stream b -> Stream c")
      --, ("Stream.unfold", "(a ->{} Optional (b, a)) -> b -> Stream a")

      --, ("Sequence.empty", "[a]")
      --, ("Sequence.cons", "a -> [a] -> [a]")
      --, ("Sequence.snoc", "[a] -> a -> [a]")
      --, ("Sequence.take", "Nat -> [a] -> [a]")
      --, ("Sequence.drop", "Nat -> [a] -> [a]")
      --, ("Sequence.++", "[a] -> [a] -> [a]")
      --, ("Sequence.size", "[a] -> Nat")
      --, ("Sequence.at", "Nat -> [a] -> Optional a")

      -- , ("Debug.watch", "Text -> a -> a")
      ]
  ]

