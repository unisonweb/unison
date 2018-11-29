{-# Language RecursiveDo #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language StrictData #-}
{-# Language BangPatterns #-}
{-# Language TupleSections #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language FlexibleContexts #-}

module Unison.Runtime.Rt1 where

-- import qualified Data.Text as Text
import Control.Monad.ST (runST, ST)
import Control.Monad.Fix (MonadFix)
import Data.Array.ST (STArray)
import Data.Array.MArray (newArray, readArray, writeArray, MArray)
import Data.Foldable
import Data.Word (Word64)
import Unison.Runtime.IR
import qualified Unison.Reference as R

--data Machine arr m =
--  Machine {
--    stack :: arr V Word64,
--    size  :: Word64
--  }

-- instance Show Machine where
--  show (Machine m) = "[ " ++ intercalateMap "\n  " show m ++ " ]"

--push :: V -> Machine -> Machine
--push v (Machine m) = Machine (v : m)
--
--pushes :: [V] -> Machine -> Machine
--pushes s (Machine m) = Machine (reverse s <> m)
--
--at :: Z -> Machine -> V
--at i (Machine m) = case i of
--  Val v -> v
--  Slot i -> m !! i
--
--ati :: Z -> Machine -> Int64
--ati i m = case at i m of
--  I i -> i
--  _ -> error "type error"
--
--atn :: Z -> Machine -> Word64
--atn i m = case at i m of
--  N i -> i
--  _ -> error "type error"
--
--atf :: Z -> Machine -> Double
--atf i m = case at i m of
--  F i -> i
--  _ -> error "type error"
--
--atb :: Z -> Machine -> Bool
--atb i m = case at i m of
--  B b -> b
--  _ -> error "type error"
--
--att :: Z -> Machine -> Text
--att i m = case at i m of
--  T t -> t
--  _ -> error "type error"

data Result = RRequest !Req | RMatchFail | RDone !V deriving (Show)

done :: V -> Result
done = RDone

arity :: V -> Int
arity (Lam n _ _) = n
arity _ = 0

data Stack arr = Bottom | Stack !(arr Index V) !(Stack arr)

type TopSize = Word64
type Index = Word64

segmentSize :: Word64
segmentSize = 512

segmentSizeMinus1 :: Word64
segmentSizeMinus1 = segmentSize - 1

run :: (R.Reference -> IR) -> IR -> Result
run _env ir = runST $ do
  arr <- newTop :: ST s (STArray s Index V)
  go 0 arr Bottom ir
  where
  newTop :: MArray arr V m => m (arr Index V)
  newTop = newArray (0,segmentSize) (T "uninitialized")

  at :: (MArray arr V m) => Z -> TopSize -> arr Index V -> Stack arr -> m V
  at z size hd tl = case z of
    Val v -> pure v
    Slot i -> atSlot i size hd tl

  atSlot i0 size hd tl =
    -- if i == 0, read the `size - 1` element of the array
    -- if i == (size - 1), read the 0th element of the array
    if i0 < size then readArray hd (size - 1 - i0)
    else go i0 hd tl
    where
      go i _oldHd (Stack hd tl) =
        if i < segmentSize then readArray hd (segmentSizeMinus1 - i)
        else go (i - segmentSize) hd tl
      go _ _ Bottom = error $ "invalid index into stack " ++ show i0

  atb :: (MArray arr V m) => Z -> TopSize -> arr Index V -> Stack arr -> m Bool
  atb z size hd tl = go <$> at z size hd tl where
    go (B b) = b
    go _ = error "type error"

  push v size hd tl ir =
    if size == segmentSize then do
      hd' <- newTop
      writeArray hd' 0 v
      go 1 hd' (Stack hd tl) ir
    else writeArray hd size v *> go (size + 1) hd tl ir

  setAt size hd tl v i =
    if i < size then writeArray hd (size - 1 - i) v
    else case tl of
      Stack hd tl -> setAt segmentSize hd tl v (i - size)
      _ -> error $ "invalid index " ++ show i

  growBy n size hd tl = case size + n of
    size | size <= segmentSize -> pure (size, hd, tl)
    _ -> do
      hd' <- newTop
      growBy (n - (segmentSize - size)) 0 hd' (Stack hd tl)

  go :: (MonadFix m, MArray arr V m) => TopSize -> arr Index V -> Stack arr -> IR -> m Result
  go !size !hd !tl !ir = case ir of
    If (Val (B c)) t f -> if c then go size hd tl t else go size hd tl f
    If (Slot i) t f -> do
      B c <- atSlot i size hd tl
      if c then go size hd tl t
           else go size hd tl f
    And i j -> do
      b@(B c) <- at i size hd tl
      if c then go size hd tl j
           else pure (done b)
    Or i j -> do
      b@(B c) <- at i size hd tl
      if c then pure (done b)
           else go size hd tl j
    Not i -> done . B . not <$> atb i size hd tl
    Let b body -> do
      r <- go size hd tl b
      case r of
        RRequest req -> pure $ RRequest (req `appendCont` body)
        RDone v -> push v size hd tl body
        e -> error $ show e
    LetRec bs body -> do
      -- todo: switch to explicit mutable references rather than relying on Haskell laziness
      let len = fromIntegral (length bs)
      (size, hd, tl) <- growBy len size hd tl
      mdo
        -- I'll be amazed if this works
        traverse_ (\(RDone v, i) -> setAt size hd tl v i) (reverse bs' `zip` [(0::Word64)..])
        bs' <- traverse (go size hd tl) bs
        pure ()
      go size hd tl body
    _ -> undefined

--  go ir m = case ir of
--    Match scrutinee cases -> match (at scrutinee m) cases m
--    MakeSequence vs -> done (Sequence (Vector.fromList (map (`at` m) vs)))
--    ApplyZ fnPos args -> call (at fnPos m) args m
--    ApplyIR (V fn) args -> call fn args m
--    ApplyIR fn args -> case go fn m of
--      RRequest _req -> error "todo"
--      RDone fn -> call fn args m
--      e -> error $ show e
--    Request r cid args -> RRequest (Req r cid ((`at` m) <$> args) (Var 0))
--    Handle handler body -> runHandler (at handler m) body m
--    Var i -> done (at (Slot i) m)
--    V v -> done v
--    Construct r cid args -> done $ Data r cid ((`at` m) <$> args)
--    -- Ints
--    AddI i j -> done $ I (ati i m + ati j m)
--    SubI i j -> done $ I (ati i m - ati j m)
--    MultI i j -> done $ I (ati i m * ati j m)
--    DivI i j -> done $ I (ati i m `div` ati j m)
--    GtI i j -> done $ B (ati i m > ati j m)
--    LtI i j -> done $ B (ati i m < ati j m)
--    GtEqI i j -> done $ B (ati i m >= ati j m)
--    LtEqI i j -> done $ B (ati i m <= ati j m)
--    EqI i j -> done $ B (ati i m == ati j m)
--
--    -- Floats
--    AddF i j -> done $ F (atf i m + atf j m)
--    SubF i j -> done $ F (atf i m - atf j m)
--    MultF i j -> done $ F (atf i m * atf j m)
--    DivF i j -> done $ F (atf i m / atf j m)
--    GtF i j -> done $ B (atf i m > atf j m)
--    LtF i j -> done $ B (atf i m < atf j m)
--    GtEqF i j -> done $ B (atf i m >= atf j m)
--    LtEqF i j -> done $ B (atf i m <= atf j m)
--    EqF i j -> done $ B (atf i m == atf j m)
--
--    -- Nats
--    AddN i j -> done $ N (atn i m + atn j m)
--    DropN i j -> done $ N (atn i m - atn j m)
--    SubN i j -> done $ I (fromIntegral (atn i m) - fromIntegral (atn j m))
--    MultN i j -> done $ N (atn i m * atn j m)
--    DivN i j -> done $ N (atn i m `div` atn j m)
--    GtN i j -> done $ B (atn i m > atn j m)
--    LtN i j -> done $ B (atn i m < atn j m)
--    GtEqN i j -> done $ B (atn i m >= atn j m)
--    LtEqN i j -> done $ B (atn i m <= atn j m)
--    EqN i j -> done $ B (atn i m == atn j m)
--
--  -- If the body issues a request, we try passing it to the
--  -- handler. If it fails, the request is reraised with the
--  -- handler attached to the continuation. If the body
--  -- completes without issuing a request, we pass `Pure` to
--  -- the handler.
--  runHandler :: V -> IR -> Machine -> Result
--  runHandler h body m = case go body m of
--    RRequest req -> case call h [Slot 0] (Requested req `push` m) of
--      RMatchFail -> RRequest (wrapHandler h req)
--      r -> r
--    RDone v -> call h [Slot 0] (Pure v `push` m)
--    r -> r
--
--  runPattern :: V -> Pattern -> Machine -> Maybe Machine
--  runPattern _ PatternIgnore m = Just m
--  runPattern v PatternVar m = Just (push v m)
--  runPattern v (PatternAs p) m = runPattern v p (push v m)
--  runPattern (I n) (PatternI n') m = if n == n' then Just m else Nothing
--  runPattern (F n) (PatternF n') m = if n == n' then Just m else Nothing
--  runPattern (N n) (PatternN n') m = if n == n' then Just m else Nothing
--  runPattern (B b) (PatternB b') m = if b == b' then Just m else Nothing
--  runPattern (T t) (PatternT t') m = if t == t' then Just m else Nothing
--  runPattern (Data rid cid args) (PatternData rid' cid' args') m | rid == rid' && cid == cid' =
--    runPatterns args args' m
--  runPattern (Sequence args) (PatternSequence args') m =
--    runPatterns (toList args) (toList args') m
--  runPattern (Pure v) (PatternPure p) m = runPattern v p m
--  runPattern (Requested (Req rid cid args k)) (PatternBind rid' cid' args' k') m | rid == rid' && cid == cid' =
--    case runPatterns args args' m of
--      Nothing -> Nothing
--      Just m -> runPattern (Cont k) k' m
--  runPattern _ _ _ = Nothing
--
--  runPatterns [] [] m = Just m
--  runPatterns (h:t) (hp:tp) m = case runPattern h hp m of
--    Nothing -> Nothing
--    Just m  -> runPatterns t tp m
--  runPatterns _ _ _ = Nothing
--
--  match :: V -> [(Pattern, Maybe IR, IR)] -> Machine -> Result
--  match _ [] _ = RMatchFail
--  match s ((pat,guard,rhs) : cases) m0 = case runPattern s pat m0 of
--    Nothing -> match s cases m0 -- try next case
--    Just m -> case guard of
--      Nothing -> go rhs m -- no guard, commit to this case
--      Just guard -> case go guard m of
--        RDone (B True) -> go rhs m -- guard passed, commit to this case
--        _ -> match s cases m0 -- guard failed, try next case
--
--  call :: V -> [Z] -> Machine -> Result
--  call (Lam arity term body) args m = let nargs = length args in
--    case nargs of
--      _ | nargs == arity -> go body (map (`at` m) args `pushes` m)
--      _ | nargs > arity ->
--        case go body (map (`at` m) (take arity args) `pushes` m) of
--          RRequest req -> RRequest $ req `appendCont` error "todo - overapplication yielding request"
--          RDone fn' -> call fn' (drop arity args) m
--          e -> error $ "type error, tried to apply: " ++ show e
--      -- nargs < arity
--      _ -> case term of
--        Right (Term.LamsNamed' vs body) -> done $ Lam (arity - nargs) (Right lam) compiled
--          where
--          argvs = map (`at` m) args
--          Just argterms = traverse decompile argvs
--          toBound vs = reverse ((,Nothing) <$> vs)
--          bound = toBound (drop nargs vs) ++ reverse (vs `zip` map Just argvs)
--          compiled = compile0 env bound body
--          lam = Term.let1' False (vs `zip` argterms) $
--                Term.lam'() (drop nargs vs) body
--        Left _builtin -> error "todo - handle partial application of builtins by forming closure"
--        _ -> error "type error"
--  call (Cont k) [arg] m = go k (push (at arg m) m)
--  call f _ _ = error $ "type error " ++ show f
--
--normalize :: (R.Reference -> IR) -> AnnotatedTerm Symbol a -> Maybe (Term Symbol)
--normalize env t =
--  let v = case run env (compile env $ Term.unannotate t) (Machine []) of
--        RRequest e -> Requested e
--        RDone a -> a
--        e -> error $ show e
--  in decompile v
--
--parseAndNormalize' :: String -> String
--parseAndNormalize' s = parseAndNormalize env s
--  where
--  env r = case Map.lookup r builtins of
--    Nothing -> error $ "unknown ref " ++ show r
--    Just ir -> ir
--
--parseAndNormalize :: (R.Reference -> IR) -> String -> String
--parseAndNormalize env s = let
--  tm = Term.unannotate $ B.tm s
--  r = normalize env tm
--  in prettyTerm (fromMaybe tm r)
--
--prettyTerm :: Term Symbol -> String
--prettyTerm t = let
--  ppEnv = PrettyPrintEnv.fromNames B.names
--  in PP.render 80 (TermPrinter.pretty ppEnv 0 t)
--
--parseANF :: String -> Term Symbol
--parseANF s = ANF.fromTerm' . Term.unannotate $ B.tm s
--
--parseANFPretty :: String -> String
--parseANFPretty s = prettyTerm (parseANF s)
