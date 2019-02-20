{-# Language BangPatterns #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language Strict #-}
{-# Language StrictData #-}
{-# LANGUAGE RankNTypes #-}
{-# Language TupleSections #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Runtime.Rt1 where

import Control.Monad (foldM, join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_, toList)
import Data.IORef
import Data.Int (Int64)
import Data.Map (Map)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Word (Word64)
import Unison.Codebase.Runtime (Runtime(Runtime))
import Unison.Runtime.IR (pattern CompilationEnv, pattern Req)
import Unison.Runtime.IR hiding (CompilationEnv, IR, Req, Value, Z)
import Unison.Symbol (Symbol)
import Unison.TermPrinter (prettyTop)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MV
import qualified Unison.Codebase.CodeLookup as CL
import qualified Unison.DataDeclaration as DD
import qualified Unison.Reference as R
import qualified Unison.Runtime.IR as IR
import qualified Unison.Term as Term
import qualified Unison.Util.Pretty as Pretty
import Debug.Trace

type CompilationEnv = IR.CompilationEnv ExternalFunction
type IR = IR.IR ExternalFunction
type Req = IR.Req ExternalFunction
type Value = IR.Value ExternalFunction
type Z = IR.Z ExternalFunction

newtype ExternalFunction = ExternalFunction (Size -> Stack -> IO Value)

type Stack = MV.IOVector Value

runtime :: Runtime Symbol
runtime = Runtime terminate eval
  where
  terminate :: forall m. MonadIO m => m ()
  terminate = pure ()
  changeVar term = Term.vmap IR.underlyingSymbol term
  eval :: (MonadIO m, Monoid a) => CL.CodeLookup m Symbol a -> Term.AnnotatedTerm Symbol a -> m (Term Symbol)
  eval cl term = do
    liftIO . putStrLn $ Pretty.render 80 (prettyTop mempty term)
    cenv <- compilationEnv cl term -- in `m`
    RDone result <- liftIO $
      run cenv (compile cenv $ Term.amap (const ()) term)
    let Just decompiled = decompile result
    pure . changeVar $ decompiled


-- compile :: Show e => CompilationEnv e -> Term Symbol -> IR e
-- compilationEnv :: Monad m
--   => CL.CodeLookup m Symbol a
--   -> Term Symbol
--   -> m CompilationEnv
-- run :: CompilationEnv -> IR -> IO Result


-- This function converts `Z` to a `Value`.
-- A bunch of variants follow.
at :: Size -> Z -> Stack -> IO Value
at size i m = case i of
  Val v -> force v
  Slot i ->
    -- the top of the stack is slot 0, at index size - 1
    force =<< MV.read m (size - i - 1)
  LazySlot i ->
    MV.read m (size - i - 1)
  External (ExternalFunction e) -> e size m

ati :: Size -> Z -> Stack -> IO Int64
ati size i m = at size i m >>= \case
  I i -> pure i
  _ -> fail "type error"

atn :: Size -> Z -> Stack -> IO Word64
atn size i m = at size i m >>= \case
  N i -> pure i
  _ -> fail "type error"

atf :: Size -> Z -> Stack -> IO Double
atf size i m = at size i m >>= \case
  F i -> pure i
  _ -> fail "type error"

atb :: Size -> Z -> Stack -> IO Bool
atb size i m = at size i m >>= \case
  B b -> pure b
  _ -> fail "type error"

att :: Size -> Z -> Stack -> IO Text
att size i m = at size i m >>= \case
  T t -> pure t
  _ -> fail "type error"

push :: Size -> Value -> Stack -> IO Stack
push size v s0 = do
  s1 <-
    if size >= MV.length s0
    then do
      -- increase the size to fit
      s1 <- MV.grow s0 size
      pure s1
    else pure s0
  MV.write s1 size v
  pure s1

pushMany :: Foldable f
  => Size -> f Value -> Stack -> IO (Size, Stack)
pushMany size values m = do
  m <- ensureSize (size + length values) m
  let pushArg :: Size -> Value -> IO Size
      pushArg size' val = do
        MV.write m size' val
        pure (size' + 1)
  length <- foldM pushArg 0 values
  pure ((size + length), m)


  -- [s3,s2,s1,s0] [i1,i2,i3]
  -- [s3,s2,s1,s0,  i1,i2,i3]
  -- [s3,s2,s1,s0,  i3,i2,i1]

pushManyZ :: Foldable f => Size -> f Z -> Stack -> IO (Size, Stack)
pushManyZ size zs m = do
  m <- ensureSize (size + length zs) m
  let pushArg size' z = do
        val <- at size z m -- variable lookup uses current size
        MV.write m size' val
        pure (size' + 1)
  size2 <- foldM pushArg size zs
  pure (size2, m)

ensureSize :: Size -> Stack -> IO Stack
ensureSize size m =
  if (size >= MV.length m) then MV.grow m size
  else pure m

type Size = Int

force :: Value -> IO Value
force (Ref _ _ r) = readIORef r >>= force
force v = pure v

data Result
  = RRequest Req
  | RMatchFail {- maybe add more info here. -}
  | RDone Value
  deriving (Show)

done :: Value -> IO Result
done v = pure (RDone v)

arity :: Value -> Int
arity (Lam n _ _) = n
arity _ = 0

-- Creates a `CompilationEnv` by pulling out all the constructor arities for
-- types that are referenced by the given term, `t`.
compilationEnv :: Monad m
  => CL.CodeLookup m Symbol a
  -> Term.AnnotatedTerm Symbol a
  -> m CompilationEnv
compilationEnv env t = do
  let typeDeps = Term.referencedDataDeclarations t
              <> Term.referencedEffectDeclarations t
  traceM "typeDeps"
  traceShowM typeDeps
  arityMap <- fmap (Map.fromList . join) . for (toList typeDeps) $ \case
    r@(R.DerivedId id) -> do
      decl <- CL.getTypeDeclaration env id
      case decl of
        Nothing -> error $ "no type declaration for " <> show id -- pure []
        Just (Left ad) -> pure $
          let arities = DD.constructorArities $ DD.toDataDecl ad
          in [ ((r, i), arity) | (arity, i) <- arities `zip` [0..] ]
        Just (Right dd) -> pure $
          let arities = DD.constructorArities dd
          in [ ((r, i), arity) | (arity, i) <- arities `zip` [0..] ]
    _ -> pure []
  let cenv = CompilationEnv mempty arityMap

    -- deps = Term.dependencies t
  -- this would rely on haskell laziness for compilation, needs more thought
  --compiledTerms <- fmap (Map.fromList . join) . for (toList deps) $ \case
  --  r@(R.DerivedId id) -> do
  --    o <- CL.getTerm env id
  --    case o of
  --      Nothing -> pure []
  --      Just e -> pure [(r, compile cenv (Term.amap (const ()) e))]
  --  _ -> pure []
  pure $ builtinCompilationEnv <> cenv

builtinCompilationEnv :: CompilationEnv
builtinCompilationEnv =
  CompilationEnv (builtinsMap <> IR.builtins) mempty
  where
    builtins :: [(Text, Int, Size -> Stack -> IO Value)]
    builtins = [
      ("Text.++", 2, mk2 att att (pure.T) (<>)),
      ("Text.take", 2, mk2 atn att (pure.T) (Text.take . fromIntegral)),
      ("Text.drop", 2, mk2 atn att (pure.T) (Text.drop . fromIntegral)),
      ("Text.size", 2, mk1 att (pure.N) (fromIntegral . Text.length)),
      ("Text.==", 2, mk2 att att (pure.B) (==)),
      ("Text./=", 2, mk2 att att (pure.B) (/=)),
      ("Text.<=", 2, mk2 att att (pure.B) (<=)),
      ("Text.>=", 2, mk2 att att (pure.B) (>=)),
      ("Text.>", 2, mk2 att att (pure.B) (>)),
      ("Text.<", 2, mk2 att att (pure.B) (<))
      ]

    builtinsMap :: Map R.Reference IR
    builtinsMap = Map.fromList
      [ (R.Builtin name, makeIR arity name ir) | (name, arity, ir) <- builtins ]
    makeIR arity name =
      Leaf . Val . Lam arity (underapply name)
           . Leaf . External . ExternalFunction
    underapply name = FormClosure (Term.ref() $ R.Builtin name)
    mk1 :: (Size -> Z -> Stack -> IO a)
        -> (b -> IO Value)
        -> (a -> b)
        -> Size -> Stack -> IO Value
    mk1 getA mkB f size stack = do
      a <- getA size (Slot 0) stack
      mkB $ f a
    mk2 :: (Size -> Z -> Stack -> IO a)
        -> (Size -> Z -> Stack -> IO b)
        -> (c -> IO Value)
        -> (a -> b -> c)
        -> Size -> Stack -> IO Value
    mk2 getA getB mkC f size stack = do
      a <- getA size (Slot 1) stack
      b <- getB size (Slot 0) stack
      mkC $ f a b

run :: CompilationEnv -> IR -> IO Result
run env ir = do
  supply <- newIORef 0
  m0 <- MV.new 256
  MV.set m0 (T "uninitialized")
  let
    fresh :: IO Int
    fresh = atomicModifyIORef' supply (\n -> (n + 1, n))

    go :: Size -> Stack -> IR -> IO Result
    go size m ir = do
     stackStuff <- traverse (MV.read m) [0..size-1]
     traceM $ "stack: " <> show stackStuff
     traceM $ "ir: " <> show ir
     traceM ""
     case ir of
      Leaf (Val v) -> done v
      Leaf slot -> done =<< at size slot m
      If c t f -> atb size c m >>= \case
        True -> go size m t
        False -> go size m f
      And i j -> atb size i m >>= \case
        True -> go size m j
        False -> done (B False)
      Or i j -> atb size i m >>= \case
        True -> done (B True)
        False -> go size m j
      Not i -> atb size i m >>= (done . B . not)
      Let b body -> go size m b >>= \case
        RRequest req -> pure $ RRequest (req `appendCont` body)
        RDone v -> push size v m >>= \m -> go (size + 1) m body
        e@RMatchFail -> error $ show e
      LetRec bs body -> letrec size m bs body
      MakeSequence vs ->
        done . Sequence . Vector.fromList =<< traverse (\i -> at size i m) vs
      Construct r cid args ->
        done . Data r cid =<< traverse (\i -> at size i m) args
      Request r cid args ->
        req <$> traverse (\i -> at size i m) args
        where
        -- The continuation of the request is initially the identity function
        -- and we append to it in `Let` as we unwind the stack
        req vs = RRequest (Req r cid vs (Leaf $ Slot 0))
      Handle handler body -> do
        h <- at size handler m
        runHandler size m h body
      Apply fn args -> do
        RDone fn <- go size m fn -- ANF should ensure this match is OK
        call size m fn args
      Match scrutinee cases -> do
        -- scrutinee : Z -- already evaluated :amazing:
        -- cases : [(Pattern, Maybe IR, IR)]
        scrute <- at size scrutinee m -- "I am scrute" / "Dwight K. Scrute"
        let
          getCapturedVars :: (Value, Pattern) -> Maybe [Value]
          getCapturedVars = \case
            (I x, PatternI x2) | x == x2 -> Just []
            (F x, PatternF x2) | x == x2 -> Just []
            (N x, PatternN x2) | x == x2 -> Just []
            (B x, PatternB x2) | x == x2 -> Just []
            (T x, PatternT x2) | x == x2 -> Just []
            (Data r cid args, PatternData r2 cid2 pats)
              | r == r2 && cid == cid2 ->
              join <$> traverse getCapturedVars (zip args pats)
            (Sequence args, PatternSequence pats) ->
              join <$> traverse getCapturedVars (zip (toList args) (toList pats))
            (Pure v, PatternPure p) -> getCapturedVars (v, p)
            (Requested (Req r cid args k), PatternBind r2 cid2 pats kpat)
              | r == r2 && cid == cid2 ->
              join <$> traverse getCapturedVars (zip (args ++ [Cont k]) (pats ++ [kpat]))
            (v, PatternAs p) -> (v:) <$> getCapturedVars (v,p)
            (_, PatternIgnore) -> Just []
            (v, PatternVar) -> Just [v]
            (v, p) -> error $
              "unpossible: getCapturedVars (" <> show v <> ", " <> show p <> ")"
          tryCases m ((pat, cond, body) : remainingCases) =
            case getCapturedVars (scrute, pat) of
              Nothing -> tryCases m remainingCases -- this pattern didn't match
              Just vars -> do
                (size, m) <- pushMany size vars m
                case cond of
                  Just cond -> do
                    (RDone (B cond)) <- go size m cond
                    if cond then go size m body else tryCases m remainingCases
                  Nothing -> go size m body
          tryCases _ _ = pure RMatchFail
        tryCases m cases

      -- Builtins
      AddI i j -> do x <- ati size i m; y <- ati size j m; done (I (x + y))
      SubI i j -> do x <- ati size i m; y <- ati size j m; done (I (x - y))
      MultI i j -> do x <- ati size i m; y <- ati size j m; done (I (x * y))
      DivI i j -> do x <- ati size i m; y <- ati size j m; done (I (x `div` y))
      GtI i j -> do x <- ati size i m; y <- ati size j m; done (B (x > y))
      GtEqI i j -> do x <- ati size i m; y <- ati size j m; done (B (x >= y))
      LtI i j -> do x <- ati size i m; y <- ati size j m; done (B (x < y))
      LtEqI i j -> do x <- ati size i m; y <- ati size j m; done (B (x <= y))
      EqI i j -> do x <- ati size i m; y <- ati size j m; done (B (x == y))
      SignumI i -> do x <- ati size i m; done (I (signum x))
      NegateI i -> do x <- ati size i m; done (I (negate x))
      ModI i j -> do x <- ati size i m; y <- ati size j m; done (I (x `mod` y))

      AddN i j -> do x <- atn size i m; y <- atn size j m; done (N (x + y))
      -- cast to `Int` and subtract
      SubN i j -> do x <- atn size i m; y <- atn size j m
                     done (I (fromIntegral x - fromIntegral y))
      -- subtraction truncated at 0 (don't wrap around)
      DropN i j -> do x <- atn size i m; y <- atn size j m
                      done (N (x - (y `min` x)))
      MultN i j -> do x <- atn size i m; y <- atn size j m; done (N (x * y))
      DivN i j -> do x <- atn size i m; y <- atn size j m; done (N (x `div` y))
      ModN i j -> do x <- atn size i m; y <- atn size j m; done (N (x `mod` y))
      GtN i j -> do x <- atn size i m; y <- atn size j m; done (B (x > y))
      GtEqN i j -> do x <- atn size i m; y <- atn size j m; done (B (x >= y))
      LtN i j -> do x <- atn size i m; y <- atn size j m; done (B (x < y))
      LtEqN i j -> do x <- atn size i m; y <- atn size j m; done (B (x <= y))
      EqN i j -> do x <- atn size i m; y <- atn size j m; done (B (x == y))

      AddF i j -> do x <- atf size i m; y <- atf size j m; done (F (x + y))
      SubF i j -> do x <- atf size i m; y <- atf size j m; done (F (x - y))
      MultF i j -> do x <- atf size i m; y <- atf size j m; done (F (x * y))
      DivF i j -> do x <- atf size i m; y <- atf size j m; done (F (x / y))
      GtF i j -> do x <- atf size i m; y <- atf size j m; done (B (x > y))
      GtEqF i j -> do x <- atf size i m; y <- atf size j m; done (B (x >= y))
      LtF i j -> do x <- atf size i m; y <- atf size j m; done (B (x < y))
      LtEqF i j -> do x <- atf size i m; y <- atf size j m; done (B (x <= y))
      EqF i j -> do x <- atf size i m; y <- atf size j m; done (B (x == y))
      -- _ -> error $ "TODO - fill in the rest of Rt1.go " <> show ir

    runHandler :: Size -> Stack -> Value -> IR -> IO Result
    runHandler size m handler body = go size m body >>= \case
      RRequest req -> do
        m <- push size (Requested req) m
        result <- call (size + 1) m handler [Slot 0]
        case result of
          RMatchFail -> pure $ RRequest (wrapHandler handler req)
          r -> pure r
      RDone v -> do
        m <- push size (Pure v) m
        call (size + 1) m handler [Slot 0]
      r -> pure r

    call :: Size -> Stack -> Value -> [Z] -> IO Result
    call size m fn@(Lam arity underapply body) args = let nargs = length args in
      -- fully applied call, `(x y -> ..) 9 10`
      if nargs == arity then do
        (size, m) <- pushManyZ size args m
        go size m body
      -- overapplied call, e.g. `id id 42`
      else if nargs > arity then do
        let (usedArgs, extraArgs) = splitAt arity args
        result <- call size m fn usedArgs
        case result of
          RDone fn' -> call size m fn' extraArgs
          RRequest req -> pure . RRequest $ req `appendCont` error "todo"
          e -> error $ "type error, tried to apply: " <> show e
      -- underapplied call, e.g. `(x y -> ..) 9`
      else do
        argvs <- for args $ \arg -> at size arg m
        case underapply of
          Specialize (Term.LamsNamed' vs body) -> do
            let
              Just argterms = traverse decompile argvs
              toBound vs = reverse ((,Nothing) <$> vs)
              bound = toBound (drop nargs vs) ++ reverse (vs `zip` map Just argvs)
              compiled = compile0 env bound body
              lam = Term.let1' False (vs `zip` argterms) $
                    Term.lam'() (drop nargs vs) body
            done $ Lam (arity - nargs) (Specialize lam) compiled
          Specialize e -> error $ "can't underapply a non-lambda: " <> show e
          FormClosure tm -> do
            let Just argterms = traverse decompile argvs
            done $ Lam (arity - nargs)
                       (FormClosure $ Term.apps' tm argterms)
                       (error "todo - gotta form an IR that calls the original body with args in the correct order")
    call _ _ fn args =
      error $ "type error - tried to apply a non-function: " <> show (fn, args)

    -- To evaluate a `let rec`, we push an empty `Ref` onto the stack for each
    -- binding, then evaluate each binding and set that `Ref` to its result.
    -- As long as the variable references occur within a function body,
    -- there's no problem.
    letrec :: Size -> Stack -> [(Symbol, IR)] -> IR -> IO Result
    letrec size m bs body = do
      refs <- for bs $ \(v,b) -> do
        r <- newIORef (LetRecBomb v bs body)
        i <- fresh
        pure (Ref i v r, b)
      -- push the empty references onto the stack
      (size', m) <- pushMany size (fst <$> refs) m
      for_ refs $ \(Ref _ _ r, ir) -> do
        let toVal (RDone a) = a
            toVal e = error ("bindings in a let rec must not have effects " ++ show e)
        result <- toVal <$> go size' m ir
        writeIORef r result
      go size' m body

  go 0 m0 ir

instance Show ExternalFunction where
  show _ = "ExternalFunction"
