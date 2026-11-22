{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | The partial evaluator behind @specialize@
-- (unison-runtime/src/Unison/Runtime/Specialize.md, "How it works").
--
-- It walks the ANF of a lambda with some of its arguments known, and
-- produces a group of new code with them folded in. It handles only what
-- involves unknown values: everything computed from known values is run on
-- the interpreter, through 'SpecEnv', so nothing here knows what a
-- primitive means. The runtime supplies the environment
-- (Unison.Runtime.Machine.specializeClosure).
module Unison.Runtime.ANF.Specialize
  ( SpecEnv (..),
    SpecLimits (..),
    defaultSpecLimits,
    specializeLambda,
  )
where

import Control.Exception (throwIO)
import Control.Exception qualified
import Control.Monad.State.Strict
import Data.Char (ord)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Unison.ABT.Normalized qualified as ABTN
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Runtime.ANF
import Unison.Runtime.Foreign.Function.Type (ForeignFunc)
import Unison.Runtime.Stack
  ( Val,
    pattern BoxedVal,
    pattern BytesC,
    pattern CharVal,
    pattern DataC,
    pattern DoubleVal,
    pattern IntVal,
    pattern NatVal,
    pattern PAp,
    pattern TextC,
  )
import Unison.Runtime.MCode (CombIx (..))
import Unison.Runtime.Stack qualified as Stack
import Unison.Runtime.TypeTags (CTag (..))
import Unison.Symbol (Symbol)
import Unison.Util.EnumContainers qualified as EC
import Unison.Var qualified as Var

-- | What the partial evaluator needs from the runtime. Each operation
-- that runs code answers Nothing when it can't do so safely now, which is
-- never an error: the code is then residualized.
data SpecEnv = SpecEnv
  { -- | the ANF of a loaded group, before optimization
    seGroup :: Reference -> Maybe (SuperGroup Reference Symbol),
    -- | a known function applied to known arguments, run on the interpreter
    seRun :: Val -> [Val] -> IO (Maybe Val),
    -- | a primitive or a foreign function applied to known arguments
    sePrim :: Either POp ForeignFunc -> [Val] -> IO (Maybe Val),
    -- | a data value from its constructor and fields
    seData :: Reference -> CTag -> [Val] -> IO (Maybe Val),
    -- | a closure of a group's lambda (by index) over the given arguments
    seClosure :: Reference -> Word64 -> [Val] -> IO (Maybe Val),
    -- | the reference of @specialize.boundary@, whose calls stay calls
    seBoundary :: Reference
  }

-- | Bounds on the work: steps of the walk, residual functions, and the
-- depth of unfolding with no unknown match above it. Running out makes
-- the whole specialization give up, and the caller falls back to the
-- original code.
data SpecLimits = SpecLimits
  { slSteps :: !Int,
    slFunctions :: !Int,
    slDepth :: !Int
  }

defaultSpecLimits :: SpecLimits
defaultSpecLimits = SpecLimits {slSteps = 200000, slFunctions = 500, slDepth = 200}

type Term = ANormal Reference Symbol

-- | A function: a group and the index of one of its lambdas (0 for the
-- entry, @k * 2^16@ for the k-th local function).
data Fn = Fn !Reference !Word64
  deriving (Eq, Ord, Show)

-- | What is known about a variable while specializing.
data Abs
  = -- | known entirely; the variable holding it in the output, if there is
    -- one already (an argument of the specialized closure)
    Known Val (Maybe Symbol)
  | -- | a known constructor with partially known fields
    Con Reference CTag [Abs]
  | -- | a known function partially applied to partially known arguments
    Clo Fn [Abs]
  | -- | unknown: a variable of the output, and how it is held
    Dyn Symbol Mem

-- | The key of a residual function: the function, and what is known of
-- its arguments, with each unknown one a hole.
data Key = Key Fn [KArg]
  deriving (Eq, Ord)

data KArg
  = KKnown Val
  | KCon Reference CTag [KArg]
  | KClo Fn [KArg]
  | KHole Mem
  deriving (Eq, Ord)

data GiveUp = GiveUp String
  deriving (Show)

instance Exception GiveUp

data S = S
  { sFresh :: !Word64,
    -- | the next number for an indirect let, per lambda
    sLetNum :: !Word16,
    sSteps :: !Int,
    sMemo :: M.Map Key Symbol,
    -- | residual functions, finished
    sFns :: [(Symbol, SuperNormal Reference Symbol)],
    -- | constants beyond the closure's own arguments, newest first
    sConsts :: [(Symbol, Val)]
  }

type M = StateT S IO

data E = E
  { eVars :: M.Map Symbol Abs,
    -- | whether an unknown match has been residualized above, in the
    -- residual function being built
    eUnderDyn :: !Bool,
    eDepth :: !Int
  }

-- | Where a value goes: the tail of a residual function, or the rest of a
-- let, given how its binder is held.
data K = KTail | KBind Mem (Abs -> M Term)

-- | The specialization of the lambda at an index of a group, on its first
-- arguments: a group whose entry takes those arguments, then the extra
-- constants returned, then the remaining parameters; or Nothing when there
-- is no code for the lambda or the work runs out.
specializeLambda ::
  SpecEnv ->
  SpecLimits ->
  Reference ->
  Word64 ->
  [Val] ->
  IO (Maybe (SuperGroup Reference Symbol, [Val]))
specializeLambda env lim r i args = case lambdaOf env (Fn r i) of
  Nothing -> pure Nothing
  Just (Lambda ccs (ABTN.TAbss vs body), locals)
    | length vs < length args -> pure Nothing
    | otherwise ->
        either (\(GiveUp _) -> Nothing) Just <$> Control.Exception.try @GiveUp do
          flip evalStateT (S 0 1 0 M.empty [] []) do
            argSyms <- traverse (const fresh) args
            restSyms <- traverse (const fresh) (drop (length args) vs)
            let restMems = drop (length args) ccs
                binds =
                  zip vs (zipWith (\v s -> Known v (Just s)) args argSyms)
                    ++ zip (drop (length args) vs) (zipWith Dyn restSyms restMems)
                e0 = E (M.fromList (localBinds r locals ++ binds)) False 0
            body' <- pe env lim e0 body KTail
            consts <- gets (reverse . sConsts)
            fns <- gets sFns
            let constSyms = map fst consts
                thread = threadConsts (S.fromList (map fst fns)) (argSyms ++ constSyms)
                entry =
                  Lambda
                    (map (const BX) argSyms ++ map (const BX) constSyms ++ restMems)
                    (ABTN.TAbss (argSyms ++ constSyms ++ restSyms) (thread body'))
                fns' =
                  [ (f, Lambda (map (const BX) (argSyms ++ constSyms) ++ ms) (ABTN.TAbss (argSyms ++ constSyms ++ ps) (thread bd)))
                  | (f, Lambda ms (ABTN.TAbss ps bd)) <- fns
                  ]
            pure (Rec fns' entry, map snd consts)

-- | A lambda of a group, with the group's local functions by variable.
lambdaOf :: SpecEnv -> Fn -> Maybe (SuperNormal Reference Symbol, [(Symbol, Word64)])
lambdaOf env (Fn r i) = do
  Rec grp ent <- seGroup env r
  let locals = zip (map fst grp) [fromIntegral k `shiftL'` 16 | k <- [1 :: Int ..]]
      k = i `div` 65536
  lam <-
    if i == 0
      then Just ent
      else
        if i `mod` 65536 == 0 && k >= 1 && fromIntegral k <= length grp
          then Just (snd (grp !! (fromIntegral k - 1)))
          else Nothing
  pure (lam, locals)
  where
    shiftL' x n = x * (2 ^ (n :: Int))

-- | The group's local functions, as known functions of nothing yet.
localBinds :: Reference -> [(Symbol, Word64)] -> [(Symbol, Abs)]
localBinds r locals = [(v, Clo (Fn r ix) []) | (v, ix) <- locals]

lamArity :: SuperNormal Reference Symbol -> Int
lamArity (Lambda ccs _) = length ccs

fresh :: M Symbol
fresh = do
  n <- gets sFresh
  modify' \s -> s {sFresh = n + 1}
  pure (Var.freshenId n (Var.named "σ"))

letNum :: M Word16
letNum = do
  n <- gets sLetNum
  modify' \s -> s {sLetNum = n + 1}
  pure n

tick :: SpecLimits -> M ()
tick lim = do
  n <- gets sSteps
  when (n >= slSteps lim) . liftIO . throwIO $ GiveUp "steps"
  modify' \s -> s {sSteps = n + 1}

look :: E -> Symbol -> Abs
look e v = fromMaybe (Dyn v BX) (M.lookup v (eVars e))

bind :: Symbol -> Abs -> E -> E
bind v a e = e {eVars = M.insert v a (eVars e)}

ret :: K -> Abs -> M Term
ret KTail a = toVar a (pure . TVar)
ret (KBind _ k) a = k a

-- | Gives an unknown result: a residual computation of it, then the rest.
dynResult :: K -> Mem -> Term -> M Term
dynResult KTail _ code = pure code
dynResult (KBind m k) _ code = do
  x <- fresh
  n <- letNum
  rest <- k (Dyn x m)
  pure (TLets (Indirect n) [x] [m] code rest)

-- | Like 'dynResult' for a primitive, bound by a direct let.
dynPrim :: K -> Term -> M Term
dynPrim KTail code = pure code
dynPrim (KBind m k) code = do
  x <- fresh
  rest <- k (Dyn x m)
  pure (TLets Direct [x] [m] code rest)

pe :: SpecEnv -> SpecLimits -> E -> Term -> K -> M Term
pe env lim e t k = tick lim >> case t of
  TVar v -> ret k (look e v)
  TLit l -> literal l
  TBLit l -> literal l
  TLets _ [v] [m] bn bo ->
    pe env lim e bn . KBind m $ \a -> pe env lim (bind v a e) bo k
  TLets {} -> opaque env e t k
  TName v f as bo -> do
    let fa = case f of
          Left r -> Clo (Fn r 0) []
          Right lv -> look e lv
    a <- partial env fa (map (look e) as)
    case a of
      Just a' -> pe env lim (bind v a' e) bo k
      Nothing -> opaque env e t k
  TMatch v br -> match env lim e v br k
  TCon r tg as -> ret k (Con r tg (map (look e) as))
  TApp (FComb r) as
    | r == seBoundary env -> boundary env e (map (look e) as) k
    | otherwise -> call env lim e (Clo (Fn r 0) []) (map (look e) as) k
  TApp (FVar f) as -> call env lim e (look e f) (map (look e) as) k
  TApp (FPrim p) as -> prim env e p (map (look e) as) k
  TFrc v -> case look e v of
    Dyn {} -> opaque env e t k
    a -> call env lim e a [] k
  _ -> opaque env e t k
  where
    literal l = case litVal l of
      Just v -> ret k (Known v Nothing)
      Nothing -> opaque env e t k

-- | A literal's value, for the literals that are values of their own.
litVal :: Lit Reference -> Maybe Val
litVal = \case
  N n -> Just (NatVal n)
  I i -> Just (Stack.IntVal (fromIntegral i))
  F d -> Just (Stack.DoubleVal d)
  C c -> Just (Stack.CharVal c)
  T t -> Just (Stack.boxedVal (TextC t))
  _ -> Nothing

-- | A known function applied to more arguments, without calling it.
partial :: SpecEnv -> Abs -> [Abs] -> M (Maybe Abs)
partial env a as = case a of
  Clo fn caps -> pure (Just (Clo fn (caps ++ as)))
  Known v _ | Just (fn, caps) <- knownClosure env v -> pure (Just (Clo fn (caps ++ as)))
  _ -> pure Nothing

-- | A known closure as a function and its arguments, when there is code
-- for the function.
knownClosure :: SpecEnv -> Val -> Maybe (Fn, [Abs])
knownClosure env = \case
  BoxedVal (PAp (CIx r _ i) _ seg)
    | Just _ <- lambdaOf env (Fn r i) ->
        Just (Fn r i, [Known v Nothing | v <- Stack.segToList seg])
  _ -> Nothing

-- | A call. Known all the way: run it. A known function: unfold it, or,
-- under an unknown match, call a residual function specialized on what is
-- known of the arguments. Anything else stays a call.
call :: SpecEnv -> SpecLimits -> E -> Abs -> [Abs] -> K -> M Term
call env lim e fa as k = case fa of
  Clo fn caps -> callFn fn (caps ++ as)
  Known v _ | Just (fn, caps) <- knownClosure env v -> callFn fn (caps ++ as)
  _ -> runOr (fa : as) (toVarApp fa as \g xs -> dynResult k BX (TApp (FVar g) xs))
  where
    -- the call run on the interpreter if everything is known, otherwise the
    -- fallback
    runOr parts fallback =
      allKnown env parts >>= \case
        Just (f : xs) ->
          liftIO (seRun env f xs) >>= \case
            Just r -> ret k (Known r Nothing)
            Nothing -> fallback
        _ -> fallback
    callFn fn@(Fn r i) args = case lambdaOf env fn of
      -- a builtin, or code not loaded here: a call, unless it can run
      Nothing
        | i == 0 ->
            runOr (Clo fn [] : args) (toVars args \xs -> dynResult k BX (TApp (FComb r) xs))
        | otherwise -> liftIO . throwIO $ GiveUp "a local function with no code"
      Just (lam@(Lambda _ (ABTN.TAbss vs body)), locals)
        | length args < lamArity lam -> ret k (Clo fn args)
        | length args > lamArity lam ->
            call env lim e (Clo fn []) (take (lamArity lam) args) . KBind BX $ \res ->
              call env lim e res (drop (lamArity lam) args) k
        | otherwise -> runOr (Clo fn [] : args) $
            if not (eUnderDyn e) && eDepth e < slDepth lim
              then
                pe env lim (E (M.fromList (localBinds r locals ++ zip vs args)) False (eDepth e + 1)) body k
              else do
                (f, leaves) <- residualFn env lim fn args
                toVars leaves \xs -> dynResult k BX (TApp (FVar f) xs)

-- | A residual function for a call, from its key: an existing one, or a new
-- one whose body is the function's, specialized on what is known of the
-- arguments, with a parameter for each unknown part. Gives the function's
-- variable and the unknown parts, which are the arguments of a call of it.
residualFn :: SpecEnv -> SpecLimits -> Fn -> [Abs] -> M (Symbol, [Abs])
residualFn env lim fn args = do
  let key = Key fn (map keyOf args)
      leaves = concatMap dynLeaves args
  gets (M.lookup key . sMemo) >>= \case
    Just f -> pure (f, leaves)
    Nothing -> do
      nfns <- gets (M.size . sMemo)
      when (nfns >= slFunctions lim) . liftIO . throwIO $ GiveUp "functions"
      f <- fresh
      modify' \s -> s {sMemo = M.insert key f (sMemo s)}
      case lambdaOf env fn of
        Nothing -> liftIO . throwIO $ GiveUp "no code"
        Just (Lambda _ (ABTN.TAbss vs body), locals) -> do
          params <- traverse (\_ -> fresh) leaves
          let mems = [m | Dyn _ m <- leaves]
              args' = evalState (traverse renameLeaves args) params
              Fn r _ = fn
              e' = E (M.fromList (localBinds r locals ++ zip vs args')) False 0
          outer <- gets sLetNum
          modify' \s -> s {sLetNum = 1}
          body' <- pe env lim e' body KTail
          modify' \s -> s {sLetNum = outer, sFns = (f, Lambda mems (ABTN.TAbss params body')) : sFns s}
          pure (f, leaves)
  where
    renameLeaves :: Abs -> State [Symbol] Abs
    renameLeaves = \case
      Dyn _ m -> state \case
        p : ps -> (Dyn p m, ps)
        [] -> error "residualFn: fewer parameters than unknown parts"
      Con r t fs -> Con r t <$> traverse renameLeaves fs
      Clo f as -> Clo f <$> traverse renameLeaves as
      a -> pure a

keyOf :: Abs -> KArg
keyOf = \case
  Known v _ -> KKnown v
  Con r t fs -> KCon r t (map keyOf fs)
  Clo f as -> KClo f (map keyOf as)
  Dyn _ m -> KHole m

dynLeaves :: Abs -> [Abs]
dynLeaves = \case
  a@Dyn {} -> [a]
  Con _ _ fs -> concatMap dynLeaves fs
  Clo _ as -> concatMap dynLeaves as
  Known {} -> []

-- | A call of @specialize.boundary@: the call of its function on the rest
-- of its arguments stays a call.
boundary :: SpecEnv -> E -> [Abs] -> K -> M Term
boundary _ _ as k = case as of
  Clo (Fn r 0) [] : xs -> toVars xs \vs -> dynResult k BX (TApp (FComb r) vs)
  f : xs -> toVarApp f xs \g vs -> dynResult k BX (TApp (FVar g) vs)
  [] -> liftIO . throwIO $ GiveUp "boundary of nothing"

-- | A primitive: run it when its arguments are known, otherwise keep it.
prim :: SpecEnv -> E -> Either POp ForeignFunc -> [Abs] -> K -> M Term
prim env _ p as k =
  allKnown env as >>= \case
    Just vs ->
      liftIO (sePrim env p vs) >>= \case
        Just r -> ret k (Known r Nothing)
        Nothing -> keep
    Nothing -> keep
  where
    keep = toVars as \xs -> dynPrim k (TApp (FPrim p) xs)

-- | A match. On something known, the arm it takes; on something unknown, a
-- residual match whose arms are specialized, each knowing which
-- constructor the scrutinee is.
match :: SpecEnv -> SpecLimits -> E -> Symbol -> Branched Reference Term -> K -> M Term
match env lim e v br k = case (look e v, br) of
  (Con _ t fs, MatchData _ cs df)
    | Just (_, ABTN.TAbss vs body) <- EC.lookup t cs,
      length vs == length fs ->
        pe env lim (foldr (uncurry bind) e (zip vs fs)) body k
    | Nothing <- EC.lookup t cs, Just d <- df -> pe env lim e d k
  (Known x _, MatchData _ cs df)
    | Just (t, fs) <- knownData x ->
        case EC.lookup t cs of
          Just (_, ABTN.TAbss vs body)
            | length vs == length fs ->
                pe env lim (foldr (uncurry bind) e (zip vs [Known f Nothing | f <- fs])) body k
          Nothing | Just d <- df -> pe env lim e d k
          _ -> dynamic
  (Known x _, MatchIntegral cs df) | Just n <- knownIntegral x -> litArm (EC.lookup n cs) df
  (Known x _, MatchNumeric _ cs df) | Just n <- knownIntegral x -> litArm (EC.lookup n cs) df
  (Known (BoxedVal (TextC s)) _, MatchText cs df) -> litArm (M.lookup s cs) df
  (Known (BoxedVal (BytesC b)) _, MatchBytes cs df) -> litArm (M.lookup b cs) df
  _ -> dynamic
  where
    litArm (Just body) _ = pe env lim e body k
    litArm Nothing (Just d) = pe env lim e d k
    litArm Nothing Nothing = dynamic
    under = e {eUnderDyn = True}
    dynamic = toVar (look e v) \x -> do
      br' <- case br of
        MatchData r cs df ->
          MatchData r
            <$> (EC.mapFromList <$> traverse (dataArm r) (EC.mapToList cs))
            <*> traverse (tailOf under) df
        MatchIntegral cs df -> MatchIntegral <$> traverse (tailOf under) cs <*> traverse (tailOf under) df
        MatchNumeric r cs df -> MatchNumeric r <$> traverse (tailOf under) cs <*> traverse (tailOf under) df
        MatchText cs df -> MatchText <$> traverse (tailOf under) cs <*> traverse (tailOf under) df
        MatchBytes cs df -> MatchBytes <$> traverse (tailOf under) cs <*> traverse (tailOf under) df
        _ -> liftIO . throwIO $ GiveUp "match form"
      dynResult k BX (TMatch x br')
    dataArm r (t, (ms, ABTN.TAbss vs body)) = do
      vs' <- traverse (const fresh) vs
      let fields = zipWith Dyn vs' ms
          e' = foldr (uncurry bind) (bind v (Con r t fields) under) (zip vs fields)
      body' <- pe env lim e' body KTail
      pure (t, (ms, ABTN.TAbss vs' body'))
    tailOf e' body = pe env lim e' body KTail

-- | The constructor tag and fields of a known data value.
knownData :: Val -> Maybe (CTag, [Val])
knownData = \case
  BoxedVal (DataC t vs) -> Just (CTag (fromIntegral (maskTags t)), vs)
  _ -> Nothing

knownIntegral :: Val -> Maybe Word64
knownIntegral = \case
  NatVal n -> Just n
  IntVal i -> Just (fromIntegral i)
  CharVal c -> Just (fromIntegral (ord c))
  _ -> Nothing

-- | Every value known, as values: constructors and closures with every
-- part known are built.
allKnown :: SpecEnv -> [Abs] -> M (Maybe [Val])
allKnown env = fmap sequence . traverse (knownVal env)

knownVal :: SpecEnv -> Abs -> M (Maybe Val)
knownVal env = \case
  Known v _ -> pure (Just v)
  Con r t fs ->
    allKnown env fs >>= \case
      Just vs -> liftIO (seData env r t vs)
      Nothing -> pure Nothing
  Clo (Fn r i) as ->
    allKnown env as >>= \case
      Just vs -> liftIO (seClosure env r i vs)
      Nothing -> pure Nothing
  Dyn {} -> pure Nothing

toVarApp :: Abs -> [Abs] -> (Symbol -> [Symbol] -> M Term) -> M Term
toVarApp f as k = toVar f \g -> toVars as (k g)

toVars :: [Abs] -> ([Symbol] -> M Term) -> M Term
toVars [] k = k []
toVars (a : as) k = toVar a \x -> toVars as \xs -> k (x : xs)

-- | A value in a variable of the output: an unknown one is already there;
-- a known literal is a literal; a constructor is built; a closure is
-- built, of a residual function when its function is a local one; any
-- other known value is a constant, a parameter of every residual function.
toVar :: Abs -> (Symbol -> M Term) -> M Term
toVar a k = case a of
  Dyn x _ -> k x
  Known _ (Just s) -> k s
  Known v Nothing -> case v of
    NatVal n -> lit UN (TLit (N n))
    IntVal i -> lit UN (TLit (I (fromIntegral i)))
    DoubleVal d -> lit UN (TLit (F d))
    CharVal c -> lit UN (TLit (C c))
    BoxedVal (TextC t) -> lit BX (TBLit (T t))
    _ -> do
      s <- fresh
      modify' \st -> st {sConsts = (s, v) : sConsts st}
      k s
  Con r t fs -> toVars fs \xs -> lit BX (TCon r t xs)
  Clo (Fn r 0) as -> toVars as \xs -> do
    x <- fresh
    rest <- k x
    pure (TName x (Left r) xs rest)
  Clo {} -> liftIO . throwIO $ GiveUp "closure of a local function"
  where
    lit m code = do
      x <- fresh
      rest <- k x
      pure (TLets Direct [x] [m] code rest)

-- | Code the partial evaluator doesn't look into, kept as it is: its free
-- variables are given values in the output first, and its indirect lets
-- are numbered anew.
opaque :: SpecEnv -> E -> Term -> K -> M Term
opaque _ e t k = do
  let fvs = S.toList (ABTN.freeVars t)
  toVars (map (look e) fvs) \xs -> do
    t' <- renumber (ABTN.renames (M.fromList (zip fvs xs)) t)
    dynResult k BX t'

-- | The indirect lets of a term, numbered anew, so that they can't clash
-- with the output's.
renumber :: Term -> M Term
renumber = go
  where
    go = \case
      ABTN.TAbs v b -> ABTN.TAbs v <$> go b
      ABTN.TTm (ALet (Indirect _) ms bn bo) -> do
        n <- letNum
        ABTN.TTm <$> (ALet (Indirect n) ms <$> go bn <*> go bo)
      ABTN.TTm f -> ABTN.TTm <$> traverse go f

-- | Each call and closure of a residual function given the constants first,
-- which are parameters of every residual function.
threadConsts :: S.Set Symbol -> [Symbol] -> Term -> Term
threadConsts fns cs = go
  where
    go = \case
      ABTN.TAbs v b -> ABTN.TAbs v (go b)
      TApp (FVar f) xs | S.member f fns -> TApp (FVar f) (cs ++ xs)
      TName x (Right f) xs bo | S.member f fns -> TName x (Right f) (cs ++ xs) (go bo)
      ABTN.TTm f -> ABTN.TTm (fmap go f)
