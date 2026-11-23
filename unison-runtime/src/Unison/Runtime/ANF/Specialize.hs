{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
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
import Data.List (find)
import System.IO.Unsafe (unsafePerformIO)
import Unison.Util.Text qualified as Util.Text
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Unison.ABT.Normalized qualified as ABTN
import Unison.Prelude
import Unison.Reference (Reference, Reference' (Builtin))
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
import Unison.Runtime.Builtin.Decls (exceptionRef)
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
    seBoundary :: Reference,
    -- | a term run on the interpreter, given values for its free variables
    -- (and how each is held): for code the partial evaluator doesn't look
    -- into but whose inputs are all known, a handler of any kind included
    seRunTerm :: [(Symbol, Mem)] -> Term -> [Val] -> IO (Maybe Val),
    -- | whether a builtin can call back into Unison code (apply or force a
    -- function it is given, resume a continuation), and so perform
    -- requests
    seCallsBack :: Reference -> Bool
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
  | -- | a request handled while specializing (a fused handler's argument):
    -- the ability, the operation, its arguments and its continuation
    SReq Reference CTag [Abs] Int
  | -- | the value a fused handler's body finished with, given to the handler
    SPure Abs
  | -- | the continuation of a request handled while specializing
    SKont Int

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

-- | Something in the scope of the fused handler with this number could
-- perform its requests without the walk seeing them: the handler can't be
-- fused, and its @handle@ is walked again, kept.
newtype Escape = Escape Int
  deriving (Show)

instance Exception Escape

-- | A handler in force over the code being walked: fused (handled while
-- specializing), or kept, a barrier to fused ones further out, since a
-- request can't be handled across it.
data Frame
  = Fused
      { frId :: !Int,
        frRefs :: [Reference],
        frHandler :: Abs,
        -- | where the @handle@'s value goes
        frOuter :: K,
        -- | how many targets there were when it was installed
        frDepth :: !Int
      }
  | Barrier

-- | The continuation of a request handled while specializing: the rest of
-- the walk from the request, and the frames and targets between it and the
-- handler, which a resumption puts back.
data Kont = Kont
  { koRest :: Abs -> M Term,
    koFrames :: [Frame],
    koTargets :: [Abs -> M Term],
    koFrame :: !Int,
    koUsed :: !Bool
  }

data S = S
  { sFresh :: !Word64,
    -- | the next number for an indirect let, per lambda
    sLetNum :: !Word16,
    sSteps :: !Int,
    sMemo :: M.Map Key Symbol,
    -- | residual functions, finished
    sFns :: [(Symbol, SuperNormal Reference Symbol)],
    -- | constants beyond the closure's own arguments, newest first
    sConsts :: [(Symbol, Val)],
    sEnv :: SpecEnv,
    sLim :: SpecLimits,
    -- | the keys of the residual functions being built, innermost first,
    -- for the whistle ('residualFn')
    sBuilding :: [(Fn, [KArg])],
    -- | the handlers in force, innermost first
    sFrames :: [Frame],
    -- | where the bodies of fused handlers return to, innermost first: the
    -- handler's own pure case, or the site that resumed the body
    sTargets :: [Abs -> M Term],
    sKonts :: M.Map Int Kont,
    sNextId :: !Int,
    -- | the continuation a handler's case is about to capture with @shift@
    sPending :: Maybe (Int, Reference)
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
  IO (Either String (SuperGroup Reference Symbol, [Val]))
specializeLambda env lim r i args = case lambdaOf env (Fn r i) of
  Nothing -> pure (Left "no code for the lambda")
  Just (Lambda ccs (ABTN.TAbss vs body), locals)
    | length vs < length args -> pure (Left "more arguments than parameters")
    | otherwise ->
        either (\(GiveUp why) -> Left why) id <$> Control.Exception.try @GiveUp do
         either (\(Escape _) -> Left "an escape from a fused handler") id <$> Control.Exception.try @Escape (Right <$> do
          flip evalStateT (S 0 1 0 M.empty [] [] env lim [] [] [] M.empty 0 Nothing) do
            argSyms <- traverse (const fresh) args
            restSyms <- traverse (const fresh) (drop (length args) vs)
            let restMems = drop (length args) ccs
                binds =
                  zip vs (zipWith (\v s -> Known v (Just s)) args argSyms)
                    ++ zip (drop (length args) vs) (zipWith Dyn restSyms restMems)
                e0 = E (M.fromList (localBinds r locals ++ binds)) False 0
            body0 <- pe env lim e0 body KTail
            consts <- gets (reverse . sConsts)
            fns0 <- gets sFns
            (fns, body') <- inlineFns fns0 body0
            let constSyms = argSyms ++ map fst consts
                needs = constNeeds constSyms fns
                thread = renumberLets . threadConsts needs
                entry =
                  Lambda
                    (map (const BX) constSyms ++ restMems)
                    (ABTN.TAbss (constSyms ++ restSyms) (thread body'))
                fns' =
                  [ (f, Lambda (map (const BX) cs ++ ms) (ABTN.TAbss (cs ++ ps) (thread bd)))
                  | (f, Lambda ms (ABTN.TAbss ps bd)) <- fns,
                    let cs = M.findWithDefault [] f needs
                  ]
            pure (Rec fns' entry, map snd consts))

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
  TApp (FReq r t) as -> request env lim e r t (map (look e) as) k
  THnd rs h Nothing body -> handler env lim e t rs h body k
  TShift r v body ->
    gets sPending >>= \case
      -- the case of a fused handler capturing its request's continuation
      Just (kid, r') | r' == r -> do
        modify' \st -> st {sPending = Nothing}
        pe env lim (bind v (SKont kid) e) body k
      _ -> do
        v' <- fresh
        body' <- withBarrier (pe env lim (bind v (Dyn v' BX) e) body KTail)
        dynResult k BX (TShift r v' body')
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
-- for the function or it is an entry (a builtin's, say), which a call can
-- name directly.
knownClosure :: SpecEnv -> Val -> Maybe (Fn, [Abs])
knownClosure env = \case
  BoxedVal (PAp (CIx r _ i) _ seg)
    | i == 0 || isJust (lambdaOf env (Fn r i)) ->
        Just (Fn r i, [Known v Nothing | v <- Stack.segToList seg])
  _ -> Nothing

-- | A call. Known all the way: run it. A known function: unfold it, or,
-- under an unknown match, call a residual function specialized on what is
-- known of the arguments. Anything else stays a call.
call :: SpecEnv -> SpecLimits -> E -> Abs -> [Abs] -> K -> M Term
call env lim e fa as k = case fa of
  -- resuming the continuation of a request handled while specializing
  Clo (Fn r 0) [SKont kid] | r == jumpCont, [v] <- as -> resume kid v k
  Clo fn caps -> callFn fn (caps ++ as)
  Known v _ | Just (fn, caps) <- knownClosure env v -> callFn fn (caps ++ as)
  _ -> runOr (fa : as) (guardFused >> toVarApp fa as \g xs -> dynResult k BX (TApp (FVar g) xs))
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
            runOr (Clo fn [] : args) do
              when (seCallsBack env r) guardFused
              toVars args \xs -> dynResult k BX (TApp (FComb r) xs)
        | otherwise -> liftIO . throwIO $ GiveUp "a local function with no code"
      Just (lam@(Lambda _ (ABTN.TAbss vs body)), locals)
        | length args < lamArity lam -> ret k (Clo fn args)
        | length args > lamArity lam ->
            call env lim e (Clo fn []) (take (lamArity lam) args) . KBind BX $ \res ->
              call env lim e res (drop (lamArity lam) args) k
        | otherwise -> runOr (Clo fn [] : args) $ do
            -- under a fused handler, calls are unfolded: a residual function
            -- would take the code out of the walk's sight
            fusedNow <- gets (any isFused . sFrames)
            if (not (eUnderDyn e) || fusedNow) && eDepth e < slDepth lim
              then
                pe env lim (E (M.fromList (localBinds r locals ++ zip vs args)) False (eDepth e + 1)) body k
              else do
                (f, leaves) <- residualFn env lim fn args
                toVars (concat leaves) \xs -> dynResult k BX (TApp (FVar f) xs)

-- | A residual function for a call, from its key: an existing one, or a new
-- one whose body is the function's, specialized on what is known of the
-- arguments, with a parameter for each unknown part. Gives the function's
-- variable and, per argument, the values its call passes for those
-- parameters (the caller puts them in variables).
--
-- The whistle: if a residual function for the same function is being built
-- further up with a key that embeds in this one (homeomorphic embedding,
-- 'embeds'), what is known is growing, as a continuation does in an
-- interpreter written in continuation-passing style. The parts of the
-- arguments where the two keys differ become unknown, passed from the call
-- as values, so that the keys stop growing.
residualFn :: SpecEnv -> SpecLimits -> Fn -> [Abs] -> M (Symbol, [[Abs]])
residualFn env lim fn args0 = do
  guardFused
  -- a continuation captured while specializing emits its code where it is
  -- resumed, which must be in the scope of the request: never in another
  -- lambda
  gets (find (not . koUsed) . M.elems . sKonts) >>= \case
    Just ko -> liftIO (throwIO (Escape (koFrame ko)))
    Nothing -> pure ()
  building <- gets sBuilding
  let k0 = map keyOf args0
      (args, leaves) = case find (\(fn', ks) -> fn' == fn && ks /= k0 && and (zipWith embeds ks k0) && length ks == length k0) building of
        Just (_, ks) -> Prelude.unzip (zipWith generalize ks args0)
        Nothing -> (args0, map dynLeaves args0)
      key = Key fn (map keyOf args)
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
          let holes = concatMap dynLeaves args
          params <- traverse (\_ -> fresh) holes
          let mems = [m | Dyn _ m <- holes]
              args' = evalState (traverse renameLeaves args) params
              Fn r _ = fn
              e' = E (M.fromList (localBinds r locals ++ zip vs args')) False 0
          outer <- gets sLetNum
          modify' \s -> s {sLetNum = 1, sBuilding = (fn, map keyOf args) : sBuilding s}
          body' <- pe env lim e' body KTail
          modify' \s ->
            s
              { sLetNum = outer,
                sBuilding = drop 1 (sBuilding s),
                sFns = (f, Lambda mems (ABTN.TAbss params body')) : sFns s
              }
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

-- | An argument generalized against an ancestor's key: the parts that
-- differ become unknown. Gives the argument as the residual function sees
-- it, and the values the call passes for its unknown parts.
generalize :: KArg -> Abs -> (Abs, [Abs])
generalize k a = case (k, a) of
  (_, Dyn {}) -> (a, [a])
  (KCon r t ks, Con r' t' fs)
    | r == r', t == t', length ks == length fs ->
        let (gs, ls) = Prelude.unzip (zipWith generalize ks fs) in (Con r t gs, concat ls)
  (KClo f ks, Clo f' as)
    | f == f', length ks == length as ->
        let (gs, ls) = Prelude.unzip (zipWith generalize ks as) in (Clo f gs, concat ls)
  _
    | safeEq k (keyOf a) -> (a, dynLeaves a)
    | otherwise -> (Dyn placeholder (memOf a), [a])
  where
    placeholder = Var.named "general"
    memOf = \case
      Known v _ | isUnboxed v -> UN
      Dyn _ m -> m
      _ -> BX
    isUnboxed = \case
      NatVal _ -> True
      IntVal _ -> True
      CharVal _ -> True
      DoubleVal _ -> True
      _ -> False

-- | Homeomorphic embedding of keys: the first is the second with parts
-- removed or grown. A literal embeds one at least as big (a natural's value,
-- an integer's magnitude, a text's length), so that a known counter going
-- down never counts as growing.
embeds :: KArg -> KArg -> Bool
embeds a b = couple a b || dive b
  where
    couple (KHole _) _ = True
    couple (KKnown v) (KKnown w) = knownLE v w
    couple (KCon r t xs) (KCon r' t' ys) = r == r' && t == t' && length xs == length ys && and (zipWith embeds xs ys)
    couple (KClo f xs) (KClo g ys) = f == g && length xs == length ys && and (zipWith embeds xs ys)
    couple _ _ = False
    dive = \case
      KCon _ _ ys -> any (embeds a) ys
      KClo _ ys -> any (embeds a) ys
      _ -> False
    knownLE v w = case (v, w) of
      (NatVal x, NatVal y) -> x <= y
      (IntVal x, IntVal y) -> abs x <= abs y
      (BoxedVal (TextC x), BoxedVal (TextC y)) -> Util.Text.size x <= Util.Text.size y
      (DoubleVal _, DoubleVal _) -> True
      _ -> safeEq (KKnown v) (KKnown w)

-- | Equality of keys that can't fail: comparing known values of different
-- types is an error in the runtime's equality, and counts as different.
safeEq :: KArg -> KArg -> Bool
safeEq x y = unsafePerformIO $ either (\(_ :: SomeException) -> False) id <$> Control.Exception.try (Control.Exception.evaluate (x == y))
{-# NOINLINE safeEq #-}

keyOf :: Abs -> KArg
keyOf = \case
  SReq {} -> KHole BX
  SPure {} -> KHole BX
  SKont {} -> KHole BX
  Known v _ -> KKnown v
  Con r t fs -> KCon r t (map keyOf fs)
  Clo f as -> KClo f (map keyOf as)
  Dyn _ m -> KHole m

dynLeaves :: Abs -> [Abs]
dynLeaves = \case
  a@Dyn {} -> [a]
  Con _ _ fs -> concatMap dynLeaves fs
  Clo _ as -> concatMap dynLeaves as
  _ -> []

-- | A call of @specialize.boundary@: the call of its function on the rest
-- of its arguments stays a call.
boundary :: SpecEnv -> E -> [Abs] -> K -> M Term
boundary _ _ as k = guardFused >> case as of
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
  -- a fused handler given a request: its case for it, with the request's
  -- continuation pending for the case's shift
  (SReq r t args kid, MatchRequest arms _)
    | Just cs <- lookup r arms,
      Just (_, ABTN.TAbss vs body) <- EC.lookup t cs,
      length vs == length args -> do
        modify' \st -> st {sPending = Just (kid, r)}
        pe env lim (foldr (uncurry bind) e (zip vs args)) body k
    | otherwise -> guardFused >> giveUp "a request its handler has no case for"
  -- a fused handler given its body's value: the pure case
  -- (the pure case binds the value, @{ a } -> ...@)
  (SPure a, MatchRequest _ df) -> case df of
    ABTN.TAbs u body -> pe env lim (bind u a e) body k
    _ -> pe env lim (bind v a e) df k
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
    dynamic = do
      -- under a fused handler, the rest of the walk goes into each arm, so
      -- that a request in an arm sees all of its continuation
      fusedNow <- gets (any isFused . sFrames)
      case k of
        KBind {} | fusedNow -> dynamicWith k (\code -> pure code)
        _ -> dynamicWith KTail (dynResult k BX)
    dynamicWith kk finish = toVar (look e v) \x -> do
      let arm e' body = isolated (pe env lim e' body kk)
      br' <- case br of
        MatchData r cs df ->
          MatchData r
            <$> (EC.mapFromList <$> traverse (dataArm arm r) (EC.mapToList cs))
            <*> traverse (arm under) df
        MatchIntegral cs df -> MatchIntegral <$> traverse (arm under) cs <*> traverse (arm under) df
        MatchNumeric r cs df -> MatchNumeric r <$> traverse (arm under) cs <*> traverse (arm under) df
        MatchText cs df -> MatchText <$> traverse (arm under) cs <*> traverse (arm under) df
        MatchBytes cs df -> MatchBytes <$> traverse (arm under) cs <*> traverse (arm under) df
        MatchRequest arms df ->
          MatchRequest
            <$> traverse (\(r, cs) -> (r,) . EC.mapFromList <$> traverse (requestArm arm) (EC.mapToList cs)) arms
            <*> pureArm arm df
        _ -> giveUp "match form"
      finish (TMatch x br')
    dataArm arm r (t, (ms, ABTN.TAbss vs body)) = do
      vs' <- traverse (const fresh) vs
      let fields = zipWith Dyn vs' ms
          e' = foldr (uncurry bind) (bind v (Con r t fields) under) (zip vs fields)
      body' <- arm e' body
      pure (t, (ms, ABTN.TAbss vs' body'))
    -- the pure case of a handler binds the value
    pureArm arm = \case
      ABTN.TAbs u body -> do
        u' <- fresh
        ABTN.TAbs u' <$> arm (bind u (Dyn u' BX) under) body
      body -> arm under body
    requestArm arm (t, (ms, ABTN.TAbss vs body)) = do
      vs' <- traverse (const fresh) vs
      let e' = foldr (uncurry bind) under (zip vs (zipWith Dyn vs' ms))
      body' <- arm e' body
      pure (t, (ms, ABTN.TAbss vs' body'))

-- | A walk whose changes to the handlers in force are undone afterwards:
-- the arms of a match are walked one after the other, each from the same
-- state.
isolated :: M a -> M a
isolated act = do
  st <- get
  r <- act
  modify' \st' -> st' {sFrames = sFrames st, sTargets = sTargets st, sKonts = sKonts st, sPending = sPending st}
  pure r

giveUp :: String -> M a
giveUp = liftIO . throwIO . GiveUp

jumpCont :: Reference
jumpCont = Builtin "jumpCont"

isFused :: Frame -> Bool
isFused Fused {} = True
isFused Barrier = False

-- | Escapes to the innermost fused handler, if there is one: the code
-- about to be produced could perform its requests unseen.
guardFused :: M ()
guardFused =
  gets (find isFused . sFrames) >>= \case
    Just f -> liftIO (throwIO (Escape (frId f)))
    Nothing -> pure ()

-- | A walk with a kept handler in force: a barrier to fused ones outside.
withBarrier :: M a -> M a
withBarrier act = do
  modify' \st -> st {sFrames = Barrier : sFrames st}
  r <- act
  modify' \st -> st {sFrames = drop 1 (sFrames st)}
  pure r

-- | A request. One a fused handler handles: the handler's case for it,
-- whose value is the @handle@'s, with the rest of the walk as the
-- request's continuation. One no fused handler handles: kept.
request :: SpecEnv -> SpecLimits -> E -> Reference -> CTag -> [Abs] -> K -> M Term
request env lim e r t args k = do
  frames <- gets sFrames
  case route frames 0 of
    Nothing -> toVars args \xs -> dynResult k BX (TReq r t xs)
    Just (Left fid) -> liftIO (throwIO (Escape fid))
    Just (Right j) -> do
      let f = frames !! j
      rest <- case k of
        KBind _ g -> pure g
        KTail -> liftIO (throwIO (Escape (frId f)))
      targets <- gets sTargets
      kid <- gets sNextId
      let above = length targets - frDepth f - 1
      modify' \st ->
        st
          { sFrames = drop (j + 1) frames,
            sTargets = drop (above + 1) targets,
            sKonts = M.insert kid (Kont rest (take j frames) (take above targets) (frId f) False) (sKonts st),
            sNextId = kid + 1
          }
      callHandler env lim e (frHandler f) (SReq r t args kid) (frOuter f)
  where
    -- the frame handling the request: Nothing for none, Left to escape to a
    -- fused handler behind a barrier
    route [] _ = Nothing
    route (Barrier : fs) _ = Left . frId <$> find (\f -> isFused f && r `elem` frRefs f) fs
    route (f : fs) j
      | r `elem` frRefs f = Just (Right j)
      | otherwise = route fs (j + 1)

-- | A fused handler applied to a request or to its body's value.
callHandler :: SpecEnv -> SpecLimits -> E -> Abs -> Abs -> K -> M Term
callHandler env lim e h arg k = case h of
  Clo fn@(Fn r _) caps
    | Just (lam@(Lambda _ (ABTN.TAbss vs body)), locals) <- lambdaOf env fn,
      length caps + 1 == lamArity lam ->
        pe env lim (E (M.fromList (localBinds r locals ++ zip vs (caps ++ [arg]))) False (eDepth e + 1)) body k
  Known v _ | Just (fn, caps) <- knownClosure env v -> callHandler env lim e (Clo fn caps) arg k
  _ -> guardFused >> giveUp "a fused handler that isn't a known function"

-- | Resuming a continuation captured while specializing: the walk goes on
-- from the request, with the value as its result, the frames and targets
-- between it and its handler put back, and the resumption site as where
-- the body returns. A second resumption escapes, for now.
resume :: Int -> Abs -> K -> M Term
resume kid v k = do
  gets (M.lookup kid . sKonts) >>= \case
    Nothing -> giveUp "an unknown continuation"
    Just ko
      | koUsed ko -> liftIO (throwIO (Escape (koFrame ko)))
      | otherwise -> do
          modify' \st ->
            st
              { sKonts = M.insert kid ko {koUsed = True} (sKonts st),
                sFrames = koFrames ko ++ sFrames st,
                sTargets = koTargets ko ++ (ret k : sTargets st)
              }
          koRest ko v

-- | A handler over a body. If everything the term uses is known, it runs.
-- If the handler is a known function over abilities builtins can't
-- request, it is fused: requests the walk reaches are handled as it goes.
-- Otherwise, or if fusing escapes, the handler stays and the body under it
-- is specialized, its requests kept.
handler :: SpecEnv -> SpecLimits -> E -> Term -> [Reference] -> Symbol -> Term -> K -> M Term
handler env lim e t rs h body k =
  runIfKnown env e t >>= \case
    Just v -> ret k (Known v Nothing)
    Nothing
      | fusable (look e h) -> fused
      | otherwise -> kept
  where
    fusable = \case
      Clo fn _ -> all ordinaryAbility rs && isJust (lambdaOf env fn)
      Known v _ -> all ordinaryAbility rs && isJust (knownClosure env v)
      _ -> False
    ordinaryAbility r = case r of
      Builtin _ -> False
      _ -> r /= exceptionRef
    kept = toVar (look e h) \h' -> do
      body' <- withBarrier (pe env lim e body KTail)
      dynResult k BX (THnd rs h' Nothing body')
    fused = do
      fid <- gets sNextId
      modify' \st -> st {sNextId = fid + 1}
      s0 <- get
      let attempt = do
            d <- gets (length . sTargets)
            modify' \st ->
              st
                { sFrames = Fused fid rs (look e h) k d : sFrames st,
                  sTargets = pureCase : sTargets st
                }
            pe env lim e body . KBind BX $ \a -> do
              tgt <- popTarget
              tgt a
          pureCase a = do
            modify' \st -> st {sFrames = dropFused fid (sFrames st)}
            callHandler env lim e (look e h) (SPure a) k
      liftIO (Control.Exception.try (runStateT attempt s0)) >>= \case
        Right (code, s1) -> put s1 >> pure code
        Left (Escape i)
          | i == fid -> put s0 >> kept
          | otherwise -> liftIO (throwIO (Escape i))
    dropFused fid = \case
      (f@Fused {} : fs) | frId f == fid -> fs
      fs -> fs
    popTarget =
      gets sTargets >>= \case
        tgt : ts -> tgt <$ modify' \st -> st {sTargets = ts}
        [] -> giveUp "no target for a fused handler's body"

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
  _ -> pure Nothing

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
  SReq {} -> guardFused >> giveUp "a request as a value"
  SPure {} -> guardFused >> giveUp "a handler's pure case as a value"
  SKont {} -> guardFused >> giveUp "a continuation as a value"
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
  -- an entry over unknown values only: a closure of the original code
  Clo (Fn r 0) as | all isDyn as -> toVars as \xs -> name (Left r) xs
  -- otherwise a closure of a residual function specialized on what is
  -- known of the captured values, whose remaining parameters are holes
  Clo fn@(Fn r i) as -> do
    guardFused
    env <- gets sEnv
    lim <- gets sLim
    case lambdaOf env fn of
      Nothing
        | i == 0 -> toVars as \xs -> name (Left r) xs
        | otherwise -> liftIO . throwIO $ GiveUp "a local function with no code"
      Just (Lambda ms _, _) -> do
        holes <- traverse (\m -> (`Dyn` m) <$> fresh) (drop (length as) ms)
        (f, leaves) <- residualFn env lim fn (as ++ holes)
        toVars (concat (take (length as) leaves)) \xs -> name (Right f) xs
  where
    lit m code = do
      x <- fresh
      rest <- k x
      pure (TLets Direct [x] [m] code rest)
    name f xs = do
      x <- fresh
      rest <- k x
      pure (TName x f xs rest)
    isDyn Dyn {} = True
    isDyn _ = False

-- | A term whose free variables are all known, run on the interpreter.
runIfKnown :: SpecEnv -> E -> Term -> M (Maybe Val)
runIfKnown env e t = do
  let fvs = S.toList (ABTN.freeVars t)
      abss = map (look e) fvs
  allKnown env abss >>= \case
    Nothing -> pure Nothing
    Just vs -> liftIO (seRunTerm env (zip fvs (map memOf abss)) t vs)
  where
    memOf = \case
      Known v _ | unboxedVal v -> UN
      Dyn _ m -> m
      _ -> BX

unboxedVal :: Val -> Bool
unboxedVal = \case
  NatVal _ -> True
  IntVal _ -> True
  CharVal _ -> True
  DoubleVal _ -> True
  _ -> False

-- | Code the partial evaluator doesn't look into: run if its inputs are
-- all known, otherwise kept as it is, its free variables given values in
-- the output first and its indirect lets numbered anew.
opaque :: SpecEnv -> E -> Term -> K -> M Term
opaque env e t k =
  runIfKnown env e t >>= \case
    Just v -> ret k (Known v Nothing)
    Nothing -> keepOpaque e t k

keepOpaque :: E -> Term -> K -> M Term
keepOpaque e t k = do
  guardFused
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

-- * Clean-up

-- | Residual functions inlined where they are called, when no closure is
-- made of them and they don't call themselves: those that aren't recursive
-- when called once or small, and those recursive through other functions
-- when not too big. That cleans up the partial evaluator's habit of making
-- a residual function at every call under an unknown match.
inlineFns :: [(Symbol, SuperNormal Reference Symbol)] -> Term -> M ([(Symbol, SuperNormal Reference Symbol)], Term)
inlineFns fns body = case find inlinable fns of
  Nothing -> pure (fns, body)
  Just (f, lam) -> do
    body' <- inlineCalls f lam body
    rest <-
      for [(g, l) | (g, l) <- fns, g /= f] \(g, Lambda ms (ABTN.TAbss ps bd)) ->
        (\bd' -> (g, Lambda ms (ABTN.TAbss ps bd'))) <$> inlineCalls f lam bd
    inlineFns rest body'
  where
    names = S.fromList (map fst fns)
    bodies = body : [bd | (_, Lambda _ (ABTN.TAbss _ bd)) <- fns]
    calls f = sum [length [() | AApp (FVar g) _ <- nodes bd, g == f] | bd <- bodies]
    closed f = or [g == f | bd <- bodies, AName (Right g) _ _ <- nodes bd]
    graph = M.fromList [(g, refsIn names bd) | (g, Lambda _ (ABTN.TAbss _ bd)) <- fns]
    recursive f = reaches S.empty (S.toList (M.findWithDefault S.empty f graph))
      where
        reaches _ [] = False
        reaches seen (g : gs)
          | g == f = True
          | S.member g seen = reaches seen gs
          | otherwise = reaches (S.insert g seen) (S.toList (M.findWithDefault S.empty g graph) ++ gs)
    -- a function recursive only through others, if small, goes into its
    -- callers, leaving one that calls itself as the loop (GHC's loop
    -- breaker): fib's two halves become one function, which saves a call
    -- per round
    selfCalls f bd = S.member f (refsIn names bd)
    inlinable (f, Lambda _ (ABTN.TAbss _ bd))
      | closed f || selfCalls f bd = False
      | recursive f = length (nodes bd) <= 40
      | otherwise = calls f == 1 || length (nodes bd) <= 4

-- | Each call of a residual function replaced by its body, with its
-- parameters renamed to the arguments and its binders made fresh.
inlineCalls :: Symbol -> SuperNormal Reference Symbol -> Term -> M Term
inlineCalls f (Lambda _ (ABTN.TAbss ps bd)) = go
  where
    go = \case
      TApp (FVar g) xs
        | g == f, length xs == length ps -> freshenAll (ABTN.renames (M.fromList (zip ps xs)) bd)
      ABTN.TAbs v b -> ABTN.TAbs v <$> go b
      ABTN.TTm x -> ABTN.TTm <$> traverse go x

freshenAll :: Term -> M Term
freshenAll = \case
  ABTN.TAbs v b -> do
    v' <- fresh
    ABTN.TAbs v' <$> freshenAll (ABTN.rename v v' b)
  ABTN.TTm x -> ABTN.TTm <$> traverse freshenAll x

-- | Every node of a term.
nodes :: Term -> [ANormalF Reference Symbol Term]
nodes = \case
  ABTN.TAbs _ b -> nodes b
  ABTN.TTm x -> x : foldMap nodes x

-- | The residual functions a term calls or makes closures of.
refsIn :: S.Set Symbol -> Term -> S.Set Symbol
refsIn names t =
  S.fromList [g | AApp (FVar g) _ <- nodes t, S.member g names]
    <> S.fromList [g | AName (Right g) _ _ <- nodes t, S.member g names]

-- | The constants each residual function needs: the ones its body uses and
-- the ones the functions it calls need, in the order of the entry's.
constNeeds :: [Symbol] -> [(Symbol, SuperNormal Reference Symbol)] -> M.Map Symbol [Symbol]
constNeeds consts fns = M.map (\used -> filter (`S.member` used) consts) (fixed direct)
  where
    cset = S.fromList consts
    names = S.fromList (map fst fns)
    direct = M.fromList [(f, S.intersection cset (ABTN.freeVars bd)) | (f, Lambda _ (ABTN.TAbss _ bd)) <- fns]
    callees = M.fromList [(f, refsIn names bd) | (f, Lambda _ (ABTN.TAbss _ bd)) <- fns]
    fixed m
      | m' == m = m
      | otherwise = fixed m'
      where
        m' = M.mapWithKey (\f used -> used <> foldMap (\g -> M.findWithDefault S.empty g m) (M.findWithDefault S.empty f callees)) m

-- | The indirect lets of a lambda numbered 1, 2, ..., unique as the
-- emitter needs them, whatever inlining brought together.
renumberLets :: Term -> Term
renumberLets t = evalState (go t) 1
  where
    go = \case
      ABTN.TAbs v b -> ABTN.TAbs v <$> go b
      ABTN.TTm (ALet (Indirect _) ms bn bo) -> do
        n <- state \n -> (n, n + 1)
        ABTN.TTm <$> (ALet (Indirect n) ms <$> go bn <*> go bo)
      ABTN.TTm x -> ABTN.TTm <$> traverse go x

-- | Each call and closure of a residual function given the constants it
-- needs first ('constNeeds').
threadConsts :: M.Map Symbol [Symbol] -> Term -> Term
threadConsts needs = go
  where
    go = \case
      ABTN.TAbs v b -> ABTN.TAbs v (go b)
      TApp (FVar f) xs | Just cs <- M.lookup f needs -> TApp (FVar f) (cs ++ xs)
      TName x (Right f) xs bo | Just cs <- M.lookup f needs -> TName x (Right f) (cs ++ xs) (go bo)
      ABTN.TTm f -> ABTN.TTm (fmap go f)
