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
import Unison.Runtime.Builtin.Decls (eitherLeftId, eitherRef, eitherRightId, exceptionRef, pairRef, unitRef)
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
    seCallsBack :: Reference -> Bool,
    -- | the codebase's name for a group, if the cache has one, to name the
    -- residual functions made from it
    seName :: Reference -> Maybe Text,
    -- | a note for whoever is watching (@UNISON_SPECIALIZE_DEBUG@): why a
    -- handler was kept
    seNote :: String -> IO ()
  }

-- | Bounds on the work: steps of the walk, residual functions, and the
-- depth of unfolding with no unknown match above it. Running out makes
-- the whole specialization give up, and the caller falls back to the
-- original code.
data SpecLimits = SpecLimits
  { slSteps :: !Int,
    slFunctions :: !Int,
    slDepth :: !Int,
    -- | how much code may be generated, in lets and matches ('emitted'):
    -- the caller sets it to a multiple of the size of the input, the
    -- function's code and the known values (Machine.specLimits)
    slOutput :: !Int,
    -- | whether handlers are fused; off for the second try, after the
    -- first ran out of 'slOutput' ('specializeLambda')
    slFuse :: !Bool,
    -- | whether the code after an unknown match under a fused handler is
    -- shared as a join point ('joined') or copied into each arm; off only
    -- to compare (@UNISON_SPECIALIZE_JOIN=off@)
    slJoin :: !Bool
  }

defaultSpecLimits :: SpecLimits
defaultSpecLimits = SpecLimits {slSteps = 200000, slFunctions = 500, slDepth = 200, slOutput = 2000, slFuse = True, slJoin = True}

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
data Key = Key Bool Fn [KArg]
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
data Escape = Escape Int String
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
        frDepth :: !Int,
        -- | the frame of a residual function's body in a fused handler's
        -- scope: it owns no target, since the body returns to the
        -- function's caller (state passing, 'residualFn')
        frSegment :: !Bool
      }
  | Barrier

-- | What a target is: a fused handler's pure case, the end of the body of a
-- fused handler (a resumption as that body, which is how a case re-installs
-- its handler), or any other site that resumed a continuation.
data Tgt = TPure Int | TBody Int | TSite

-- | The continuation of a request handled while specializing: the rest of
-- the walk from the request, and the frames and targets between it and the
-- handler, which a resumption puts back.
data Kont = Kont
  { koRest :: Abs -> M Term,
    koFrames :: [Frame],
    koTargets :: [(Tgt, Abs -> M Term)],
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
    sTargets :: [(Tgt, Abs -> M Term)],
    sKonts :: M.Map Int Kont,
    sNextId :: !Int,
    -- | the continuation a handler's case is about to capture with @shift@
    sPending :: Maybe (Int, Reference),
    -- | the abilities and the handler as passed, of each state-passing
    -- residual function being built, by the number of its frame
    sReturns :: M.Map Int ([Reference], Abs),
    -- | the lets and matches generated so far ('emitted')
    sEmitted :: !Int,
    -- | per join point being made, where arms reached the code after the
    -- match: the placeholder left there, the value, and the continuations
    -- resumed by then ('joined')
    sJoins :: M.Map Int [(Symbol, Abs, [Int], [Abs])],
    -- | what follows each fused @handle@, by the number of its first frame
    -- ('KOuter')
    sOuters :: M.Map Int K,
    -- | the residual functions that are join points
    sJoinFns :: S.Set Symbol
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
data K
  = KTail
  | -- | the binder's name in the output
    KBind Mem Symbol (Abs -> M Term)
  | -- | the end of the body of the fused handler with this number: the
    -- value goes to the innermost target ('sTargets')
    KDelim Int
  | -- | the end of the body of the state-passing residual function whose
    -- frame has this number: @Right (result, state)@ ('returnRight')
    KReturn Int
  | -- | the value of a case that didn't resume, in that function:
    -- @Left value@
    KAbort Int
  | -- | what follows the @handle@ first fused as the frame with this
    -- number ('sOuters'), named so that two handlers can be seen to return
    -- to the same place: a case that re-installs its handler as its last
    -- act gives the new one this same continuation
    KOuter Int Mem

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
specializeLambda env lim r i args =
  specializeLambda1 env lim r i args >>= \case
    -- fused handlers copy code, and the ones that grew it may be finished
    -- by the time the budget runs out: once more, with every handler kept
    Left why | why == tooBig, slFuse lim -> specializeLambda1 env lim {slFuse = False} r i args
    made -> pure made

tooBig :: String
tooBig = "the size of the generated code"

specializeLambda1 ::
  SpecEnv ->
  SpecLimits ->
  Reference ->
  Word64 ->
  [Val] ->
  IO (Either String (SuperGroup Reference Symbol, [Val]))
specializeLambda1 env lim r i args = case lambdaOf env (Fn r i) of
  Nothing -> pure (Left "no code for the lambda")
  Just (Lambda ccs (ABTN.TAbss vs body), locals)
    | length vs < length args -> pure (Left "more arguments than parameters")
    | otherwise ->
        either (\(GiveUp why) -> Left why) id <$> Control.Exception.try @GiveUp do
         either (\(Escape _ why) -> Left ("an escape from a fused handler: " ++ why)) id <$> Control.Exception.try @Escape (Right <$> do
          flip evalStateT (S 0 1 0 M.empty [] [] env lim [] [] [] M.empty 0 Nothing M.empty 0 M.empty M.empty S.empty) do
            argSyms <- traverse freshNamed (take (length args) vs)
            restSyms <- traverse freshNamed (drop (length args) vs)
            let restMems = drop (length args) ccs
                binds =
                  zip vs (zipWith (\v s -> Known v (Just s)) args argSyms)
                    ++ zip (drop (length args) vs) (zipWith Dyn restSyms restMems)
                e0 = E (M.fromList (localBinds r locals ++ binds)) False 0
            body0 <- pe env lim e0 body KTail
            consts <- gets (reverse . sConsts)
            fns0 <- gets sFns
            let constSyms = argSyms ++ map fst consts
                -- a join point uses variables of where it is called: they
                -- become parameters, before inlining, which renames the
                -- variables of the copies it makes
                free = freeNeeds (S.fromList constSyms) fns0
                mems =
                  M.fromList $
                    zip restSyms restMems
                      ++ binderMems body0
                      ++ concat [zip ps ms ++ binderMems bd | (_, Lambda ms (ABTN.TAbss ps bd)) <- fns0]
                memOfVar x = M.findWithDefault BX x mems
                fns1 =
                  [ (f, Lambda (map memOfVar xs ++ ms) (ABTN.TAbss (xs ++ ps) (threadConsts free bd)))
                  | (f, Lambda ms (ABTN.TAbss ps bd)) <- fns0,
                    let xs = M.findWithDefault [] f free
                  ]
            joins <- gets sJoinFns
            (fns, body') <- inlineFns joins fns1 (threadConsts free body0)
            let needs = constNeeds constSyms fns
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

-- | A new variable with the name given, made unique: the name of the
-- program's variable it stands for, so that 'runtime.ir' shows the
-- program's names, or a synthetic one ('syn') for what the program has no
-- variable for. The ids start above any the program's
-- own variables have, so a new variable never equals one of them (code
-- kept as it is keeps its binders).
freshNamed :: Symbol -> M Symbol
freshNamed v = do
  n <- gets sFresh
  modify' \s -> s {sFresh = n + 1}
  pure (Var.freshenId (n + 2 ^ (40 :: Int)) v)

syn :: Text -> Symbol
syn = Var.named

-- | The tags of a state-passing function's results ('resultMatch').
leftTag, rightTag :: CTag
leftTag = CTag (fromIntegral eitherLeftId)
rightTag = CTag (fromIntegral eitherRightId)

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
ret (KBind _ _ k) a = k a
ret (KDelim _) a = popTarget >>= \(_, tgt) -> tgt a
ret (KReturn fid) a = returnRight fid a
ret (KAbort _) v = toVar v \x -> pure (TCon eitherRef leftTag [x])
ret (KOuter fid _) a =
  gets (M.lookup fid . sOuters) >>= \case
    Just k -> ret k a
    Nothing -> giveUp "the continuation of an unknown handler"

popTarget :: M (Tgt, Abs -> M Term)
popTarget =
  gets sTargets >>= \case
    tgt : ts -> tgt <$ modify' \st -> st {sTargets = ts}
    [] -> giveUp "no target for a fused handler's body"

-- | Gives an unknown result: a residual computation of it, then the rest.
dynResult :: K -> Mem -> Term -> M Term
dynResult KTail _ code = pure code
dynResult (KBind m h k) _ code = do
  emitted
  x <- freshNamed h
  n <- letNum
  rest <- k (Dyn x m)
  pure (TLets (Indirect n) [x] [m] code rest)
dynResult k _ code = dynResult (KBind BX (syn "r") (ret k)) BX code

-- | Like 'dynResult' for a primitive, bound by a direct let.
dynPrim :: K -> Term -> M Term
dynPrim KTail code = pure code
dynPrim (KBind m h k) code = do
  emitted
  x <- freshNamed h
  rest <- k (Dyn x m)
  pure (TLets Direct [x] [m] code rest)
dynPrim k code = dynPrim (KBind BX (syn "r") (ret k)) code

pe :: SpecEnv -> SpecLimits -> E -> Term -> K -> M Term
pe env lim e t k = tick lim >> case t of
  TVar v -> ret k (look e v)
  TLit l -> literal l
  TBLit l -> literal l
  TLets _ [v] [m] bn bo ->
    pe env lim e bn . KBind m v $ \a -> pe env lim (bind v a e) bo k
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
        v' <- freshNamed v
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
  _ -> runOr (fa : as) (guardFused ("a call of an unknown function" ++ (case fa of Dyn x _ -> ", " ++ show (Var.name x); _ -> "")) >> toVarApp fa as \g xs -> dynResult k BX (TApp (FVar g) xs))
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
    kind = \case
      SReq {} -> "a request"
      SPure {} -> "a pure case's value"
      SKont {} -> "a known continuation"
      Known {} -> "a known value"
      Con {} -> "a constructor"
      Clo {} -> "a closure"
      Dyn {} -> "an unknown value"
    callFn fn@(Fn r i) args = case lambdaOf env fn of
      -- a builtin, or code not loaded here: a call, unless it can run
      Nothing
        | i == 0 ->
            runOr (Clo fn [] : args) do
              when (seCallsBack env r) (guardFused ("a builtin that calls back into Unison code, " ++ show r ++ " on " ++ unwords (map kind args)))
              toVars args \xs -> dynResult k BX (TApp (FComb r) xs)
        | otherwise -> liftIO . throwIO $ GiveUp "a local function with no code"
      Just (lam@(Lambda _ (ABTN.TAbss vs body)), locals)
        | length args < lamArity lam -> ret k (Clo fn args)
        | length args > lamArity lam ->
            call env lim e (Clo fn []) (take (lamArity lam) args) . KBind BX (syn "r") $ \res ->
              call env lim e res (drop (lamArity lam) args) k
        | otherwise -> runOr (Clo fn [] : args) $ do
            if not (eUnderDyn e) && eDepth e < slDepth lim
              then
                pe env lim (E (M.fromList (localBinds r locals ++ zip vs args)) False (eDepth e + 1)) body k
              else residualCall env lim fn args k

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
residualFn env lim fn args = (\(f, _, leaves) -> (f, leaves)) <$> residualFnWith env lim Nothing fn args

-- | A call that becomes a call of a residual function. In the scope of a
-- fused handler the function is passed the handler's state and gives it
-- back (state passing): it returns @Left v@ when a case of the handler
-- didn't resume, @v@ being the @handle@'s value, or @Right (result, state)@,
-- and the caller goes on accordingly.
residualCall :: SpecEnv -> SpecLimits -> Fn -> [Abs] -> K -> M Term
residualCall env lim fn args k = do
  frames <- gets sFrames
  case frames of
    f@Fused {} : rest | not (any isFused rest) -> do
      (rf, tmpl, leaves) <- residualFnWith env lim (Just f) fn args
      inTail <- tailInSame f tmpl
      if inTail
        then toVars (concat leaves) \xs -> pure (TApp (FVar rf) xs)
        else toVars (concat leaves) \xs -> do
          r <- freshNamed (syn "r")
          n <- letNum
          rest' <- resultMatch f tmpl r k
          pure (TLets (Indirect n) [r] [BX] (TApp (FVar rf) xs) rest')
    _ -> do
      (rf, _, leaves) <- residualFnWith env lim Nothing fn args
      toVars (concat leaves) \xs -> dynResult k BX (TApp (FVar rf) xs)
  where
    -- a call in tail position of a state-passing function, under the
    -- handler it was passed or one of the same shape, with nothing between:
    -- what the callee returns is what this function would, so the match is
    -- the identity and the call a tail call
    tailInSame f tmpl = case k of
      KReturn rfid -> do
        frames <- gets sFrames
        targets <- gets sTargets
        returns <- gets sReturns
        pure case (frames, M.lookup rfid returns) of
          ([top@Fused {}], Just (refs, hAbs)) ->
            frId top == frId f
              && frRefs top == refs
              && isAbort rfid (frOuter top)
              && all (okTarget (frId top) . fst) targets
              && safeEq (keyOf tmpl) (keyOf hAbs)
          _ -> False
      _ -> pure False
    isAbort rfid (KAbort i) = i == rfid
    isAbort _ _ = False

okTarget :: Int -> Tgt -> Bool
okTarget top = \case
  TBody _ -> True
  TPure i -> i == top
  TSite -> False

-- | The caller's side of state passing: a match on what the residual
-- function returned. @Left v@: the @handle@ is over, with value @v@.
-- @Right (result, state)@: the handler, rebuilt from the state, stays in
-- force, and the walk goes on with the result.
resultMatch :: Frame -> Abs -> Symbol -> K -> M Term
resultMatch f tmpl r k = do
  v <- freshNamed (syn "done")
  abortArm <- isolated do
    frames <- gets sFrames
    targets <- gets sTargets
    let own = if frSegment f then 0 else 1
        above = length targets - frDepth f - own
    modify' \st ->
      st
        { sFrames = drop 1 (dropWhile (\g -> not (isFused g && frId g == frId f)) frames),
          sTargets = drop (above + own) targets
        }
    ret (frOuter f) (Dyn v BX)
  p <- freshNamed (syn "p")
  rightArm <- isolated do
    res <- freshNamed (syn "r")
    let mems = [m | Dyn _ m <- dynLeaves tmpl]
    leafVars <- traverse (const (freshNamed (syn "s"))) mems
    let hNew = evalState (fillLeaves tmpl) (zipWith Dyn leafVars mems)
    let setHandler = \case
          g@Fused {} | frId g == frId f -> g {frHandler = hNew}
          g -> g
    modify' \st -> st {sFrames = map setHandler (sFrames st)}
    inner <- ret k (Dyn res (kMem k))
    c0 <- freshNamed (syn "rest")
    chain <- decodeChain c0 (zip leafVars mems) inner
    pure (TMatch p (MatchData pairRef (EC.mapFromList [(CTag 0, ([BX, BX], ABTN.TAbss [res, c0] chain))]) Nothing))
  pure . TMatch r $
    MatchData
      eitherRef
      (EC.mapFromList [(leftTag, ([BX], ABTN.TAbss [v] abortArm)), (rightTag, ([BX], ABTN.TAbss [p] rightArm))])
      Nothing
  where
    decodeChain _ [] inner = pure inner
    decodeChain c ((l, m) : ls) inner = do
      c' <- freshNamed (syn "rest")
      rest <- decodeChain c' ls inner
      pure (TMatch c (MatchData pairRef (EC.mapFromList [(CTag 0, ([m, BX], ABTN.TAbss [l, c'] rest))]) Nothing))

-- | The unknown parts of a value replaced, in order.
fillLeaves :: Abs -> State [Abs] Abs
fillLeaves = \case
  Dyn {} -> state \case
    a : as -> (a, as)
    [] -> error "fillLeaves: fewer values than unknown parts"
  Con r t fs -> Con r t <$> traverse fillLeaves fs
  Clo f as -> Clo f <$> traverse fillLeaves as
  a -> pure a

-- | The values of the unknown parts of a template in a value of its shape,
-- or Nothing if the value doesn't have its shape.
alignTo :: Abs -> Abs -> Maybe [Abs]
alignTo t a = case (t, a) of
  (Dyn {}, _) -> Just [a]
  (Con r tg fs, Con r' tg' gs)
    | r == r', tg == tg', length fs == length gs -> concat <$> zipWithM alignTo fs gs
  (Clo f as, Clo g bs)
    | f == g, length as == length bs -> concat <$> zipWithM alignTo as bs
  (Known v _, _) | safeEq (KKnown v) (keyOf a) -> Just []
  _ -> Nothing

-- | A residual function for a call, from its key: an existing one, or a new
-- one whose body is the function's, specialized on what is known of the
-- arguments, with a parameter for each unknown part. Gives the function's
-- variable, the handler as the call passes it (for state passing), and,
-- per argument, the values its call passes for those parameters.
--
-- The whistle: if a residual function for the same function is being built
-- further up with a key that embeds in this one (homeomorphic embedding,
-- 'embeds'), what is known is growing, as a continuation does in an
-- interpreter written in continuation-passing style. The parts of the
-- arguments where the two keys differ become unknown, passed from the call
-- as values, so that the keys stop growing.
--
-- With a fused handler's frame, the function passes the handler's state:
-- the handler is one more argument, its body is walked under a frame of its
-- own that owns no target (a segment), and it returns @Left v@ from a case
-- that doesn't resume, or @Right (result, state)@ at its end, where the
-- handler in force must have the shape it was passed in, every resumption
-- having re-installed a handler as its case's last act.
residualFnWith :: SpecEnv -> SpecLimits -> Maybe Frame -> Fn -> [Abs] -> M (Symbol, Abs, [[Abs]])
residualFnWith env lim mframe fn args0 = do
  case mframe of
    Nothing -> guardFused "a residual function that isn't a single handler's loop"
    Just _ -> pure ()
  -- a continuation captured while specializing emits its code where it is
  -- resumed, which must be in the scope of the request: never in another
  -- lambda
  gets (find (not . koUsed) . M.elems . sKonts) >>= \case
    Just ko -> liftIO (throwIO (Escape (koFrame ko) "a residual function with a continuation not yet resumed"))
    Nothing -> pure ()
  building <- gets sBuilding
  let allArgs0 = args0 ++ maybe [] (\f -> [frHandler f]) mframe
      k0 = map keyOf allArgs0
      (allArgs, leaves) = case find (\(fn', ks) -> fn' == fn && ks /= k0 && length ks == length k0 && and (zipWith embeds ks k0)) building of
        Just (_, ks) -> Prelude.unzip (zipWith generalize ks allArgs0)
        Nothing -> (allArgs0, map dynLeaves allArgs0)
      key = Key (isJust mframe) fn (map keyOf allArgs)
      tmpl = if isJust mframe then last allArgs else Known (NatVal 0) Nothing
  gets (M.lookup key . sMemo) >>= \case
    Just f -> pure (f, tmpl, leaves)
    Nothing -> do
      nfns <- gets (M.size . sMemo)
      when (nfns >= slFunctions lim) . liftIO . throwIO $ GiveUp "functions"
      -- named after the function it specializes, with a prime, so that it
      -- never reads as that function, which the code may also call: a
      -- local function by its binder, a group by its name in the codebase
      f <- case lambdaOf env fn of
        Just (_, locals)
          | Fn _ i <- fn,
            Just (v, _) <- find ((== i) . snd) locals,
            Var.User n <- Var.typeOf v ->
              freshNamed (syn (n <> "'"))
        _ | Fn r 0 <- fn, Just n <- seName env r -> freshNamed (syn (n <> "'"))
        _ -> freshNamed (syn "fn")
      modify' \s -> s {sMemo = M.insert key f (sMemo s)}
      case lambdaOf env fn of
        Nothing -> liftIO . throwIO $ GiveUp "no code"
        Just (Lambda _ (ABTN.TAbss vs body), locals) -> do
          let holes = concatMap dynLeaves allArgs
              -- a parameter is named after the function's own, when the
              -- whole argument is unknown, or else after the leaf's variable
              hints = concat $ zipWith hint (map Just vs ++ repeat Nothing) allArgs
              hint (Just v) Dyn {} = [v]
              hint _ a = [s | Dyn s _ <- dynLeaves a]
          params <- traverse freshNamed hints
          let mems = [m | Dyn _ m <- holes]
              allArgs' = evalState (traverse renameLeaves allArgs) params
              args' = take (length args0) allArgs'
              Fn r _ = fn
              e' = E (M.fromList (localBinds r locals ++ zip vs args')) False 0
          outer <- get
          modify' \s -> s {sLetNum = 1, sBuilding = (fn, map keyOf allArgs) : sBuilding s}
          body' <- case mframe of
            Nothing -> pe env lim e' body KTail
            Just fr -> do
              fid <- gets sNextId
              let hAbs = last allArgs'
              modify' \s ->
                s
                  { sNextId = fid + 1,
                    sFrames = [Fused fid (frRefs fr) hAbs (KAbort fid) 0 True],
                    sTargets = [],
                    sKonts = M.empty,
                    sPending = Nothing,
                    sReturns = M.insert fid (frRefs fr, hAbs) (sReturns s)
                  }
              s1 <- get
              liftIO (Control.Exception.try (runStateT (pe env lim e' body (KReturn fid)) s1)) >>= \case
                Right (b, s2) -> b <$ put s2
                -- an escape to a handler of the residual function's own makes
                -- the state passing impossible: an escape to the caller's
                Left (Escape i why)
                  | i >= fid -> liftIO (throwIO (Escape (frId fr) ("in a loop, " ++ why)))
                  | otherwise -> liftIO (throwIO (Escape i why))
          modify' \s ->
            s
              { sLetNum = sLetNum outer,
                sBuilding = sBuilding outer,
                sFrames = sFrames outer,
                sTargets = sTargets outer,
                sKonts = sKonts outer,
                sPending = sPending outer,
                sFns = (f, Lambda mems (ABTN.TAbss params body')) : sFns s
              }
          pure (f, tmpl, leaves)
  where
    renameLeaves :: Abs -> State [Symbol] Abs
    renameLeaves = \case
      Dyn _ m -> state \case
        p : ps -> (Dyn p m, ps)
        [] -> error "residualFn: fewer parameters than unknown parts"
      Con r t fs -> Con r t <$> traverse renameLeaves fs
      Clo f as -> Clo f <$> traverse renameLeaves as
      a -> pure a

-- | The end of the body of a state-passing residual function: @Right@ of
-- its value and the state of the handler in force, if every resumption
-- re-installed a handler of the shape the function was passed, as its
-- case's last act: the handler in force returns where the function does
-- (@KAbort@). A case that does more with what the @handle@ gives back
-- (@match handle k () with h with ...@) has work waiting that state
-- passing would drop, so it escapes.
returnRight :: Int -> Abs -> M Term
returnRight fid a = do
  frames <- gets sFrames
  targets <- gets sTargets
  gets (M.lookup fid . sReturns) >>= \case
    Nothing -> giveUp "the end of an unknown state-passing function"
    Just (refs, hAbs) -> case frames of
      [top@Fused {}]
        | frRefs top == refs,
          KAbort i <- frOuter top,
          i == fid,
          all (okTarget (frId top) . fst) targets,
          Just vals <- alignTo hAbs (frHandler top) ->
            toVar a \res -> toVars vals \ls -> do
              (chain, wrap) <- tupleChain ls
              p <- freshNamed (syn "p")
              o <- freshNamed (syn "out")
              pure . wrap $
                TLets Direct [p] [BX] (TCon pairRef (CTag 0) [res, chain]) $
                  TLets Direct [o] [BX] (TCon eitherRef rightTag [p]) (TVar o)
      _ -> liftIO (throwIO (Escape fid "a loop that ends with its handler not as it was passed, or not installed as a case's last act"))

-- | The values in a chain of pairs ending in unit: the chain's variable,
-- and the lets that build it.
tupleChain :: [Symbol] -> M (Symbol, Term -> Term)
tupleChain ls = do
  u <- freshNamed (syn "nil")
  go (TLets Direct [u] [BX] (TCon unitRef (CTag 0) [])) u (reverse ls)
  where
    go wrap c [] = pure (c, wrap)
    go wrap c (l : rest) = do
      c' <- freshNamed (syn "rest")
      go (wrap . TLets Direct [c'] [BX] (TCon pairRef (CTag 0) [l, c])) c' rest

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

-- | How a value is held.
memOf :: Abs -> Mem
memOf = \case
  Known v _ | isUnboxed v -> UN
  Dyn _ m -> m
  _ -> BX
  where
    isUnboxed = \case
      NatVal _ -> True
      IntVal _ -> True
      CharVal _ -> True
      DoubleVal _ -> True
      _ -> False

-- | How the value a continuation takes is held.
kMem :: K -> Mem
kMem (KBind m _ _) = m
kMem (KOuter _ m) = m
kMem _ = BX

-- | Whether two continuations are the same one, for those that say which
-- they are.
sameK :: K -> K -> Bool
sameK a b = case (a, b) of
  (KTail, KTail) -> True
  (KDelim i, KDelim j) -> i == j
  (KReturn i, KReturn j) -> i == j
  (KAbort i, KAbort j) -> i == j
  (KOuter i _, KOuter j _) -> i == j
  _ -> False

-- | The same value, down to its variables.
sameAbs :: Abs -> Abs -> Bool
sameAbs a b = case (a, b) of
  (Dyn x _, Dyn y _) -> x == y
  (Known v _, Known w _) -> safeEq (KKnown v) (KKnown w)
  (Con r t fs, Con r' t' gs) -> r == r' && t == t' && length fs == length gs && and (zipWith sameAbs fs gs)
  (Clo f as, Clo g bs) -> f == g && length as == length bs && and (zipWith sameAbs as bs)
  _ -> False

-- | The most that is known of both values, with a hole ('joinHole') where
-- they differ, if making that a hole loses little: one of them is unknown
-- there already, or both are numbers, or constructors with no fields (a
-- Boolean). Nothing if they differ in anything else that is known, such as
-- a piece of an interpreted program: code that went on without knowing
-- which it has would have to interpret it.
lubAbs :: Abs -> Abs -> Maybe Abs
lubAbs a b = case (a, b) of
  _ | sameAbs a b -> Just a
  (Con r t fs, Con r' t' gs)
    | r == r', t == t', length fs == length gs -> Con r t <$> zipWithM lubAbs fs gs
    | r == r', null fs, null gs -> hole
  (Con r t fs, Known v _)
    | Just (t', vs) <- knownData v, t == t', length fs == length vs ->
        Con r t <$> zipWithM lubAbs fs [Known x Nothing | x <- vs]
  (Known v _, Con r t gs)
    | Just (t', vs) <- knownData v, t == t', length gs == length vs ->
        Con r t <$> zipWithM lubAbs [Known x Nothing | x <- vs] gs
  (Clo f as, Clo g bs)
    | f == g, length as == length bs -> Clo f <$> zipWithM lubAbs as bs
  (Dyn {}, _) | material b -> hole
  (_, Dyn {}) | material a -> hole
  (Known v _, Known w _)
    | memOf a == UN, memOf b == UN -> hole
    | Just (_, []) <- knownData v, Just (_, []) <- knownData w -> hole
  _ -> Nothing
  where
    hole = Just (Dyn joinHole (memOf a))

joinHole :: Symbol
joinHole = Var.named "join hole"

-- | The holes of a template filled, in order.
fillHoles :: Abs -> State [Abs] Abs
fillHoles = \case
  Dyn h _ | h == joinHole -> state \case
    a : as -> (a, as)
    [] -> error "fillHoles: fewer values than holes"
  Con r t fs -> Con r t <$> traverse fillHoles fs
  Clo f as -> Clo f <$> traverse fillHoles as
  a -> pure a

-- | What a value has where a template has holes.
atHoles :: Abs -> Abs -> [Abs]
atHoles t a = case (t, a) of
  (Dyn h _, _) | h == joinHole -> [a]
  (Con _ _ fs, Con _ _ gs) -> concat (zipWith atHoles fs gs)
  (Con _ _ fs, Known v _) | Just (_, vs) <- knownData v -> concat (zipWith atHoles fs [Known x Nothing | x <- vs])
  (Clo _ as, Clo _ bs) -> concat (zipWith atHoles as bs)
  _ -> []

-- | Whether a value can be put in a variable: not a request or a
-- continuation of a fused handler.
material :: Abs -> Bool
material = \case
  SReq {} -> False
  SPure {} -> False
  SKont {} -> False
  Con _ _ fs -> all material fs
  Clo _ as -> all material as
  _ -> True

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
boundary _ _ as k = guardFused "specialize.boundary" >> case as of
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
    | otherwise -> guardFused "a request its handler has no case for" >> giveUp "a request its handler has no case for"
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
        KTail -> dynamicWith KTail (dynResult k BX)
        -- the end of a state-passing function is a few nodes, and a call
        -- there must stay in its tail ('tailInSame')
        KReturn _ | fusedNow -> dynamicWith k (\code -> pure code)
        KAbort _ | fusedNow -> dynamicWith k (\code -> pure code)
        _ | slJoin lim -> joined fusedNow
        _ | fusedNow -> dynamicWith k (\code -> pure code)
        _ -> dynamicWith KTail (dynResult k BX)
    -- Under a fused handler the rest of the walk can't be a let's body, as
    -- a request in an arm must see it. It becomes a join point: a residual
    -- function of the match's value, which each arm calls at its end. The
    -- arms are walked first, each leaving a placeholder where it reaches
    -- the rest; the values they give are generalized together into the
    -- join point's parameters; then the rest is walked once.
    --
    -- The rest depends on the handlers in force, so an arm can share it
    -- only if its handlers at its end stand for the ones at the match
    -- ('standFor'): the same ones, or ones that cases installed as their
    -- last act to go on under, which differ in their state. The states
    -- are generalized together too, and passed to the join point. Any
    -- other arm gets its own copy of the rest.
    --
    -- With no fused handler the rest could be a let's body, the match
    -- bound to a variable, and it is when the arms' values have nothing
    -- known in common. When they do (each gives a function of the same
    -- code, or the same constructor), a join point keeps that for the
    -- rest, where a variable would lose it.
    joined fusedNow = do
      st0 <- get
      let jid = sNextId st0
          m = kMem k
          kJoin = KBind m (syn "r") \a -> do
            now <- get
            case standFor st0 now of
              Just hs | material a, all material hs -> do
                ph <- freshNamed (syn "join")
                modify' \st -> st {sJoins = M.insertWith (++) jid [(ph, a, usedKonts now, hs)] (sJoins st)}
                pure (TVar ph)
              _ -> ret k a
      put st0 {sNextId = jid + 1}
      code <- dynamicWith kJoin (\c -> pure c)
      recs <- gets (M.findWithDefault [] jid . sJoins)
      modify' \st -> st {sJoins = M.delete jid (sJoins st)}
      case recs of
        [] -> pure code
        _ | not fusedNow, all (\(_, a, _, _) -> plain a) recs -> do
          -- nothing known in any arm's value: the match bound to a variable
          vals <- for recs \(ph, a, _, _) -> (ph,) <$> toVar a (pure . TVar)
          dynResult k BX (returnBound (substVars (M.fromList vals) code))
        _ -> do
          -- the arms that can share a join point, in groups: one group if
          -- they all agree on what is known ('lubAbs')
          let groups = foldr addTo [] recs
              addTo rec@(_, a, _, hs) = \case
                [] -> [((a, hs), [rec])]
                g@((t, ths), rs) : gs
                  | length ths == length hs,
                    Just t' <- lubAbs t a,
                    Just ths' <- zipWithM lubAbs ths hs ->
                      ((t', ths'), rec : rs) : gs
                  | otherwise -> g : addTo rec gs
          made <- for groups \((tmpl, htmpls), grecs) -> isolated do
            let holeMems = [hm | t <- tmpl : htmpls, Dyn h hm <- dynLeaves t, h == joinHole]
            holes <- traverse (\hm -> freshNamed (syn (if hm == UN then "n" else "r"))) holeMems
            let (res, hsNew) = evalState ((,) <$> fillHoles tmpl <*> traverse fillHoles htmpls) (zipWith Dyn holes holeMems)
                -- the handlers the rest runs under: the ones at the match,
                -- each with the state its parameters give
                setHandlers fs hs = case (fs, hs) of
                  (f@Fused {} : fs', h : hs') -> f {frHandler = h} : setHandlers fs' hs'
                  (f : fs', _) -> f : setHandlers fs' hs
                  ([], _) -> []
            modify' \st -> st {sFrames = setHandlers (sFrames st) hsNew}
            body <- ret k res
            -- a continuation resumed in an arm and again in the rest is
            -- resumed twice
            after <- get
            let fresh0 kid = kid `notElem` usedKonts st0
                byRest = filter fresh0 (usedKonts after)
            forM_ grecs \(_, _, used, _) -> forM_ (filter (`elem` byRest) (filter fresh0 used)) \kid ->
              forM_ (M.lookup kid (sKonts after)) \ko ->
                liftIO (throwIO (Escape (koFrame ko) "a continuation resumed in an arm and again after the match"))
            j <- freshNamed (syn "join")
            -- a function takes at least one argument
            (params, pmems, call) <- case holes of
              [] -> do
                u <- freshNamed (syn "nil")
                u' <- freshNamed (syn "nil")
                pure ([u'], [BX], \_ -> TLets Direct [u] [BX] (TCon unitRef (CTag 0) []) (TApp (FVar j) [u]))
              _ -> pure (holes, holeMems, TApp (FVar j))
            calls <- for grecs \(ph, a, _, hs) ->
              (ph,) <$> toVars (atHoles tmpl a ++ concat (zipWith atHoles htmpls hs)) (pure . call)
            pure (calls, (j, Lambda pmems (ABTN.TAbss params body)))
          let patch = substVars (M.fromList (concatMap fst made))
          -- a placeholder is in another function when the rest was reached
          -- from an inner join point
          modify' \st ->
            st
              { sJoinFns = foldr (S.insert . fst . snd) (sJoinFns st) made,
                sFns =
                  map snd made
                    ++ [(f, Lambda ms (ABTN.TAbss ps (patch bd))) | (f, Lambda ms (ABTN.TAbss ps bd)) <- sFns st]
              }
          pure (patch code)
    usedKonts st = [kid | (kid, ko) <- M.toList (sKonts st), koUsed ko]
    -- a value with nothing known that a variable wouldn't keep: unknown,
    -- or a literal
    plain = \case
      Dyn {} -> True
      Known v _ -> isNothing (knownData v) && isNothing (knownClosure env v)
      _ -> False
    -- Whether the handlers in force in the second state stand for the ones
    -- in the first, and if so their handlers, one per fused frame of the
    -- first. A frame stands for one if it is that frame, or handles the
    -- same abilities with the same function and returns to the same place
    -- ('sameK'): the handler a case installed as its last act. The places
    -- their bodies return to must match too. The end of a resumed body
    -- (TBody) only passes its value on, so it doesn't count; neither does
    -- the pure case of a frame standing for a state-passing function's,
    -- which gives its value back without one ('returnRight').
    standFor a b = do
      guard (length (sFrames a) == length (sFrames b))
      pairs <- zipWithM pairFrame (sFrames a) (sFrames b)
      let fused = [p | Just p <- pairs]
          ids = [(frId f, frId g) | (f, g) <- fused]
          forSegment = [frId g | (f, g) <- fused, frSegment f, not (frSegment g)]
          counts st skip = [t | (t, _) <- sTargets st, keep t]
            where
              keep = \case
                TBody _ -> False
                TPure i -> i `notElem` skip
                TSite -> True
          ta = counts a []
          tb = counts b forSegment
          same x y = case (x, y) of
            (TPure i, TPure j) -> (i, j) `elem` ids
            (TSite, TSite) -> True
            _ -> False
      guard (length ta == length tb && and (zipWith same ta tb))
      pure [frHandler g | (_, g) <- fused]
    pairFrame Barrier Barrier = Just Nothing
    pairFrame f@Fused {} g@Fused {}
      | sameFn (frHandler f) (frHandler g),
        frId f == frId g || (frRefs f == frRefs g && sameK (frOuter f) (frOuter g)) =
          Just (Just (f, g))
    pairFrame _ _ = Nothing
    sameFn x y = case (x, y) of
      (Clo f as, Clo g bs) -> f == g && length as == length bs
      _ -> sameAbs x y
    dynamicWith kk finish = toVar (look e v) \x -> do
      emitted
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
      vs' <- traverse freshNamed vs
      let fields = zipWith Dyn vs' ms
          e' = foldr (uncurry bind) (bind v (Con r t fields) under) (zip vs fields)
      body' <- arm e' body
      pure (t, (ms, ABTN.TAbss vs' body'))
    -- the pure case of a handler binds the value
    pureArm arm = \case
      ABTN.TAbs u body -> do
        u' <- freshNamed u
        ABTN.TAbs u' <$> arm (bind u (Dyn u' BX) under) body
      body -> arm under body
    requestArm arm (t, (ms, ABTN.TAbss vs body)) = do
      vs' <- traverse freshNamed vs
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

-- | Counts a let or a match generated. Past the budget ('slOutput'), a
-- fused handler in force is given up on (it copies the rest of the walk
-- into each arm of an unknown match, which is what grows the code), its
-- handler kept and the count rolled back with the rest of the state; with
-- none in force, the specialization is ('specializeLambda' then tries once
-- more with no handler fused).
emitted :: M ()
emitted = do
  n <- gets sEmitted
  lim <- gets (slOutput . sLim)
  when (n >= lim) (guardFused tooBig >> giveUp tooBig)
  modify' \s -> s {sEmitted = n + 1}

jumpCont :: Reference
jumpCont = Builtin "jumpCont"

isFused :: Frame -> Bool
isFused Fused {} = True
isFused Barrier = False

-- | Escapes to the innermost fused handler, if there is one: the code
-- about to be produced could perform its requests unseen.
guardFused :: String -> M ()
guardFused why =
  gets (find isFused . sFrames) >>= \case
    Just f -> liftIO (throwIO (Escape (frId f) why))
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
    Just (Left fid) -> liftIO (throwIO (Escape fid "a request under a kept handler"))
    Just (Right j) -> do
      let f = frames !! j
      rest <- case k of
        KBind _ _ g -> pure g
        KTail -> liftIO (throwIO (Escape (frId f) "a request with the end of a residual function as its continuation"))
        _ -> pure (ret k)
      targets <- gets sTargets
      kid <- gets sNextId
      let above = length targets - frDepth f - (if frSegment f then 0 else 1)
          own = if frSegment f then 0 else 1
      modify' \st ->
        st
          { sFrames = drop (j + 1) frames,
            sTargets = drop (above + own) targets,
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
  _ -> guardFused "a handler that isn't a known function" >> giveUp "a fused handler that isn't a known function"

-- | A kept handler's body as the compiler makes one: a call. A
-- continuation captured under the handler holds the stack down to the
-- @handle@ and no further, and it may be resumed with more or less stack
-- below it, so the code it runs must not use a variable of the frame the
-- @handle@ is in: those are found by their distance down the stack. So the
-- body becomes a residual function of the variables it uses from outside,
-- and what is under the @handle@ is a call of it. A body that only builds
-- values and then makes one call stays as it is: it has read its variables
-- before anything can capture.
handledBody :: E -> Term -> (Term -> Term) -> M Term
handledBody e body handle
  | straight body = pure (handle body)
  | otherwise = do
    let mems = M.fromList [(x, m) | a <- M.elems (eVars e), Dyn x m <- dynLeaves a]
        outside = [(x, m) | x <- S.toList (ABTN.freeVars body), Just m <- [M.lookup x mems]]
    f <- freshNamed (syn "handled")
    case outside of
      -- a function takes at least one argument
      [] -> do
        u <- freshNamed (syn "nil")
        u' <- freshNamed (syn "nil")
        modify' \st -> st {sFns = (f, Lambda [BX] (ABTN.TAbss [u'] body)) : sFns st}
        pure (TLets Direct [u] [BX] (TCon unitRef (CTag 0) []) (handle (TApp (FVar f) [u])))
      _ -> do
        modify' \st -> st {sFns = (f, Lambda (map snd outside) (ABTN.TAbss (map fst outside) body)) : sFns st}
        pure (handle (TApp (FVar f) (map fst outside)))
  where
    straight = \case
      TApp {} -> True
      TVar {} -> True
      TLets Direct _ _ bn bo -> value bn && straight bo
      TName _ _ _ bo -> straight bo
      _ -> False
    value = \case
      TLit {} -> True
      TBLit {} -> True
      TCon {} -> True
      _ -> False

-- | Resuming a continuation captured while specializing: the walk goes on
-- from the request, with the value as its result, the frames and targets
-- between it and its handler put back, and the resumption site as where
-- the body returns. A second resumption escapes, for now.
resume :: Int -> Abs -> K -> M Term
resume kid v k = do
  gets (M.lookup kid . sKonts) >>= \case
    Nothing -> giveUp "an unknown continuation"
    Just ko
      | koUsed ko -> liftIO (throwIO (Escape (koFrame ko) "a continuation resumed twice"))
      | otherwise -> do
          modify' \st ->
            st
              { sKonts = M.insert kid ko {koUsed = True} (sKonts st),
                sFrames = koFrames ko ++ sFrames st,
                sTargets = koTargets ko ++ ((tgtOf k, ret k) : sTargets st)
              }
          koRest ko v

tgtOf :: K -> Tgt
tgtOf (KDelim fid) = TBody fid
tgtOf _ = TSite

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
      | slFuse lim, fusable (look e h) -> fused
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
      code <- handledBody e body' (THnd rs h' Nothing)
      dynResult k BX code
    fused = do
      fid <- gets sNextId
      modify' \st -> st {sNextId = fid + 1}
      s0 <- get
      let -- what follows the handle, by a name if it has none
          outer = case k of
            KBind m _ _ -> KOuter fid m
            _ -> k
          attempt = do
            d <- gets (length . sTargets)
            modify' \st ->
              st
                { sFrames = Fused fid rs (look e h) outer d False : sFrames st,
                  sTargets = (TPure fid, pureCase) : sTargets st,
                  sOuters = M.insert fid k (sOuters st)
                }
            pe env lim e body (KDelim fid)
          -- with the handler as it is in its frame by then: a loop that
          -- passed the state back has put a new one there ('resultMatch')
          pureCase a = do
            now <- gets (find (\f -> isFused f && frId f == fid) . sFrames)
            modify' \st -> st {sFrames = dropFused fid (sFrames st)}
            callHandler env lim e (maybe (look e h) frHandler now) (SPure a) outer
      liftIO (Control.Exception.try (runStateT attempt s0)) >>= \case
        Right (code, s1) -> put s1 >> pure code
        Left (Escape i why)
          | i == fid -> do
              put s0
              liftIO (seNote env ("a handler kept: " ++ why))
              kept
          | otherwise -> liftIO (throwIO (Escape i why))
    dropFused fid = \case
      (f@Fused {} : fs) | frId f == fid -> fs
      fs -> fs

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
  SReq {} -> guardFused "a request used as a value" >> giveUp "a request as a value"
  SPure {} -> guardFused "a handler's pure case used as a value" >> giveUp "a handler's pure case as a value"
  SKont {} -> guardFused "a continuation used as a value" >> giveUp "a continuation as a value"
  Known _ (Just s) -> k s
  Known v Nothing -> case v of
    NatVal n -> lit "lit" UN (TLit (N n))
    IntVal i -> lit "lit" UN (TLit (I (fromIntegral i)))
    DoubleVal d -> lit "lit" UN (TLit (F d))
    CharVal c -> lit "lit" UN (TLit (C c))
    BoxedVal (TextC t) -> lit "lit" BX (TBLit (T t))
    _ -> do
      s <- freshNamed (syn "c")
      modify' \st -> st {sConsts = (s, v) : sConsts st}
      k s
  Con r t fs -> toVars fs \xs -> lit "con" BX (TCon r t xs)
  -- an entry over unknown values only: a closure of the original code
  Clo (Fn r 0) as | all isDyn as -> toVars as \xs -> name (Left r) xs
  -- otherwise a closure of a residual function specialized on what is
  -- known of the captured values, whose remaining parameters are holes
  Clo fn@(Fn r i) as -> do
    guardFused "a closure of a function specialized on what it captures"
    env <- gets sEnv
    lim <- gets sLim
    case lambdaOf env fn of
      Nothing
        | i == 0 -> toVars as \xs -> name (Left r) xs
        | otherwise -> liftIO . throwIO $ GiveUp "a local function with no code"
      Just (Lambda ms (ABTN.TAbss vs _), _) -> do
        holes <- zipWithM (\v m -> (`Dyn` m) <$> freshNamed v) (drop (length as) vs) (drop (length as) ms)
        (f, leaves) <- residualFn env lim fn (as ++ holes)
        toVars (concat (take (length as) leaves)) \xs -> name (Right f) xs
  where
    lit nm m code = do
      emitted
      x <- freshNamed (syn nm)
      rest <- k x
      pure (TLets Direct [x] [m] code rest)
    name f xs = do
      emitted
      x <- freshNamed (syn "clo")
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
  guardFused "code kept as it is"
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
inlineFns :: S.Set Symbol -> [(Symbol, SuperNormal Reference Symbol)] -> Term -> M ([(Symbol, SuperNormal Reference Symbol)], Term)
inlineFns joins fns body = case find inlinableJoin fns <|> find inlinable fns of
  Nothing -> pure (fns, body)
  Just (f, lam) -> do
    body' <- inlineCalls f lam body
    rest <-
      for [(g, l) | (g, l) <- fns, g /= f] \(g, Lambda ms (ABTN.TAbss ps bd)) ->
        (\bd' -> (g, Lambda ms (ABTN.TAbss ps bd'))) <$> inlineCalls f lam bd
    inlineFns joins rest body'
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
    -- the body of a kept handler stays a call ('handledBody')
    handled f = or [g == f | bd <- bodies, AHnd _ _ _ (TApp (FVar g) _) <- nodes bd]
    -- A join point is shared to keep the code from doubling at every
    -- match, and a call of it costs what a copy doesn't. One with a single
    -- call goes into it. One with more is copied into them if the copies
    -- add little, which bounds what each match adds; of those the
    -- innermost go first, so that one is judged with the join points it
    -- calls already in it: an outer one copied first would double the
    -- calls of the next, and so on down. And join points go
    -- before any other function: a loop's function copied into the join
    -- points it calls gives each of them more calls, and leaves a call
    -- per join point in every round of the loop.
    inlinableJoin (f, Lambda _ (ABTN.TAbss _ bd)) =
      S.member f joins
        && not (closed f || selfCalls f bd || handled f)
        && ( calls f == 1
               || S.null (S.delete f (refsIn (S.intersection joins names) bd))
                 && length (nodes bd) * (calls f - 1) <= joinCopyLimit
           )
    -- how many nodes copying a join point into its callers may add
    joinCopyLimit = 100 :: Int
    inlinable (f, Lambda _ (ABTN.TAbss _ bd))
      | closed f || selfCalls f bd || handled f || S.member f joins = False
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
    v' <- freshNamed v
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

-- | A let whose body is its variable is what it binds: @x = e; x@ is @e@,
-- which leaves a call as an arm's last act where a placeholder followed it.
returnBound :: Term -> Term
returnBound = \case
  TLets _ [x] _ bn (TVar y) | x == y -> returnBound bn
  ABTN.TAbs v b -> ABTN.TAbs v (returnBound b)
  ABTN.TTm x -> ABTN.TTm (fmap returnBound x)

-- | Variables replaced by terms.
substVars :: M.Map Symbol Term -> Term -> Term
substVars m
  | M.null m = id
  | otherwise = go
  where
    go = \case
      TVar v | Just c <- M.lookup v m -> c
      ABTN.TAbs v b -> ABTN.TAbs v (go b)
      ABTN.TTm x -> ABTN.TTm (fmap go x)

-- | The variables a term binds, with how each is held.
binderMems :: Term -> [(Symbol, Mem)]
binderMems = \case
  ABTN.TAbs _ b -> binderMems b
  ABTN.TTm x -> own x ++ foldMap binderMems x
  where
    own = \case
      ALet _ ms _ bo -> zip (absVars bo) ms
      AName _ _ bo -> boxed bo
      AShift _ bo -> boxed bo
      AMatch _ br -> case br of
        MatchData _ cs _ -> concat [zip (absVars b) ms | (ms, b) <- map snd (EC.mapToList cs)]
        MatchSum cs -> concat [zip (absVars b) ms | (ms, b) <- map snd (EC.mapToList cs)]
        MatchRequest arms df -> concat [zip (absVars b) ms | (_, cs) <- arms, (ms, b) <- map snd (EC.mapToList cs)] ++ boxed df
        MatchRec _ b -> boxed b
        _ -> []
      _ -> []
    boxed b = map (,BX) (absVars b)

absVars :: Term -> [Symbol]
absVars = \case
  ABTN.TAbs v b -> v : absVars b
  _ -> []

-- | The variables each residual function uses from where it is called,
-- which only a join point does ('joined'): the ones free in its body and
-- the ones the functions it calls need, less the ones it binds itself.
-- The globals (the constants) are left to 'constNeeds'.
freeNeeds :: S.Set Symbol -> [(Symbol, SuperNormal Reference Symbol)] -> M.Map Symbol [Symbol]
freeNeeds globals fns = M.map S.toList (fixed direct)
  where
    names = S.fromList (map fst fns)
    direct = M.fromList [(f, ABTN.freeVars t S.\\ (globals <> names)) | (f, Lambda _ t) <- fns]
    bound = M.fromList [(f, S.fromList (absVars t ++ map fst (binderMems t))) | (f, Lambda _ t) <- fns]
    callees = M.fromList [(f, refsIn names t) | (f, Lambda _ t) <- fns]
    fixed m
      | m' == m = m
      | otherwise = fixed m'
      where
        m' =
          M.mapWithKey
            ( \f used ->
                (used <> foldMap (\g -> M.findWithDefault S.empty g m) (M.findWithDefault S.empty f callees))
                  S.\\ M.findWithDefault S.empty f bound
            )
            m

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
