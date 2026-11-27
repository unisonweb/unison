{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | The partial evaluator behind @specialize@
-- (unison-runtime/src/Unison/Runtime/ANF/Specialize/design.md, "How it works").
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
import Data.Graph qualified as Graph
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.IntMap.Strict qualified as IM
import Data.List (find, mapAccumL)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import System.IO.Unsafe (unsafePerformIO)
import Unison.ABT.Normalized qualified as ABTN
import Unison.Prelude
import Unison.Reference (Reference, Reference' (Builtin))
import Unison.Runtime.ANF
import Unison.Runtime.Builtin.Decls (eitherLeftId, eitherRef, eitherRightId, exceptionRef, pairRef, unitRef)
import Unison.Runtime.Foreign.Function.Type (ForeignFunc)
import Unison.Runtime.MCode (CombIx (..))
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
import Unison.Runtime.Stack qualified as Stack
import Unison.Runtime.TypeTags (CTag (..))
import Unison.Symbol (Symbol)
import Unison.Type qualified as Ty
import Unison.Util.EnumContainers qualified as EC
import Unison.Util.Text qualified as Util.Text
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
    slJoin :: !Bool,
    -- | whether a call of an unknown function under a fused handler is
    -- made under a bridge ('bridgeCall') or keeps the handler; off only to
    -- compare (@UNISON_SPECIALIZE_BRIDGE=off@)
    slBridge :: !Bool
  }

defaultSpecLimits :: SpecLimits
defaultSpecLimits = SpecLimits {slSteps = 200000, slFunctions = 500, slDepth = 200, slOutput = 2000, slFuse = True, slJoin = True, slBridge = True}

type Term = ANormal Reference Symbol

-- | A function: a group and the index of one of its lambdas (0 for the
-- entry, @k * 2^16@ for the k-th local function).
data Fn = Fn !Reference !Word64
  deriving (Ord, Show)

-- | The index first: the functions compared are mostly of one group, and
-- equal references are compared hash against hash.
instance Eq Fn where
  Fn r i == Fn r' i' = i == i' && r == r'

-- | What is known about a variable while specializing.
data Abs
  = -- | known entirely; the variable holding it in the output, if there is
    -- one already (an argument of the specialized closure)
    Known Val (Maybe Symbol)
  | -- | a known constructor with partially known fields, and whether
    -- every one is fully known ('fullyKnown'); made and matched as 'Con'
    Con_ Bool Reference CTag [Abs]
  | -- | a known function partially applied to partially known arguments,
    -- and whether every one is fully known; made and matched as 'Clo'
    Clo_ Bool Fn [Abs]
  | -- | unknown: a variable of the output, and how it is held
    Dyn Symbol Mem
  | -- | a request handled while specializing (a fused handler's argument):
    -- the ability, the operation, its arguments and its continuation
    SReq Reference CTag [Abs] Int
  | -- | the value a fused handler's body finished with, given to the handler
    SPure Abs
  | -- | the continuation of a request handled while specializing
    SKont Int
  | -- | the request a bridge's handler is given, unknown: its variable, and
    -- the number of the bridge's frames ('bridgeFn')
    SBridge Symbol Int

pattern Con :: Reference -> CTag -> [Abs] -> Abs
pattern Con r t fields <- Con_ _ r t fields
  where
    Con r t fields = Con_ (all fullyKnown fields) r t fields

pattern Clo :: Fn -> [Abs] -> Abs
pattern Clo f args <- Clo_ _ f args
  where
    Clo f args = Clo_ (all fullyKnown args) f args

{-# COMPLETE Known, Con, Clo, Dyn, SReq, SPure, SKont, SBridge #-}

-- | Whether nothing in the value is unknown, in constant time: a
-- constructor or a closure records it when it is made. A value that isn't
-- fully known can't be built as a value of the interpreter ('knownVal').
fullyKnown :: Abs -> Bool
fullyKnown = \case
  Known {} -> True
  Con_ known _ _ _ -> known
  Clo_ known _ _ -> known
  _ -> False

-- | The key of a residual function: the function, and what is known of
-- its arguments, with each unknown one a hole.
-- | With how the function is called: 0 as it is, 1 passing handlers'
-- states, 2 returning a @handle@'s value ('residualFnWith').
--
-- And with the abilities of the fused handlers there are outside a kept
-- one, for a function made inside the kept one: its body was walked with a
-- request to any of them escaping, so it makes none, and it can be shared
-- only where the same holds. A function made with no fused handler around
-- may request anything.
data Key = Key Int [Reference] Fn [KArg]
  deriving (Eq, Ord)

-- | The abilities of the fused handlers in force, for the key of a
-- function called as it is.
fusedRefs :: [Frame] -> [Reference]
fusedRefs frames = S.toList (S.fromList (concat [frRefs f | f <- frames, isFused f]))

data KArg
  = KKnown Val
  | KCon Reference CTag [KArg]
  | KClo Fn [KArg]
  | KHole Mem
  | -- | a continuation captured while specializing: code specialized on
    -- one resumes that one
    KKont Int

-- | Known values are compared by address before they are compared by
-- structure: the runtime's equality has no such shortcut, and nearly every
-- pair of equal known values met here is one value, a table of a program
-- or an interpreter's code, reached twice.
instance Eq KArg where
  a == b = case (a, b) of
    (KKnown v, KKnown w) -> sameBoxed v w || v == w
    (KCon r t xs, KCon r' t' ys) -> r == r' && t == t' && xs == ys
    (KClo f xs, KClo g ys) -> f == g && xs == ys
    (KHole m, KHole n) -> m == n
    (KKont i, KKont j) -> i == j
    _ -> False

instance Ord KArg where
  compare a b = case (a, b) of
    (KKnown v, KKnown w) -> if sameBoxed v w then EQ else compare v w
    (KCon r t xs, KCon r' t' ys) -> compare r r' <> compare t t' <> compare xs ys
    (KClo f xs, KClo g ys) -> compare f g <> compare xs ys
    (KHole m, KHole n) -> compare m n
    (KKont i, KKont j) -> compare i j
    _ -> compare (rank a) (rank b)
    where
      rank :: KArg -> Int
      rank = \case
        KKnown {} -> 0
        KCon {} -> 1
        KClo {} -> 2
        KHole {} -> 3
        KKont {} -> 4

-- | Whether two values are one boxed value, by address. False for some
-- that are (one not yet evaluated), never true for two that differ.
sameBoxed :: Val -> Val -> Bool
sameBoxed v w = case (v, w) of
  (BoxedVal !x, BoxedVal !y) -> isTrue# (reallyUnsafePtrEquality# x y)
  _ -> False

data GiveUp = GiveUp String
  deriving (Show)

instance Exception GiveUp

-- | The call being unfolded with this number is the start of a loop: a call
-- inside it, of the same function with the same key, is about to become a
-- call of a residual function. The unfolding is abandoned and the call is
-- walked again as a call of that residual function, so that the loop's body
-- is generated once, not once inline and once as the function. Carries the
-- steps taken, which a rollback doesn't give back.
data Redo = Redo Int Int
  deriving (Show)

instance Exception Redo

-- | Something in the scope of the fused handler with this number could
-- perform its requests without the walk seeing them: the handler can't be
-- fused, and its @handle@ is walked again, kept.
data Escape = Escape Int String
  deriving (Show)

instance Exception Escape

-- | The continuation with this number, of a request handled while
-- specializing, is needed as more than the rest of the walk: resumed a
-- second time, or used in another function. The walk goes back to the
-- request and makes it a residual function ('request').
newtype Reify = Reify Int
  deriving (Show)

instance Exception Reify

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

-- | How a continuation made a function deals with the fused handlers
-- outside the one that handled its request ('restAsFunction').
data Outside
  = -- | there are none
    NoneOutside
  | -- | it is passed their states and gives them back with its value, as a
    -- state-passing function does: the handlers with holes for the states
    PassOutside [Abs]
  | -- | it runs on through them to the end of a value-returning function:
    -- the handlers with holes, their frames' numbers and where each
    -- returns, and where the handler of the request returned
    ThroughOutside [Abs] [(Int, K)] K

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
    sReturns :: M.Map Int [Frame],
    -- | the continuation parameter and the frames at the start, of each
    -- value-returning residual function being built ('KEnd')
    sEnds :: M.Map Int (Symbol, [Frame]),
    -- | the lets and matches generated so far ('emitted')
    sEmitted :: !Int,
    -- | per join point being made, where arms reached the code after the
    -- match: the placeholder left there, the value, and the continuations
    -- resumed by then ('joined')
    sJoins :: M.Map Int [(Symbol, Abs, [Int], [Abs])],
    -- | what follows each fused @handle@, by the number of its first frame
    -- ('KOuter')
    sOuters :: M.Map Int K,
    -- | the continuations made residual functions ('request'): the
    -- function, its handler with holes where its parameters go, the
    -- abilities, and the frame that handled the request
    -- and what it does about the handlers outside that one
    sReified :: M.Map Int (Symbol, Abs, [Reference], Int, Outside),
    -- | the residual functions that are join points
    sJoinFns :: S.Set Symbol,
    -- | the functions whose loops under handlers of these abilities
    -- couldn't pass the handlers' states and return a @handle@'s value
    -- instead ('valueCall'), with how many of the handlers, from the
    -- innermost, that is the value of: done that way from then on. Outside the state
    -- that an escape rolls back, so that what one attempt learned isn't
    -- learned again by each attempt around it
    sValued :: IORef (M.Map (Fn, [[Reference]]) Int),
    -- | the continuations of the requests a bridge's handler is given
    -- ('bridgeFn'): the number of the bridge's frames, and the variable
    -- the real continuation is in once the case's shift is reached
    sBridgeK :: M.Map Int (Int, Maybe Symbol),
    -- | the bridge whose body has the frames with this number, and whether
    -- all of its handler's state is unknown in it ('bridgeFn')
    sBridgeOf :: M.Map Int (Symbol, Bool),
    -- | the unfoldings the walk is inside of, by number, and the next
    -- number ('Redo')
    sUnfolding :: S.Set Int,
    sNextUnfold :: !Int,
    -- | the calls found to start a loop ('Redo'): residual from the start
    -- when met again. Outside the state that a rollback restores, so that
    -- a loop inside another is found once, not once per attempt around it
    sLoops :: IORef (M.Map Fn [([KArg], [KArg])])
  }

type M = StateT S IO

data E = E
  { eVars :: M.Map Symbol Abs,
    -- | the variables of 'eVars' that aren't 'fullyKnown': a term with one
    -- of them free can't be run ('runIfKnown')
    eUnknown :: S.Set Symbol,
    -- | whether an unknown match has been residualized above, in the
    -- residual function being built
    eUnderDyn :: !Bool,
    eDepth :: !Int,
    -- | the calls being unfolded around this code, innermost first, each
    -- with the key of its arguments: what the whistle looks at before
    -- unfolding another under an unknown match ('call'), and for 'Redo'
    -- the fused handlers it was under and its number
    eStack :: [(Fn, [KArg], [KArg], Int)]
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
  | -- | the value of a case that didn't resume, in that function, of
    -- the handler it was passed at this place (0 the innermost): @Left@ of
    -- which handler and the value ('abortValue')
    KAbort Int Int
  | -- | the end of the body of the value-returning residual function with
    -- this number: its continuation parameter called with the value and
    -- the handler's state ('valueCall')
    KEnd Int
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
    Left why
      | why == tooBig,
        slFuse lim -> do
          seNote env "too much code with handlers fused: once more with every handler kept"
          specializeLambda1 env lim {slFuse = False} r i args
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
          fmap (either (\(Reify _) -> Left "a continuation needed as a function, with no request to go back to") id) . Control.Exception.try @Reify $
            either (\(Escape _ why) -> Left ("an escape from a fused handler: " ++ why)) id
              <$> Control.Exception.try @Escape
                ( Right <$> do
                    newIORef M.empty >>= \loopsRef -> newIORef M.empty >>= \valuedRef -> flip evalStateT (S 0 1 0 M.empty [] [] env lim [] [] [] M.empty 0 Nothing M.empty M.empty 0 M.empty M.empty M.empty S.empty valuedRef M.empty M.empty S.empty 1 loopsRef) do
                      argSyms <- traverse freshNamed (take (length args) vs)
                      restSyms <- traverse freshNamed (drop (length args) vs)
                      let restMems = drop (length args) ccs
                          binds =
                            zip vs (zipWith (\v s -> Known v (Just s)) args argSyms)
                              ++ zip (drop (length args) vs) (zipWith Dyn restSyms restMems)
                          -- the entry is the first call being unfolded
                          -- (its number is 0)
                          e0 = newEnv (localBinds r locals ++ binds) False 0 [(Fn r i, map (keyOf . snd) binds, [], 0)]
                      body0 <-
                        unfolding 0 (partialEval env lim e0 body KTail) $
                          residualCall env lim (Fn r i) (map snd binds) KTail
                      consts <- gets (reverse . sConsts)
                      fns0 <- gets sFns
                      let constSyms = argSyms ++ map fst consts
                          -- a join point uses variables of where it is called: they
                          -- become parameters, before inlining: 'freeNeeds' takes
                          -- every name to be bound once, and two copies of a function
                          -- bind the same names
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
                      let (fns, body') = inlineFns joins fns1 (threadConsts free body0)
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
                      pure (Rec fns' entry, map snd consts)
                )

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
bind v a e =
  e
    { eVars = M.insert v a (eVars e),
      eUnknown = (if fullyKnown a then S.delete else S.insert) v (eUnknown e)
    }

-- | The environment of a function's body: its variables, whether an
-- unknown match is above, the depth, and the calls being unfolded.
newEnv :: [(Symbol, Abs)] -> Bool -> Int -> [(Fn, [KArg], [KArg], Int)] -> E
newEnv binds = E vars (M.keysSet (M.filter (not . fullyKnown) vars))
  where
    vars = M.fromList binds

ret :: K -> Abs -> M Term
ret KTail a = toVar a (pure . TVar)
ret (KBind _ _ k) a = k a
ret (KDelim _) a = popTarget >>= \(_, tgt) -> tgt a
ret (KReturn fid) a = returnRight fid a
ret (KAbort rid i) v = do
  valued <- gets (M.member rid . sEnds)
  when valued . liftIO . throwIO $
    Escape (rid + i) "a case that doesn't resume, of a handler inside another, in a loop returning the outer handle's value"
  n <- gets (maybe 1 (length . takeWhile isFused) . M.lookup rid . sReturns)
  toVar v (abortValue n i)
ret (KEnd lid) a = do
  now <- get
  case M.lookup lid (sEnds now) >>= \(kp, entry) -> (kp,) <$> endsAs now entry of
    Just (kp, (vals, outer, behind, targets)) -> do
      put now {sFrames = behind, sTargets = targets}
      toVar a \x -> toVars vals \ls -> dynResult outer BX (TApp (FVar kp) (x : ls))
    Nothing -> liftIO (throwIO (Escape lid "a loop that ends with a handler not as it was passed"))
ret (KOuter fid _) a =
  gets (M.lookup fid . sOuters) >>= \case
    Just k -> ret k a
    Nothing -> giveUp "the continuation of an unknown handler"

-- | Whether the body of a value-returning residual function, given the
-- frames it started with, can end in this state by calling its
-- continuation parameter: the handlers in force are its own, or ones that
-- cases installed in their place around a resumption, of the same
-- functions. The continuation runs the rest of the caller's body under
-- handlers with the states it is passed, through their pure cases, which
-- is what would follow here. Gives the states to pass, where the outermost
-- of those handles returns (the function's own end, or the case that
-- installed the handler and goes on after it), and the frames and targets
-- left once those handles are over.
endsAs :: S -> [Frame] -> Maybe ([Abs], K, [Frame], [(Tgt, Abs -> M Term)])
endsAs now entry = do
  let segs = takeWhile isFused entry
      n = length segs
      (cur, behind) = splitAt n (sFrames now)
  guard (n > 0 && length cur == n && all isFused cur && length behind == length entry - n)
  guard (not (any isFused (take 1 behind)))
  guard (and (zipWith (\f g -> frRefs f == frRefs g) segs cur))
  -- a handler inside the outermost must return where the function's own
  -- did: a case that installed it and goes on after it has work waiting
  -- inside the outer handle, which the continuation knows nothing of
  guard (and (zipWith (\f g -> frId f == frId g || sameK (frOuter f) (frOuter g)) (init segs) (init cur)))
  vals <- concat <$> zipWithM alignTo (map frHandler segs) (map (opened (sEnv now) . frHandler) cur)
  let outermost = last cur
      own = if frSegment outermost then 0 else 1
      above = length (sTargets now) - frDepth outermost - own
      -- what is between here and the outermost handle's end: the ends of
      -- resumed bodies, which pass their value on, and the pure cases of
      -- these handlers, nothing else
      ids = map frId cur
      between = \case
        TBody _ -> True
        TPure i -> i `elem` ids
        TSite -> False
  guard (above >= 0 && all (between . fst) (take (above + own) (sTargets now)))
  pure (vals, frOuter outermost, behind, drop (above + own) (sTargets now))

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

partialEval :: SpecEnv -> SpecLimits -> E -> Term -> K -> M Term
partialEval env lim e t k =
  tick lim >> case t of
    TVar v -> ret k (look e v)
    TLit l -> literal l
    TBLit l -> literal l
    TLets _ [v] [m] bn bo ->
      partialEval env lim e bn . KBind m v $ \a -> partialEval env lim (bind v a e) bo k
    TLets {} -> opaque env e t k
    TName v f as bo -> do
      let fa = case f of
            Left r -> Clo (Fn r 0) []
            Right lv -> look e lv
      a <- partial env fa (map (look e) as)
      case a of
        Just a' -> partialEval env lim (bind v a' e) bo k
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
          gets (M.lookup kid . sBridgeK) >>= \case
            -- a bridge's case: the shift is real, and the continuation it
            -- binds is the rest of the code the bridge is around
            Just (rid, _) -> do
              v' <- freshNamed v
              modify' \st -> st {sBridgeK = M.insert kid (rid, Just v') (sBridgeK st)}
              TShift r v' <$> partialEval env lim (bind v (SKont kid) e) body k
            Nothing -> partialEval env lim (bind v (SKont kid) e) body k
        _ -> do
          v' <- freshNamed v
          body' <- withBarrier (partialEval env lim (bind v (Dyn v' BX) e) body KTail)
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
  _ -> runOr (fa : as) do
    fs <- gets (takeWhile isFused . sFrames)
    case fs of
      -- under one fused handler: the call under a bridge, which hands its
      -- requests to the handler
      [_] | slBridge lim -> toVarApp fa as \g xs -> bridgeCall env lim (TApp (FVar g) xs) k
      _ -> guardFused ("a call of an unknown function" ++ (case fa of Dyn x _ -> ", " ++ show (Var.name x); _ -> "")) >> toVarApp fa as \g xs -> dynResult k BX (TApp (FVar g) xs)
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
      SBridge {} -> "a request"
      Known {} -> "a known value"
      Con {} -> "a constructor"
      Clo {} -> "a closure"
      Dyn {} -> "an unknown value"
    -- An element of a known list at an unknown index (@List.at@), when
    -- the list is short and holds code or data: a match on the index, each
    -- arm knowing its element, so that what follows is specialized on it.
    -- A handler that finds its case in a list by the operation's number
    -- has each case specialized this way.
    atKnown fn@(Fn r _) args orElse = case args of
      [Dyn x m, lst]
        | r == Builtin "List.at" ->
            allKnown env [Clo fn [], lst] >>= \case
              Just [fv, lv] -> do
                let elemsFrom j acc
                      | j > (8 :: Word64) = pure Nothing
                      | otherwise =
                          liftIO (seRun env fv [NatVal j, lv]) >>= \case
                            Just v
                              | Just (_, []) <- knownData v -> pure (Just (reverse acc, v))
                              | otherwise -> elemsFrom (j + 1) (v : acc)
                            Nothing -> pure Nothing
                    structured v = case knownData v of
                      Just (_, fs) -> any (\f -> isJust (knownData f) || isJust (knownClosure env f)) fs
                      Nothing -> False
                elemsFrom 0 [] >>= \case
                  Just (elems, none) | any structured elems -> do
                    ys <- traverse (const (freshNamed (syn "at"))) elems
                    yn <- freshNamed (syn "at")
                    ix <- freshNamed (syn "i")
                    let arms = EC.mapFromList (zip [0 ..] (map TVar ys))
                        br
                          | m == UN = MatchIntegral arms (Just (TVar yn))
                          | otherwise = MatchNumeric Ty.natRef arms (Just (TVar yn))
                        binds = (ix, Dyn x m) : (yn, Known none Nothing) : zip ys [Known v Nothing | v <- elems]
                    match env lim (foldr (uncurry bind) e binds) ix br k
                  _ -> orElse
              _ -> orElse
      _ -> orElse
    callFn fn@(Fn r i) args = case lambdaOf env fn of
      -- a builtin, or code not loaded here: a call, unless it can run
      Nothing
        | i == 0 ->
            runOr (Clo fn [] : args) . atKnown fn args $ do
              when (seCallsBack env r) (guardFused ("a builtin that calls back into Unison code, " ++ show r ++ " on " ++ unwords (map kind args)))
              toVars args \xs -> dynResult k BX (TApp (FComb r) xs)
        | otherwise -> liftIO . throwIO $ GiveUp "a local function with no code"
      Just (lam@(Lambda _ (ABTN.TAbss vs body)), locals)
        | length args < lamArity lam -> ret k (Clo fn args)
        | length args > lamArity lam ->
            call env lim e (Clo fn []) (take (lamArity lam) args) . KBind BX (syn "r") $ \res ->
              call env lim e res (drop (lamArity lam) args) k
        | otherwise -> runOr (Clo fn [] : args) $ do
            -- With no unknown match above, the call is unfolded: whether
            -- that ends is up to what is known, as it is when the program
            -- runs. Under one it is unfolded too, so that what it gives is
            -- known to what follows (a closure it builds, a constructor),
            -- unless that could go on forever: the same function is being
            -- unfolded or built around this call with arguments no bigger
            -- (the whistle, 'embeds'), or it has a residual function for
            -- these arguments already. Then it is a call of a residual
            -- function, whose result is unknown. An interpreter going into
            -- a part of its program unfolds; coming back round to the same
            -- code, it makes the residual function that is the loop.
            building <- gets sBuilding
            memo <- gets sMemo
            behindRefs <- gets (fusedRefs . sFrames)
            let k0 = map keyOf args
                -- a state-passing function's key has its handler after.
                -- An argument unknown now that was known then counts too:
                -- this call is the general one, the loop
                grown (fn', ks) = fn' == fn && length ks >= length k0 && and (zipWith noSmaller ks k0)
                noSmaller a b = case b of
                  KHole _ -> True
                  _ -> embeds a b
                again =
                  any grown [(f, ks) | (f, ks, _, _) <- eStack e]
                    || any grown building
                    || M.member (Key 0 behindRefs fn k0) memo
                e' hk uid = newEnv (localBinds r locals ++ zip vs args) (eUnderDyn e) (eDepth e + 1) ((fn, k0, hk, uid) : eStack e)
                -- the same call again: the same key, under fused handlers
                -- in the same states (a loop whose first round has a
                -- handler's state known and the rest don't is not the
                -- same call, and its first round stays unfolded)
                sameKey hk ks hs = same ks k0 && same hs hk
                same xs ys = length xs == length ys && and (zipWith safeEq xs ys)
            hk <- gets (map (keyOf . frHandler) . takeWhile isFused . sFrames)
            loops <- gets sLoops >>= liftIO . readIORef
            active <- gets sUnfolding
            if eDepth e < slDepth lim && not (eUnderDyn e && again) && not (any (\(ks, hs) -> sameKey hk ks hs) (M.findWithDefault [] fn loops))
              then do
                uid <- gets sNextUnfold
                modify' \s -> s {sNextUnfold = uid + 1}
                unfolding uid (partialEval env lim (e' hk uid) body k) (residualCall env lim fn args k)
              else case [uid | (f, ks, hs, uid) <- eStack e, f == fn, sameKey hk ks hs, S.member uid active] of
                -- this call closes a loop that an unfolding around it
                -- started: that one becomes the call of the loop
                uid : _ -> do
                  ref <- gets sLoops
                  liftIO (modifyIORef' ref (M.insertWith (++) fn [(k0, hk)]))
                  steps <- gets sSteps
                  liftIO (seNote env ("a loop starts at an unfolding of " ++ maybe (show fn) show (case fn of Fn r' 0 -> seName env r'; _ -> Nothing) ++ ": walked again as a call"))
                  liftIO (throwIO (Redo uid steps))
                [] -> residualCall env lim fn args k

-- | An unfolding with this number, or what to do in its place if a call
-- inside it turns out to close a loop it started ('Redo'): the state goes
-- back to where the unfolding began, but for the steps taken.
unfolding :: Int -> M Term -> M Term -> M Term
unfolding uid walk instead = do
  modify' \s -> s {sUnfolding = S.insert uid (sUnfolding s)}
  s0 <- get
  liftIO (Control.Exception.try (runStateT walk s0)) >>= \case
    Right (t, s1) -> t <$ put s1 {sUnfolding = S.delete uid (sUnfolding s1)}
    Left (Redo i steps)
      | i == uid -> put s0 {sSteps = steps, sUnfolding = S.delete uid (sUnfolding s0)} >> instead
      | otherwise -> liftIO (throwIO (Redo i steps))

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
residualFn env lim fn args = (\(f, _, leaves) -> (f, leaves)) <$> residualFnWith env lim [] fn args

-- | A call that becomes a call of a residual function. In the scope of
-- fused handlers the function is passed their states and gives them back
-- (state passing): it returns @Left@ of which handler and a value when a
-- case of that handler didn't resume, the value being its @handle@'s, or
-- @Right (result, states)@, and the caller goes on accordingly.
residualCall :: SpecEnv -> SpecLimits -> Fn -> [Abs] -> K -> M Term
residualCall env lim fn args k = do
  frames <- gets sFrames
  -- the fused handlers in force, down to a kept one if there is one
  let fs = takeWhile isFused frames
      passing = do
        (rf, tmpls, leaves) <- residualFnWith env lim fs fn args
        inTail <- tailInSame tmpls
        if inTail
          then toVars (concat leaves) \xs -> pure (TApp (FVar rf) xs)
          else toVars (concat leaves) \xs -> do
            r <- freshNamed (syn "r")
            n <- letNum
            rest' <- resultMatch fs tmpls r k
            pure (TLets (Indirect n) [r] [BX] (TApp (FVar rf) xs) rest')
  valuedRef <- gets sValued
  valued <- liftIO (readIORef valuedRef)
  let asValued = (fn, map frRefs fs)
      ids = map frId fs
      -- With the function returning a handle's value: the outermost
      -- one's, or if that doesn't work the value of one further in, the
      -- handlers outside it left alone (a request to one of them from the
      -- loop escapes, 'residualFnAs'). The first escape is the one that
      -- counts if nothing works.
      returning m first
        | m == 0 = liftIO (throwIO first)
        | otherwise = do
            st0 <- get
            liftIO (Control.Exception.try (runStateT (valueCall env lim (take m fs) fn args k) st0)) >>= \case
              Right (code, st1) -> do
                put st1
                liftIO (modifyIORef' valuedRef (M.insert asValued m))
                pure code
              Left (Escape i _) | i `elem` ids -> put st0 >> returning (m - 1) first
              Left other -> liftIO (throwIO other)
  case fs of
    -- passing the handlers' states if that works, or if not (a case
    -- resumes twice, or goes on after resuming) with the function
    -- returning a handle's value
    _ : _
      | Just m <- M.lookup asValued valued -> valueCall env lim (take m fs) fn args k
      | otherwise -> do
          st0 <- get
          liftIO (Control.Exception.try (runStateT passing st0)) >>= \case
            Right (code, st1) -> code <$ put st1
            Left (Escape i why)
              | i `elem` ids -> do
                  put st0
                  liftIO (seNote env ("a loop returning its handle's value: " ++ why))
                  returning (length fs) (Escape i why)
              | otherwise -> liftIO (throwIO (Escape i why))
    [] -> do
      (rf, _, leaves) <- residualFnWith env lim [] fn args
      toVars (concat leaves) \xs -> dynResult k BX (TApp (FVar rf) xs)
  where
    -- a call in tail position of a state-passing function, under the
    -- handlers it was passed or ones standing for them, of the same
    -- shapes: what the callee returns is what this function would, so the
    -- match is the identity and the call a tail call
    tailInSame tmpls = case k of
      KReturn rid -> do
        now <- get
        pure case M.lookup rid (sReturns now) of
          Just entry
            | Just _ <- standFor (entry, []) (sFrames now, map fst (sTargets now)),
              segs <- takeWhile isFused entry,
              length segs == length tmpls ->
                and (zipWith (\seg t -> safeEq (keyOf t) (keyOf (frHandler seg))) segs tmpls)
          _ -> False
      _ -> pure False

-- | A call under a fused handler as a call of a residual function that
-- returns the @handle@'s value: the function is passed the handler's state
-- and the rest of the body after the call, up to the @handle@, as a
-- closure. The walk goes on after the @handle@ with what it returns.
--
-- A call in the last position of such a function, under handlers standing
-- for its own, passes on the continuation it was given: a loop written
-- with a tail call makes no closure.
valueCall :: SpecEnv -> SpecLimits -> [Frame] -> Fn -> [Abs] -> K -> M Term
valueCall env lim fs fn args k = do
  frames <- gets sFrames
  targets <- gets sTargets
  let -- the outermost: the function gives its handle's value
      f = last fs
      own = if frSegment f then 0 else 1
      above = length targets - frDepth f - own
  ends <- gets sEnds
  now <- get
  (lf, tmpls, leaves) <- residualFnAs True env lim fs fn args
  let shape t = [m | Dyn _ m <- dynLeaves t]
  cont <- case k of
    -- the continuation this function was given takes the states the
    -- callee's would
    KEnd lid
      | Just (kp, entry) <- M.lookup lid ends,
        Just _ <- endsAs now entry,
        map shape tmpls == map (shape . frHandler) (takeWhile isFused entry) ->
          pure (Left kp)
    _ -> do
      rest <- case k of
        KBind _ _ g -> pure g
        KTail -> liftIO (throwIO (Escape (frId f) "a call with the end of a residual function as its continuation"))
        _ -> pure (ret k)
      -- its parameters for the states are the ones the function has
      let holes = map dynHoled tmpls
      Right . fst <$> restAsFunction env lim (map Just (init holes)) (last holes) Nothing f rest above (kMem k)
  modify' \st -> st {sFrames = drop (length fs) frames, sTargets = drop (above + own) targets}
  toVars (concat leaves) \xs -> case cont of
    Left kp -> dynResult (frOuter f) BX (TApp (FVar lf) (xs ++ [kp]))
    Right rk -> do
      kx <- freshNamed (syn "k")
      emitted
      TName kx (Right rk) [] <$> dynResult (frOuter f) BX (TApp (FVar lf) (xs ++ [kx]))
  where
    dynHoled = \case
      Dyn _ m -> Dyn joinHole m
      Con r' t' gs -> Con r' t' (map dynHoled gs)
      Clo g as -> Clo g (map dynHoled as)
      a -> a

-- | A value with the closures known as values inside it opened into their
-- functions and what they hold, so that it compares with one built while
-- specializing.
opened :: SpecEnv -> Abs -> Abs
opened env = \case
  Known v _ | Just (g, caps) <- knownClosure env v -> Clo g (map (opened env) caps)
  Con r t fs -> Con r t (map (opened env) fs)
  Clo g as -> Clo g (map (opened env) as)
  a -> a

-- | A handler as a value-returning function takes it ('valueCall'), given
-- the values for its unknown parts: with its numbers unknown too, since a
-- handler's state changes from one round of a loop to the next, and the
-- function's continuation must take whatever state the loop ends with.
-- Gives the values for the unknown parts it has then.
widened :: SpecEnv -> Abs -> [Abs] -> (Abs, [Abs])
widened env a0 = evalState (go a0)
  where
    go = \case
      a@Dyn {} -> state \case
        l : ls -> ((a, [l]), ls)
        [] -> error "widened: fewer values than unknown parts"
      a@(Known v _)
        | memOf a == UN -> pure (Dyn (Var.named "general") UN, [a])
        | Just (g, caps) <- knownClosure env v -> go (Clo g caps)
      Con r t fs -> (\rs -> (Con r t (map fst rs), concatMap snd rs)) <$> traverse go fs
      Clo g as -> (\rs -> (Clo g (map fst rs), concatMap snd rs)) <$> traverse go as
      a -> pure (a, [])

-- | The rest of a body, from a request or a call under the fused handler
-- of a frame, as a residual function of a value (the request's result, the
-- call's) and the handler's state: the body run on from there under the
-- handler with that state, through its pure case, so that it gives the
-- @handle@'s value. Given the handler with holes for its state, and the
-- same for each handler between the place and it that the function is to
-- be passed the state of (innermost first, Nothing for one left as it is),
-- and how many targets are between; gives the function and the handler
-- with its holes. The function's parameters are the value, then the states
-- from the innermost handler out.
--
-- With templates for fused handlers outside it too, the function is
-- passed their states as well, after the others, and runs under them, in
-- one of two ways. Passing: it gives back @Right@ of the handle's value
-- and their states, or @Left@ when a case of one of them didn't resume, as
-- a state-passing function does ('residualCall'), and its caller goes on
-- accordingly. Through, for the handlers of a value-returning residual
-- function ('valueCall'): it runs under all of them to the end of that
-- function's body, where it calls that function's continuation. So it
-- gives the outermost handle's value, and a call of it must be the last
-- thing done before that handle ends ('resumeFunction').
restAsFunction :: SpecEnv -> SpecLimits -> [Maybe Abs] -> Abs -> Maybe (Bool, [Abs]) -> Frame -> (Abs -> M Term) -> Int -> Mem -> M (Symbol, Abs)
restAsFunction env lim innerTmpls tmpl outside f rest above vm = do
  let j = length innerTmpls
      through = maybe False fst outside
      passing = maybe False (not . fst) outside
      outerTmpls = maybe [] snd outside
      holeMems = [m | t <- catMaybes innerTmpls ++ [tmpl] ++ outerTmpls, Dyn h m <- dynLeaves t, h == joinHole]
  v <- freshNamed (syn "r")
  hs <- traverse (\hm -> freshNamed (syn (if hm == UN then "n" else "s"))) holeMems
  -- the state as it is with those names taken: the function's body must
  -- not be given them again
  outer <- get
  let rid = sNextId outer
      (innerFilled, hFilled, outerFilled) =
        evalState
          ((,,) <$> traverse (traverse fillHoles) innerTmpls <*> fillHoles tmpl <*> traverse fillHoles outerTmpls)
          (zipWith Dyn hs holeMems)
      -- the frames between the place and its handler counted their
      -- targets from below the handler's, which aren't in the function;
      -- the handler's own is, as the function's pure case, unless the
      -- function runs on through the handlers outside
      shiftBy
        | through = frDepth f + (if frSegment f then 0 else 1)
        | otherwise = frDepth f - (if frSegment f then 1 else 0)
      rebase = \case
        Fused i rs h o d seg -> Fused i rs h o (d - shiftBy) seg
        g -> g
      setState g mh = case (g, mh) of
        (Fused i rs _ o d seg, Just h) -> Fused i rs h o d seg
        _ -> g
      inner = zipWith setState (map rebase (take j (sFrames outer))) innerFilled
      -- where the handle's value goes: the function's own end, or, giving
      -- the outside handlers' states back, the end of a state-passing
      -- function whose frames are theirs
      pid = rid + 1
      end = if passing then KReturn pid else KTail
      frameR
        | through = Fused rid (frRefs f) hFilled (frOuter f) 0 True
        | otherwise = Fused rid (frRefs f) hFilled end 0 False
      asSegment i g h = case g of
        Fused gi rs _ o _ _
          | passing -> Fused (pid + i) rs h (KAbort pid i) 0 True
          | otherwise -> Fused gi rs h o 0 True
        _ -> g
      outerSegs = zipWith3 asSegment [0 ..] (drop (j + 1) (sFrames outer)) outerFilled
      -- the handlers outside whose states the function is passed, then
      -- the rest behind a barrier: a request for one of those escapes
      behind =
        outerSegs
          ++ case drop (j + 1 + length outerTmpls) (sFrames outer) of
            bs@(g : _) | isFused g -> Barrier : bs
            bs -> bs
      pureR a = do
        now <- gets (find (\g -> isFused g && frId g == rid) . sFrames)
        modify' \st -> st {sFrames = filter (\g -> not (isFused g && frId g == rid)) (sFrames st)}
        callHandler env lim (newEnv [] False 0 []) (maybe hFilled frHandler now) (SPure a) end
  put
    outer
      { sNextId = rid + 1 + (if passing then length outerTmpls else 0),
        sReturns = if passing then M.insert pid behind (sReturns outer) else sReturns outer,
        sLetNum = 1,
        sFrames = inner ++ [frameR] ++ behind,
        sTargets = take above (sTargets outer) ++ [(TPure rid, pureR) | not through],
        sKonts = M.empty,
        sPending = Nothing
      }
  s1 <- get
  body <-
    liftIO (Control.Exception.try (runStateT (rest (Dyn v vm)) s1)) >>= \case
      Right (b, s2) -> b <$ put s2
      Left (Escape i why)
        -- an escape to an outside handler's frame in the function is one
        -- to that handler
        | passing,
          i >= pid,
          g : _ <- drop (i - pid) (takeWhile isFused (drop (j + 1) (sFrames outer))) ->
            liftIO (throwIO (Escape (frId g) ("in the rest of a body made a function, " ++ why)))
        | i >= rid -> liftIO (throwIO (Escape (frId f) ("in the rest of a body made a function, " ++ why)))
        | otherwise -> liftIO (throwIO (Escape i why))
  rsym <- freshNamed (syn "resume")
  modify' \st ->
    st
      { sLetNum = sLetNum outer,
        sFrames = sFrames outer,
        sTargets = sTargets outer,
        sKonts = sKonts outer,
        sPending = sPending outer,
        sFns = (rsym, Lambda (vm : holeMems) (ABTN.TAbss (v : hs) body)) : sFns st
      }
  pure (rsym, tmpl)

-- | What a state-passing function of this many handlers returns when a
-- case of the one at this place didn't resume: @Left@ of the value under
-- as many @Right@s as its place, and under a @Left@ unless it is the
-- last. With one handler that is @Left value@.
abortValue :: Int -> Int -> Symbol -> M Term
abortValue n i x = go (if i == n - 1 then i else i + 1) (i /= n - 1) x
  where
    -- the constructors still to put around the value, the innermost first
    go 0 False v = pure (TCon eitherRef leftTag [v])
    go m isLeft v = do
      v' <- freshNamed (syn "done")
      emitted
      TLets Direct [v'] [BX] (TCon eitherRef (if isLeft then leftTag else rightTag) [v]) <$> go (if isLeft then m - 1 else m - 1) False v'

-- | Whether the handlers in force in the second state stand for the ones
-- in the first, each state given as its frames and what its targets are,
-- and if so their handlers, one per fused frame of the first. A frame
-- stands for one if it is that frame, or handles the same abilities with
-- the same function and returns to the same place ('sameK'): the handler a
-- case installed as its last act. The places their bodies return to must
-- match too. The end of a resumed body (TBody) only passes its value on,
-- so it doesn't count; neither does the pure case of a frame standing for
-- a state-passing function's, which gives its value back without one
-- ('returnRight').
standFor :: ([Frame], [Tgt]) -> ([Frame], [Tgt]) -> Maybe [Abs]
standFor (fa, tga) (fb, tgb) = do
  guard (length fa == length fb)
  pairs <- zipWithM pairFrame fa fb
  let fused = [p | Just p <- pairs]
      ids = [(frId f, frId g) | (f, g) <- fused]
      forSegment = [frId g | (f, g) <- fused, frSegment f, not (frSegment g)]
      counts ts skip = filter keep ts
        where
          keep = \case
            TBody _ -> False
            TPure i -> i `notElem` skip
            TSite -> True
      ta = counts tga []
      tb = counts tgb forSegment
      same x y = case (x, y) of
        (TPure i, TPure j) -> (i, j) `elem` ids
        (TSite, TSite) -> True
        _ -> False
  guard (length ta == length tb && and (zipWith same ta tb))
  pure [frHandler g | (_, g) <- fused]
  where
    pairFrame Barrier Barrier = Just Nothing
    pairFrame f@Fused {} g@Fused {}
      | sameFn (frHandler f) (frHandler g),
        frId f == frId g || (frRefs f == frRefs g && sameK (frOuter f) (frOuter g)) =
          Just (Just (f, g))
    pairFrame _ _ = Nothing
    sameFn x y = case (x, y) of
      (Clo f as, Clo g bs) -> f == g && length as == length bs
      _ -> sameAbs x y

-- | The caller's side of state passing: a match on what the residual
-- function returned. @Left@: a @handle@ is over, with a value; which one
-- is in how the value is wrapped ('abortValue'). @Right (result, states)@:
-- the handlers, rebuilt from the states, stay in force, and the walk goes
-- on with the result.
resultMatch :: [Frame] -> [Abs] -> Symbol -> K -> M Term
resultMatch fs tmpls r k = do
  v <- freshNamed (syn "done")
  aborts <- whichAbort 0 v
  p <- freshNamed (syn "p")
  rightArm <- isolated do
    res <- freshNamed (syn "r")
    let mems = [m | t <- tmpls, Dyn _ m <- dynLeaves t]
    leafVars <- traverse (const (freshNamed (syn "s"))) mems
    let hsNew = evalState (traverse fillLeaves tmpls) (zipWith Dyn leafVars mems)
        byId = M.fromList (zip (map frId fs) hsNew)
        setHandler = \case
          g@Fused {} | Just h <- M.lookup (frId g) byId -> g {frHandler = h}
          g -> g
    modify' \st -> st {sFrames = map setHandler (sFrames st)}
    inner <- ret k (Dyn res (kMem k))
    c0 <- freshNamed (syn "rest")
    chain <- decodeChain c0 (zip leafVars mems) inner
    pure (TMatch p (MatchData pairRef (EC.mapFromList [(CTag 0, ([BX, BX], ABTN.TAbss [res, c0] chain))]) Nothing))
  pure . TMatch r $
    MatchData
      eitherRef
      (EC.mapFromList [(leftTag, ([BX], ABTN.TAbss [v] aborts)), (rightTag, ([BX], ABTN.TAbss [p] rightArm))])
      Nothing
  where
    n = length fs
    -- which handler's case didn't resume, from how the value is wrapped
    whichAbort i v
      | i == n - 1 = abortArm (fs !! i) v
      | otherwise = do
          v' <- freshNamed (syn "done")
          p' <- freshNamed (syn "done")
          here <- abortArm (fs !! i) v'
          further <- whichAbort (i + 1) p'
          pure . TMatch v $
            MatchData
              eitherRef
              (EC.mapFromList [(leftTag, ([BX], ABTN.TAbss [v'] here)), (rightTag, ([BX], ABTN.TAbss [p'] further))])
              Nothing
    -- the handle of this frame is over with this value: on from after it,
    -- with the frames and targets inside it gone
    abortArm f v = isolated do
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
residualFnWith :: SpecEnv -> SpecLimits -> [Frame] -> Fn -> [Abs] -> M (Symbol, [Abs], [[Abs]])
residualFnWith env lim = residualFnAs False env lim

-- | 'residualFnWith', or with the flag the value-returning form for one
-- handler ('valueCall'): the function takes the rest of the body as one
-- more parameter, after the others, and gives the @handle@'s value. A case
-- that doesn't resume returns its value; the end of the body calls the
-- parameter with the body's value and the handler's state ('KEnd').
residualFnAs :: Bool -> SpecEnv -> SpecLimits -> [Frame] -> Fn -> [Abs] -> M (Symbol, [Abs], [[Abs]])
residualFnAs valued env lim fs fn args0 = do
  -- with no fused handler to pass the state of, there may still be some
  -- outside a kept one: the function's body is walked under them as this
  -- call is, so a request to one of them escapes
  behindRefs <- gets (fusedRefs . drop (length fs) . sFrames)
  building <- gets sBuilding
  let allArgs0 = args0 ++ map frHandler fs
      k0 = map keyOf allArgs0
      (allArgs1, leaves1) = case find (\(fn', ks) -> fn' == fn && ks /= k0 && length ks == length k0 && and (zipWith embeds ks k0)) building of
        Just (_, ks) -> Prelude.unzip (zipWith generalize ks allArgs0)
        Nothing -> (allArgs0, map dynLeaves allArgs0)
      (allArgs, leaves)
        | valued =
            let (as, hs) = splitAt (length args0) allArgs1
                (ls, hls) = splitAt (length args0) leaves1
                (hs', hls') = Prelude.unzip (zipWith (widened env) hs hls)
             in (as ++ hs', ls ++ hls')
        | otherwise = (allArgs1, leaves1)
      key
        | null fs = Key 0 behindRefs fn (map keyOf allArgs)
        | otherwise = Key (if valued then 2 else 1) behindRefs fn (map keyOf allArgs)
      tmpls = drop (length args0) allArgs
  gets (M.lookup key . sMemo) >>= \case
    Just f -> pure (f, tmpls, leaves)
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
              e' = newEnv (localBinds r locals ++ zip vs args') False 0 []
          kp <- freshNamed (syn "k")
          outer <- get
          -- A continuation captured while specializing emits its code
          -- where it is resumed, which must be in the scope of its
          -- request, never in another lambda: the function's body knows
          -- of none, and resuming one there makes it a function ('Reify')
          modify' \s -> s {sLetNum = 1, sBuilding = (fn, map keyOf allArgs) : sBuilding s, sKonts = M.empty}
          body' <- case fs of
            [] -> partialEval env lim e' body KTail
            _ -> do
              rid <- gets sNextId
              -- a frame of its own for each handler, which owns no target:
              -- the body returns to the function's caller
              let hAbss = drop (length args0) allArgs'
                  -- the function returns what the outermost handle gives
                  toOuter j = if valued && j == length fs - 1 then KTail else KAbort rid j
                  segs = [Fused (rid + j) (frRefs fr) h (toOuter j) 0 True | (j, fr, h) <- zip3 [0 ..] fs hAbss]
              -- what is behind the fused handlers stays behind the
              -- function's: a request for a handler out there escapes
              -- (a fused handler there that the function isn't passed the
              -- state of is left alone: behind a barrier)
              behind <-
                gets (drop (length fs) . sFrames) <&> \case
                  bs@(g : _) | isFused g -> Barrier : bs
                  bs -> bs
              modify' \s ->
                s
                  { sNextId = rid + length fs,
                    sFrames = segs ++ behind,
                    sTargets = [],
                    sKonts = M.empty,
                    sPending = Nothing,
                    sReturns = if valued then sReturns s else M.insert rid (segs ++ behind) (sReturns s),
                    sEnds = if valued then M.insert rid (kp, segs ++ behind) (sEnds s) else sEnds s
                  }
              s1 <- get
              liftIO (Control.Exception.try (runStateT (partialEval env lim e' body (if valued then KEnd rid else KReturn rid)) s1)) >>= \case
                Right (b, s2) -> b <$ put s2
                -- an escape to a handler of the residual function's own makes
                -- the state passing impossible: an escape to the caller's
                -- handler it stands for, or the innermost
                Left (Escape i why)
                  | i >= rid ->
                      let to = if i < rid + length fs then fs !! (i - rid) else head fs
                       in liftIO (throwIO (Escape (frId to) ("in a loop, " ++ why)))
                  | otherwise -> liftIO (throwIO (Escape i why))
          modify' \s ->
            s
              { sLetNum = sLetNum outer,
                sBuilding = sBuilding outer,
                sFrames = sFrames outer,
                sTargets = sTargets outer,
                sKonts = sKonts outer,
                sPending = sPending outer,
                sFns =
                  ( if valued
                      then (f, Lambda (mems ++ [BX]) (ABTN.TAbss (params ++ [kp]) body'))
                      else (f, Lambda mems (ABTN.TAbss params body'))
                  )
                    : sFns s
              }
          pure (f, tmpls, leaves)
  where
    renameLeaves :: Abs -> State [Symbol] Abs
    renameLeaves = \case
      Dyn _ m -> state \case
        p : ps -> (Dyn p m, ps)
        [] -> error "residualFn: fewer parameters than unknown parts"
      Con r t fs -> Con r t <$> traverse renameLeaves fs
      Clo f as -> Clo f <$> traverse renameLeaves as
      a -> pure a

-- | A call of code that isn't walked, under one fused handler
-- (unison-runtime/src/Unison/Runtime/ANF/Specialize/design.md, "The bridge"): the
-- code makes real requests, and there is no real handler for them. So it
-- is called under one, the bridge, made of the fused handler's cases. The
-- bridge is passed the handler's state and gives it back with the result,
-- or says that a case didn't resume, as a state-passing function does, and
-- the walk goes on accordingly ('resultMatch').
bridgeCall :: SpecEnv -> SpecLimits -> Term -> K -> M Term
bridgeCall env lim code k = do
  fs <- gets (takeWhile isFused . sFrames)
  (hf, tmpls, leaves) <- bridgeFn env lim False fs
  toVars (concat leaves) \xs -> do
    emitted
    h <- freshNamed (syn "bridge")
    r <- freshNamed (syn "r")
    n <- letNum
    rest <- resultMatch fs tmpls r k
    pure (TName h (Right hf) xs (TLets (Indirect n) [r] [BX] (THnd (concatMap frRefs fs) h Nothing code) rest))

-- | The bridge for a fused handler: a residual function that is a real
-- handler, of the handler's state and a request. It is the handler's own
-- function walked on a request that isn't known ('match' on 'SBridge'),
-- under a frame of its own as a state-passing function's body is, so that
-- a case that doesn't resume gives @Left@ and the end of the code under it
-- gives @Right@ of the value and the state. Keyed on what is known of the
-- handler, with the numbers in it unknown: its state changes from one
-- request to the next. Gives the function, the handler as it takes it, and
-- the values a use passes for its unknown parts.
--
-- What is known of the handler's state stays known in the bridge if every
-- case installs the handler with that state as it was. If one doesn't (a
-- state that changes), the bridge is made again with all of the state
-- unknown, which every state the handler is installed with then fits.
bridgeFn :: SpecEnv -> SpecLimits -> Bool -> [Frame] -> M (Symbol, [Abs], [[Abs]])
bridgeFn env lim wide fs = case fs of
  [f]
    | h0@(Clo hfn _) <- opened env (frHandler f) -> do
        behindRefs <- gets (fusedRefs . drop 1 . sFrames)
        let general = \case
              -- boxed or not is not kept: a state that starts as a known
              -- number and is later an operation's argument is one state
              Dyn {} -> state \case
                l : ls' -> ((Dyn (Var.named "general") BX, [l]), ls')
                [] -> error "bridgeFn: fewer values than unknown parts"
              a@(Known v _)
                | Just (g', caps) <- knownClosure env v -> general (Clo g' caps)
                | otherwise -> pure (Dyn (Var.named "general") BX, [a])
              Con r t gs -> (\rs -> (Con r t (map fst rs), concatMap snd rs)) <$> traverse general gs
              Clo g' as -> (\rs -> (Clo g' (map fst rs), concatMap snd rs)) <$> traverse general as
              a -> pure (a, [])
            (h, ls)
              | wide = evalState (general h0) (dynLeaves h0)
              | otherwise = widened env h0 (dynLeaves h0)
            key = Key 4 (frRefs f ++ behindRefs) hfn [keyOf h]
        gets (M.lookup key . sMemo) >>= \case
          Just g -> pure (g, [h], [ls])
          Nothing -> do
            before <- get
            nfns <- gets (M.size . sMemo)
            when (nfns >= slFunctions lim) (giveUp "functions")
            g <- freshNamed (syn "bridge'")
            modify' \s -> s {sMemo = M.insert key g (sMemo s)}
            params <- traverse (\case Dyn x _ -> freshNamed x; _ -> freshNamed (syn "s")) (dynLeaves h)
            req <- freshNamed (syn "request")
            let mems = [m | Dyn _ m <- dynLeaves h]
                h' = evalState (fillLeaves h) (zipWith Dyn params mems)
            outer <- get
            let rid = sNextId outer
                seg = Fused rid (frRefs f) h' (KAbort rid 0) 0 True
                behind = case drop 1 (sFrames outer) of
                  bs@(b : _) | isFused b -> Barrier : bs
                  bs -> bs
            put
              outer
                { sNextId = rid + 1,
                  sLetNum = 1,
                  sFrames = seg : behind,
                  sTargets = [],
                  sKonts = M.empty,
                  sPending = Nothing,
                  sReturns = M.insert rid (seg : behind) (sReturns outer),
                  sBridgeOf = M.insert rid (g, wide) (sBridgeOf outer)
                }
            s1 <- get
            liftIO (Control.Exception.try (runStateT (callHandler env lim (newEnv [] False 0 []) h' (SBridge req rid) (KAbort rid 0)) s1)) >>= \case
              Right (body, s2) -> do
                put s2
                finish outer g mems params req body
                pure (g, [h], [ls])
              Left (Escape i why)
                | i >= rid, not wide, why == otherShape -> put before >> bridgeFn env lim True fs
                | i >= rid -> liftIO (throwIO (Escape (frId f) ("in a bridge, " ++ why)))
                | otherwise -> liftIO (throwIO (Escape i why))
  [f] -> liftIO (throwIO (Escape (frId f) "a bridge for a handler that isn't a known function"))
  _ -> giveUp "a bridge under other than one fused handler"
  where
    finish outer g mems params req body =
      modify' \s ->
        s
          { sLetNum = sLetNum outer,
            sFrames = sFrames outer,
            sTargets = sTargets outer,
            sKonts = sKonts outer,
            sPending = sPending outer,
            -- not counted against the size of the code: the bridge
            -- is the handler's own code once, as the original has it
            sEmitted = sEmitted outer,
            sFns = (g, Lambda (mems ++ [BX]) (ABTN.TAbss (params ++ [req]) body)) : sFns s
          }

-- | The end of the body of a state-passing residual function: @Right@ of
-- its value and the states of the handlers in force, if they stand for the
-- ones the function was passed ('standFor'): every resumption re-installed
-- a handler of the same shape as its case's last act, so that the handler
-- in force returns where the function does (@KAbort@). A case that does
-- more with what the @handle@ gives back (@match handle k () with h with
-- ...@) has work waiting that state passing would drop, so it escapes.
returnRight :: Int -> Abs -> M Term
returnRight rid a = do
  now <- get
  case M.lookup rid (sReturns now) of
    Nothing -> giveUp "the end of an unknown state-passing function"
    Just entry
      | Just hs <- standFor (entry, []) (sFrames now, map fst (sTargets now)),
        Just vals <- concat <$> zipWithM alignTo (map frHandler (takeWhile isFused entry)) hs ->
          toVar a \res -> toVars vals \ls -> do
            (chain, wrap) <- tupleChain ls
            p <- freshNamed (syn "p")
            o <- freshNamed (syn "out")
            pure . wrap $
              TLets Direct [p] [BX] (TCon pairRef (CTag 0) [res, chain]) $
                TLets Direct [o] [BX] (TCon eitherRef rightTag [p]) (TVar o)
      | otherwise ->
          liftIO (throwIO (Escape rid "a loop that ends with a handler not as it was passed, or not installed as a case's last act"))

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
    | r == r',
      t == t',
      length ks == length fs ->
        let (gs, ls) = Prelude.unzip (zipWith generalize ks fs) in (Con r t gs, concat ls)
  (KClo f ks, Clo f' as)
    | f == f',
      length ks == length as ->
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
  (KAbort r i, KAbort r' j) -> r == r' && i == j
  (KOuter i _, KOuter j _) -> i == j
  (KEnd i, KEnd j) -> i == j
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
    | Just (t', vs) <- knownData v,
      t == t',
      length fs == length vs ->
        Con r t <$> zipWithM lubAbs fs [Known x Nothing | x <- vs]
  (Known v _, Con r t gs)
    | Just (t', vs) <- knownData v,
      t == t',
      length gs == length vs ->
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
  SBridge {} -> False
  Con _ _ fs -> all material fs
  Clo _ as -> all material as
  _ -> True

-- | Homeomorphic embedding of keys: the first is the second with parts
-- removed or grown. A literal embeds one at least as big (a natural's value,
-- an integer's magnitude, a text's length), so that a known counter going
-- down never counts as growing.
--
-- It is tried first with no table, for a fixed number of steps
-- ('embedsWithin'), which decides nearly every pair. Past that the answer
-- for a pair of subterms is kept ('embedsTabled'), so each pair is decided
-- once: the time is the product of the two sizes (times a logarithm for
-- the table). Without a table the same pair is reached along many paths,
-- by coupling then diving and by diving then coupling, and the time is
-- exponential in the depth.
embeds :: KArg -> KArg -> Bool
embeds a0 b0 = case embedsWithin embedsSteps a0 b0 of
  Finished r _ -> r
  OutOfSteps -> embedsTabled a0 b0

-- | The steps 'embeds' takes without a table before it starts again with
-- one. Nearly every pair is decided in a few steps: the keys are equal, or
-- differ at the top.
embedsSteps :: Int
embedsSteps = 1000

-- | An answer and the steps left, or none in the steps given.
data Within = Finished !Bool !Int | OutOfSteps

-- | 'embeds' with no table, giving up after the given number of steps: a
-- step is a pair of subterms looked at, and with no table a pair may be
-- looked at many times.
embedsWithin :: Int -> KArg -> KArg -> Within
embedsWithin steps a b
  | steps <= 0 = OutOfSteps
  | otherwise = case couple of
      Finished False left -> case b of
        KCon _ _ ys -> dive left ys
        KClo _ ys -> dive left ys
        _ -> Finished False left
      answer -> answer
  where
    couple = case (a, b) of
      (KHole _, _) -> Finished True (steps - 1)
      (KKont _, KKont _) -> Finished True (steps - 1)
      (KKnown v, KKnown w) -> Finished (knownEmbeds v w) (steps - 1)
      (KCon r t xs, KCon r' t' ys) | r == r', t == t' -> both (steps - 1) xs ys
      (KClo f xs, KClo g ys) | f == g -> both (steps - 1) xs ys
      _ -> Finished False (steps - 1)
    both left = curry \case
      ([], []) -> Finished True left
      (x : xs, y : ys) -> case embedsWithin left x y of
        Finished True left' -> both left' xs ys
        answer -> answer
      _ -> Finished False left
    dive left = \case
      [] -> Finished False left
      y : ys -> case embedsWithin left a y of
        Finished False left' -> dive left' ys
        answer -> answer

-- | 'embeds' with the answer for each pair of subterms kept.
embedsTabled :: KArg -> KArg -> Bool
embedsTabled a0 b0 = evalState (go na nb) IM.empty
  where
    (na, _) = numbered 0 a0
    (nb, width) = numbered 0 b0
    go :: Numbered -> Numbered -> State (IM.IntMap Bool) Bool
    go x@(Numbered i _ _) y@(Numbered j _ _) =
      gets (IM.lookup slot) >>= \case
        Just r -> pure r
        Nothing -> do
          r <- orM (couple x y) (dive x y)
          r <$ modify' (IM.insert slot r)
      where
        slot = i * width + j
    couple (Numbered _ a xs) (Numbered _ b ys) = case (a, b) of
      (KHole _, _) -> pure True
      (KKont _, KKont _) -> pure True
      (KKnown v, KKnown w) -> pure (knownEmbeds v w)
      (KCon r t _, KCon r' t' _) | r == r', t == t', length xs == length ys -> allM (zip xs ys)
      (KClo f _, KClo g _) | f == g, length xs == length ys -> allM (zip xs ys)
      _ -> pure False
    dive x (Numbered _ _ ys) = anyM x ys
    orM p q = p >>= \r -> if r then pure True else q
    allM = \case
      [] -> pure True
      (x, y) : rest -> go x y >>= \r -> if r then allM rest else pure False
    anyM x = \case
      [] -> pure False
      y : rest -> go x y >>= \r -> if r then pure True else anyM x rest

-- | A known value embeds one at least as big, and otherwise only an equal
-- one.
knownEmbeds :: Val -> Val -> Bool
knownEmbeds v w = case (v, w) of
  (NatVal x, NatVal y) -> x <= y
  (IntVal x, IntVal y) -> abs x <= abs y
  (BoxedVal (TextC x), BoxedVal (TextC y)) -> Util.Text.size x <= Util.Text.size y
  (DoubleVal _, DoubleVal _) -> True
  _ -> sameBoxed v w || safeEq (KKnown v) (KKnown w)

-- | Equality of keys that can't fail: comparing known values of different
-- types is an error in the runtime's equality, and counts as different.
safeEq :: KArg -> KArg -> Bool
safeEq x y = unsafePerformIO $ either (\(_ :: SomeException) -> False) id <$> Control.Exception.try (Control.Exception.evaluate (x == y))
{-# NOINLINE safeEq #-}

-- | A key with its subterms numbered from the given number, in order, and
-- the next number free. A known value is one subterm.
data Numbered = Numbered !Int KArg [Numbered]

numbered :: Int -> KArg -> (Numbered, Int)
numbered i k = (Numbered i k kids, next)
  where
    (kids, next) = case k of
      KCon _ _ xs -> children xs
      KClo _ xs -> children xs
      _ -> ([], i + 1)
    children xs = swap (mapAccumL (\n x -> swap (numbered n x)) (i + 1) xs)
    swap (x, y) = (y, x)

keyOf :: Abs -> KArg
keyOf = \case
  SReq {} -> KHole BX
  SPure {} -> KHole BX
  SKont kid -> KKont kid
  SBridge {} -> KHole BX
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
boundary _ _ as k =
  guardFused "specialize.boundary" >> case as of
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
        partialEval env lim (foldr (uncurry bind) e (zip vs args)) body k
    | otherwise -> guardFused "a request its handler has no case for" >> giveUp "a request its handler has no case for"
  -- a fused handler given its body's value: the pure case
  -- (the pure case binds the value, @{ a } -> ...@)
  -- A bridge's handler given its request, which is unknown: a real match
  -- on it. Each case is the fused handler's own, with the handler's frame
  -- gone while it runs as for a request made while specializing, and a
  -- continuation that is the real one ('resumeBridge'). The pure case is
  -- not the handler's: the code under the bridge is over, not the handle,
  -- so it gives the value and the handler's state back, as a state-passing
  -- function does at its end.
  (SBridge req rid, MatchRequest arms _) -> do
    frames <- gets sFrames
    arms' <- for arms \(r, cs) ->
      (r,) . EC.mapFromList <$> for (EC.mapToList cs) \(t, (ms, ABTN.TAbss vs body)) -> isolated do
        vs' <- traverse freshNamed vs
        kid <- gets sNextId
        modify' \st ->
          st
            { sNextId = kid + 1,
              sBridgeK = M.insert kid (rid, Nothing) (sBridgeK st),
              sPending = Just (kid, r),
              sFrames = drop 1 frames
            }
        body' <- partialEval env lim (foldr (uncurry bind) e (zip vs (zipWith Dyn vs' ms))) body k
        pure (t, (ms, ABTN.TAbss vs' body'))
    res <- freshNamed (syn "r")
    pureArm <- isolated (returnRight rid (Dyn res BX))
    pure (TMatch req (MatchRequest arms' (ABTN.TAbs res pureArm)))
  (SPure a, MatchRequest _ df) -> case df of
    ABTN.TAbs u body -> partialEval env lim (bind u a e) body k
    _ -> partialEval env lim (bind v a e) df k
  (Con _ t fs, MatchData _ cs df)
    | Just (_, ABTN.TAbss vs body) <- EC.lookup t cs,
      length vs == length fs ->
        partialEval env lim (foldr (uncurry bind) e (zip vs fs)) body k
    | Nothing <- EC.lookup t cs, Just d <- df -> partialEval env lim e d k
  (Known x _, MatchData _ cs df)
    | Just (t, fs) <- knownData x ->
        case EC.lookup t cs of
          Just (_, ABTN.TAbss vs body)
            | length vs == length fs ->
                partialEval env lim (foldr (uncurry bind) e (zip vs [Known f Nothing | f <- fs])) body k
          Nothing | Just d <- df -> partialEval env lim e d k
          _ -> dynamic
  (Known x _, MatchIntegral cs df) | Just n <- knownIntegral x -> litArm (EC.lookup n cs) df
  (Known x _, MatchNumeric _ cs df) | Just n <- knownIntegral x -> litArm (EC.lookup n cs) df
  (Known (BoxedVal (TextC s)) _, MatchText cs df) -> litArm (M.lookup s cs) df
  (Known (BoxedVal (BytesC b)) _, MatchBytes cs df) -> litArm (M.lookup b cs) df
  _ -> dynamic
  where
    litArm (Just body) _ = partialEval env lim e body k
    litArm Nothing (Just d) = partialEval env lim e d k
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
        KAbort {} | fusedNow -> dynamicWith k (\code -> pure code)
        KEnd _ | fusedNow -> dynamicWith k (\code -> pure code)
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
            case standFor (shape st0) (shape now) of
              -- (only while the arms are being walked: this continuation
              -- can be kept, as what follows a handler an arm installed,
              -- and reached again once the join point is made; then it
              -- is the code after the match, once more)
              Just hs
                | material a,
                  all material hs,
                  M.member jid (sJoins now) -> do
                    ph <- freshNamed (syn "join")
                    modify' \st -> st {sJoins = M.insertWith (++) jid [(ph, a, usedKonts now, hs)] (sJoins st)}
                    pure (TVar ph)
              _ -> ret k a
      put st0 {sNextId = jid + 1, sJoins = M.insert jid [] (sJoins st0)}
      code <- dynamicWith kJoin (\c -> pure c)
      recs <- gets (M.findWithDefault [] jid . sJoins)
      modify' \st -> st {sJoins = M.delete jid (sJoins st)}
      case recs of
        [] -> pure code
        _
          | not fusedNow,
            all (\(_, a, _, _) -> plain a) recs -> do
              -- nothing known in any arm's value: the match bound to a variable
              vals <- for recs \(ph, a, _, _) -> (ph,) <$> toVar a (pure . TVar)
              -- an arm that reached here through an inner join point left its
              -- placeholder in that function
              let patch = substVars (M.fromList vals)
              modify' \st -> st {sFns = [(f, Lambda ms (ABTN.TAbss ps (patch bd))) | (f, Lambda ms (ABTN.TAbss ps bd)) <- sFns st]}
              dynResult k BX (returnBound (patch code))
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
    shape st = (sFrames st, map fst (sTargets st))
    dynamicWith kk finish = toVar (look e v) \x -> do
      emitted
      let arm e' body = isolated (partialEval env lim e' body kk)
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
-- Afterwards the handlers in force are the ones before it, whatever the
-- walk left: a continuation resumed under the kept handler put its frames
-- and its target back, and its body ended as the kept handler's does,
-- without going through them.
withBarrier :: M a -> M a
withBarrier act = do
  before <- get
  put before {sFrames = Barrier : sFrames before}
  r <- act
  modify' \st -> st {sFrames = sFrames before, sTargets = sTargets before}
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
      -- With the rest of the walk as the continuation, if that will do. If
      -- the continuation turns out to be resumed twice, or wanted in
      -- another function ('Reify'), back to here, and with the rest of
      -- the body made a residual function.
      st0 <- get
      let kid = sNextId st0
      liftIO (Control.Exception.try (runStateT (handled j kid False) st0)) >>= \case
        Right (code, st1) -> code <$ put st1
        Left (Reify kid')
          | kid' == kid -> put st0 >> handled j kid True
          | otherwise -> liftIO (throwIO (Reify kid'))
  where
    handled j kid reify = do
      frames <- gets sFrames
      let f = frames !! j
      rest <- case k of
        KBind _ _ g -> pure g
        KTail -> liftIO (throwIO (Escape (frId f) "a request with the end of a residual function as its continuation"))
        _ -> pure (ret k)
      targets <- gets sTargets
      let above = length targets - frDepth f - (if frSegment f then 0 else 1)
          own = if frSegment f then 0 else 1
      modify' \st -> st {sNextId = kid + 1}
      when reify (asFunction j kid f rest above)
      modify' \st ->
        st
          { sFrames = drop (j + 1) frames,
            sTargets = drop (above + own) targets,
            sKonts = M.insert kid (Kont rest (take j frames) (take above targets) (frId f) reify) (sKonts st)
          }
      callHandler env lim e (frHandler f) (SReq r t args kid) (frOuter f)
    -- The rest of the body from the request as a residual function of the
    -- request's result and the handler's state: the body run under the
    -- handler with that state, through its pure case, so that it gives the
    -- handle's value. Resuming is then a call ('resume'), which a case can
    -- make any number of times and go on after.
    asFunction j kid f rest above = do
      now <- get
      let h0 = opened env (frHandler f)
          outer = takeWhile isFused (drop (j + 1) (sFrames now))
          outerTmpls = map (holed . opened env . frHandler) outer
          -- In a value-returning function, under its handlers with nothing
          -- waiting between them, a request to one that isn't the
          -- outermost: the rest runs on through the ones outside
          through = case frOuter f of
            KAbort lid i
              | i == j,
                Just (_, entry) <- M.lookup lid (sEnds now),
                Just (_, KTail, _, _) <- endsAs now entry ->
                  length outer == length (takeWhile isFused entry) - i - 1
            _ -> False
          outside
            | null outer = Nothing
            | otherwise = Just (through, outerTmpls)
      (rsym, tmpl) <- restAsFunction env lim (replicate j Nothing) (holed h0) outside f rest above (kMem k)
      let how
            | null outer = NoneOutside
            | through = ThroughOutside outerTmpls [(frId g, frOuter g) | g <- outer] (frOuter f)
            | otherwise = PassOutside outerTmpls
      modify' \st -> st {sReified = M.insert kid (rsym, tmpl, frRefs f, frId f, how) (sReified st)}
    -- a handler with a hole for each part of its state
    holed = \case
      Dyn _ m -> Dyn joinHole m
      a@Known {} | memOf a == UN -> Dyn joinHole UN
      Con r' t' fs -> Con r' t' (map holed fs)
      Clo fn as -> Clo fn (map holed as)
      a -> a
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
        partialEval env lim (newEnv (localBinds r locals ++ zip vs (caps ++ [arg])) False (eDepth e + 1) (eStack e)) body k
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
resume kid v k =
  gets (M.lookup kid . sBridgeK) >>= \case
    Just (rid, Just kr) -> resumeBridge rid kr v k
    Just (rid, Nothing) -> liftIO (throwIO (Escape rid "a bridge's continuation resumed before its case's shift"))
    Nothing ->
      gets (M.lookup kid . sReified) >>= \case
        Just made -> resumeFunction made v k
        Nothing -> resumeWalk kid v k

otherShape :: String
otherShape = "a bridge's case resuming under a handler of another shape"

escapeTo :: Int -> String -> M a
escapeTo i why = liftIO (throwIO (Escape i why))

-- | Resuming the continuation of a request a bridge's handler was given:
-- the real continuation, under a bridge for the handler the case installs
-- around the resumption. What that gives is what the bridge it is in
-- gives, so the resumption must be the last thing its case does: the
-- handler installed returns where the bridge's own did. A case that goes on
-- after resuming, or resumes twice, escapes, and its handler is kept.
resumeBridge :: Int -> Symbol -> Abs -> K -> M Term
resumeBridge rid kr v k = do
  env <- gets sEnv
  lim <- gets sLim
  frames <- gets sFrames
  targets <- gets sTargets
  entry <- gets (M.lookup rid . sReturns)
  mine <- gets (M.lookup rid . sBridgeOf)
  case (k, frames, targets, entry) of
    (KDelim fid, top@Fused {} : below, (TPure tid, _) : tbelow, Just (seg@Fused {} : _))
      | frId top == fid,
        tid == fid,
        frRefs top == frRefs seg,
        sameK (frOuter top) (frOuter seg) -> do
          modify' \st -> st {sFrames = below, sTargets = tbelow}
          (hf, _, leaves) <- bridgeFn env lim (maybe False snd mine) [top]
          -- what the handler installed gives is what this bridge gives,
          -- and its state is read as this bridge's: it must be this one
          unless (Just hf == fmap fst mine) (escapeTo rid otherShape)
          toVar v \x -> toVars (concat leaves) \xs -> do
            h <- freshNamed (syn "bridge")
            emitted
            pure (TName h (Right hf) xs (THnd (frRefs top) h Nothing (TApp (FComb jumpCont) [kr, x])))
    _ -> escapeTo rid "a case that goes on after resuming code under a bridge, or resumes it twice"

-- | Resuming a continuation that was made a residual function ('request'):
-- a call of it with the state of the handler the case installs around the
-- resumption, whose value is what that @handle@ gives.
resumeFunction :: (Symbol, Abs, [Reference], Int, Outside) -> Abs -> K -> M Term
resumeFunction (rsym, tmpl, refs, handledBy, outside) v k = do
  frames <- gets sFrames
  targets <- gets sTargets
  env <- gets sEnv
  let no why = liftIO (throwIO (Escape handledBy why))
  case (k, frames, targets) of
    (KDelim fid, top@Fused {} : below, (TPure tid, _) : tbelow)
      | frId top == fid,
        tid == fid,
        frRefs top == refs,
        Just ls <- matchHoles tmpl (opened env (frHandler top)) -> case outside of
          NoneOutside -> do
            modify' \st -> st {sFrames = below, sTargets = tbelow}
            toVar v \x -> toVars ls \xs -> dynResult (frOuter top) BX (TApp (FVar rsym) (x : xs))
          -- The function is passed the states of the handlers outside
          -- and gives them back: on from its result as from a
          -- state-passing function's
          PassOutside outerTmpls
            | outerNow <- take (length outerTmpls) below,
              length outerNow == length outerTmpls,
              all isFused outerNow,
              Just ols <- concat <$> zipWithM matchHoles outerTmpls (map (opened env . frHandler) outerNow) -> do
                modify' \st -> st {sFrames = below, sTargets = tbelow}
                toVar v \x -> toVars (ls ++ ols) \xs -> do
                  r <- freshNamed (syn "r")
                  n <- letNum
                  rest' <- resultMatch outerNow outerTmpls r (frOuter top)
                  pure (TLets (Indirect n) [r] [BX] (TApp (FVar rsym) (x : xs)) rest')
            | otherwise -> no "a continuation made a function, resumed under other handlers than it was made under"
          -- The function gives the outermost handle's value: its call
          -- must be the last thing before that handle ends. So the handler
          -- installed around it returns where the request's did, the
          -- handlers outside are the ones it was made under or ones
          -- installed in their place as a case's last act, and nothing but
          -- their pure cases is waiting.
          ThroughOutside outerTmpls outerWere exitK
            | sameK (frOuter top) exitK,
              outerNow <- take (length outerTmpls) below,
              length outerNow == length outerTmpls,
              all isFused outerNow,
              and (zipWith (\g (i, o) -> frId g == i || sameK (frOuter g) o) outerNow outerWere),
              all (waiting (map frId outerNow) . fst) tbelow,
              Just ols <- concat <$> zipWithM matchHoles outerTmpls (map (opened env . frHandler) outerNow) ->
                toVar v \x -> toVars (ls ++ ols) \xs -> pure (TApp (FVar rsym) (x : xs))
            | otherwise -> no "a continuation that runs on to the end of a loop, resumed with more to do after it"
    _ -> no "a continuation made a function, resumed with no handler of its handler's shape installed around it"
  where
    waiting ids = \case
      TBody _ -> True
      TPure i -> i `elem` ids
      TSite -> False

-- | What a value has where a template has holes, if it has the template's
-- shape.
matchHoles :: Abs -> Abs -> Maybe [Abs]
matchHoles t a = case (t, a) of
  (Dyn h _, _) | h == joinHole -> Just [a]
  (Con r tg fs, Con r' tg' gs)
    | r == r', tg == tg', length fs == length gs -> concat <$> zipWithM matchHoles fs gs
  (Clo f as, Clo g bs)
    | f == g, length as == length bs -> concat <$> zipWithM matchHoles as bs
  _ | sameAbs t a -> Just []
  _ -> Nothing

resumeWalk :: Int -> Abs -> K -> M Term
resumeWalk kid v k = do
  gets (M.lookup kid . sKonts) >>= \case
    -- from outside this residual function: it must be a function itself
    Nothing -> liftIO (throwIO (Reify kid))
    Just ko
      | koUsed ko -> liftIO (throwIO (Reify kid))
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
      body' <- withBarrier (partialEval env lim e body KTail)
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
            partialEval env lim e body (KDelim fid)
          -- with the handler as it is in its frame by then: a loop that
          -- passed the state back has put a new one there ('resultMatch')
          pureCase a = do
            now <- gets (find (\f -> isFused f && frId f == fid) . sFrames)
            modify' \st -> st {sFrames = dropFused fid (sFrames st)}
            callHandler env lim e (maybe (look e h) frHandler now) (SPure a) outer
      liftIO (Control.Exception.try (Control.Exception.try (runStateT attempt s0))) >>= \case
        Right (Right (code, s1)) -> put s1 >> pure code
        Right (Left (Escape i why))
          | i == fid -> do
              put s0
              liftIO (seNote env ("a handler kept: " ++ why))
              kept
          | otherwise -> liftIO (throwIO (Escape i why))
        -- a continuation of a request made under this handler, wanted as a
        -- function where its request can't be gone back to
        Left (Reify kid)
          | kid > fid -> do
              put s0
              liftIO (seNote env "a handler kept: a continuation needed as a function after its request's walk was over")
              kept
          | otherwise -> liftIO (throwIO (Reify kid))
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
allKnown env as
  | all fullyKnown as = sequence <$> traverse (knownVal env) as
  | otherwise = pure Nothing

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
  SBridge _ rid -> liftIO (throwIO (Escape rid "a bridge's request used as a value"))
  SKont kid -> do
    gets (M.lookup kid . sBridgeK) >>= \case
      Just (rid, _) -> liftIO (throwIO (Escape rid "a bridge's continuation used as a value"))
      Nothing -> pure ()
    reified <- gets (M.member kid . sReified)
    unless reified (liftIO (throwIO (Reify kid)))
    guardFused "a continuation used as a value" >> giveUp "a continuation as a value"
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
runIfKnown env e t =
  if S.disjoint (ABTN.freeVars t) (eUnknown e)
    then
      allKnown env abss >>= \case
        Nothing -> pure Nothing
        Just vs -> liftIO (seRunTerm env (zip fvs (map memOf abss)) t vs)
    else pure Nothing
  where
    fvs = S.toList (ABTN.freeVars t)
    abss = map (look e) fvs
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
inlineFns :: S.Set Symbol -> [(Symbol, SuperNormal Reference Symbol)] -> Term -> ([(Symbol, SuperNormal Reference Symbol)], Term)
inlineFns joins fns0 body0 =
  rounds (map fst fns0) infos0 (inlineInfo names0 body0) totals0 (holders inCloses) (holders inHandles)
  where
    names0 = S.fromList (map fst fns0)
    sigs = M.fromList [(f, (ms, ps)) | (f, Lambda ms (ABTN.TAbss ps _)) <- fns0]
    infos0 = M.fromList [(f, inlineInfo names0 bd) | (f, Lambda _ (ABTN.TAbss _ bd)) <- fns0]
    all0 = inlineInfo names0 body0 : M.elems infos0
    totals0 = M.unionsWith (+) (map inCalls all0)
    holders part = M.unionsWith (+) [M.fromSet (const (1 :: Int)) (part i) | i <- all0]
    -- A function is recursive when it is on a cycle of calls. Inlining one
    -- function into its callers leaves every other function on a cycle
    -- exactly when it was on one before: a cycle through the function
    -- inlined goes through its caller now, and no call is added that
    -- wasn't a path. So the cycles are found once.
    recursive =
      S.fromList . concat $
        [ vs
        | Graph.CyclicSCC vs <-
            Graph.stronglyConnComp [(f, f, S.toList (inlineRefs i)) | (f, i) <- M.toList infos0]
        ]
    rounds order infos entry totals closers handlers =
      case find inlinableJoin order <|> find inlinable order of
        Nothing ->
          ( [ (f, Lambda ms (ABTN.TAbss ps (inBody (infos M.! f))))
            | f <- order,
              let (ms, ps) = sigs M.! f
            ],
            inBody entry
          )
        Just f ->
          rounds
            (filter (/= f) order)
            (M.map paste (M.delete f infos))
            (paste entry)
            (M.delete f (M.unionWith (+) totals (M.map (* (calls f - 1)) (inCalls fi))))
            (moved inCloses closers)
            (moved inHandles handlers)
          where
            fi = infos M.! f
            (_, ps) = sigs M.! f
            -- the function's body in place of each of its calls in a
            -- caller, and what is counted of the caller brought up to date
            -- from the counts of the two, without reading the new body
            paste g = case M.lookup f (inCalls g) of
              Nothing -> g
              Just k ->
                Inlining
                  { inBody = inlineCalls f ps (inBody fi) (inBody g),
                    inSize = inSize g + k * (inSize fi - 1),
                    inCalls = M.unionWith (+) (M.delete f (inCalls g)) (M.map (* k) (inCalls fi)),
                    inCloses = inCloses g <> inCloses fi,
                    inHandles = inHandles g <> inHandles fi
                  }
            -- how many bodies make a closure of (or handle the call of)
            -- each function: the callers gain what the function had and
            -- they lacked, and the function's own are gone
            moved part count =
              M.unionWith
                (+)
                count
                ( M.unionsWith
                    (+)
                    ( M.fromSet (const (-1)) (part fi)
                        : [ M.fromSet (const 1) (part fi S.\\ part g)
                          | g <- entry : M.elems (M.delete f infos),
                            M.member f (inCalls g)
                          ]
                    )
                )
      where
        calls f = M.findWithDefault 0 f totals
        closed f = M.findWithDefault 0 f closers > 0
        -- the body of a kept handler stays a call ('handledBody')
        handled f = M.findWithDefault 0 f handlers > 0
        -- a function recursive only through others, if small, goes into its
        -- callers, leaving one that calls itself as the loop (GHC's loop
        -- breaker): fib's two halves become one function, which saves a call
        -- per round
        selfCalls f = S.member f (inlineRefs (infos M.! f))
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
        inlinableJoin f =
          S.member f joins
            && not (closed f || selfCalls f || handled f)
            && ( calls f == 1
                   || S.null (S.delete f (S.intersection joins (inlineRefs i)))
                     && inSize i * (calls f - 1) <= joinCopyLimit
               )
          where
            i = infos M.! f
        inlinable f
          | closed f || selfCalls f || handled f || S.member f joins = False
          | S.member f recursive = inSize (infos M.! f) <= 40
          | otherwise = calls f == 1 || inSize (infos M.! f) <= 4
    -- how many nodes copying a join point into its callers may add
    joinCopyLimit = 100 :: Int

-- | What 'inlineFns' keeps of a body, so that a round reads no body: the
-- body (a function's under its parameters), how many nodes it has, how
-- many times it calls each residual function, and the residual functions
-- it makes closures of and those whose call it handles.
data Inlining = Inlining
  { inBody :: Term,
    inSize :: !Int,
    inCalls :: M.Map Symbol Int,
    inCloses :: S.Set Symbol,
    inHandles :: S.Set Symbol
  }

inlineInfo :: S.Set Symbol -> Term -> Inlining
inlineInfo names bd =
  Inlining
    { inBody = bd,
      inSize = length ns,
      inCalls = M.fromListWith (+) [(g, 1) | AApp (FVar g) _ <- ns, S.member g names],
      inCloses = S.fromList [g | AName (Right g) _ _ <- ns, S.member g names],
      inHandles = S.fromList [g | AHnd _ _ _ (TApp (FVar g) _) <- ns, S.member g names]
    }
  where
    ns = nodes bd

-- | The residual functions a body calls or makes closures of.
inlineRefs :: Inlining -> S.Set Symbol
inlineRefs i = M.keysSet (inCalls i) <> inCloses i

-- | Each call of a residual function, given its parameters and body,
-- replaced by the body with the parameters renamed to the arguments. A
-- part of the term the function isn't free in is left as it is, unread.
-- The copies keep the names their binders had, so a name can be bound more
-- than once after this.
inlineCalls :: Symbol -> [Symbol] -> Term -> Term -> Term
inlineCalls f ps bd = go
  where
    go t
      | S.notMember f (ABTN.freeVars t) = t
      | otherwise = case t of
          TApp (FVar g) xs
            | g == f, length xs == length ps -> ABTN.renames (M.fromList (zip ps xs)) bd
          ABTN.TAbs v b -> ABTN.TAbs v (go b)
          ABTN.TTm x -> ABTN.TTm (fmap go x)

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
