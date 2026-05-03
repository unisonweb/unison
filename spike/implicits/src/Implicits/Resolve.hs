{-# LANGUAGE NamedFieldPuns #-}

-- | The implicit-resolution algorithm itself.
--
-- Implements the pseudocode of @docs/implicits-plan.md@ §1.3:
--
-- @
--   resolve(T, stack, depth) =
--     if depth > maxDepth: error DepthExceeded(stack)
--     if any(unifies(T, S)) for S in stack: fail this branch (cycle)
--     candidates = { g | conclusion(g) unifies with T under sigma }
--     ... try each, picking most-specific or reporting ambiguity
-- @
--
-- Plus the spike-specific success criteria from §4.2: per-resolution
-- memoization for diamond dependencies, a parameterised depth limit, and
-- an instrumented variant ('resolveCounted') that returns how many
-- distinct sub-goals were actually solved (so tests can assert the
-- diamond memoization bound).
module Implicits.Resolve
  ( -- * Entry points
    resolve,
    resolveWith,
    resolveCounted,

    -- * Parameters
    ResolveOptions (..),
    defaultOptions,
  )
where

import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl1')
import qualified Data.Map.Strict as Map
import Implicits.Types
import Implicits.Unify

------------------------------------------------------------------------------
-- Options
------------------------------------------------------------------------------

data ResolveOptions = ResolveOptions
  { -- | Maximum chain depth before bailing with 'DepthExceeded'. Default
    -- 50 per ADR 009.
    optMaxDepth :: !Int
  }
  deriving stock (Eq, Show)

defaultOptions :: ResolveOptions
defaultOptions = ResolveOptions {optMaxDepth = 50}

------------------------------------------------------------------------------
-- Resolution monad
------------------------------------------------------------------------------

-- | All the bookkeeping a single top-level 'resolve' call carries.
data RState = RState
  { -- | Fresh-variable supply. Starts well above any user-supplied
    -- 'TyVar' integer; we also re-freshen on every candidate match so
    -- collisions are impossible in practice.
    sNext :: !Int,
    -- | Memoization table: mapping fully-substituted goal type to its
    -- resolution. Cleared between top-level 'resolve' calls. Keying on
    -- the goal-as-given is the diamond-dependency property of success
    -- criterion #6 — a sub-goal like @Show Nat@ reached along multiple
    -- chain paths is solved exactly once.
    sMemo :: !(Map.Map Ty (Either ResolveError ResolutionTree)),
    -- | Number of memo *misses* (i.e. distinct sub-goals actually
    -- attempted). Exposed via 'resolveCounted' so tests can verify the
    -- diamond bound.
    sWork :: !Int,
    -- | Resolution options.
    sOpts :: !ResolveOptions,
    -- | Pool of available givens, threaded through the state so we can
    -- recurse without an extra Reader.
    sPool :: !Pool
  }

type R = State RState

initialState :: ResolveOptions -> Pool -> RState
initialState opts pool =
  RState
    { sNext = 1000000000, -- well above anything a user constructs
      sMemo = Map.empty,
      sWork = 0,
      sOpts = opts,
      sPool = pool
    }

bumpWork :: R ()
bumpWork = modify' $ \st -> st {sWork = sWork st + 1}

-- | Lift a 'Supply' computation into 'R', threading the fresh-variable
-- counter through the resolver state.
withSupply :: Supply a -> R a
withSupply m = do
  st <- get
  let (a, n') = runState m (sNext st)
  put st {sNext = n'}
  pure a

------------------------------------------------------------------------------
-- Top-level entry points
------------------------------------------------------------------------------

-- | Resolve a goal in the given pool with default options.
resolve :: Pool -> Ty -> Either ResolveError ResolutionTree
resolve = resolveWith defaultOptions

-- | Resolve with explicit options.
resolveWith :: ResolveOptions -> Pool -> Ty -> Either ResolveError ResolutionTree
resolveWith opts pool goal = fst (resolveCounted opts pool goal)

-- | Resolve and also return the number of distinct sub-resolution
-- *attempts* (memo misses). Used by tests to verify the diamond bound:
-- for a goal whose chain reaches @n@ unique sub-types, the work count
-- equals @n@ even when those types occur along multiple paths.
resolveCounted ::
  ResolveOptions ->
  Pool ->
  Ty ->
  (Either ResolveError ResolutionTree, Int)
resolveCounted opts pool goal =
  let (result, finalSt) = runState (resolveImpl goal [] 0) (initialState opts pool)
   in (result, sWork finalSt)

------------------------------------------------------------------------------
-- The algorithm
------------------------------------------------------------------------------

-- | Recursive resolution worker. Parameters:
--
-- * @goal@ — the type to resolve.
-- * @stack@ — outer goals currently in flight on this branch (for
--   per-branch cycle detection, §1.3).
-- * @depth@ — current depth (for the global limit).
resolveImpl :: Ty -> [Ty] -> Int -> R (Either ResolveError ResolutionTree)
resolveImpl goal stack depth = do
  opts <- gets sOpts
  if depth > optMaxDepth opts
    then pure (Left (DepthExceeded (reverse (goal : stack))))
    else do
      -- Per-branch cycle detection: if any in-flight goal unifies with
      -- the new goal, *this branch* fails. Other candidates may still
      -- succeed; the overall query may still succeed via a non-cyclic
      -- alternative.
      if any (cycleHit goal) stack
        then pure (Left (NoGiven goal []))
        else do
          memo <- gets sMemo
          case Map.lookup goal memo of
            Just hit -> pure hit
            Nothing -> do
              bumpWork
              result <- attemptCandidates goal stack depth
              modify' $ \st -> st {sMemo = Map.insert goal result (sMemo st)}
              pure result

-- | Two types are "the same in-flight goal" iff they unify under the
-- empty substitution. Stricter than '==' (catches alpha-renamed cycles)
-- but cheap enough to do once per stack entry.
cycleHit :: Ty -> Ty -> Bool
cycleHit a b = case unify emptySubst a b of
  Just _ -> True
  Nothing -> False

-- | Try every given in the pool that head-unifies with the goal, then
-- pick the unique winner.
attemptCandidates ::
  Ty ->
  [Ty] ->
  Int ->
  R (Either ResolveError ResolutionTree)
attemptCandidates goal stack depth = do
  pool <- gets sPool
  -- Step 1: head-unify each candidate with the goal.
  candidates0 <- mapMaybeM (matchHead goal) (poolGivens pool)
  -- Step 2: recursively resolve each candidate's premises. A candidate
  -- whose premises don't all resolve becomes a near-miss.
  attempts <- mapM (tryCandidate goal stack depth) candidates0
  let (nearMisses, successes) = partitionAttempts attempts
  case successes of
    [] -> pure (Left (NoGiven goal nearMisses))
    [single] -> pure (Right (rebuildTree single))
    multiple -> pure (decideMostSpecific goal multiple)

-- | A candidate that has passed head-unification but not yet had its
-- premises resolved.
data Candidate = Candidate
  { candGiven :: !Given,
    -- | Premises after freshening + head-unification substitution.
    candPremises :: ![Ty],
    -- | Specialised conclusion (matches the goal under unification).
    candConclusion :: !Ty,
    -- | Conclusion *before* head-unification, but *after* freshening.
    -- Used for specificity comparison: two candidates' specialised
    -- conclusions are equal at the goal, so we compare the shape they
    -- had before specialisation.
    candFreshConclusion :: !Ty
  }

data Attempt
  = Miss !NearMiss
  | Hit !Candidate ![ResolutionTree]

partitionAttempts :: [Attempt] -> ([NearMiss], [(Candidate, [ResolutionTree])])
partitionAttempts = foldr step ([], [])
  where
    step (Miss m) (ms, hs) = (m : ms, hs)
    step (Hit c rs) (ms, hs) = (ms, (c, rs) : hs)

rebuildTree :: (Candidate, [ResolutionTree]) -> ResolutionTree
rebuildTree (c, prems) =
  ResolutionTree
    { rtChosen = candGiven c,
      rtSpecialisedConclusion = candConclusion c,
      rtPremises = prems
    }

-- | Head-unify a single given's conclusion with the goal. If they
-- unify, return a 'Candidate' with freshened quantified variables and
-- the head-unification substitution applied.
matchHead :: Ty -> Given -> R (Maybe Candidate)
matchHead goal g = do
  let Given {givenTyVars, givenPremises, givenConclusion} = g
  (premises', concl') <- withSupply (freshen givenTyVars givenPremises givenConclusion)
  case unify emptySubst concl' goal of
    Nothing -> pure Nothing
    Just s ->
      pure $
        Just
          Candidate
            { candGiven = g,
              candPremises = map (applySubst s) premises',
              candConclusion = applySubst s concl',
              candFreshConclusion = concl'
            }

-- | Resolve every premise of a candidate. If they all succeed, return a
-- 'Hit'; the first failing premise turns the whole candidate into a
-- 'Miss'.
tryCandidate :: Ty -> [Ty] -> Int -> Candidate -> R Attempt
tryCandidate goal stack depth cand =
  let stack' = goal : stack
   in go [] (candPremises cand) stack' depth
  where
    go acc [] _ _ = pure (Hit cand (reverse acc))
    go acc (p : ps) stk d = do
      sub <- resolveImpl p stk (d + 1)
      case sub of
        Right tree -> go (tree : acc) ps stk d
        Left err -> pure (Miss (NearMiss (candGiven cand) err))

------------------------------------------------------------------------------
-- Specificity ordering
------------------------------------------------------------------------------

-- | Decide the unique winner among candidates whose premises all
-- resolved. Ordering rules from §1.3:
--
--   * Lexical proximity: smaller 'Lexical' beats larger; both beat
--     'Ambient'. (The 'Ord Scope' instance already encodes this.)
--   * Subsumption: if one candidate's conclusion is a strict instance
--     of another's, the more-specific one wins.
--   * If neither rule produces a unique maximum, return 'Ambiguous'.
decideMostSpecific ::
  Ty ->
  [(Candidate, [ResolutionTree])] ->
  Either ResolveError ResolutionTree
decideMostSpecific goal hits =
  let -- Step 1: keep only the most-inner-scoped survivors.
      best = filterMostInner hits
   in case best of
        [single] -> Right (rebuildTree single)
        multiple ->
          -- Step 2: among them, drop any that are strictly subsumed
          -- by another candidate (i.e., another candidate is more
          -- specific). What remains should be a single most-specific
          -- candidate; if not, it's ambiguous.
          case filter (notStrictlySubsumedIn multiple) multiple of
            [single] -> Right (rebuildTree single)
            ambiguous ->
              Left
                ( Ambiguous
                    goal
                    (map (candGiven . fst) ambiguous)
                )

filterMostInner ::
  [(Candidate, [ResolutionTree])] ->
  [(Candidate, [ResolutionTree])]
filterMostInner [] = []
filterMostInner xs =
  let scopeKey = givenScope . candGiven . fst
      best = foldl1' min (map scopeKey xs)
   in filter ((== best) . scopeKey) xs

-- | True iff no *other* candidate is strictly more specific than this
-- one.
notStrictlySubsumedIn ::
  [(Candidate, [ResolutionTree])] ->
  (Candidate, [ResolutionTree]) ->
  Bool
notStrictlySubsumedIn others me =
  not (any (`strictlyMoreSpecific` me) others)

-- | @a `strictlyMoreSpecific` b@ iff @a@'s conclusion is a strict
-- instance of @b@'s conclusion: i.e., @b@ pattern-matches @a@ (so @b@
-- is more general) and @a@ does not pattern-match @b@.
strictlyMoreSpecific ::
  (Candidate, [ResolutionTree]) ->
  (Candidate, [ResolutionTree]) ->
  Bool
strictlyMoreSpecific (a, _) (b, _)
  | candGiven a == candGiven b = False
  | otherwise =
      -- Compare on the *fresh, un-specialised* conclusions: post-
      -- unification, both candidates' specialised conclusions equal
      -- the goal and carry no information.
      let ca = candFreshConclusion a
          cb = candFreshConclusion b
          bMatchesA = case oneWayMatch cb ca of Just _ -> True; Nothing -> False
          aMatchesB = case oneWayMatch ca cb of Just _ -> True; Nothing -> False
       in bMatchesA && not aMatchesB

-- | One-way structural match: is there a substitution σ with σ(pat) =
-- target, treating only pat's free variables as unifiable? (Variables
-- in @target@ are treated as rigid constants.)
oneWayMatch :: Ty -> Ty -> Maybe Subst
oneWayMatch pat target = go IntMap.empty pat target
  where
    go s a b = case (a, b) of
      (TVar (TyVar i), _) ->
        case IntMap.lookup i s of
          Just t' -> if t' == b then Just s else Nothing
          Nothing -> Just (IntMap.insert i b s)
      (TCon c1, TCon c2)
        | c1 == c2 -> Just s
        | otherwise -> Nothing
      (TApp f1 x1, TApp f2 x2) -> do
        s1 <- go s f1 f2
        go s1 x1 x2
      _ -> Nothing

------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = go []
  where
    go acc [] = pure (reverse acc)
    go acc (x : xs) = do
      mb <- f x
      case mb of
        Just b -> go (b : acc) xs
        Nothing -> go acc xs
