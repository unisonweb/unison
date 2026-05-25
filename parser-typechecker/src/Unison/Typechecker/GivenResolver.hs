{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Implicit-resolution algorithm operating over the 'Unison.Type.Type'
-- AST.
--
-- The algorithm is:
--
-- @
--   resolve(T, stack, depth) =
--     if depth > maxDepth: error DepthExceeded(stack)
--     if any(unifies(T, S)) for S in stack: fail this branch (cycle)
--     candidates = { g | conclusion(g) unifies with T under sigma }
--     ... try each, picking most-specific or reporting ambiguity
-- @
--
-- Key properties:
--
-- * Specificity uses *one-way* matching of *declared* conclusions
--   ('oneWayMatch'). The candidate-under-comparison's quantified
--   variables are flexible; the other candidate's are rigid.
--
-- * Memoization is per-top-level-resolve, keyed on the goal type, and
--   caches both successes and failures.
--
-- ## Unification choice
--
-- The typechecker's 'Unison.Typechecker.Context' provides 'subtype'
-- and 'equate' but they are tightly bound to the @M v loc@ monad
-- (existential markers, scope tracking, 'Blank' annotations,
-- 'TypeVar.Existential' / 'TypeVar.Universal' wrapping). They cannot
-- be reused without dragging in the entire context state.
--
-- A standalone Robinson unifier is a far better fit: the resolver
-- freshens each candidate's quantified variables to its own pool and
-- unifies with the goal under a pure substitution.
module Unison.Typechecker.GivenResolver
  ( -- * Givens and pool
    Given (..),
    Scope (..),
    Pool (..),
    poolFromList,

    -- * Result tree
    ResolutionTree (..),
    Substitution,

    -- * Errors
    ResolveError (..),
    NearMiss (..),

    -- * Entry points
    resolve,
    resolveWith,
    resolveCounted,

    -- * Options
    ResolveOptions (..),
    defaultOptions,
  )
where

import Control.Monad.State.Strict (State, get, modify', runState)
import Data.List (foldl1')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.Reference (Reference)
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Var (Var)
import Unison.Var qualified as Var

------------------------------------------------------------------------------
-- Public types
------------------------------------------------------------------------------

-- | Substitution from variable to type. The resolver's substitutions
-- are kept idempotent (applying twice is the same as applying once).
type Substitution v loc = Map v (Type v loc)

-- | Lexical depth or ambient scope. Smaller 'Lexical' values are
-- *more* inner (e.g. @Lexical 0@ is the innermost binder); 'Ambient'
-- is the outermost. Lexical-inner candidates win over ambient ones at
-- the same subsumption level.
data Scope = Lexical !Int | Ambient
  deriving stock (Eq, Ord, Show)

-- | A single available given. Universally quantified over
-- 'givenTyVars'.
--
-- Example: @given Show.list : Show a => Show (List a)@ is
--
-- @
--   Given { givenName       = Reference.Builtin "Show.list"
--         , givenTyVars     = [a]
--         , givenPremises   = [App Show (Var a)]
--         , givenConclusion = App Show (App List (Var a))
--         , givenScope      = Ambient
--         }
-- @
data Given v loc = Given
  { givenName :: !Reference,
    givenTyVars :: ![v],
    givenPremises :: ![Type v loc],
    givenConclusion :: !(Type v loc),
    givenScope :: !Scope
  }
  deriving stock (Show)

-- | Two givens are considered equal by their reference: the rest of
-- the record is metadata recoverable from the term whose hash is the
-- 'Reference'. Equality on 'Reference' is what callers want when they
-- need set-membership of givens.
instance Eq (Given v loc) where
  a == b = givenName a == givenName b

-- | The candidate pool. Order is irrelevant; we keep it as a list
-- because we always traverse linearly.
newtype Pool v loc = Pool {poolGivens :: [Given v loc]}
  deriving stock (Show)

poolFromList :: [Given v loc] -> Pool v loc
poolFromList = Pool

-- | A successful resolution.
data ResolutionTree v loc = ResolutionTree
  { -- | The chosen given.
    rtGiven :: !(Given v loc),
    -- | The substitution that specialised the given's conclusion to
    -- the goal (and was propagated into the premises).
    rtSubst :: !(Substitution v loc),
    -- | Resolution proofs for each premise, in order.
    rtChildren :: ![ResolutionTree v loc]
  }
  deriving stock (Show)

-- | Resolution errors. Keep these structured so the rendering pass
-- can format them.
data ResolveError v loc
  = -- | No matching given for the goal; near-misses are candidates
    -- whose conclusions head-unified with the goal but whose premises
    -- failed to resolve.
    NoGiven (Type v loc) [NearMiss v loc]
  | -- | Multiple givens of incomparable specificity match.
    Ambiguous (Type v loc) [Given v loc]
  | -- | Resolution chain exceeded the depth limit; the chain
    -- (outermost first) is included for diagnosis.
    DepthExceeded [Type v loc]
  | -- | Hard cycle: the goal recurred along its own chain. Currently
    -- emitted only when the resolver detects a self-referential goal
    -- with no escape hatch. The list is the chain at the point of
    -- detection (outermost first). Ordinary cycles short-circuit
    -- per-branch and surface as 'NoGiven'; this variant is the
    -- explicit hard-cycle diagnosis exposed for rendering.
    Cycle [Type v loc]
  | -- | The goal contained an unresolved metavariable (an inference
    -- variable that was not pinned by surrounding inference). The
    -- resolver cannot meaningfully proceed: head-unification would
    -- accept any candidate. Surfaced as a distinct diagnostic so the
    -- user is told to add a type annotation rather than chasing a
    -- missing given.
    --
    -- The 'Type' is the goal as the resolver saw it (with the
    -- metavar still present).
    UnresolvedMetavarInGoal (Type v loc)
  deriving stock (Show)

-- | A candidate that head-unified with the goal but failed somewhere
-- in its premises. Carries the sub-error so the user can see *why*
-- it didn't work.
data NearMiss v loc = NearMiss
  { nmGiven :: !(Given v loc),
    nmReason :: !(ResolveError v loc)
  }
  deriving stock (Show)

------------------------------------------------------------------------------
-- Options
------------------------------------------------------------------------------

data ResolveOptions = ResolveOptions
  { -- | Maximum chain depth before bailing with 'DepthExceeded'.
    -- Default 50.
    optMaxDepth :: !Int
  }
  deriving stock (Eq, Show)

defaultOptions :: ResolveOptions
defaultOptions = ResolveOptions {optMaxDepth = 50}

------------------------------------------------------------------------------
-- Resolution monad
------------------------------------------------------------------------------

-- | Map keyed by the structural shape of the goal type. We discard
-- the goal's annotation when comparing for memoization — annotations
-- carry source locations, but two goals at different source locations
-- with the same shape are still the same resolution problem.
newtype MemoKey v = MemoKey (Type v ())
  deriving stock (Eq, Ord)

mkMemoKey :: (Ord v) => Type v loc -> MemoKey v
mkMemoKey = MemoKey . void'
  where
    void' :: (Ord v) => Type v loc -> Type v ()
    void' = ABT.amap (const ())

data RState v loc = RState
  { -- | Running supply of fresh variables. Each @freshen@ pulls from
    -- here and the resolver guarantees no collision with the
    -- candidate's own variables or any goal variable already in
    -- play.
    sUsed :: !(Set v),
    -- | Memoization table: 'MemoKey' to either error or success.
    -- Cleared between top-level 'resolve' calls. Caches both hits and
    -- misses.
    sMemo :: !(Map (MemoKey v) (Either (ResolveError v loc) (ResolutionTree v loc))),
    -- | Number of memo *misses* — distinct sub-goals attempted.
    -- Exposed via 'resolveCounted' so tests can verify the diamond
    -- bound from §4.2#6 of the plan.
    sWork :: !Int,
    -- | Resolution options.
    sOpts :: !ResolveOptions,
    -- | Pool of available givens.
    sPool :: !(Pool v loc)
  }

type R v loc = State (RState v loc)

initialState :: forall v loc. (Var v) => ResolveOptions -> Pool v loc -> Type v loc -> RState v loc
initialState opts pool goal =
  RState
    { sUsed = seedUsed,
      sMemo = Map.empty,
      sWork = 0,
      sOpts = opts,
      sPool = pool
    }
  where
    -- Seed the fresh-variable supply with every variable that occurs
    -- in the goal or in any candidate so freshening can never collide.
    seedUsed =
      Set.unions
        ( ABT.freeVars goal
            : map givenAllVars (poolGivens pool)
        )
    givenAllVars g =
      Set.unions
        ( Set.fromList (givenTyVars g)
            : ABT.freeVars (givenConclusion g)
            : map ABT.freeVars (givenPremises g)
        )

------------------------------------------------------------------------------
-- Top-level entry points
------------------------------------------------------------------------------

-- | Resolve a goal in the given pool with default options.
resolve ::
  (Var v, Ord loc) =>
  Pool v loc ->
  Type v loc ->
  Either (ResolveError v loc) (ResolutionTree v loc)
resolve = resolveWith defaultOptions

-- | Resolve with explicit options.
resolveWith ::
  (Var v, Ord loc) =>
  ResolveOptions ->
  Pool v loc ->
  Type v loc ->
  Either (ResolveError v loc) (ResolutionTree v loc)
resolveWith opts pool goal = fst (resolveCounted opts pool goal)

-- | Like 'resolveWith' but also returns the number of distinct
-- sub-goals actually attempted (memo misses). Used by the diamond
-- tests to verify the memoization bound.
resolveCounted ::
  (Var v, Ord loc) =>
  ResolveOptions ->
  Pool v loc ->
  Type v loc ->
  (Either (ResolveError v loc) (ResolutionTree v loc), Int)
resolveCounted opts pool goal =
  -- The resolver runs unconditionally even when the goal contains an
  -- 'Var.Inference'-typed variable. The unifier treats free variables
  -- symmetrically and the constraint goal often carries an unresolved
  -- metavar that surrounding inference has already pinned. If
  -- resolution succeeds the unifier pins the metavar to the
  -- candidate; if no candidate matches, the 'NoGiven' surface
  -- diagnostic accurately reports that there is no instance — adding
  -- a type annotation is a special case of that.
  let (result, finalSt) =
        runState
          (resolveImpl goal [] 0)
          (initialState opts pool goal)
   in (result, sWork finalSt)

-- | Whether a 'Var.Type' indicates a variable created by the
-- typechecker's inference machinery. Used by 'matchHead' to decide
-- which goal-side variables to treat as flexible.
isInferenceVar :: Var.Type -> Bool
isInferenceVar = \case
  Var.Inference _ -> True
  _ -> False

------------------------------------------------------------------------------
-- The algorithm
------------------------------------------------------------------------------

-- | Recursive resolution worker.
resolveImpl ::
  (Var v, Ord loc) =>
  Type v loc ->
  [Type v loc] ->
  Int ->
  R v loc (Either (ResolveError v loc) (ResolutionTree v loc))
resolveImpl goal stack depth = do
  opts <- gets' sOpts
  if depth > optMaxDepth opts
    then pure (Left (DepthExceeded (reverse (goal : stack))))
    else
      -- Per-branch cycle detection: if any in-flight goal unifies
      -- with the new goal, *this branch* fails. Other candidates may
      -- still succeed; the overall query may still succeed via a
      -- non-cyclic alternative.
      if any (cycleHit goal) stack
        then pure (Left (NoGiven goal []))
        else do
          memo <- gets' sMemo
          let key = mkMemoKey goal
          case Map.lookup key memo of
            Just hit -> pure hit
            Nothing -> do
              bumpWork
              result <- attemptCandidates goal stack depth
              modify' (\st -> st {sMemo = Map.insert key result (sMemo st)})
              pure result

bumpWork :: R v loc ()
bumpWork = modify' (\st -> st {sWork = sWork st + 1})

gets' :: (RState v loc -> a) -> R v loc a
gets' f = fmap f get

-- | Two types are "the same in-flight goal" iff they /unify/ under
-- the empty substitution, treating every free variable on either side
-- as flexible. This catches alpha-renamed cycles such as
-- @Foo a@ vs. @Foo b@ where the renamed copy was produced by
-- 'freshenGiven' on a recursive given.
--
-- The flex set is /all/ free variables on either side because the
-- goal can carry metavariables that outer inference has not yet
-- pinned.
--
-- Note: this is intentionally a /heuristic/ for cycle pruning; it is
-- safe to over-approximate (a false-positive cycle just causes /this
-- branch/ to fail, while other candidates may still resolve the goal,
-- under the per-branch cycle policy).
cycleHit :: (Var v, Ord loc) => Type v loc -> Type v loc -> Bool
cycleHit a b =
  let flex = ABT.freeVars a `Set.union` ABT.freeVars b
   in case unify mempty flex a b of
        Just _ -> True
        Nothing -> False

-- | Try every given that head-unifies with the goal, then pick the
-- unique winner.
attemptCandidates ::
  forall v loc.
  (Var v, Ord loc) =>
  Type v loc ->
  [Type v loc] ->
  Int ->
  R v loc (Either (ResolveError v loc) (ResolutionTree v loc))
attemptCandidates goal stack depth = do
  pool <- gets' sPool
  -- Step 1: head-unify each candidate with the goal.
  candidates0 <- mapMaybeM (matchHead goal) (poolGivens pool)
  -- Step 2: recursively resolve each candidate's premises. A
  -- candidate whose premises don't all resolve becomes a near-miss.
  attempts <- mapM (tryCandidate goal stack depth) candidates0
  let (nearMisses, successes) = partitionAttempts attempts
  case successes of
    [] -> pure (Left (NoGiven goal nearMisses))
    [single] -> pure (Right (rebuildTree single))
    multiple -> pure (decideMostSpecific goal multiple)

-- | A candidate that has passed head-unification but not yet had its
-- premises resolved.
data Candidate v loc = Candidate
  { candGiven :: !(Given v loc),
    -- | Premises after freshening + head-unification substitution
    -- applied.
    candPremises :: ![Type v loc],
    -- | Substitution produced by head-unification. Stored on the
    -- 'ResolutionTree' so post-passes (D3) can apply it to terms.
    candSubst :: !(Substitution v loc),
    -- | Conclusion *after* freshening but *before* head-unification.
    -- Used for specificity comparison: post-unification, both
    -- candidates' specialised conclusions equal the goal and carry no
    -- information.
    candFreshConclusion :: !(Type v loc),
    -- | The fresh-renamed quantified variables. These are treated as
    -- *flexible* (unifiable) when this candidate's conclusion is the
    -- pattern in 'oneWayMatch', and as *rigid* when this candidate's
    -- conclusion is the target of someone else's pattern.
    candFreshTyVars :: ![v]
  }

data Attempt v loc
  = Miss !(NearMiss v loc)
  | Hit !(Candidate v loc) ![ResolutionTree v loc]

partitionAttempts ::
  [Attempt v loc] ->
  ([NearMiss v loc], [(Candidate v loc, [ResolutionTree v loc])])
partitionAttempts = foldr step ([], [])
  where
    step (Miss m) (ms, hs) = (m : ms, hs)
    step (Hit c rs) (ms, hs) = (ms, (c, rs) : hs)

rebuildTree :: (Candidate v loc, [ResolutionTree v loc]) -> ResolutionTree v loc
rebuildTree (c, prems) =
  ResolutionTree
    { rtGiven = candGiven c,
      rtSubst = candSubst c,
      rtChildren = prems
    }

-- | Head-unify a single given's conclusion with the goal. Returns a
-- 'Candidate' iff they unify. The candidate's quantified variables
-- are first renamed to a fresh, collision-free pool.
matchHead ::
  (Var v, Ord loc) =>
  Type v loc ->
  Given v loc ->
  R v loc (Maybe (Candidate v loc))
matchHead goal g = do
  let Given {givenTyVars, givenPremises, givenConclusion} = g
  (fresh, prems', concl') <- freshenGiven givenTyVars givenPremises givenConclusion
  -- The candidate's freshened variables are *flexible* (unifiable);
  -- variables in the goal are *rigid* (we have already committed to
  -- them at the call site). Implementation: pass the fresh set as
  -- the unifier's flexible-variable whitelist; everything else is
  -- treated rigid.
  --
  -- Exception: unresolved inference variables that surrounding type
  -- inference hasn't yet pinned are also treated as flexible. When
  -- the elaborator emits a 'ConstraintGoal' for a polymorphic
  -- function used in a context that fully determines its type
  -- elsewhere, the goal's existential may not have been substituted
  -- into the note by the time the resolver runs. Treating those
  -- existentials as flexible lets the unifier bind them to whatever
  -- the candidate exposes — which is exactly what surrounding
  -- inference will end up doing.
  let inferenceVarsInGoal =
        Set.filter (isInferenceVar . Var.typeOf) (ABT.freeVars goal)
      flex = Set.fromList fresh `Set.union` inferenceVarsInGoal
  case unify mempty flex concl' goal of
    Nothing -> pure Nothing
    Just s ->
      pure $
        Just
          Candidate
            { candGiven = g,
              candPremises = map (applySubst s) prems',
              candSubst = s,
              candFreshConclusion = concl',
              candFreshTyVars = fresh
            }

-- | Resolve every premise of a candidate. If they all succeed, return
-- a 'Hit'; the first failing premise turns the whole candidate into a
-- 'Miss'.
tryCandidate ::
  (Var v, Ord loc) =>
  Type v loc ->
  [Type v loc] ->
  Int ->
  Candidate v loc ->
  R v loc (Attempt v loc)
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
-- Specificity ordering (one-way match of declared
-- conclusions; the pattern's quantified variables are flexible, the
-- target's are rigid).
------------------------------------------------------------------------------

decideMostSpecific ::
  (Var v, Ord loc) =>
  Type v loc ->
  [(Candidate v loc, [ResolutionTree v loc])] ->
  Either (ResolveError v loc) (ResolutionTree v loc)
decideMostSpecific goal hits =
  -- Step 1: keep only the most-inner-scoped survivors.
  let best = filterMostInner hits
   in case best of
        [single] -> Right (rebuildTree single)
        multiple ->
          -- Step 2: drop any that are strictly subsumed by another.
          case filter (notStrictlySubsumedIn multiple) multiple of
            [single] -> Right (rebuildTree single)
            ambiguous ->
              Left (Ambiguous goal (map (candGiven . fst) ambiguous))

filterMostInner ::
  [(Candidate v loc, [ResolutionTree v loc])] ->
  [(Candidate v loc, [ResolutionTree v loc])]
filterMostInner [] = []
filterMostInner xs =
  let scopeKey = givenScope . candGiven . fst
      best = foldl1' min (map scopeKey xs)
   in filter ((== best) . scopeKey) xs

-- | True iff no *other* candidate is strictly more specific than this
-- one.
notStrictlySubsumedIn ::
  (Var v, Ord loc) =>
  [(Candidate v loc, [ResolutionTree v loc])] ->
  (Candidate v loc, [ResolutionTree v loc]) ->
  Bool
notStrictlySubsumedIn others me =
  not (any (`strictlyMoreSpecific` me) others)

-- | @a `strictlyMoreSpecific` b@ iff @a@'s conclusion is a strict
-- instance of @b@'s conclusion: i.e., @b@ pattern-matches @a@
-- one-way (so @b@ is more general) and @a@ does not pattern-match
-- @b@.
strictlyMoreSpecific ::
  forall v loc.
  (Var v, Ord loc) =>
  (Candidate v loc, [ResolutionTree v loc]) ->
  (Candidate v loc, [ResolutionTree v loc]) ->
  Bool
strictlyMoreSpecific (a, _) (b, _)
  | candGiven a == candGiven b = False
  | otherwise =
      let ca = candFreshConclusion a
          cb = candFreshConclusion b
          -- "b matches a" : try to instantiate b's pattern (b's
          -- fresh vars are flexible) so that b == a (a's vars are
          -- rigid).
          bMatchesA = matched (Set.fromList (candFreshTyVars b)) cb ca
          aMatchesB = matched (Set.fromList (candFreshTyVars a)) ca cb
       in bMatchesA && not aMatchesB
  where
    matched flex pat target = case oneWayMatch flex pat target of
      Just _ -> True
      Nothing -> False

------------------------------------------------------------------------------
-- Robinson unification on 'Type v loc'
--
-- Standalone, not the 'Context'-monad unifier. See module-level
-- comment for rationale.
------------------------------------------------------------------------------

-- | Apply a substitution to a type. The substitution must be
-- idempotent (which 'unify' maintains), so a single pass suffices.
applySubst :: (Var v) => Substitution v loc -> Type v loc -> Type v loc
applySubst s
  | Map.null s = id
  | otherwise = ABT.substsInheritAnnotation (Map.toList s)

-- | Unify two types, treating only variables in @flex@ as
-- unifiable. Variables outside @flex@ are rigid (constants).
--
-- Two free occurrences of a rigid variable @v@ unify with each other
-- (since @TVar v == TVar v@ structurally) but not with anything else.
unify ::
  (Var v, Ord loc) =>
  Substitution v loc ->
  Set v ->
  Type v loc ->
  Type v loc ->
  Maybe (Substitution v loc)
unify s0 flex t u = go s0 (applySubst s0 t) (applySubst s0 u)
  where
    go s a b = case (ABT.out a, ABT.out b) of
      _ | structuralEq a b -> Just s
      (ABT.Var va, _) | va `Set.member` flex -> bind s va b
      (_, ABT.Var vb) | vb `Set.member` flex -> bind s vb a
      -- Two rigid vars that didn't structurally match: not unifiable.
      (ABT.Var _, _) -> Nothing
      (_, ABT.Var _) -> Nothing
      (ABT.Tm fa, ABT.Tm fb) -> goF s fa fb
      _ -> Nothing

    goF s fa fb = case (fa, fb) of
      (Type.Ref r1, Type.Ref r2)
        | r1 == r2 -> Just s
        | otherwise -> Nothing
      (Type.App f1 x1, Type.App f2 x2) -> do
        s1 <- go s f1 f2
        go s1 (applySubst s1 x1) (applySubst s1 x2)
      (Type.Arrow i1 o1, Type.Arrow i2 o2) -> do
        s1 <- go s i1 i2
        go s1 (applySubst s1 o1) (applySubst s1 o2)
      (Type.ImplicitArrow i1 o1, Type.ImplicitArrow i2 o2) -> do
        s1 <- go s i1 i2
        go s1 (applySubst s1 o1) (applySubst s1 o2)
      (Type.Effect e1 t1, Type.Effect e2 t2) -> do
        s1 <- go s e1 e2
        go s1 (applySubst s1 t1) (applySubst s1 t2)
      (Type.Effects es1, Type.Effects es2)
        | length es1 == length es2 -> goList s es1 es2
        | otherwise -> Nothing
      _ -> Nothing

    goList s [] [] = Just s
    goList s (x : xs) (y : ys) = do
      s1 <- go s (applySubst s x) (applySubst s y)
      goList s1 xs ys
    goList _ _ _ = Nothing

    bind s v t1
      | ABT.Var v' <- ABT.out t1, v == v' = Just s
      | v `Set.member` ABT.freeVars (applySubst s t1) = Nothing -- occurs check
      | otherwise =
          -- extend s with [v := t1] and propagate into existing image
          let single = Map.singleton v t1
              s' = Map.insert v t1 (Map.map (applySubst single) s)
           in Just s'

-- | Structural equality up to annotations.
structuralEq :: (Var v, Ord loc) => Type v loc -> Type v loc -> Bool
structuralEq = (==)

-- | One-way structural match: is there a substitution σ with
-- σ(@pat@) = @target@, treating *only* variables in @flex@ as
-- unifiable? Variables in @target@ are rigid by construction.
--
-- Used for specificity comparisons: a candidate whose pattern
-- *one-way matches* another's conclusion is more general than that
-- other.
oneWayMatch ::
  forall v loc.
  (Var v, Ord loc) =>
  Set v ->
  Type v loc ->
  Type v loc ->
  Maybe (Substitution v loc)
oneWayMatch flex pat target = go Map.empty pat target
  where
    go :: Substitution v loc -> Type v loc -> Type v loc -> Maybe (Substitution v loc)
    go s a b = case (ABT.out a, ABT.out b) of
      (ABT.Var va, _) | va `Set.member` flex ->
        case Map.lookup va s of
          Just t' -> if structuralEq t' b then Just s else Nothing
          Nothing -> Just (Map.insert va b s)
      (ABT.Var va, ABT.Var vb)
        | va == vb -> Just s
        | otherwise -> Nothing
      (ABT.Tm fa, ABT.Tm fb) -> goF s fa fb
      _ -> Nothing

    goF :: Substitution v loc -> Type.F (Type v loc) -> Type.F (Type v loc) -> Maybe (Substitution v loc)
    goF s fa fb = case (fa, fb) of
      (Type.Ref r1, Type.Ref r2)
        | r1 == r2 -> Just s
        | otherwise -> Nothing
      (Type.App f1 x1, Type.App f2 x2) -> do
        s1 <- go s f1 f2
        go s1 x1 x2
      (Type.Arrow i1 o1, Type.Arrow i2 o2) -> do
        s1 <- go s i1 i2
        go s1 o1 o2
      (Type.ImplicitArrow i1 o1, Type.ImplicitArrow i2 o2) -> do
        s1 <- go s i1 i2
        go s1 o1 o2
      (Type.Effect e1 t1, Type.Effect e2 t2) -> do
        s1 <- go s e1 e2
        go s1 t1 t2
      (Type.Effects es1, Type.Effects es2)
        | length es1 == length es2 -> goList s es1 es2
        | otherwise -> Nothing
      _ -> Nothing

    goList s [] [] = Just s
    goList s (x : xs) (y : ys) = do
      s1 <- go s x y
      goList s1 xs ys
    goList _ _ _ = Nothing

------------------------------------------------------------------------------
-- Freshening
------------------------------------------------------------------------------

-- | Replace every variable in @vs@ with a fresh, collision-free
-- equivalent throughout @prems@ and @concl@. Returns the fresh
-- variables (in the same order as @vs@), the renamed premises, and
-- the renamed conclusion.
freshenGiven ::
  (Var v) =>
  [v] ->
  [Type v loc] ->
  Type v loc ->
  R v loc ([v], [Type v loc], Type v loc)
freshenGiven vs prems concl = do
  used <- gets' sUsed
  let (used', fresh) = freshenAll used vs
  modify' (\st -> st {sUsed = used'})
  let pairs = zip vs (map ABT.var fresh)
      ren = ABT.substsInheritAnnotation pairs
  pure (fresh, map ren prems, ren concl)
  where
    freshenAll usedSet [] = (usedSet, [])
    freshenAll usedSet (v : rest) =
      let v' = Var.freshIn usedSet v
          (usedSet'', rest') = freshenAll (Set.insert v' usedSet) rest
       in (usedSet'', v' : rest')

------------------------------------------------------------------------------
-- Misc helpers
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
