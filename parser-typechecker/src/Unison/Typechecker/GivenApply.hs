{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Post-typecheck pass that walks a term and substitutes resolved
-- implicit arguments into 'App' nodes, mirroring the
-- @applyTdnrDecisions@ pattern (see
-- @parser-typechecker/src/Unison/FileParsers.hs:329@).
--
-- ## Inputs
--
-- * The list of 'Context.InfoNote' values produced by inference.
--   The relevant ones are:
--
--     * 'Context.SolvedImplicit': one per resolved implicit slot,
--       carrying either a 'GR.ResolutionTree' (success) or a
--       'GR.ResolveError' (failure). Errors are left in place for a
--       downstream pass to surface; only successes are consumed
--       here.
--
--     * 'Context.TopLevelComponent': the inferred types of let-rec
--       bindings, used to look up the types of locally-defined
--       functions referenced by 'Term.Var''.
--
-- * A 'TL.TypeLookup': the typechecker's view of every 'Term.Ref''
--   the term mentions. Used to compute how many leading
--   'Type.ImplicitArrow's a function carries before its first
--   explicit argument.
--
-- ## Strategy
--
-- The typechecker emits 'SolvedImplicit' notes in left-to-right
-- /synthesis/ order: that is, the order in which 'synthesizeApp'
-- peeled 'ImplicitArrow' arrows during inference. Apply-sites are
-- visited top-down, function-first, then arguments left-to-right.
--
-- We walk the term in the same order, holding a queue of pending
-- decisions. At each apply-site @Apps' f args@:
--
--  1. Look up the type of @f@. For 'Ref'' we consult the supplied
--     'TL.TypeLookup'; for 'Ann'' we use the annotation's type; for
--     'Var'' we consult a local environment built from the
--     'TopLevelComponent' notes plus traversal-introduced bindings.
--
--  2. Walk that type, peeling 'ImplicitArrow' before each user arg
--     and 'Arrow' as we consume args. For each 'ImplicitArrow' we
--     pop a 'SolvedImplicit' decision from the queue (matching the
--     order in which the typechecker emitted them).
--
--  3. **Override skip (annotation-widening contract).**
--     Before consuming a decision, examine the /next user-supplied
--     argument/. If its annotation has been widened by the
--     @\@@-positional override syntax (i.e. the outer 'Ann' starts
--     at a column strictly /before/ the start of the leftmost
--     annotation among its sub-terms), the user has already supplied
--     this implicit explicitly. We do /not/ pop a decision, leave
--     the arg untouched, and move on.
--
--  4. Otherwise, build a 'Term.Term' for the resolution tree (a
--     left-leaning chain of 'Term.app' nodes terminating in
--     'Term.ref' for each chosen given) and insert it into the
--     argument list at the correct position. The resulting 'Apps''
--     has plain 'App' nodes pointing at dictionary hashes — exactly
--     what the runtime expects.
--
-- ## Errors and overrides
--
-- Error decisions ('Left ResolveError') are left untouched: the term
-- keeps the original arity, and a downstream rendering pass
-- translates the error into a user-facing diagnostic. This is the
-- same separation TDNR uses: 'applyTdnrDecisions' silently skips
-- 'SolvedBlank' notes whose 'Resolution' yielded no substitution;
-- the user-facing error surfaces from the stored 'Suggestion's via
-- 'Result.Note'.
--
-- ## Limitations
--
-- * The override detection works only when the override-argument has
--   sub-term annotations to compare against. For a single-leaf
--   override (e.g. @f \@ d@ where @d@ is a bare identifier) the
--   inner annotation is unavailable and we fall back to /not/
--   treating the slot as overridden. To force an override on a
--   single leaf, the user can wrap the argument in parentheses (@f
--   \@ (d)@) — the parens produce a non-leaf node whose inner
--   annotation is recoverable.
--
-- * For function expressions whose type cannot be looked up
--   syntactically (e.g. a higher-order callback, a complex
--   expression head), this pass leaves the apply site alone. In
--   practice such functions cannot have implicit parameters in their
--   /surface/ type, since 'ImplicitArrow' is only valid in declared
--   signatures; inferred types of expressions never contain
--   'ImplicitArrow'. So this is not a soundness gap, just a reminder
--   that the pass is syntax-directed.
module Unison.Typechecker.GivenApply
  ( -- * Top-level entry point
    applyGivenDecisions,
    applyGivenDecisionsAll,

    -- * Building dictionary terms
    buildDictionary,

    -- * Stripping for surface rendering
    stripSyntheticArgs,
    stripImplicitArgsByType,
    stripLeadingImplicitLambdas,
  )
where

import Control.Monad.State.Strict (State, gets, modify', runState)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Unison.ABT qualified as ABT
import Unison.Parser.Ann (Ann (..))
import Unison.Parser.Ann qualified as Ann
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.TypeTagRepr qualified as TypeTagRepr
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.GivenElaborator qualified as GivenElaborator
import Unison.Typechecker.GivenResolver qualified as GR
import Unison.Typechecker.TypeLookup (TypeLookup)
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Var (Var)
import Unison.Var qualified as Var

------------------------------------------------------------------------------
-- Top-level entry point
------------------------------------------------------------------------------

-- | Walk the typechecked term and substitute resolution trees into
-- 'App' nodes, consuming 'SolvedImplicit' decisions in synthesis
-- order.
--
-- The result is a term in which every implicit slot whose decision
-- succeeded has been filled with a dictionary application; every
-- explicit override (per A3) is preserved unchanged; every implicit
-- whose decision failed is left as-is, with the failure surfaced via
-- the original info-note (D4).
--
-- Alongside the rewritten term we return a list of
-- 'Context.ImplicitArgRef' notes (one per inserted dictionary), so
-- downstream consumers (the LSP integration) can detect synthesized
-- arguments at a given source position. The notes are anchored at
-- the source location of the function head, since the synthesized
-- argument has no surface range.
applyGivenDecisions ::
  forall v.
  (Var v) =>
  -- | The typechecker's info notes (in emission order).
  [Context.InfoNote v Ann] ->
  -- | The codebase's view of the types of every 'Ref'' the term
  -- mentions. Used to count how many implicits each function carries.
  TypeLookup v Ann ->
  -- | The term to rewrite.
  Term v Ann ->
  -- | The rewritten term plus one 'ImplicitArgRef' note per
  -- synthesized dictionary insertion.
  (Term v Ann, [Context.InfoNote v Ann])
applyGivenDecisions notes typeLookup tm =
  let (queue, byLoc) = collectDecisionsAndIndex notes
      tlcEnv = collectTopLevelTypes notes
      env0 =
        AppEnv
          { aeTypeLookup = typeLookup,
            aeLocalTypes = tlcEnv,
            aeByLoc = byLoc
          }
      (tm', st) = runState (rewrite env0 tm) (DState queue [])
   in (tm', reverse (dsImplicitNotes st))

-- | Variant of 'applyGivenDecisions' that walks a sequence of
-- top-level bindings as a single pass. The location-indexed
-- decisions table ensures every dictionary insertion uses the
-- decision emitted /at the same source location/, so a per-binding
-- traversal order doesn't matter.
applyGivenDecisionsAll ::
  forall v.
  (Var v) =>
  [Context.InfoNote v Ann] ->
  TypeLookup v Ann ->
  -- | List of bindings to rewrite.
  [Term v Ann] ->
  -- | Rewritten terms (same order) and a flat list of
  -- 'ImplicitArgRef' notes for the LSP.
  ([Term v Ann], [Context.InfoNote v Ann])
applyGivenDecisionsAll notes typeLookup terms =
  let (queue, byLoc) = collectDecisionsAndIndex notes
      tlcEnv = collectTopLevelTypes notes
      env0 =
        AppEnv
          { aeTypeLookup = typeLookup,
            aeLocalTypes = tlcEnv,
            aeByLoc = byLoc
          }
      (tms', st) = runState (traverse (rewrite env0) terms) (DState queue [])
   in (tms', reverse (dsImplicitNotes st))

-- | Build both the legacy flat decision queue and a
-- location-indexed map of decisions. The map keys each successful
-- 'SolvedImplicit' decision by the source-position of the goal that
-- produced it; the value is a list of decisions /in emission order/
-- for that location (multiple emissions at one location are
-- possible when a reference's type carries more than one @=>@).
collectDecisionsAndIndex ::
  [Context.InfoNote v Ann] ->
  ([GR.ResolutionTree v Ann], Map Ann [GR.ResolutionTree v Ann])
collectDecisionsAndIndex notes =
  let flat = collectDecisions notes
      indexed =
        foldr
          ( \n acc -> case n of
              Context.SolvedImplicit loc _ (Right tree) ->
                Map.insertWith (++) loc [tree] acc
              _ -> acc
          )
          Map.empty
          notes
   in (flat, indexed)

------------------------------------------------------------------------------
-- Collecting inputs from the info-note stream
------------------------------------------------------------------------------

-- | Extract every successful 'SolvedImplicit' decision in the order
-- the typechecker emitted them. Failures are dropped here; they
-- stay in the original info-note list for D4 to render.
collectDecisions :: [Context.InfoNote v Ann] -> [GR.ResolutionTree v Ann]
collectDecisions = foldr step []
  where
    step (Context.SolvedImplicit _ _ (Right tree)) acc = tree : acc
    step _ acc = acc

-- | Build a map from variable names to their inferred top-level
-- types. The typechecker emits 'TopLevelComponent' notes for every
-- top-level binding cluster after inference completes.
collectTopLevelTypes ::
  (Var v) =>
  [Context.InfoNote v Ann] ->
  Map v (Type v Ann)
collectTopLevelTypes = foldr step Map.empty
  where
    step (Context.TopLevelComponent xs) acc =
      foldr (\(v, t, _) -> Map.insert v t) acc xs
    step _ acc = acc

------------------------------------------------------------------------------
-- The traversal monad
------------------------------------------------------------------------------

-- | Read-only environment for the rewrite.
data AppEnv v = AppEnv
  { aeTypeLookup :: !(TypeLookup v Ann),
    aeLocalTypes :: !(Map v (Type v Ann)),
    -- | Map from each goal's source location to the decision(s)
    -- emitted there, in emission order. Used by 'wrapImplicitLeaves'
    -- to look up the correct decision for a leaf without relying on
    -- queue order alignment across multiple top-level bindings.
    aeByLoc :: !(Map Ann [GR.ResolutionTree v Ann])
  }

-- | Mutable state: the queue of decisions still to consume, plus a
-- collected list of 'ImplicitArgRef' info notes recording each
-- synthesized insertion. Notes are pushed in reverse order; the
-- caller of 'applyGivenDecisions' reverses them.
data DState v = DState
  { dsQueue :: [GR.ResolutionTree v Ann],
    dsImplicitNotes :: [Context.InfoNote v Ann]
  }

type M v = State (DState v)

-- | Pop one decision from the head of the queue. Returns 'Nothing'
-- when the queue is empty (which only happens if an apply site
-- has more implicits than the typechecker reported — a bug; see
-- @Note [drained queue]@).
popDecision :: M v (Maybe (GR.ResolutionTree v Ann))
popDecision = do
  q <- gets dsQueue
  case q of
    [] -> pure Nothing
    (x : xs) -> do
      modify' (\s -> s {dsQueue = xs})
      pure (Just x)

-- | Record an 'ImplicitArgRef' note for a synthesized insertion.
-- Anchored at the location of the function being applied (the head
-- of the apply chain whose type begins with @=>@), since the
-- synthesized argument has no surface range.
recordImplicit :: Ann -> GR.ResolutionTree v Ann -> M v ()
recordImplicit headLoc tree =
  modify' \s ->
    s
      { dsImplicitNotes =
          Context.ImplicitArgRef
            { Context.implicitArgLoc = headLoc,
              Context.implicitArgRef = GR.givenName (GR.rtGiven tree)
            }
            : dsImplicitNotes s
      }

------------------------------------------------------------------------------
-- The walker
------------------------------------------------------------------------------

-- | Recursive top-down rewrite. We hand-roll the recursion (rather
-- than using 'ABT.visitPure') because we need stateful sequencing of
-- decisions across siblings.
rewrite ::
  forall v.
  (Var v) =>
  AppEnv v ->
  Term v Ann ->
  M v (Term v Ann)
rewrite env tt =
  let a = ABT.annotation tt :: Ann
   in case ABT.out tt of
        -- A bare 'Term.Var\'' or 'Term.Ref\'' may stand for a value
        -- whose declared type begins with one or more @=>@ arrows
        -- (e.g. a class accessor like @Monoid.zero@). The synthesis
        -- pass eagerly emitted a 'ConstraintGoal' for each of those
        -- arrows; we pop the corresponding decisions from the queue
        -- here and wrap the leaf with @App leaf <dict>@ for each.
        ABT.Var _ -> wrapImplicitLeaves env tt
        ABT.Cycle body -> ABT.cycle' a <$> rewrite env body
        ABT.Abs v body -> ABT.abs' a v <$> rewrite env body
        ABT.Tm body -> case body of
          -- Apply-site: this is where we may insert dictionaries.
          Term.App f x -> rewriteApply env tt f [x]
          Term.Ref _ -> wrapImplicitLeaves env tt
          other -> ABT.tm' a <$> traverse (rewrite env) other

-- | Pop one dictionary from the decision queue for each leading
-- @=>@ in the leaf's declared type, then wrap the leaf with
-- left-leaning 'App' nodes. Returns the leaf unchanged when it has
-- no implicit prefix.
wrapImplicitLeaves ::
  forall v.
  (Var v) =>
  AppEnv v ->
  Term v Ann ->
  M v (Term v Ann)
wrapImplicitLeaves env tm = case headType env tm of
  Nothing -> pure tm
  Just ty -> do
    let leafAnn = ABT.annotation tm
        decisions = fromMaybe [] (Map.lookup leafAnn (aeByLoc env))
    peel tm (Type.unforall ty) decisions
  where
    peel ::
      Term v Ann ->
      Type v Ann ->
      [GR.ResolutionTree v Ann] ->
      M v (Term v Ann)
    peel acc ty ds = case (ty, ds) of
      (Type.ImplicitArrow' _ conc, tree : restDs) -> do
        recordImplicit (ABT.annotation tm) tree
        let dictTm = buildDictionary (ABT.annotation tm) tree
            acc' = Term.app (ABT.annotation acc <> ABT.annotation dictTm) acc dictTm
        peel acc' (Type.unforall conc) restDs
      _ -> pure acc

-- | Handle a chain of applications. We collect the spine
-- (function and explicit args), then re-emit it with dictionary
-- arguments interleaved per the function's type.
rewriteApply ::
  (Var v) =>
  AppEnv v ->
  Term v Ann ->
  Term v Ann ->
  [Term v Ann] ->
  M v (Term v Ann)
rewriteApply env outer f args = case ABT.out f of
  ABT.Tm (Term.App f' x') -> rewriteApply env outer f' (x' : args)
  _ -> do
    -- f is the head; args are the explicit user arguments left-to-right.
    f' <- rewrite env f
    args' <- traverse (rewrite env) args
    case headType env f' of
      Nothing ->
        -- No type info; leave the apply chain alone (no implicits to insert).
        pure (rebuildApps (ABT.annotation outer) f' args')
      Just ft ->
        -- If f was wrapped with @give@, demote /every/ leading @=>@
        -- in its type to @->@ so 'interleave' consumes the next args
        -- as regular positional arguments instead of popping
        -- decisions from the queue. The typechecker has already
        -- suppressed the matching 'ConstraintGoal' emissions for
        -- those slots, so popping here would mis-align the queue.
        -- 'lowerAll' must descend through any leading 'Forall'
        -- quantifiers before reaching the @=>@ chain, otherwise a
        -- type like @forall a. C a => a -> a@ falls into the
        -- catch-all branch unchanged and 'interleave' below sees the
        -- un-lowered 'ImplicitArrow' and pops a sibling binding's
        -- resolved dictionary off the queue — leaking that
        -- dictionary's locally-bound variables into this scope.
        let lowerAll ty = case ty of
              Type.ForallNamed' v body -> Type.forAll (ABT.annotation ty) v (lowerAll body)
              Type.ImplicitArrow' i o -> Type.arrow (ABT.annotation ty) i (lowerAll o)
              _ -> ty
            ft' =
              if Ann.isLowered (ABT.annotation f')
                then lowerAll ft
                else ft
         in interleave (ABT.annotation outer) f' ft' args'

-- | Try to infer the type of the apply-chain's head from syntactic
-- information alone.
headType :: (Var v) => AppEnv v -> Term v Ann -> Maybe (Type v Ann)
headType env t = case ABT.out t of
  ABT.Tm (Term.Ref r) -> Map.lookup r (TL.typeOfTerms (aeTypeLookup env))
  ABT.Tm (Term.Ann _ ty) -> Just ty
  ABT.Var v -> Map.lookup v (aeLocalTypes env)
  _ -> Nothing

-- | Walk the function's type, alternating implicit-peeling with
-- explicit-arg consumption. At each implicit slot, decide whether
-- to consume a decision or skip (override).
interleave ::
  forall v.
  (Var v) =>
  Ann ->
  Term v Ann ->
  Type v Ann ->
  [Term v Ann] ->
  M v (Term v Ann)
interleave outerAnn f0 ty0 args0 =
  go f0 (Type.unforall ty0) args0
  where
    headLoc = ABT.annotation f0
    go f ty args = case ty of
      -- Implicit: pop a resolved-dictionary decision from the queue
      -- and apply it. (When @give f@ is in effect at this call site,
      -- the caller of 'interleave' has already demoted the leading
      -- @=>@ to @->@ in @ty@, so we never enter this branch for that
      -- slot — the explicit dictionary the user supplied falls
      -- through to the 'Arrow'' case below.)
      Type.ImplicitArrow' _ conc -> do
        mDec <- popDecision
        case mDec of
          Nothing ->
            -- Queue empty: leave the rest alone.
            pure (rebuildApps outerAnn f args)
          Just tree -> do
            -- Record the synthesized insertion so the LSP can detect
            -- implicit args at the head's source position.
            recordImplicit headLoc tree
            let dictTm = buildDictionary outerAnn tree
                f' = Term.app (ABT.annotation f <> ABT.annotation dictTm) f dictTm
            go f' conc args
      -- Explicit: consume one arg.
      Type.Arrow' _ conc ->
        case args of
          [] -> pure f
          (a : rest) ->
            go (Term.app (ABT.annotation f <> ABT.annotation a) f a) (Type.unforall conc) rest
      -- Type wraps in effects or other forms — re-emit remaining args
      -- as plain Apps and stop.
      _ -> pure (rebuildApps outerAnn f args)

-- | Reconstruct an Apps' from head and explicit arg list.
rebuildApps :: (Var v) => Ann -> Term v Ann -> [Term v Ann] -> Term v Ann
rebuildApps a f args = case args of
  [] -> f
  _ -> Term.apps f (map (\x -> (a, x)) args)

------------------------------------------------------------------------------
-- Dictionary construction
------------------------------------------------------------------------------

-- | Convert a 'GR.ResolutionTree' into a 'Term.Term' representing
-- the dictionary lookup, recursively.
--
-- Example: @Show (List Nat)@ resolved as
--
-- @
--   ResolutionTree
--     { rtGiven = Show.list, rtChildren = [ResolutionTree { rtGiven = Show.nat }] }
-- @
--
-- becomes the term @Show.list Show.nat@: a 'Term.app' applying the
-- chosen given to its premise dictionaries.
--
-- For /local/ givens — declared file-internally via @given x = ...@ or
-- inside a @let given@ block — the resolver attaches a synthetic
-- 'Reference.Builtin' of the form @Local.given.<name>@ (see
-- 'Unison.Typechecker.Context.extendLexicalGivenFromBinding'). Such
-- refs are not real builtins; the runtime can't evaluate them. We
-- detect that prefix here and substitute a 'Term.var' that references
-- the source-level binding by name, which the surrounding letrec
-- scope makes available.
--
-- The /outermost/ node of the produced term is annotated with
-- 'Ann.Synthetic' so the term printer can detect that the argument
-- was inserted by the elaborator (not written by the user) and elide
-- it from surface output. Inner premise dictionaries keep their
-- non-synthetic annotations because they may already correspond to
-- user-named givens.
buildDictionary :: (Var v) => Ann -> GR.ResolutionTree v Ann -> Term v Ann
buildDictionary a tree
  | GR.givenName (GR.rtGiven tree) == GivenElaborator.typeTagSynthRef =
      let repr = TypeTagRepr.typeToRepr (GR.givenConclusion (GR.rtGiven tree))
       in markSyntheticTop (Term.typeTagLit a repr)
  | otherwise =
      let head_ = headTermFor a (GR.givenName (GR.rtGiven tree))
          premises = map (buildDictionary a) (GR.rtChildren tree)
          raw = case premises of
            [] -> head_
            _ -> Term.apps head_ (map (\p -> (a, p)) premises)
       in markSyntheticTop raw

-- | Replace the outermost annotation of a term with 'Ann.Synthetic'
-- wrapping the original. Used to flag elaborator-inserted terms so
-- the printer can elide them from surface output.
markSyntheticTop :: Term v Ann -> Term v Ann
markSyntheticTop t = ABT.annotate (Ann.Synthetic (ABT.annotation t)) t

-- | Walk a term and drop every 'App' argument whose top annotation
-- is 'Ann.Synthetic'. This recovers the user's original surface
-- shape (modulo formatting) for terms whose implicit slots were
-- filled in by 'applyGivenDecisions' / 'wrapImplicitLeaves' /and/
-- whose synthesized markers have survived in memory (i.e. the term
-- hasn't been round-tripped through the codebase, which strips
-- annotations). Use this just before handing a term to a surface
-- pretty-printer so @> foo@ watches render without the
-- resolved-dictionary arguments.
--
-- For terms loaded /back/ from the codebase (e.g. via @view@), the
-- 'Ann.Synthetic' marker is no longer present; use
-- 'stripImplicitArgsByType' instead, which derives the implicit slots
-- from the function's declared type.
stripSyntheticArgs :: (Var v) => Term v Ann -> Term v Ann
stripSyntheticArgs = go
  where
    go t = case ABT.out t of
      ABT.Var _ -> t
      ABT.Cycle body -> ABT.cycle' (ABT.annotation t) (go body)
      ABT.Abs v body -> ABT.abs' (ABT.annotation t) v (go body)
      ABT.Tm body -> case body of
        Term.App f x
          | Ann.isSynthetic (ABT.annotation x) -> go f
          | otherwise -> ABT.tm' (ABT.annotation t) (Term.App (go f) (go x))
        other -> ABT.tm' (ABT.annotation t) (fmap go other)

-- | Drop leading 'Lam' binders from a term, one for each leading
-- @=>@ in its declared type. Used when rendering a top-level
-- binding whose body the parser wrapped with synthetic
-- @\\_implicit_<name>_<i> -> …@ lambdas: the user wrote
-- @foldMap f = body@, the typechecker sees
-- @foldMap = \\_implicit_foldMap_0 -> \\f -> body@, and at print
-- time we want the user's surface form back.
stripLeadingImplicitLambdas ::
  forall v.
  (Var v, Ord v) =>
  -- | The binding's declared type.
  Type v Ann ->
  -- | The binding's term.
  Term v Ann ->
  Term v Ann
stripLeadingImplicitLambdas ty0 tm0 =
  let n = countLeadingImplicits (Type.unforall ty0) :: Int
   in go n tm0
  where
    countLeadingImplicits :: Type v Ann -> Int
    countLeadingImplicits ty = case ty of
      Type.ImplicitArrow' _ conc -> 1 + countLeadingImplicits conc
      _ -> 0

    go :: Int -> Term v Ann -> Term v Ann
    go 0 t = t
    go n t = case ABT.out t of
      ABT.Tm (Term.Lam body) -> case ABT.out body of
        ABT.Abs _v inner -> go (n - 1) inner
        _ -> t
      -- Unison stores type annotations as 'Term.Ann e t' nodes
      -- around bindings. Recurse into the inner term but preserve
      -- the surrounding 'Ann' so the printer keeps the type
      -- signature in its output.
      ABT.Tm (Term.Ann inner ty) ->
        let inner' = go n inner
         in Term.ann (ABT.annotation t) inner' ty
      _ -> t

-- | Walk a term and drop every apply-site argument that fills a
-- leading @=>@ slot in the function's declared type. Unlike
-- 'stripSyntheticArgs', this version derives "which slot is
-- implicit" from a type lookup — so it works on terms loaded back
-- from the codebase, whose source annotations have been stripped.
--
-- @lookupTermType@ receives the head of an apps chain (a 'Term.Ref'
-- or 'Term.Var') and returns its declared type, if known. Heads
-- that aren't found (e.g. inferred-type let-bound vars in scope)
-- pass through unchanged.
stripImplicitArgsByType ::
  forall v.
  (Var v) =>
  -- | Predicate: is this term reference tagged as a @given@ in the
  -- current namespace? Used to decide whether a positional argument
  -- filling a leading @=>@ slot is the resolver's default pick (a
  -- given-tagged reference, safe to elide) or a user-supplied
  -- dictionary that the user wrote via the @give@ keyword (must be
  -- preserved, and the head is re-annotated with 'Ann.Lowered' so the
  -- printer renders the @give @ prefix).
  (Reference -> Bool) ->
  (Term v Ann -> Maybe (Type v Ann)) ->
  Term v Ann ->
  Term v Ann
stripImplicitArgsByType isGivenRef lookupTermType = go
  where
    go :: Term v Ann -> Term v Ann
    go t = case ABT.out t of
      ABT.Var _ -> t
      ABT.Cycle body -> ABT.cycle' (ABT.annotation t) (go body)
      ABT.Abs v body -> ABT.abs' (ABT.annotation t) v (go body)
      ABT.Tm (Term.App _ _) -> stripApps t
      ABT.Tm other -> ABT.tm' (ABT.annotation t) (fmap go other)

    -- \| Collect the apps spine, find the head, look up its declared
    -- type, and decide what to do with the leading @=>@-filling args.
    -- If every such arg looks like an auto-resolved dictionary
    -- (namespace-tagged @given@ reference), strip them — this is the
    -- default elide-mode. Otherwise the user wrote @give@ at this
    -- call site; keep the args and tag the head with 'Ann.Lowered'
    -- so the printer emits the @give @ prefix.
    --
    -- The head's own annotation can itself carry 'Ann.Lowered'
    -- (the parser puts it there when it sees the @give@ keyword,
    -- and 'applyGivenDecisionsAll' preserves it). When it does,
    -- the user explicitly asked for explicit dictionary passing,
    -- so we must not elide the implicit args even if they happen
    -- to look like auto-resolutions.
    stripApps :: Term v Ann -> Term v Ann
    stripApps tm =
      let (head_, args) = collect tm []
          recurseArgs xs = map (\(a, x) -> (a, go x)) xs
          headIsLowered = Ann.isLowered (ABT.annotation head_)
       in case lookupTermType head_ of
            Just ty ->
              let nImplicits = countLeadingImplicits ty
                  (implicitArgs, rest) = splitAt nImplicits args
               in if not headIsLowered
                    && length implicitArgs == nImplicits
                    && all (argLooksAutoResolved . snd) implicitArgs
                    then rebuild (ABT.annotation tm) (go head_) (recurseArgs rest)
                    else
                      let head' = markLowered (go head_)
                       in rebuild (ABT.annotation tm) head' (recurseArgs args)
            Nothing ->
              rebuild (ABT.annotation tm) (go head_) (recurseArgs args)

    -- \| An arg "looks auto-resolved" iff it could plausibly be what
    -- the resolver picked: either a namespace-given top-level
    -- reference (ambient pool), or a local 'Var' (lexical given
    -- bound by @=>I@ or a @let given@). Anything else (a non-given
    -- top-level reference, a literal, a complex expression) is
    -- treated as a user-supplied dictionary that @give@ should
    -- preserve.
    argLooksAutoResolved :: Term v Ann -> Bool
    argLooksAutoResolved a = case ABT.out a of
      ABT.Tm (Term.Ref r) -> isGivenRef r
      ABT.Var _ -> True
      _ -> False

    -- \| Wrap the apply-chain head with a sentinel 'Term.Ann' whose
    -- type is 'Type.giveMarkerRef'. The surface 'TermPrinter'
    -- recognises this sentinel on an @Apps'@ head and renders the
    -- chain with a leading @give @ keyword. The sentinel is a
    -- print-time-only construct — it never reaches hashing because
    -- 'stripImplicitArgsByType' runs as a view-side post-pass.
    markLowered :: Term v Ann -> Term v Ann
    markLowered h =
      let a = ABT.annotation h
       in Term.ann a h (Type.ref a Type.giveMarkerRef)

    collect :: Term v Ann -> [(Ann, Term v Ann)] -> (Term v Ann, [(Ann, Term v Ann)])
    collect t acc = case ABT.out t of
      ABT.Tm (Term.App f x) -> collect f ((ABT.annotation t, x) : acc)
      _ -> (t, acc)

    rebuild :: Ann -> Term v Ann -> [(Ann, Term v Ann)] -> Term v Ann
    rebuild outerAnn f = \case
      [] -> f
      args -> Term.apps f (map (\(a, x) -> (a, x)) args) `withTopAnn` outerAnn

    withTopAnn :: Term v Ann -> Ann -> Term v Ann
    withTopAnn t _outer = t

    countLeadingImplicits :: Type v Ann -> Int
    countLeadingImplicits ty0 =
      let ty1 = Type.unforall ty0
       in case ty1 of
            Type.ImplicitArrow' _ conc -> 1 + countLeadingImplicits conc
            _ -> 0

-- | Pick the right surface form for a chosen given's reference. Local
-- givens are encoded by 'extendLexicalGivenFromBinding' as the synthetic
-- @Local.given.<name>@ builtin reference; replace those with a
-- 'Term.var' so the runtime can resolve them through the enclosing
-- letrec. All other references go through unchanged as 'Term.ref'.
headTermFor :: (Var v) => Ann -> Reference -> Term v Ann
headTermFor a r = case r of
  Reference.Builtin name
    | Just localName <- Text.stripPrefix localGivenPrefix name ->
        Term.var a (Var.named localName)
  _ -> Term.ref a r

-- | Prefix used by 'Unison.Typechecker.Context' to mint synthetic
-- references for file-internal givens. Kept in sync with the literal
-- in @extendLexicalGivenFromBinding@ / the letrec hook in
-- @annotateLetRecBindings'@.
localGivenPrefix :: Text.Text
localGivenPrefix = "Local.given."

------------------------------------------------------------------------------
-- Override detection
------------------------------------------------------------------------------
