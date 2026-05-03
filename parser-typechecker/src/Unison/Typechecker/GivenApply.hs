{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Phase-2 chunk D3: post-typecheck pass that walks a term and
-- substitutes resolved implicit arguments into 'App' nodes, mirroring
-- the @applyTdnrDecisions@ pattern (see
-- @parser-typechecker/src/Unison/FileParsers.hs:329@).
--
-- ## Inputs
--
-- * The list of 'Context.InfoNote' values produced by inference.
--   The relevant ones are:
--
--     * 'Context.SolvedImplicit': one per resolved implicit slot,
--       carrying either a 'GR.ResolutionTree' (success) or a
--       'GR.ResolveError' (failure). Errors are left in place for
--       chunk D4 to surface; D3 only consumes successes.
--
--     * 'Context.TopLevelComponent': the inferred types of let-rec
--       bindings, used to look up the types of locally-defined
--       functions referenced by 'Term.Var''.
--
-- * A 'TL.TypeLookup': the typechecker's view of every 'Term.Ref''
--   the term mentions. Used to compute how many leading
--   'Type.ImplicitArrow's a function carries before its first explicit
--   argument.
--
-- ## Strategy
--
-- The typechecker emits 'SolvedImplicit' notes in left-to-right
-- /synthesis/ order: that is, the order in which 'synthesizeApp'
-- peeled 'ImplicitArrow' arrows during inference. Apply-sites are
-- visited top-down, function-first, then arguments left-to-right.
--
-- D3 walks the term in the same order, holding a queue of pending
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
--  3. **Override skip (per A3's annotation-widening contract).**
--     Before consuming a decision, examine the /next user-supplied
--     argument/. If its annotation has been widened by A3's
--     @\@@-positional override syntax (i.e. the outer 'Ann' starts at
--     a column strictly /before/ the start of the leftmost annotation
--     among its sub-terms), the user has already supplied this
--     implicit explicitly. We do /not/ pop a decision, leave the arg
--     untouched, and move on.
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
-- D3 leaves error decisions ('Left ResolveError') untouched: the term
-- keeps the original arity, and a downstream rendering pass (chunk
-- D4) translates the error into a user-facing diagnostic. This is
-- the same separation TDNR uses today: 'applyTdnrDecisions' silently
-- skips 'SolvedBlank' notes whose 'Resolution' yielded no
-- substitution; the user-facing error surfaces from the stored
-- 'Suggestion's via 'Result.Note'.
--
-- ## Limitations
--
-- * The override detection works only when the override-argument has
--   sub-term annotations to compare against. For a single-leaf
--   override (e.g. @f \@ d@ where @d@ is a bare identifier) the
--   inner annotation is unavailable and we fall back to /not/
--   treating the slot as overridden. The user-visible effect is the
--   same as before A3 landed: the resolver's chosen dictionary will
--   be used. To force an override on a single leaf, the user can
--   wrap the argument in parentheses (@f \@ (d)@) — the parens
--   produce a non-leaf node whose inner annotation is recoverable.
--   This limitation is recorded in chunks.md as a follow-up for
--   chunk A4.
--
-- * For function expressions whose type cannot be looked up
--   syntactically (e.g. a higher-order callback, a complex
--   expression head), D3 leaves the apply site alone. In practice
--   such functions cannot have implicit parameters in their
--   /surface/ type, since 'ImplicitArrow' is only valid in declared
--   signatures (per ADR-019); inferred types of expressions never
--   contain 'ImplicitArrow'. So this is not a soundness gap, just a
--   reminder that D3 is syntax-directed.
module Unison.Typechecker.GivenApply
  ( -- * Top-level entry point
    applyGivenDecisions,

    -- * Building dictionary terms
    buildDictionary,

    -- * Override detection (exposed for tests)
    isOverrideArg,
  )
where

import Control.Monad.State.Strict (State, gets, modify', runState)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Unison.ABT qualified as ABT
import Unison.Lexer.Pos qualified as L
import Unison.Parser.Ann (Ann (..))
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.GivenResolver qualified as GR
import Unison.Typechecker.TypeLookup (TypeLookup)
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Var (Var)

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
-- downstream consumers (chunk F1's LSP integration) can detect
-- synthesized arguments at a given source position. The notes are
-- anchored at the source location of the function head, since the
-- synthesized argument has no surface range.
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
  let queue = collectDecisions notes
      tlcEnv = collectTopLevelTypes notes
      env0 =
        AppEnv
          { aeTypeLookup = typeLookup,
            aeLocalTypes = tlcEnv
          }
      (tm', st) = runState (rewrite env0 tm) (DState queue [])
   in (tm', reverse (dsImplicitNotes st))

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
    aeLocalTypes :: !(Map v (Type v Ann))
  }

-- | Mutable state: the queue of decisions still to consume, plus a
-- collected list of 'ImplicitArgRef' info notes (chunk F1) recording
-- each synthesized insertion. Notes are pushed in reverse order; the
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
        ABT.Var _ -> pure tt
        ABT.Cycle body -> ABT.cycle' a <$> rewrite env body
        ABT.Abs v body -> ABT.abs' a v <$> rewrite env body
        ABT.Tm body -> case body of
          -- Apply-site: this is where we may insert dictionaries.
          Term.App f x -> rewriteApply env tt f [x]
          other -> ABT.tm' a <$> traverse (rewrite env) other

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
        interleave (ABT.annotation outer) f' ft args'

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
      -- Implicit: try to fill it.
      Type.ImplicitArrow' _ conc -> do
        case args of
          [] -> pure (rebuildApps outerAnn f args)
          (a : rest) ->
            if isOverrideArg a
              then -- User supplied this implicit explicitly; consume the arg
              -- and keep going.
                go (Term.app (ABT.annotation f <> ABT.annotation a) f a) conc rest
              else do
                mDec <- popDecision
                case mDec of
                  Nothing ->
                    -- Queue empty: leave the rest alone.
                    pure (rebuildApps outerAnn f args)
                  Just tree -> do
                    -- Chunk F1: record the synthesized insertion so
                    -- the LSP can detect implicit args at the head's
                    -- source position.
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
buildDictionary :: (Var v) => Ann -> GR.ResolutionTree v Ann -> Term v Ann
buildDictionary a tree =
  let head_ = Term.ref a (GR.givenName (GR.rtGiven tree))
      premises = map (buildDictionary a) (GR.rtChildren tree)
   in case premises of
        [] -> head_
        _ -> Term.apps head_ (map (\p -> (a, p)) premises)

------------------------------------------------------------------------------
-- Override detection
------------------------------------------------------------------------------

-- | True iff the argument's annotation has been widened by A3's
-- @\@@-positional override syntax (per
-- @parser-typechecker/src/Unison/Syntax/TermParser.hs:~967@):
--
-- @
--   overrideArg = do
--     atTok <- reserved "@"
--     d <- termLeaf
--     let widened = ann atTok <> ann d
--     pure d {ABT.annotation = widened}
-- @
--
-- The widening shifts the outer annotation's start strictly to the
-- left of the inner term's natural start. We detect this by
-- comparing the outer annotation's start position to the start of
-- the leftmost annotation among the term's sub-children. If the
-- outer is strictly earlier, the term was widened.
--
-- For terms with no inner sub-children (a single 'Var', 'Ref',
-- 'Builtin', or literal), we cannot reliably tell from annotations
-- alone — see the module-level comment for the limitation. We
-- return 'False' in that case (treat as non-override).
isOverrideArg :: Term v Ann -> Bool
isOverrideArg t =
  case ABT.annotation t of
    Ann outerStart _ ->
      case innerLeftmostStart t of
        Just innerStart -> outerStart < innerStart
        Nothing -> False
    _ -> False

-- | Find the leftmost source-position start among the term's direct
-- structural children, if any. Returns 'Nothing' for a leaf.
innerLeftmostStart :: Term v Ann -> Maybe L.Pos
innerLeftmostStart t = case ABT.out t of
  ABT.Var _ -> Nothing
  ABT.Cycle body -> annStart (ABT.annotation body)
  ABT.Abs _ body -> annStart (ABT.annotation body)
  ABT.Tm body ->
    let kids = foldMap (\c -> [ABT.annotation c]) body
        starts = [s | a <- kids, Just s <- [annStart a]]
     in case starts of
          [] -> Nothing
          xs -> Just (minimum xs)

annStart :: Ann -> Maybe L.Pos
annStart = \case
  Ann s _ -> Just s
  GeneratedFrom a -> annStart a
  _ -> Nothing
