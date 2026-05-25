{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Phase-2 chunk D2: drive the standalone resolver
-- ('Unison.Typechecker.GivenResolver') from real typechecker output.
--
-- D1 ported the resolver into the codebase as a pure function on
-- @Type.Type v loc@. C2.1 added a lexical given environment to
-- 'Unison.Typechecker.Context'. C2.2 emits 'Context.ConstraintGoal'
-- info notes at every @ImplicitArrow@ apply-site, each carrying the
-- goal type, the apply-site location, and a snapshot of the lexical
-- given environment.
--
-- This module is the bridge: given a sequence of typechecker info
-- notes plus a list of /ambient/ givens (top-level givens harvested
-- from the namespace per @Unison.Codebase.Givens.namesMarkedGiven@,
-- chunk B1), it constructs a per-goal 'GR.Pool' and calls 'GR.resolve'
-- once per goal. The verdict is recorded as a 'Context.SolvedImplicit'
-- info note so that the D3 post-pass can walk the term and substitute
-- the resolved dictionary terms back into the AST — exactly how
-- 'applyTdnrDecisions' walks 'Context.Decision' notes today (see
-- @parser-typechecker/src/Unison/FileParsers.hs:329@).
--
-- ## Pool construction
--
-- Each goal has its own pool because the lexical-given snapshot is
-- per-goal (the goal at a deep let-binding sees more local givens than
-- the goal at top level). The ambient pool is shared across all goals
-- in the file.
--
-- For each goal:
--
-- 1. Take the 'goalScope' map (lexical givens captured by C2.1) and
--    decompose each entry's type with 'decomposeGivenType'. The
--    resulting 'GR.Given' records are tagged with @Lexical 0@ — D2
--    does not yet preserve binder-depth, so all lexical givens are
--    treated as equally inner. ADR-008's most-inner-wins rule still
--    chooses lexical over ambient correctly because @Lexical 0 < Ambient@.
--
-- 2. Append the supplied ambient pool, tagged @Ambient@.
--
-- 3. Run 'GR.resolve' on the goal type.
--
-- ## Decomposition (top-level givens)
--
-- The user-facing API for top-level givens is a name in the namespace
-- whose 'MdValues' contains 'Unison.Codebase.Givens.givenSentinel' (B1).
-- The caller (FileParsers / the file-elaboration entry point) is
-- responsible for resolving each such name to its 'Reference' and
-- 'Type.Type' and passing the result here as a list of 'AmbientGiven'
-- records.
--
-- The decomposition itself is straightforward: strip leading @forall@s
-- to extract 'givenTyVars'; walk the @ImplicitArrow@ chain in the body
-- to extract premises; what remains is the conclusion. (Outer 'Arrow's
-- — i.e. value-level parameters — are part of the conclusion. A
-- @Show a => List a -> Text@ given concludes @List a -> Text@, with
-- premise @Show a@.)
--
-- ## Lexical-depth convention
--
-- Per ADR-008 the resolver picks lexical-inner candidates over
-- lexical-outer ones at the same subsumption level. The 'goalScope'
-- snapshot we receive from C2.1 is a flat 'Map' that does /not/ carry
-- depth — it is simply "every given visible at the apply-site". For
-- D2 we tag every lexical given with @GR.Lexical 0@: the resolver's
-- ordering rule then uses 'Ambient' only when no lexical hit exists,
-- and ties between two equally-lexical givens are broken by
-- specificity (also fine).
--
-- A future chunk may extend C2.1 to track binder depth so the resolver
-- can distinguish two lexical givens of different scopes.
--
-- ## Shared-metavar awareness (ADR-023)
--
-- D2 runs the resolver as a /post-pass/ over the typechecker's info
-- notes, after all inference has reached its fixed point. By that
-- point 'substituteSolved' (in 'Context') has already been applied to
-- every 'ConstraintGoal' (its wildcard arm propagates the final
-- context substitution into 'goalType' and 'goalScope'). The types we
-- pass to the resolver therefore contain only those existentials that
-- remained unsolved at the end of inference — and the resolver's
-- standalone unifier treats them as flexible variables when the
-- candidate's freshened skolems suggest, and otherwise rigid.
--
-- Because we are post-typechecker and not interleaved with it, the
-- 'metavar-invalidation' rule from ADR-023 (Option A) is satisfied
-- trivially: there is no later substitution that can invalidate a
-- memoized resolution. Memoization is per-call to 'GR.resolve' and is
-- discarded between goals; that is sound because each goal carries an
-- independent type, and even when the type variables happen to share
-- names across goals the per-goal memo table is fresh.
--
-- A future refactor that interleaves resolution with the @M v loc@
-- monad — folding the resolver back into the typechecker proper —
-- would need to revisit this and either invalidate memo entries on
-- substitution (per ADR-023) or restrict memoization to fully-applied
-- goal types. We defer that decision; D2 stays standalone.
module Unison.Typechecker.GivenElaborator
  ( -- * Top-level givens
    AmbientGiven (..),
    ambientPool,
    mergePool,
    decomposeGivenType,

    -- * Goal-driven elaboration
    elaborateGoals,
    elaborateInfoNotes,

    -- * Diagnostics
    extractConstraintGoals,
    implicitDecisions,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Unison.Reference (Reference)
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.GivenResolver qualified as GR
import Unison.Typechecker.TypeVar qualified as TypeVar
import Unison.Var (Var)

------------------------------------------------------------------------------
-- Ambient givens (top-level / namespace)
------------------------------------------------------------------------------

-- | A top-level given fetched from the namespace. The caller is
-- responsible for filtering to /just/ those names the user marked
-- with 'Unison.Codebase.Givens.givenSentinel' and looking up each
-- one's 'Reference' and full type signature in the codebase.
--
-- Decomposition into @forall@-bound type variables, premises, and
-- conclusion is performed by 'decomposeGivenType'.
data AmbientGiven v loc = AmbientGiven
  { ambientName :: !Reference,
    ambientType :: !(Type v loc)
  }
  deriving stock (Show)

-- | Build a 'GR.Pool' of @Ambient@-scoped givens from a list of
-- top-level givens.
ambientPool :: (Var v) => [AmbientGiven v loc] -> GR.Pool v loc
ambientPool xs = GR.poolFromList (map toGiven xs)
  where
    toGiven (AmbientGiven r ty) =
      let (vs, prems, concl) = decomposeGivenType ty
       in GR.Given
            { GR.givenName = r,
              GR.givenTyVars = vs,
              GR.givenPremises = prems,
              GR.givenConclusion = concl,
              GR.givenScope = GR.Ambient
            }

-- | Decompose a given's declared type into
-- @(forall-bound vars, premises, conclusion)@.
--
-- Strips leading 'Type.ForallNamed'' to extract type variables, then
-- walks the @ImplicitArrow@ chain to peel premises. What remains is
-- the conclusion. Regular @Arrow@s are /not/ peeled — a value-level
-- parameter is part of the conclusion.
--
-- Examples:
--
-- @
--   forall a. Show a => List a -> Text
--     ↳ ([a], [Show a], List a -> Text)
--
--   forall a b. (Eq a, Ord b) => Map a b
--     ↳ ([a, b], [Eq a, Ord b], Map a b)
--
--   List a   (no foralls, no premises)
--     ↳ ([], [], List a)
-- @
decomposeGivenType ::
  (Var v) =>
  Type v loc ->
  ([v], [Type v loc], Type v loc)
decomposeGivenType ty =
  let (vs, body) = Type.unforall' ty
      (prems, concl0) = peelImplicits body
      -- After 'addAbilities' / 'existentializeArrows' the generalized
      -- signature wraps its conclusion in an 'Effect [] _'. The
      -- resolver unifies head-only against a 'ConstraintGoal' whose
      -- type was lowered without that wrapper, so we strip the
      -- (always empty for a constraint) effect row here. Without
      -- this, @forall a. Show a => Show [a]@ stored from
      -- 'topLevelComponents' fails to unify with the goal @Show [Nat]@.
      concl = snd (Type.unEffect0 concl0)
   in (vs, map (snd . Type.unEffect0) prems, concl)
  where
    peelImplicits t = case t of
      Type.ImplicitArrow' i o ->
        let (rest, c) = peelImplicits o
         in (i : rest, c)
      _ -> ([], t)

------------------------------------------------------------------------------
-- Goal-driven elaboration
------------------------------------------------------------------------------

-- | A pending 'Context.ConstraintGoal' projected into surface form.
-- The typechecker emits 'Context.ConstraintGoal' notes whose types
-- carry 'TypeVar' wrappers; we lower them once here so the rest of
-- the elaborator (and the resolver) sees plain 'Type.Type v loc'.
data PendingGoal v loc = PendingGoal
  { pgLoc :: !loc,
    pgType :: !(Type v loc),
    pgScope :: !(Map Reference (Type v loc))
  }
  deriving stock (Show)

-- | Project the 'Context.ConstraintGoal' notes out of a heterogeneous
-- info-note list, lowering each goal's types from
-- 'Context.Type v loc' to surface 'Type.Type v loc'.
extractConstraintGoals ::
  (Var v) =>
  [Context.InfoNote v loc] ->
  [PendingGoal v loc]
extractConstraintGoals = foldr step []
  where
    step (Context.ConstraintGoal loc ty scope) acc =
      PendingGoal
        { pgLoc = loc,
          pgType = TypeVar.lowerType ty,
          pgScope = TypeVar.lowerType <$> scope
        }
        : acc
    step _ acc = acc

-- | Run the resolver on each pending goal, returning one
-- 'Context.SolvedImplicit' info note per goal.
--
-- Each goal's pool is built fresh: it is the union of (the supplied
-- ambient pool) and (the goal's own lexical-givens snapshot, tagged
-- @Lexical 0@). This per-goal construction is what makes lexical
-- shadowing work — a goal that sees a deep @given x@ binding has
-- @x@ in its scope; goals at outer scope do not.
elaborateGoals ::
  (Var v, Ord loc) =>
  -- | Ambient (top-level / namespace) givens, shared by every goal.
  GR.Pool v loc ->
  -- | The goals emitted by the typechecker, projected with
  -- 'extractConstraintGoals'.
  [PendingGoal v loc] ->
  [Context.InfoNote v loc]
elaborateGoals ambient = map (resolveOne ambient)

-- | Convenience: run the entire elaboration pass over a list of info
-- notes. The result is the original list with one 'SolvedImplicit'
-- note appended per 'ConstraintGoal' (the original 'ConstraintGoal'
-- notes are preserved so D4's error rendering still has access to
-- them).
elaborateInfoNotes ::
  (Var v, Ord loc) =>
  GR.Pool v loc ->
  [Context.InfoNote v loc] ->
  [Context.InfoNote v loc]
elaborateInfoNotes ambient infos =
  infos <> elaborateGoals ambient (extractConstraintGoals infos)

-- | Project the 'Context.SolvedImplicit' notes out of a heterogeneous
-- info-note list. Useful for the D3 post-pass and for tests.
implicitDecisions ::
  [Context.InfoNote v loc] ->
  [(loc, Type v loc, Either (GR.ResolveError v loc) (GR.ResolutionTree v loc))]
implicitDecisions = foldr step []
  where
    step (Context.SolvedImplicit loc ty d) acc = (loc, ty, d) : acc
    step _ acc = acc

------------------------------------------------------------------------------
-- The per-goal pool + resolve
------------------------------------------------------------------------------

resolveOne ::
  forall v loc.
  (Var v, Ord loc) =>
  GR.Pool v loc ->
  PendingGoal v loc ->
  Context.InfoNote v loc
resolveOne ambient PendingGoal {pgLoc, pgType, pgScope} =
  let lexical = lexicalPool pgScope
      pool = mergePool lexical ambient
      decision = GR.resolve pool pgType
   in Context.SolvedImplicit
        { Context.implicitGoalLoc = pgLoc,
          Context.implicitGoalType = pgType,
          Context.implicitDecision = decision
        }

-- | Build a 'GR.Pool' of @Lexical 0@-scoped givens from a per-goal
-- 'goalScope' snapshot.
--
-- Each entry @(reference, type)@ is decomposed into @forall@-vars +
-- premises + conclusion exactly like an ambient given; the only
-- difference is the 'GR.givenScope' tag.
--
-- D2 does not yet track binder depth, so every lexical given gets
-- @Lexical 0@. ADR-008's most-inner-wins rule between two lexical
-- givens at the same depth therefore degenerates into the
-- specificity ordering — fine for D2's tests, and revisable in a
-- later chunk by extending C2.1's snapshot to carry depth.
lexicalPool :: (Var v) => Map Reference (Type v loc) -> GR.Pool v loc
lexicalPool m = GR.poolFromList (map mk (Map.toList m))
  where
    mk (r, ty) =
      let (vs, prems, concl) = decomposeGivenType ty
       in GR.Given
            { GR.givenName = r,
              GR.givenTyVars = vs,
              GR.givenPremises = prems,
              GR.givenConclusion = concl,
              GR.givenScope = GR.Lexical 0
            }

mergePool :: GR.Pool v loc -> GR.Pool v loc -> GR.Pool v loc
mergePool a b = GR.poolFromList (GR.poolGivens a <> GR.poolGivens b)
