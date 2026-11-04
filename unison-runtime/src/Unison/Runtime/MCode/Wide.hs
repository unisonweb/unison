{-# LANGUAGE PatternSynonyms #-}

-- | The wide calling convention's escape analysis, as a pass over the ANF
-- (Unison.Runtime.Wide; design.md "The wide calling convention").
--
-- A variable bound by a let whose value arrives by a yield (a call, a
-- match, a handle: everything 'letYields' says) is /wide/: it occupies
-- 'wideWidth' slots, and when the yield delivered a small constructor (at
-- most 'wideK' fields) its value slot holds a witness and the slots below
-- it the fields, with nothing allocated. So is a let of one variable
-- binding a small constructor application, which the emitter packs into
-- the slots ('wideCon'), when the body matches on the variable or returns
-- it somewhere ('usedWide'): those are the uses the slots serve, and a
-- constructor only ever passed on is packed as a heap value at once, since
-- packing it wide and boxing it before the call would be the same
-- allocation plus the box. Such a variable can be matched on and returned
-- as it is. Where it /escapes/ (an argument of a call, a constructor, a
-- record, a primitive or a request; a captured variable of a closure; the
-- value of an affine update) it has to be an ordinary heap value, which
-- a box makes it, in place: after that the variable holds a boxed value
-- for the rest of the path.
--
-- 'wideBody' marks each such binder 'WD' and places a 'TBox' before the
-- first escape of the variable on each path: once per path however many
-- times the variable escapes after, and not at all on a path where it
-- never does, so a value that is only matched on costs no allocation. The
-- result says everything the emitter (Unison.Runtime.MCode) needs: it lays
-- a 'WD' binder out wide, emits a 'Box' for each 'TBox', checks the two
-- invariants ('WD' only on a let of one variable that yields or builds a
-- small constructor; a 'TBox' only of a wide variable in scope) and
-- decides nothing itself. The pass runs last over every body the emitter
-- sees, the unoptimized copy of a rewritten lambda included, so a box is
-- at the same frame in both.
--
-- With the convention off the yielding binders are still marked, since
-- 'wideWidth' is one then and a wide variable is laid out like any other,
-- and no box is placed.
module Unison.Runtime.MCode.Wide
  ( wideBody,
    letYields,
    wideCon,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Unison.ABT.Normalized qualified as ABTN
import Unison.Reference (Reference)
import Unison.Runtime.ANF
  ( ANormal,
    Direction (..),
    Func (..),
    Mem (..),
    pattern TApp,
    pattern TBLit,
    pattern TBox,
    pattern TDiscard,
    pattern TLets,
    pattern TLit,
    pattern TMatch,
    pattern TName,
    pattern TUpdate,
    pattern TVar,
  )
import Unison.Runtime.Wide (wideK, wideOn)
import Unison.Var (Var)

-- | Whether a let's binding is emitted as a section of its own, whose
-- value arrives by a yield, rather than as an instruction (a literal, a
-- constructor, a record, a record update, a primitive, a discard, an
-- affine update, a box): the complement of the instruction cases of the
-- emitter's emitLet.
letYields :: (Var v) => ANormal Reference v -> Bool
letYields = \case
  TLit _ -> False
  TBLit _ -> False
  TApp (FCon _ _) _ -> False
  TApp (FRec _) _ -> False
  TApp (FRecSet _) _ -> False
  TApp (FPrim _) _ -> False
  TDiscard _ -> False
  TUpdate {} -> False
  TBox _ -> False
  _ -> True

-- | Whether a let's binding is a small constructor application, which
-- stays in the variable's slots under the convention.
wideCon :: (Var v) => ANormal Reference v -> Bool
wideCon (TApp (FCon _ _) args) = wideOn && length args <= wideK
wideCon _ = False

-- | Whether a body matches on a variable or returns it: the uses a wide
-- variable serves unboxed. Walks only the subterms that mention the
-- variable, which every term records.
usedWide :: (Var v) => v -> ANormal Reference v -> Bool
usedWide u = go
  where
    go tm
      | not (Set.member u (ABTN.freeVars tm)) = False
      | otherwise = case tm of
          TMatch v bs -> v == u || any go bs
          TVar v -> v == u
          ABTN.Term _ (ABTN.Abs _ b) -> go b
          ABTN.Term _ (ABTN.Tm f) -> any go f

-- | The body with its wide binders marked and its boxes placed. Linear in
-- the body: one walk, with the set of the wide variables that may still
-- hold an unboxed constructor on the current path, forked at every branch.
wideBody :: (Var v) => ANormal Reference v -> ANormal Reference v
wideBody = go Set.empty
  where
    go :: (Var v) => Set v -> ANormal Reference v -> ANormal Reference v
    go un = \case
      -- a parameter or an arm's field is narrow, whatever it shadows
      ABTN.Term _ (ABTN.Abs v b) -> ABTN.TAbs v (go (Set.delete v un) b)
      TLets d us ms bn bo
        -- the binding runs in a section of its own, and boxes what
        -- escapes inside it; the body boxes again, since the binding's
        -- boxes are not its own. One variable is wide; several (the
        -- leaves of a product-return worker's result) are narrow.
        | letYields bn ->
            let (ms', un') = case us of
                  [u] -> ([WD], wide u (narrow us un))
                  _ -> (ms, narrow us un)
             in TLets d us ms' (go un bn) (go un' bo)
        | [u] <- us,
          wideCon bn,
          usedWide u bo,
          TApp _ args <- bn ->
            boxes un args \un' -> TLets d us [WD] bn (go (wide u (narrow us un')) bo)
        -- an instruction: its arguments escape before it
        | TApp _ args <- bn ->
            boxes un args \un' -> TLets d us ms bn (go (narrow us un') bo)
        | TUpdate _ _ x <- bn ->
            boxes un [x] \un' -> TLets d us ms bn (go (narrow us un') bo)
        | otherwise -> TLets d us ms (go un bn) (go (narrow us un) bo)
      TName u f args bo ->
        boxes un args \un' -> TName u f args (go (Set.delete u un') bo)
      TApp f args -> boxes un args \_ -> TApp f args
      -- a match, a handle, a shift, a local, a force, a variable, a
      -- literal, a discard: nothing escapes, and the walk goes on into
      -- each child with the same set
      ABTN.Term _ (ABTN.Tm f) -> ABTN.TTm (fmap (go un) f)
    wide u un
      | wideOn = Set.insert u un
      | otherwise = un
    narrow us un = foldr Set.delete un us
    -- a box before the node for each argument still unboxed, once each
    boxes un [] k = k un
    boxes un (a : as) k
      | Set.member a un = TLets Direct [] [] (TBox a) (boxes (Set.delete a un) as k)
      | otherwise = boxes un as k
