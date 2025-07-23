module Unison.Runtime.Referenced
  ( Referenced (..),
    dereference,
    RTrav,
    Canonize,
    canonicalizeRefs,
    recanonicalizeRefs,
    toReferenced,
  )
where

import Control.Monad.State.Strict
import Data.Maybe (mapMaybe)
import Unison.Reference
import Unison.Runtime.Canonicalizer

-- A value with optional optimization information for serialization.
-- The references are required for serialization V5, and are assumed
-- to be the only references used in the value _up to in-memory
-- uniqueness_.
--
-- This is parameterized so that it can be used with both Value and
-- Code.
--
-- Also note, the stored referenced might not be 'tight' in the sense
-- that they all actually occur in the value. Maintaining this
-- invariant together with actual canonicalization would be onerous
-- and isn't done at this time.
data Referenced a
  = -- types, terms
    WithRefs [Reference] [Reference] a
  | Plain a
  deriving (Show, Eq)

dereference :: Referenced a -> a
dereference (WithRefs _ _ x) = x
dereference (Plain x) = x

type RTrav a =
  forall f.
  (Applicative f) =>
  (Bool -> Reference -> f Reference) ->
  (a -> f a)

type Canonize =
  StateT (Canonicalizer Reference, [Reference], [Reference]) IO

-- Given a reference traversal, canonicalizes the references in a
-- value. The operation is presented as a state transformation, so
-- that it can hook into a larger canonicalization procedure. The
-- lists of canonical references of each sort are yielded as part of
-- the state.
canonicalizeRefs :: RTrav a -> a -> Canonize a
canonicalizeRefs trav = trav h
  where
    h isTy r = StateT \st@(canon, tys, tms) ->
      categorize canon r >>= \case
        Canonical -> pure (r, st)
        Novel canon ->
          pure (r, if isTy then (canon, r : tys, tms) else (canon, tys, r : tms))
        Equivalent s canon -> pure (s, (canon, tys, tms))
{-# INLINE canonicalizeRefs #-}

-- Given a `Referenced` value, this recanonicalizes the references in
-- the wrapped value. The intention is for this to be hooked into a
-- larger canonicalization procedure, so that already canonicalized
-- values can be more efficiently brought in line with other values
-- that are already canonicalized.
--
-- If the `Referenced` value is `Plain`, then all we can do is
-- traverse it, canonicalizing the references. However, if it is
-- tagged with canonical refs, we can see if they all match existing
-- canonical refs. If so, we don't need to traverse the value. Even if
-- not, we can traverse with marginally more efficient lookups.
recanonicalizeRefs :: RTrav a -> Referenced a -> Canonize a
recanonicalizeRefs trav = \case
  Plain v -> canonicalizeRefs trav v
  WithRefs tys tms v -> do
    typs <- mapMaybe id <$> traverse (g True) tys
    tmps <- mapMaybe id <$> traverse (g False) tms

    ctys <- lift $ fromList typs
    ctms <- lift $ fromList tmps

    let f isTy r = findWithDefault r r (if isTy then ctys else ctms)

    if null typs && null tmps
      then pure v -- already canonical
      else lift $ trav f v
  where
    g isTy r = StateT \st@(canon, tys, tms) ->
      categorize canon r >>= \case
        Canonical -> pure (Nothing, st)
        Novel canon ->
          pure
            ( Nothing,
              ( canon,
                if isTy then r : tys else tys,
                if isTy then tms else r : tms
              )
            )
        Equivalent s canon -> pure (Just (r, s), (canon, tys, tms))
{-# INLINE recanonicalizeRefs #-}

toReferenced :: Canonize a -> IO (Referenced a)
toReferenced cn = finalize <$> runStateT cn (empty, [], [])
  where
    finalize (x, (_, tys, tms)) = WithRefs tys tms x
{-# INLINE toReferenced #-}
