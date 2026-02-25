{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.Runtime.Referenced
  ( Referenced (..),
    dereference,
    Referential (..),
    RefNum (..),
    CanonST (..),
    emptyCST,
    Canonize,
    resolveRef,
    canonicalizeRefs,
    recanonicalizeRefs,
    toReferenced,
  )
where

import Control.Monad.State.Strict
import Data.Foldable (toList)
import Data.Functor.Const
import Data.Functor.Identity
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Primitive.Array (arrayFromList, indexArray, sizeofArray)
import Data.Sequence (Seq, (|>))
import Unison.ConstructorReference
import Unison.Reference
import Unison.ReferentPrime
import Unison.Runtime.Canonicalizer
import Prelude hiding (lookup)

-- A number for indexing into a list of common references.
newtype RefNum = RefNum Int
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable)

getRefNum :: RefNum -> Int
getRefNum (RefNum i) = i

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
data Referenced t
  = -- types, terms
    WithRefs [Reference] [Reference] (t RefNum)
  | Plain (t Reference)

instance (forall r. (Eq r) => Eq (t r)) => Eq (Referenced t) where
  Plain x == Plain y = x == y
  WithRefs tysx tmsx x == WithRefs tysy tmsy y =
    tysx == tysy && tmsx == tmsy && x == y
  _ == _ = False

instance (forall r. (Show r) => Show (t r)) => Show (Referenced t) where
  showsPrec p = \case
    Plain t -> showParen (p > 10) (showString "Plain " . showsPrec 11 t)
    WithRefs tys tms t ->
      showParen (p > 10) $
        showString "WithRefs "
          . shows tys
          . showString " "
          . shows tms
          . showString " "
          . showsPrec 11 t

-- A class categorizing types that contain something like a codebase
-- reference, providing traversal functions over the references. The
-- difference between just being a Functor/Traversable is that
-- references may be either term or type references, and the
-- traversals make that information available.
--
-- In all cases, the boolean argument is 'is the reference a type,' so
-- `False` indicates a term reference and `True` indicates a type
-- reference.
class Referential t where
  overRefs :: (Bool -> r -> s) -> t r -> t s
  foldMapRefs :: (Monoid m) => (Bool -> r -> m) -> t r -> m
  traverseRefs :: (Applicative f) => (Bool -> r -> f s) -> t r -> f (t s)

  overRefs f = runIdentity . traverseRefs (\isTy -> Identity . f isTy)
  foldMapRefs f = getConst . traverseRefs (\isTy -> Const . f isTy)

instance Referential Referent' where
  overRefs f = \case
    Con' (ConstructorReference r j) i ->
      Con' (ConstructorReference (f True r) j) i
    Ref' r -> Ref' (f False r)

  foldMapRefs f = \case
    Con' (ConstructorReference r _) _ -> f True r
    Ref' r -> f False r

  traverseRefs f = \case
    Con' (ConstructorReference r j) i ->
      flip Con' i . flip ConstructorReference j <$> f True r
    Ref' r -> Ref' <$> f False r

dereference :: (Referential t) => Referenced t -> t Reference
dereference (Plain x) = x
dereference (WithRefs tysl tmsl x) = overRefs lkup x
  where
    tys = arrayFromList tysl
    tms = arrayFromList tmsl
    lkup isTy (RefNum i)
      | 0 <= i, i < sizeofArray arr = indexArray arr i
      | otherwise = error "dereference: index out of bounds"
      where
        arr = if isTy then tys else tms

data CanonST = CST
  { canon :: Canonicalizer Reference,
    _tyNums :: CanonMap Reference RefNum,
    _tmNums :: CanonMap Reference RefNum,
    _tys :: Seq Reference,
    _tms :: Seq Reference
  }

emptyCST :: CanonST
emptyCST = CST empty emptyCM emptyCM mempty mempty

type Canonize = StateT CanonST IO

resolveRef :: Bool -> Reference -> Canonize RefNum
resolveRef isTy = resolveRef0 "resolveRef" isTy

resolveRef0 :: String -> Bool -> Reference -> Canonize RefNum
resolveRef0 _funName isTy r = StateT \case
  st@(CST canon tym tmm tys tms) ->
    categorize canon r >>= \case
      Canonical ->
        look r >>= \case
          Just rn -> pure (rn, st)
          Nothing -> updated r canon
      Novel canon -> updated r canon
      Equivalent s canon ->
        look s >>= \case
          Just rn -> pure (rn, st {canon = canon})
          Nothing -> updated s canon
    where
      look r = lookup r (if isTy then tym else tmm)

      updated s canon = do
        tym <- if isTy then insert s rn tym else pure tym
        tmm <- if isTy then pure tmm else insert s rn tmm
        tys <- pure $ if isTy then tys |> s else tys
        tms <- pure $ if isTy then tms else tms |> s
        pure (rn, CST canon tym tmm tys tms)
        where
          rn
            | isTy = RefNum (length tys)
            | otherwise = RefNum (length tms)
      {-# INLINE updated #-}


-- Given a reference traversal, canonicalizes the references in a
-- value. The operation is presented as a state transformation, so
-- that it can hook into a larger canonicalization procedure. The
-- lists of canonical references of each sort are yielded as part of
-- the state.
canonicalizeRefs :: (Referential t) => t Reference -> Canonize (t RefNum)
canonicalizeRefs = traverseRefs $ resolveRef0 "canonicalizeRefs"
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
recanonicalizeRefs ::
  (Referential t) => Referenced t -> Canonize (t RefNum)
recanonicalizeRefs = \case
  Plain v -> canonicalizeRefs v
  WithRefs tys tms v -> do
    tyns <- traverse (resolveRef0 "recanonicalizeRefs" True) tys
    tmns <- traverse (resolveRef0 "recanonicalizeRefs" False) tms

    rtys <- pure . HM.fromList . filter notSame $ zip [0 ..] tyns
    rtms <- pure . HM.fromList . filter notSame $ zip [0 ..] tmns

    let f isTy r = HM.findWithDefault r (getRefNum r) m
          where
            m = if isTy then rtys else rtms

    if HM.null rtys && HM.null rtms
      then pure v -- already canonical
      else pure $ overRefs f v
  where
    notSame (i, RefNum j) = i /= j

toReferenced :: Canonize (t RefNum) -> IO (Referenced t)
toReferenced act = finalize <$> runStateT act emptyCST
  where
    finalize (x, CST _ _ _ tys tms) =
      WithRefs (toList tys) (toList tms) x
{-# INLINE toReferenced #-}
