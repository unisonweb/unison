module Unison.Typechecker.Extractor where

import Control.Monad.Reader
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import qualified Unison.Blank as B
import Unison.ConstructorReference (ConstructorReference)
import Unison.Prelude hiding (whenM)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Typechecker.Context as C
import Unison.Util.Monoid (whenM)
import Unison.Var (Var)
import qualified Unison.Var as Var

type RedundantTypeAnnotation = Bool

type Extractor e a = MaybeT (Reader e) a

type ErrorExtractor v loc a = Extractor (C.ErrorNote v loc) a

type InfoExtractor v loc a = Extractor (C.InfoNote v loc) a

type PathExtractor v loc a = Extractor (C.PathElement v loc) a

type SubseqExtractor v loc a = SubseqExtractor' (C.ErrorNote v loc) a

extractor :: (e -> Maybe a) -> Extractor e a
extractor = MaybeT . reader

extract :: Extractor e a -> e -> Maybe a
extract = runReader . runMaybeT

subseqExtractor :: (C.ErrorNote v loc -> [Ranged a]) -> SubseqExtractor v loc a
subseqExtractor f = SubseqExtractor' f

traceSubseq :: Show a => String -> SubseqExtractor' n a -> SubseqExtractor' n a
traceSubseq s ex = SubseqExtractor' $ \n ->
  let rs = runSubseq ex n
   in trace (if null s then show rs else s ++ ": " ++ show rs) rs

traceNote ::
  Show a => String -> ErrorExtractor v loc a -> ErrorExtractor v loc a
traceNote s ex = extractor $ \n ->
  let result = extract ex n
   in trace (if null s then show result else s ++ ": " ++ show result) result

unique :: SubseqExtractor v loc a -> ErrorExtractor v loc a
unique ex = extractor $ \note -> case runSubseq ex note of
  [Pure a] -> Just a
  [Ranged a _ _] -> Just a
  _ -> Nothing

data SubseqExtractor' n a = SubseqExtractor' {runSubseq :: n -> [Ranged a]}

data Ranged a
  = Pure a
  | Ranged {get :: a, start :: Int, end :: Int}
  deriving (Functor, Show)

-- | collects the regions where `xa` doesn't match / aka invert a set of intervals
-- unused, but don't want to delete it yet - Aug 30, 2018
_no :: SubseqExtractor' n a -> SubseqExtractor' n ()
_no xa = SubseqExtractor' $ \note ->
  let as = runSubseq xa note
   in if null [a | Pure a <- as]
        then -- results are not full

          if null as
            then [Pure ()] -- results are empty, make them full
            -- not full and not empty, find the negation
            else
              reverse . fst $
                foldl'
                  go
                  ([], Nothing)
                  (List.sort $ fmap toPairs as)
        else [] -- results were full, make them empty
  where
    toPairs :: Ranged a -> (Int, Int)
    toPairs (Pure _) = error "this case should be avoided by the if!"
    toPairs (Ranged _ start end) = (start, end)

    go :: ([Ranged ()], Maybe Int) -> (Int, Int) -> ([Ranged ()], Maybe Int)
    go ([], Nothing) (0, r) = ([], Just (r + 1))
    go ([], Nothing) (l, r) = ([Ranged () 0 (l - 1)], Just r)
    go (_ : _, Nothing) _ = error "state machine bug in Extractor2.no"
    go (rs, Just r0) (l, r) =
      (if r0 + 1 <= l - 1 then Ranged () (r0 + 1) (l - 1) : rs else rs, Just r)

-- unused / untested
_any :: SubseqExtractor v loc ()
_any = _any' (\n -> pathLength n - 1)
  where
    pathLength :: C.ErrorNote v loc -> Int
    pathLength = length . toList . C.path

_any' :: (n -> Int) -> SubseqExtractor' n ()
_any' getLast = SubseqExtractor' $ \note ->
  Pure () : do
    let last = getLast note
    start <- [0 .. last]
    end <- [0 .. last]
    pure $ Ranged () start end

-- Kind of a newtype for Ranged.Ranged.
-- The Eq instance ignores the embedded value
data DistinctRanged a = DistinctRanged a Int Int

instance Eq (DistinctRanged a) where
  DistinctRanged _ l r == DistinctRanged _ l' r' = l == l' && r == r'

instance Ord (DistinctRanged a) where
  DistinctRanged _ l r <= DistinctRanged _ l' r' =
    l < l' || (l == l' && r <= r')

-- todo: this could return NonEmpty
some :: forall n a. SubseqExtractor' n a -> SubseqExtractor' n [a]
some xa = SubseqExtractor' $ \note ->
  let as :: [Ranged a]
      as = runSubseq xa note
   in -- Given a list of subseqs [Ranged a], find the adjacent groups [Ranged [a]].
      -- `Pure`s arguably can't be adjacent; not sure what to do with them. Currently ignored.
      fmap reverse <$> go Set.empty as
  where
    fromDistinct (DistinctRanged a l r) = Ranged a l r
    go :: Set (DistinctRanged [a]) -> [Ranged a] -> [Ranged [a]]
    go seen [] = fmap fromDistinct . toList $ seen
    go seen (rh@(Ranged h start end) : t) =
      let seen' :: Set (DistinctRanged [a])
          seen' =
            Set.fromList . join . fmap (toList . consRange rh) . toList $ seen
       in go (Set.insert (DistinctRanged [h] start end) seen `Set.union` seen') t
    go seen (Pure _ : t) = go seen t

    consRange :: Ranged a -> DistinctRanged [a] -> Maybe (DistinctRanged [a])
    consRange new group@(DistinctRanged as start' _) =
      if isAdjacent group new
        then Just (DistinctRanged (get new : as) start' (end new))
        else Nothing

    -- Returns true if inputs are adjacent Ranged regions
    -- Question: Should a Pure be considered adjacent?
    isAdjacent :: forall a b. DistinctRanged a -> Ranged b -> Bool
    isAdjacent (DistinctRanged _ _ endA) (Ranged _ startB _) = endA + 1 == startB
    isAdjacent _ _ = False

pathStart :: SubseqExtractor' n ()
pathStart = SubseqExtractor' $ \_ -> [Ranged () (-1) (-1)]

-- Scopes --
asPathExtractor :: (C.PathElement v loc -> Maybe a) -> SubseqExtractor v loc a
asPathExtractor = fromPathExtractor . extractor
  where
    fromPathExtractor :: PathExtractor v loc a -> SubseqExtractor v loc a
    fromPathExtractor ex =
      subseqExtractor $ join . fmap go . (`zip` [0 ..]) . toList . C.path
      where
        go (e, i) = case extract ex e of
          Just a -> [Ranged a i i]
          Nothing -> []

inSynthesize :: SubseqExtractor v loc (C.Term v loc)
inSynthesize = asPathExtractor $ \case
  C.InSynthesize t -> Just t
  _ -> Nothing

inSubtype :: SubseqExtractor v loc (C.Type v loc, C.Type v loc)
inSubtype = asPathExtractor $ \case
  C.InSubtype found expected -> Just (found, expected)
  C.InEquate found expected -> Just (found, expected)
  _ -> Nothing

inEquate :: SubseqExtractor v loc (C.Type v loc, C.Type v loc)
inEquate = asPathExtractor $ \case
  C.InEquate lhs rhs -> Just (lhs, rhs)
  _ -> Nothing

inCheck :: SubseqExtractor v loc (C.Term v loc, C.Type v loc)
inCheck = asPathExtractor $ \case
  C.InCheck e t -> Just (e, t)
  _ -> Nothing

-- inInstantiateL
-- inInstantiateR

inSynthesizeApp :: SubseqExtractor v loc (C.Type v loc, C.Term v loc, Int)
inSynthesizeApp = asPathExtractor $ \case
  C.InSynthesizeApp t e n -> Just (t, e, n)
  _ -> Nothing

inFunctionCall ::
  SubseqExtractor v loc ([v], C.Term v loc, C.Type v loc, [C.Term v loc])
inFunctionCall = asPathExtractor $ \case
  C.InFunctionCall vs f ft e -> case f of
    Term.Ann' f _ -> Just (vs, f, ft, e)
    f -> Just (vs, f, ft, e)
  _ -> Nothing

inAndApp,
  inOrApp,
  inIfCond,
  inMatchGuard,
  inMatchBody ::
    SubseqExtractor v loc ()
inAndApp = asPathExtractor $ \case
  C.InAndApp -> Just ()
  _ -> Nothing
inOrApp = asPathExtractor $ \case
  C.InOrApp -> Just ()
  _ -> Nothing
inIfCond = asPathExtractor $ \case
  C.InIfCond -> Just ()
  _ -> Nothing
inMatchGuard = asPathExtractor $ \case
  C.InMatchGuard -> Just ()
  _ -> Nothing
inMatchBody = asPathExtractor $ \case
  C.InMatchBody -> Just ()
  _ -> Nothing

inMatch, inVector, inIfBody :: SubseqExtractor v loc loc
inMatch = asPathExtractor $ \case
  C.InMatch loc -> Just loc
  _ -> Nothing
inVector = asPathExtractor $ \case
  C.InVectorApp loc -> Just loc
  _ -> Nothing
inIfBody = asPathExtractor $ \case
  C.InIfBody loc -> Just loc
  _ -> Nothing

-- Causes --
cause :: ErrorExtractor v loc (C.Cause v loc)
cause = extractor $ pure . C.cause

duplicateDefinitions :: ErrorExtractor v loc (NonEmpty (v, [loc]))
duplicateDefinitions =
  cause >>= \case
    C.DuplicateDefinitions vs -> pure vs
    _ -> mzero

typeMismatch :: ErrorExtractor v loc (C.Context v loc)
typeMismatch =
  cause >>= \case
    C.TypeMismatch c -> pure c
    _ -> mzero

illFormedType :: ErrorExtractor v loc (C.Context v loc)
illFormedType =
  cause >>= \case
    C.IllFormedType c -> pure c
    _ -> mzero

unknownSymbol :: ErrorExtractor v loc (loc, v)
unknownSymbol =
  cause >>= \case
    C.UnknownSymbol loc v -> pure (loc, v)
    _ -> mzero

unknownTerm :: Var v => ErrorExtractor v loc (loc, v, [C.Suggestion v loc], C.Type v loc)
unknownTerm =
  cause >>= \case
    C.UnknownTerm loc v suggestions expectedType -> do
      let k = Var.Inference Var.Ability
          cleanup = Type.cleanup . Type.removePureEffects . Type.generalize' k
      pure (loc, v, suggestions, cleanup expectedType)
    _ -> mzero

abilityCheckFailure ::
  ErrorExtractor v loc ([C.Type v loc], [C.Type v loc], C.Context v loc)
abilityCheckFailure =
  cause >>= \case
    C.AbilityCheckFailure ambient requested ctx -> pure (ambient, requested, ctx)
    _ -> mzero

abilityEqFailure ::
  ErrorExtractor v loc ([C.Type v loc], [C.Type v loc], C.Context v loc)
abilityEqFailure =
  cause >>= \case
    C.AbilityEqFailure lhs rhs ctx -> pure (lhs, rhs, ctx)
    _ -> mzero

effectConstructorWrongArgCount ::
  ErrorExtractor
    v
    loc
    (C.ExpectedArgCount, C.ActualArgCount, ConstructorReference)
effectConstructorWrongArgCount =
  cause >>= \case
    C.EffectConstructorWrongArgCount expected actual r ->
      pure (expected, actual, r)
    _ -> mzero

malformedEffectBind ::
  ErrorExtractor v loc (C.Type v loc, C.Type v loc, [C.Type v loc])
malformedEffectBind =
  cause >>= \case
    C.MalformedEffectBind ctor ctorResult es -> pure (ctor, ctorResult, es)
    _ -> mzero

solvedBlank :: InfoExtractor v loc (B.Recorded loc, v, C.Type v loc)
solvedBlank = extractor $ \n -> case n of
  C.SolvedBlank b v t -> pure (b, v, t)
  _ -> mzero

-- Misc --
errorNote :: ErrorExtractor v loc (C.ErrorNote v loc)
errorNote = extractor $ Just . id

infoNote :: InfoExtractor v loc (C.InfoNote v loc)
infoNote = extractor $ Just . id

innermostTerm :: ErrorExtractor v loc (C.Term v loc)
innermostTerm = extractor $ \n -> case C.innermostErrorTerm n of
  Just e -> pure e
  Nothing -> mzero

path :: ErrorExtractor v loc [C.PathElement v loc]
path = extractor $ pure . toList . C.path

-- Informational notes --
topLevelComponent ::
  InfoExtractor
    v
    loc
    [(v, Type v loc, RedundantTypeAnnotation)]
topLevelComponent = extractor go
  where
    go (C.TopLevelComponent c) = Just c
    go _ = Nothing

instance Functor (SubseqExtractor' n) where
  fmap = liftM

instance Applicative (SubseqExtractor' n) where
  pure = return
  (<*>) = ap

instance MonadFail (SubseqExtractor' n) where
  fail _ = mzero

instance Monad (SubseqExtractor' n) where
  return a = SubseqExtractor' $ \_ -> [Pure a]
  xa >>= f = SubseqExtractor' $ \note ->
    let as = runSubseq xa note
     in do
          ra <- as
          case ra of
            Pure a -> runSubseq (f a) note
            Ranged a startA endA ->
              let rbs = runSubseq (f a) note
               in do
                    rb <- rbs
                    case rb of
                      Pure b -> pure (Ranged b startA endA)
                      Ranged b startB endB ->
                        whenM (startB == endA + 1) (pure (Ranged b startA endB))

instance Alternative (SubseqExtractor' n) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (SubseqExtractor' n) where
  mzero = SubseqExtractor' $ \_ -> []
  mplus (SubseqExtractor' f1) (SubseqExtractor' f2) =
    SubseqExtractor' (\n -> f1 n `mplus` f2 n)

instance Monoid (SubseqExtractor' n a) where
  mempty = mzero
  mappend = mplus

instance Semigroup (SubseqExtractor' n a) where
  (<>) = mappend
