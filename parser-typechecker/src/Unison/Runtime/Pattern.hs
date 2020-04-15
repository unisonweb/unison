{-# language ViewPatterns #-}
{-# language PatternGuards #-}
{-# language TupleSections #-}
{-# language PatternSynonyms #-}

module Unison.Runtime.Pattern
  ( splitPatterns
  ) where

import Control.Monad.State (State, state, runState)

import Data.List (splitAt)

import Data.Set as Set (Set, insert, fromList)

import Unison.ABT
  (absChain', freshIn, visitPure, pattern AbsN', changeVars)
-- import qualified Unison.ABT as ABT
import Unison.Pattern
-- import Unison.Reference (Reference)
import Unison.Term
import Unison.Var (Var, typed, pattern Pattern)

import Data.Map.Strict as Map (Map, fromListWith, toList, (!))

-- newtype DataSpec = DS (Map Reference [Int])

type PatternV a v = PatternP (a, v)

data PatternRow v a
  = PR
  { _pats :: [PatternV a v]
  , guard :: Maybe (Term v a)
  , body :: Term v a
  }

-- collectRowVars
--   :: Var v
--   => [PatternV a v]
--   -> Maybe (Term v a)
--   -> Term v a
--   -> Set v
-- collectRowVars ps g b
--   = (foldMap.foldMap.foldMap) singleton ps
--       <> foldMap freeVars g <> freeVars b

data PatternMatrix v a
  = PM { _rows :: [PatternRow v a] }

type Heuristic v a = PatternMatrix v a -> Maybe Int

choose :: [Heuristic v a] -> PatternMatrix v a -> Int
choose [] _ = 0
choose (h:hs) m
  | Just i <- h m = i
  | otherwise = choose hs m

refutable :: PatternP a -> Bool
refutable (UnboundP _) = False
refutable (VarP _) = False
refutable _ = True

rowIrrefutable :: PatternRow v a -> Bool
rowIrrefutable (PR ps _ _) = all (not.refutable) ps

heuristics :: [Heuristic v a]
heuristics = []

extractVar :: Var v => PatternV a v -> ((a, v), PatternV a v)
extractVar p = (loc p, p)

decomposePattern :: Var v => PatternV a v -> [((a, v), PatternV a v)]
decomposePattern (ConstructorP _ _ _ ps)
  = extractVar <$> ps
decomposePattern (EffectBindP _ _ _ ps pk)
  = fmap extractVar $ ps ++ [pk]
decomposePattern (EffectPureP _ p)
  = [extractVar p]
decomposePattern (SequenceLiteralP _ _)
  = error "decomposePattern: sequence literal"
decomposePattern _ = []

splitRow
  :: Var v
  => Int
  -> PatternRow v a
  -> (PatternP a, [([(a,v)], PatternRow v a)])
splitRow i (PR (splitAt i -> (pl, sp : pr)) g b)
  = (fst <$> sp, [(vars, PR (pl ++ subs ++ pr) g b)])
  where
  (vars, subs) = unzip $ decomposePattern sp
splitRow _ _ = error "splitRow: bad index"

renameRow :: Var v => Map v v -> PatternRow v a -> PatternRow v a
renameRow m (PR p0 g0 b0) = PR p g b
  where
  p = (fmap.fmap.fmap) (m!) p0
  g = changeVars m <$> g0
  b = changeVars m b0

buildMatrix
  :: Var v
  => [([(a,v)], PatternRow v a)]
  -> ([(a,v)], PatternMatrix v a)
buildMatrix [] = error "buildMatrix: empty rows"
buildMatrix vrs@((avs,_):_) = (avs, PM $ fixRow <$> vrs)
  where
  cvs = snd <$> avs
  fixRow (fmap snd -> rvs, pr)
    = renameRow (fromListWith const . zip rvs $ cvs) pr

splitMatrix
  :: Var v
  => Int
  -> PatternMatrix v a
  -> [(PatternP a, [(a, v)], PatternMatrix v a)]
splitMatrix i (PM rs)
  = map (\(a, (b, c)) -> (a,b,c)) . toList . fmap buildMatrix $ mmap
  where
  mmap = Map.fromListWith (++) $ splitRow i <$> rs

type PPM v a = State (Set v, [v], Map v v) a

freshVar :: Var v => PPM v v
freshVar = state $ \(avoid, vs, rn) ->
  let v = freshIn avoid $ typed Pattern
  in (v, (Set.insert v avoid, vs, rn))

useVar :: PPM v v
useVar = state $ \(avoid, v:vs, rn) -> (v, (avoid, vs, rn))

preparePattern :: Var v => PatternP a -> PPM v (PatternV a v)
preparePattern (VarP a) = VarP . (a,) <$> useVar
preparePattern (AsP a p) = do
  v <- useVar
  AsP (a,v) <$> preparePattern p
preparePattern (ConstructorP a r i ps) = do
  v <- freshVar
  ConstructorP (a,v) r i <$> traverse preparePattern ps
preparePattern (EffectPureP a p) = do
  v <- freshVar
  EffectPureP (a,v) <$> preparePattern p
preparePattern (EffectBindP a r i ps k) = do
  v <- freshVar
  EffectBindP (a,v) r i
    <$> traverse preparePattern ps
    <*> preparePattern k
preparePattern (SequenceLiteralP a ps) = do
  v <- freshVar
  SequenceLiteralP (a,v) <$> traverse preparePattern ps
preparePattern (SequenceOpP a p op q) = do
  v <- freshVar
  flip (SequenceOpP (a,v)) op
    <$> preparePattern p
    <*> preparePattern q
preparePattern p = (\v -> (,v) <$> p) <$> freshVar

compile
  :: (Var v, Monoid a) => [Term v a] -> PatternMatrix v a -> Term v a
compile _ (PM [])
  = error "compile: empty matrix" -- TODO: maybe generate error term
compile tms m@(PM (r:rs))
  | rowIrrefutable r
  = case guard r of
      Nothing -> body r
      Just g -> iff mempty g (body r) $ compile tms (PM rs)
  | otherwise
  = case splitAt i tms of
      (tmsl, scrut : tmsr) -> match mempty scrut $ f tmsl tmsr <$> sm
      _ -> error "inconsistent terms and pattern matrix"
  where
  i = choose heuristics m
  sm = splitMatrix i m
  f tmsl tmsr (p, vs, m)
    = MatchCase p Nothing . absChain' vs
    $ compile (tmsl ++ fmap (uncurry var) vs ++ tmsr) m

mkRow :: Var v => MatchCase a (Term v a) -> PatternRow v a
mkRow (MatchCase p0 g (AbsN' vs b))
  = case runState (preparePattern p0) (avoid, vs, mempty) of
      (p, (_, [], _)) -> PR [p] g b
      _ -> error "mkRow: not all variables used"
  where
  avoid = fromList vs <> maybe mempty freeVars g <> freeVars b
mkRow _ = error "mkRow: impossible"

splitPatterns
  :: (Var v, Monoid a) => Term v a -> Term v a
splitPatterns = visitPure $ \case
  Match' sc0 cs0 -> Just . compile [sc] . PM $ mkRow <$> cs
    where
    sc = splitPatterns sc0
    cs = fmap splitPatterns <$> cs0
  _ -> Nothing
