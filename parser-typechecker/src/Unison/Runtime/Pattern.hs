{-# language ViewPatterns #-}
{-# language PatternGuards #-}
{-# language TupleSections #-}
{-# language PatternSynonyms #-}

module Unison.Runtime.Pattern
  ( splitPatterns
  ) where

import Control.Monad.State (State, state, runState, modify)

import Data.List (splitAt, findIndex)

import Data.Set as Set (Set, insert, fromList)

import Unison.ABT
  (absChain', freshIn, visitPure, pattern AbsN', changeVars)
-- import qualified Unison.ABT as ABT
import Unison.Pattern
-- import Unison.Reference (Reference)
import Unison.Term
import Unison.Var (Var, typed, pattern Pattern)

import Data.Map.Strict as Map
  (Map, fromListWith, lookup, toList, insertWith)

-- newtype DataSpec = DS (Map Reference [Int])

type PatternV a v = PatternP (a, v)

data PatternRow v a
  = PR
  { _pats :: [PatternV a v]
  , guard :: Maybe (Term v a)
  , body :: Term v a
  }

rebind
  :: Semigroup a
  => Var v
  => [Term v a]
  -> PatternRow v a
  -> Term v a
  -> Term v a
rebind tms0 (PR ps0 _ _) = let1' False . reverse $ collect [] tms0 ps0
  where
  collect acc (Var' u : tms) (VarP (_, v) : ps)
    | u == v = collect acc tms ps
    | otherwise
    = error $ "pattern rebind: mismatched variables: " ++ show (u,v)
  collect acc (tm : tms) (VarP (_, v) : ps)
    = collect ((v, tm) : acc) tms ps
  collect acc [] [] = acc
  collect _ tms@(_:_) []
    = error $ "pattern rebind: more terms than patterns: " ++ show tms
  collect _ [] ps@(_:_)
    = error $ "pattern rebind: more patterns than terms: " ++ show ps
  collect _ (_:_) (p:_)
    = error $ "pattern rebind: unsplit pattern" ++ show p

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

firstRow :: ([PatternV a v] -> Maybe Int) -> Heuristic v a
firstRow f (PM (r:_)) = f $ _pats r
firstRow _ _ = Nothing

heuristics :: [Heuristic v a]
heuristics = [firstRow $ findIndex refutable]

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
  | VarP av@(a,_) <- sp
  = (VarP a, [([av], PR (pl ++ pr) g b)])
  | otherwise
  = (fst <$> sp, [(vars, PR (pl ++ subs ++ pr) g b)])
  where
  (vars, subs) = unzip $ decomposePattern sp
splitRow _ _ = error "splitRow: bad index"

renameRow :: Var v => Map v v -> PatternRow v a -> PatternRow v a
renameRow m (PR p0 g0 b0) = PR p g b
  where
  access k
    | Just v <- Map.lookup k m = v
    | otherwise = k
  p = (fmap.fmap.fmap) access p0
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
  in (v, (insert v avoid, vs, rn))

useVar :: PPM v v
useVar = state $ \(avoid, v:vs, rn) -> (v, (avoid, vs, rn))

renameTo :: Var v => v -> v -> PPM v ()
renameTo to from
  = modify $ \(avoid, vs, rn) ->
      ( avoid, vs
      , insertWith (error "renameTo: duplicate rename") from to rn
      )

prepareAs :: Var v => PatternP a -> v -> PPM v (PatternV a v)
prepareAs (UnboundP a) u = pure $ VarP (a, u)
prepareAs (AsP _ p) u = prepareAs p u <* (renameTo u =<< useVar)
prepareAs (VarP a) u = VarP (a, u) <$ (renameTo u =<< useVar)
prepareAs (ConstructorP a r i ps) u = do
  ConstructorP (a,u) r i <$> traverse preparePattern ps
prepareAs (EffectPureP a p) u = do
  EffectPureP (a,u) <$> preparePattern p
prepareAs (EffectBindP a r i ps k) u = do
  EffectBindP (a,u) r i
    <$> traverse preparePattern ps
    <*> preparePattern k
prepareAs (SequenceLiteralP a ps) u = do
  SequenceLiteralP (a,u) <$> traverse preparePattern ps
prepareAs (SequenceOpP a p op q) u = do
  flip (SequenceOpP (a,u)) op
    <$> preparePattern p
    <*> preparePattern q
prepareAs p u = pure $ (,u) <$> p

preparePattern :: Var v => PatternP a -> PPM v (PatternV a v)
preparePattern (VarP a) = VarP . (a,) <$> useVar
preparePattern (AsP _ p) = prepareAs p =<< useVar
preparePattern p = prepareAs p =<< freshVar

varp :: PatternP a -> PatternP a
varp p = VarP $ loc p

chopPattern :: PatternP a -> PatternP a
chopPattern (ConstructorP a r i ps)
  = ConstructorP a r i $ varp <$> ps
chopPattern (EffectBindP a r i ps k)
  = EffectBindP a r i (varp <$> ps) (varp k)
chopPattern (EffectPureP a p)
  = EffectPureP a (varp p)
chopPattern (SequenceLiteralP a ps)
  = SequenceLiteralP a $ varp <$> ps
chopPattern (SequenceOpP a p op q)
  = SequenceOpP a (varp p) op (varp q)
chopPattern p = p

compile
  :: (Var v, Monoid a) => [Term v a] -> PatternMatrix v a -> Term v a
compile _ (PM [])
  = error "compile: empty matrix" -- TODO: maybe generate error term
compile tms m@(PM (r:rs))
  | rowIrrefutable r
  = rebind tms r $ case guard r of
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
    = MatchCase (chopPattern p) Nothing . absChain' vs
    $ compile tms m
    where
    tms | VarP _ <- p = tmsl ++ tmsr
        | otherwise = tmsl ++ fmap (uncurry var) vs ++ tmsr

mkRow :: Var v => MatchCase a (Term v a) -> PatternRow v a
mkRow (MatchCase p0 g0 (AbsN' vs b))
  = case runState (preparePattern p0) (avoid, vs, mempty) of
      (p, (_, [], rn)) -> PR [p] (changeVars rn <$> g) (changeVars rn b)
      _ -> error "mkRow: not all variables used"
  where
  g = case g0 of
        Just (AbsN' us g)
          | us == vs -> Just g
          | otherwise -> error "mkRow: guard variables do not match body"
        Nothing -> Nothing
        _ -> error "mkRow: impossible"
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
