{-# language BangPatterns #-}
{-# language ViewPatterns #-}
{-# language PatternGuards #-}
{-# language TupleSections #-}
{-# language PatternSynonyms #-}

module Unison.Runtime.Pattern
  ( DataSpec
  , splitPatterns
  , builtinDataSpec
  ) where

import Control.Applicative ((<|>))
import Control.Lens ((<&>))
import Control.Monad.State (State, state, runState, modify)

import Data.Bifunctor (bimap)
import Data.List (splitAt, findIndex)
import Data.Maybe (catMaybes)

import Data.Set as Set (Set, insert, fromList, member)

import Unison.ABT
  (absChain', freshIn, visitPure, pattern AbsN', changeVars)
import Unison.Builtin.Decls (builtinDataDecls)
import Unison.DataDeclaration (constructorFields)
import Unison.Pattern
import Unison.Reference (Reference(..))
import Unison.Symbol (Symbol)
import Unison.Term hiding (Term)
import qualified Unison.Term as Tm
import Unison.Var (Var, typed, pattern Pattern)

import qualified Unison.Type as Rf

import Data.Map.Strict
  (Map, toList, fromListWith, insertWith)
import qualified Data.Map.Strict as Map

type Term v = Tm.Term v ()
type Cons = [[Reference]]

type DataSpec = Map Reference Cons

type PatternV v = PatternP v
type Scrut v = (v, Reference)

data PatternRow v
  = PR
  { _pats :: [PatternV v]
  , guard :: Maybe (Term v)
  , body :: Term v
  }

builtinDataSpec :: DataSpec
builtinDataSpec
  = Map.fromList
  $ bimap DerivedId constructorFields . (\(_,x,y) -> (x,y))
 <$> builtinDataDecls @Symbol

data PatternMatrix v
  = PM { _rows :: [PatternRow v] }

type Heuristic v = PatternMatrix v -> Maybe Int

choose :: [Heuristic v] -> PatternMatrix v -> Int
choose [] _ = 0
choose (h:hs) m
  | Just i <- h m = i
  | otherwise = choose hs m

refutable :: PatternP a -> Bool
refutable (UnboundP _) = False
refutable (VarP _) = False
refutable _ = True

rowIrrefutable :: PatternRow v -> Bool
rowIrrefutable (PR ps _ _) = all (not.refutable) ps

firstRow :: ([PatternV v] -> Maybe Int) -> Heuristic v
firstRow f (PM (r:_)) = f $ _pats r
firstRow _ _ = Nothing

heuristics :: [Heuristic v]
heuristics = [firstRow $ findIndex refutable]

extractVar
  :: Var v
  => Reference
  -> PatternV v
  -> (Maybe (v, Reference), PatternV v)
extractVar r p
  | UnboundP{} <- p = (Nothing, p)
  | otherwise = (Just (loc p, r), p)

zipWithExact :: String -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithExact _ _ [] [] = []
zipWithExact s f (x:xs) (y:ys)
  = let !zs = zipWithExact s f xs ys in f x y : zs
zipWithExact s _ _ _ = error s

decomposePattern
  :: Var v
  => Int -> [Reference] -> PatternV v
  -> [[(Maybe (v,Reference), PatternV v)]]
decomposePattern t flds p@(ConstructorP _ _ u ps)
  | t == u
  = [zipWithExact err extractVar flds ps]
  where
  err = "decomposePattern: mismatched constructor fields: "
     ++ show (flds, p)
decomposePattern t flds p@(EffectBindP _ _ u ps pk)
  | t == u
  = [zipWithExact err extractVar flds $ ps ++ [pk]]
  where
  err = "decomposePattern: mismatched ability fields: "
     ++ show (flds, p)
decomposePattern t flds (EffectPureP _ p)
  | t == -1
  = [zipWithExact err extractVar flds [p]]
  where
  err = "decomposePattern: wrong number of fields for effect-pure: "
     ++ show flds
decomposePattern _ flds (VarP _)
  = [(Nothing, UnboundP (typed Pattern)) <$ flds]
decomposePattern _ flds (UnboundP _)
  = [(Nothing, UnboundP (typed Pattern)) <$ flds]
decomposePattern _ _ (SequenceLiteralP _ _)
  = error "decomposePattern: sequence literal"
decomposePattern _ _ _ = []

matchBuiltin :: PatternP a -> Maybe (PatternP ())
matchBuiltin (VarP _) = Just $ VarP ()
matchBuiltin (UnboundP _) = Just $ UnboundP ()
matchBuiltin (NatP _ n) = Just $ NatP () n
matchBuiltin (IntP _ n) = Just $ IntP () n
matchBuiltin (TextP _ t) = Just $ TextP () t
matchBuiltin (CharP _ c) = Just $ CharP () c
matchBuiltin (FloatP _ d) = Just $ FloatP () d
matchBuiltin _ = Nothing

splitRow
  :: Var v
  => Int
  -> Int
  -> [Reference]
  -> PatternRow v
  -> [([(v,Reference)], PatternRow v)]
splitRow i t flds (PR (splitAt i -> (pl, sp : pr)) g b)
  = bimap catMaybes (\subs -> PR (pl ++ subs ++ pr) g b)
  . unzip <$> decomposePattern t flds sp
splitRow _ _ _ _ = error "splitRow: bad index"

splitRowBuiltin
  :: Var v
  => Int
  -> PatternRow v
  -> [(PatternP (), [([(v,Reference)], PatternRow v)])]
splitRowBuiltin i (PR (splitAt i -> (pl, sp : pr)) g b)
  | Just p <- matchBuiltin sp = [(p, [([], PR (pl ++ pr) g b)])]
  | otherwise = []
splitRowBuiltin _ _ = error "splitRowBuiltin: bad index"

renameRow :: Var v => Map v v -> PatternRow v -> PatternRow v
renameRow m (PR p0 g0 b0) = PR p g b
  where
  access k
    | Just v <- Map.lookup k m = v
    | otherwise = k
  p = (fmap.fmap) access p0
  g = changeVars m <$> g0
  b = changeVars m b0

buildMatrix
  :: Var v
  => [([(v,Reference)], PatternRow v)]
  -> ([(v,Reference)], PatternMatrix v)
buildMatrix [] = error "buildMatrix: empty rows"
buildMatrix vrs@((avrs,_):_) = (avrs, PM $ fixRow <$> vrs)
  where
  cvs = fst <$> avrs
  fixRow (fmap fst -> rvs, pr)
    = renameRow (fromListWith const . zip rvs $ cvs) pr

splitMatrixBuiltin
  :: Var v
  => Int
  -> PatternMatrix v
  -> [(Either (PatternP ()) Int, [(v,Reference)], PatternMatrix v)]
splitMatrixBuiltin i (PM rs)
  = fmap (\(a,(b,c)) -> (Left a,b,c))
  . toList
  . fmap buildMatrix
  . fromListWith (++)
  $ splitRowBuiltin i =<< rs

splitMatrix
  :: Var v
  => Int
  -> Cons
  -> PatternMatrix v
  -> [(Either (PatternP ()) Int, [(v,Reference)], PatternMatrix v)]
splitMatrix i cons (PM rs)
  = fmap (\(a, (b, c)) -> (a,b,c)) . (fmap.fmap) buildMatrix $ mmap
  where
  mmap = zipWith (\t fs -> (Right t , splitRow i t fs =<< rs)) [0..] cons

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

prepareAs :: Var v => PatternP a -> v -> PPM v (PatternV v)
prepareAs (UnboundP _) u = pure $ UnboundP u
prepareAs (AsP _ p) u = prepareAs p u <* (renameTo u =<< useVar)
prepareAs (VarP _) u = UnboundP u <$ (renameTo u =<< useVar)
prepareAs (ConstructorP _ r i ps) u = do
  ConstructorP u r i <$> traverse preparePattern ps
prepareAs (EffectPureP _ p) u = do
  EffectPureP u <$> preparePattern p
prepareAs (EffectBindP _ r i ps k) u = do
  EffectBindP u r i
    <$> traverse preparePattern ps
    <*> preparePattern k
prepareAs (SequenceLiteralP _ ps) u = do
  SequenceLiteralP u <$> traverse preparePattern ps
prepareAs (SequenceOpP _ p op q) u = do
  flip (SequenceOpP u) op
    <$> preparePattern p
    <*> preparePattern q
prepareAs p u = pure $ u <$ p

preparePattern :: Var v => PatternP a -> PPM v (PatternV v)
preparePattern (UnboundP _) = UnboundP <$> freshVar
preparePattern (VarP _) = UnboundP <$> useVar
preparePattern (AsP _ p) = prepareAs p =<< useVar
preparePattern p = prepareAs p =<< freshVar

buildPattern :: Reference -> Int -> [v] -> [Reference] -> PatternP ()
buildPattern r t vs rs = ConstructorP () r t vps
  where
  vps | length vs < length rs
      = UnboundP () <$ rs
      | otherwise
      = VarP () <$ vs

compile :: Var v => DataSpec -> [Scrut v] -> PatternMatrix v -> Term v
compile _ _ (PM [])
  = error "compile: empty matrix" -- TODO: maybe generate error term
compile spec scs m@(PM (r:rs))
  | rowIrrefutable r
  = case guard r of
      Nothing -> body r
      Just g -> iff mempty g (body r) $ compile spec scs (PM rs)
  | (scsl, (scrut,r) : scsr) <- splitAt i scs
  , r `member` builtinCase
  = match () (var () scrut)
      $ buildCase spec r [] scsl scsr <$> splitMatrixBuiltin i m
  | (scsl, (scrut,r) : scsr) <- splitAt i scs
  = case Map.lookup r spec of
      Just cons ->
        match () (var () scrut)
          $ buildCase spec r cons scsl scsr
         <$> splitMatrix i cons m
      Nothing -> error $ "unknown data reference: " ++ show r
  where
  i = choose heuristics m
compile _ _ _ = error "inconsistent terms and pattern matrix"

buildCase
  :: Var v
  => DataSpec
  -> Reference
  -> Cons
  -> [Scrut v]
  -> [Scrut v]
  -> (Either (PatternP ()) Int, [(v,Reference)], PatternMatrix v)
  -> MatchCase () (Term v)
buildCase spec r cons scsl scsr (epi, vrs, m)
  = MatchCase pat Nothing . absChain' vs $ compile spec scs m
  where
  pat | Left  p <- epi = p
      | Right t <- epi = buildPattern r t vs $ cons !! t
  (scsn, vs) = unzip $ vrs <&> \(v,r) -> ((v,r),((),v))
  scs = scsl ++ scsn ++ scsr

mkRow :: Var v => v -> MatchCase a (Term v) -> PatternRow v
mkRow sv (MatchCase p0 g0 (AbsN' vs b))
  = case runState (prepareAs p0 sv) (avoid, vs, mempty) of
      (p, (_, [], rn)) -> PR [p] (changeVars rn <$> g) (changeVars rn b)
      _ -> error "mkRow: not all variables used"
  where
  g = case g0 of
        Just (AbsN' us g)
          | us == vs -> Just g
          | otherwise -> error "mkRow: guard variables do not match body"
        Nothing -> Nothing
        _ -> error "mkRow: impossible"
  avoid = fromList (sv:vs) <> maybe mempty freeVars g <> freeVars b
mkRow _ _ = error "mkRow: impossible"

initialize
  :: Var v
  => Reference
  -> Term v
  -> [MatchCase () (Term v)]
  -> (Maybe v, Scrut v, PatternMatrix v)
initialize r sc cs = (lv, (sv, r), PM $ mkRow sv <$> cs)
  where
  avoid = freeVars sc
  (lv, sv) | Var' v <- sc = (Nothing, v)
           | pv <- freshIn avoid $ typed Pattern
           = (Just pv, pv)

splitPatterns :: Var v => DataSpec -> Term v -> Term v
splitPatterns spec = visitPure $ \case
  Match' sc0 cs0
    | Just r <- determineType cs0
    , (lv, scrut, pm) <- initialize r sc cs
    , body <- compile spec [scrut] pm
   -> Just $ case lv of
        Just v -> let1 False [(((),v), sc)] body
        _ -> body
    where
    sc = splitPatterns spec sc0
    cs = fmap (splitPatterns spec) <$> cs0
  _ -> Nothing

builtinCase :: Set Reference
builtinCase
  = fromList
  [ Rf.intRef
  , Rf.natRef
  , Rf.floatRef
  , Rf.textRef
  , Rf.charRef
  ]

determineType :: [MatchCase a b] -> Maybe Reference
determineType = foldr ((<|>) . f . p) Nothing
  where
  p (MatchCase p _ _) = p
  f (AsP _ p) = f p
  f IntP{} = Just Rf.intRef
  f NatP{} = Just Rf.natRef
  f FloatP{} = Just Rf.floatRef
  f BooleanP{} = Just Rf.booleanRef
  f TextP{} = Just Rf.textRef
  f CharP{} = Just Rf.charRef
  f SequenceLiteralP{} = Just Rf.vectorRef
  f SequenceOpP{} = Just Rf.vectorRef
  f (ConstructorP _ r _ _) = Just r
  f (EffectBindP _ r _ _ _) = Just r
  f _ = Nothing
