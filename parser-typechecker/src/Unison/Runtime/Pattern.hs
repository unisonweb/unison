{-# language BangPatterns #-}
{-# language ViewPatterns #-}
{-# language PatternGuards #-}
{-# language TupleSections #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Unison.Runtime.Pattern
  ( DataSpec
  , splitPatterns
  , builtinDataSpec
  ) where

import Control.Applicative ((<|>))
import Control.Lens ((<&>))
import Control.Monad.State (State, state, runState, modify)

import Data.List (splitAt, findIndex, transpose)
import Data.Maybe (catMaybes, fromMaybe)

import Data.Set as Set (Set, insert, fromList, member)

import Unison.ABT
  (absChain', freshIn, visitPure, pattern AbsN', changeVars)
import Unison.Builtin.Decls (builtinDataDecls, builtinEffectDecls)
import Unison.DataDeclaration (declFields)
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
type Cons = [Int]

type DataSpec = Map Reference (Either Cons Cons)

type PatternV v = PatternP v
type Scrut v = (v, Reference)

data PatternRow v
  = PR
  { _pats :: [PatternV v]
  , guard :: Maybe (Term v)
  , body :: Term v
  }

builtinDataSpec :: DataSpec
builtinDataSpec = Map.fromList decls
 where
 decls = [ (DerivedId x, declFields $ Right y)
         | (_,x,y) <- builtinDataDecls @Symbol ]
      ++ [ (DerivedId x, declFields $ Left y)
         | (_,x,y) <- builtinEffectDecls @Symbol ]

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

extractVar :: Var v => PatternV v -> Maybe v
extractVar p
  | UnboundP{} <- p = Nothing
  | otherwise = Just (loc p)

extractVars :: Var v => [PatternV v] -> [v]
extractVars = catMaybes . fmap extractVar

decomposePattern
  :: Var v
  => Int -> Int -> PatternV v
  -> [[PatternV v]]
decomposePattern t nfields p@(ConstructorP _ _ u ps)
  | t == u
  = if length ps == nfields
    then [ps]
    else error err
  where
  err = "decomposePattern: wrong number of constructor fields: "
     ++ show (nfields, p)
decomposePattern t nfields p@(EffectBindP _ _ u ps pk)
  | t == u
  = if length ps == nfields
    then [ps ++ [pk]]
    else error err
  where
  err = "decomposePattern: wrong number of ability fields: "
     ++ show (nfields, p)
decomposePattern t _ (EffectPureP _ p)
  | t == -1 = [[p]]
decomposePattern _ nfields (VarP _)
  = [replicate nfields (UnboundP (typed Pattern))]
decomposePattern _ nfields (UnboundP _)
  = [replicate nfields (UnboundP (typed Pattern))]
decomposePattern _ _ (SequenceLiteralP _ _)
  = error "decomposePattern: sequence literal"
decomposePattern _ _ _ = []

matchBuiltin :: PatternP a -> Maybe (PatternP ())
matchBuiltin (VarP _) = Just $ UnboundP ()
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
  -> Int
  -> PatternRow v
  -> [([PatternV v], PatternRow v)]
splitRow i t nfields (PR (splitAt i -> (pl, sp : pr)) g b)
  = decomposePattern t nfields sp
      <&> \subs -> (subs, PR (pl ++ subs ++ pr) g b)
splitRow _ _ _ _ = error "splitRow: bad index"

splitRowBuiltin
  :: Var v
  => Int
  -> PatternRow v
  -> [(PatternP (), [([PatternV v], PatternRow v)])]
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
  => [([PatternV v], PatternRow v)]
  -> ([(v,Reference)], PatternMatrix v)
buildMatrix [] = error "buildMatrix: empty rows"
buildMatrix vrs@((pvs,_):_) = (zip cvs rs, PM $ fixRow <$> vrs)
  where
  rs = fmap determineType . transpose . fmap fst $ vrs
  cvs = extractVars pvs
  fixRow (extractVars -> rvs, pr)
    = renameRow (fromListWith const . zip rvs $ cvs) pr

splitMatrixBuiltin
  :: Var v
  => Int
  -> PatternMatrix v
  -> [(PatternP (), [(v,Reference)], PatternMatrix v)]
splitMatrixBuiltin i (PM rs)
  = fmap (\(a,(b,c)) -> (a,b,c))
  . toList
  . fmap buildMatrix
  . fromListWith (++)
  $ splitRowBuiltin i =<< rs

splitMatrix
  :: Var v
  => Int
  -> Either Cons Cons
  -> PatternMatrix v
  -> [(Int, [(v,Reference)], PatternMatrix v)]
splitMatrix i econs (PM rs)
  = fmap (\(a, (b, c)) -> (a,b,c)) . (fmap.fmap) buildMatrix $ mmap
  where
  cons = either (((-1,1):) . zip [0..]) (zip [0..]) econs
  mmap = fmap (\(t,fs) -> (t, splitRow i t fs =<< rs)) cons

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
prepareAs (UnboundP _) u = pure $ VarP u
prepareAs (AsP _ p) u = prepareAs p u <* (renameTo u =<< useVar)
prepareAs (VarP _) u = VarP u <$ (renameTo u =<< useVar)
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
preparePattern (UnboundP _) = VarP <$> freshVar
preparePattern (VarP _) = VarP <$> useVar
preparePattern (AsP _ p) = prepareAs p =<< useVar
preparePattern p = prepareAs p =<< freshVar

buildPattern :: Bool -> Reference -> Int -> [v] -> Int -> PatternP ()
buildPattern ef r t vs nfields =
  case ef of
    False -> ConstructorP () r t vps
    True | t == -1 -> EffectPureP () (VarP ())
         | otherwise -> EffectBindP () r t aps kp
     where
     (aps,kp) | [] <- vps = error "too few patterns for effect bind"
              | otherwise = (init vps, last vps)
  where
  vps | length vs < nfields
      = replicate nfields $ UnboundP ()
      | otherwise
      = VarP () <$ vs

compile :: Var v => DataSpec -> [Scrut v] -> PatternMatrix v -> Term v
compile _ _ (PM []) = blank ()
compile spec scs m@(PM (r:rs))
  | rowIrrefutable r
  = case guard r of
      Nothing -> body r
      Just g -> iff mempty g (body r) $ compile spec scs (PM rs)
  | (scsl, (scrut,r) : scsr) <- splitAt i scs
  , r `member` builtinCase
  = match () (var () scrut)
      $ buildCaseBuiltin spec scsl scsr <$> splitMatrixBuiltin i m
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

buildCaseBuiltin
  :: Var v
  => DataSpec
  -> [Scrut v]
  -> [Scrut v]
  -> (PatternP (), [(v,Reference)], PatternMatrix v)
  -> MatchCase () (Term v)
buildCaseBuiltin spec scsl scsr (p, vrs, m)
  = MatchCase p Nothing . absChain' vs $ compile spec scs m
  where
  (scsn, vs) = unzip $ vrs <&> \p@(v,_) -> (p,((),v))
  scs = scsl ++ scsn ++ scsr

buildCase
  :: Var v
  => DataSpec
  -> Reference
  -> Either Cons Cons
  -> [Scrut v]
  -> [Scrut v]
  -> (Int, [(v,Reference)], PatternMatrix v)
  -> MatchCase () (Term v)
buildCase spec r econs scsl scsr (t, vrs, m)
  = MatchCase pat Nothing . absChain' vs $ compile spec scs m
  where
  (eff, cons) = either (True,) (False,) econs
  pat = buildPattern eff r t vs $ cons !! t
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
splitPatterns spec0 = visitPure $ \case
  Match' sc0 cs0
    | r <- determineType $ p <$> cs0
    , (lv, scrut, pm) <- initialize r sc cs
    , body <- compile spec [scrut] pm
   -> Just $ case lv of
        Just v -> let1 False [(((),v), sc)] body
        _ -> body
    where
    sc = splitPatterns spec sc0
    cs = fmap (splitPatterns spec) <$> cs0
  _ -> Nothing
  where
  p (MatchCase pp _ _) = pp
  spec = Map.insert Rf.booleanRef (Right [0,0]) spec0

builtinCase :: Set Reference
builtinCase
  = fromList
  [ Rf.intRef
  , Rf.natRef
  , Rf.floatRef
  , Rf.textRef
  , Rf.charRef
  ]

defaultRef :: Reference
defaultRef = Builtin "PatternMatchUnknown"

determineType :: [PatternP a] -> Reference
determineType = fromMaybe defaultRef . foldr ((<|>) . f) Nothing
  where
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
