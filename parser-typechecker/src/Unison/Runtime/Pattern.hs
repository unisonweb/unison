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
import Control.Monad.State (State, state, evalState, runState, modify)

import Data.List (transpose)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Word (Word64)

import Data.Set as Set (Set, fromList, member)

import Unison.ABT
  (absChain', visitPure, pattern AbsN', changeVars)
import Unison.Builtin.Decls (builtinDataDecls, builtinEffectDecls)
import Unison.DataDeclaration (declFields)
import Unison.Pattern
import Unison.Reference (Reference(..))
import Unison.Symbol (Symbol)
import Unison.Term hiding (Term)
import qualified Unison.Term as Tm
import Unison.Var (Var, typed, freshIn, freshenId, Type(Pattern))

import qualified Unison.Type as Rf

import Data.Map.Strict
  (Map, toList, fromListWith, insertWith)
import qualified Data.Map.Strict as Map

type Term v = Tm.Term v ()
type Cons = [Int]

type DataSpec = Map Reference (Either Cons Cons)

type Ctx v = Map v Reference

data PatternRow v
  = PR
  { matches :: [PatternP v]
  , guard :: Maybe (Term v)
  , body :: Term v
  } deriving (Show)

builtinDataSpec :: DataSpec
builtinDataSpec = Map.fromList decls
 where
 decls = [ (DerivedId x, declFields $ Right y)
         | (_,x,y) <- builtinDataDecls @Symbol ]
      ++ [ (DerivedId x, declFields $ Left y)
         | (_,x,y) <- builtinEffectDecls @Symbol ]

data PatternMatrix v
  = PM { _rows :: [PatternRow v] }
  deriving Show

type Heuristic v = PatternMatrix v -> Maybe v

choose :: [Heuristic v] -> PatternMatrix v -> v
choose [] (PM (PR (p:_) _ _ : _)) = loc p
choose [] _ = error "pattern matching: failed to choose a splitting"
choose (h:hs) m
  | Just i <- h m = i
  | otherwise = choose hs m

refutable :: PatternP a -> Bool
refutable (UnboundP _) = False
refutable (VarP _) = False
refutable _ = True

rowIrrefutable :: PatternRow v -> Bool
rowIrrefutable (PR ps _ _) = null ps

firstRow :: ([PatternP v] -> Maybe v) -> Heuristic v
firstRow f (PM (r:_)) = f $ matches r
firstRow _ _ = Nothing

heuristics :: [Heuristic v]
heuristics = [firstRow $ fmap loc . listToMaybe]

extractVar :: Var v => PatternP v -> Maybe v
extractVar p
  | UnboundP{} <- p = Nothing
  | otherwise = Just (loc p)

extractVars :: Var v => [PatternP v] -> [v]
extractVars = catMaybes . fmap extractVar

decomposePattern
  :: Var v
  => Int -> Int -> PatternP v
  -> [[PatternP v]]
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

data SeqMatch = E | C | S | L Int | R Int | DL Int | DR Int
  deriving (Eq,Ord,Show)

seqPSize :: PatternP v -> Maybe Int
seqPSize (SequenceLiteralP _ l) = Just $ length l
seqPSize (SequenceOpP _ _ Cons r) = (1+) <$> seqPSize r
seqPSize (SequenceOpP _ l Snoc _) = (1+) <$> seqPSize l
seqPSize (SequenceOpP _ l Concat r) = (+) <$> seqPSize l <*> seqPSize r
seqPSize _ = Nothing

decideSeqPat :: [PatternP v] -> [SeqMatch]
decideSeqPat = go False
  where
  go _ [] = [E,C]
  go _ (SequenceLiteralP{} : ps) = go True ps
  go _ (SequenceOpP _ _ Snoc _ : _) = [E,S]
  go _ (SequenceOpP _ _ Cons _ : _) = [E,C]

  go guard (SequenceOpP _ l Concat r : _)
    | guard = [E,C] -- prefer prior literals
    | Just n <- seqPSize l = [L n, DL n]
    | Just n <- seqPSize r = [R n, DR n]
  go b (UnboundP _ : ps) = go b ps
  go b (VarP _ : ps) = go b ps
  go _ (p:_)
    = error $ "Cannot process sequence pattern: " ++ show p

data SeqCover v
  = Cover [PatternP v]
  | Disjoint
  | Overlap

decomposeSeqP :: Var v => Set v -> SeqMatch -> PatternP v -> SeqCover v
decomposeSeqP _ E (SequenceLiteralP _ []) = Cover []
decomposeSeqP _ E _ = Disjoint

decomposeSeqP _ C (SequenceOpP _ l Cons r) = Cover [l,r]
decomposeSeqP _ C (SequenceOpP _ _ Concat _) = Overlap
decomposeSeqP _ C (SequenceLiteralP _ []) = Disjoint
decomposeSeqP avoid C (SequenceLiteralP _ (p:ps))
  = Cover [p, SequenceLiteralP u ps]
  where u = freshIn avoid $ typed Pattern

decomposeSeqP _ S (SequenceOpP _ l Snoc r) = Cover [l,r]
decomposeSeqP _ S (SequenceOpP _ _ Concat _) = Overlap
decomposeSeqP _ S (SequenceLiteralP _ []) = Disjoint
decomposeSeqP avoid S (SequenceLiteralP _ ps)
  = Cover [SequenceLiteralP u (init ps), last ps]
  where u = freshIn avoid $ typed Pattern

decomposeSeqP _ (L n) (SequenceOpP _ l Concat r)
  | Just m <- seqPSize l
  , n == m
  = Cover [l,r]
  | otherwise = Overlap
decomposeSeqP avoid (L n) (SequenceLiteralP _ ps)
  | length ps >= n
  , (pl, pr) <- splitAt n ps
  = Cover $ SequenceLiteralP u <$> [pl,pr]
  | otherwise = Disjoint
  where u = freshIn avoid $ typed Pattern

decomposeSeqP _ (R n) (SequenceOpP _ l Concat r)
  | Just m <- seqPSize r
  , n == m
  = Cover [l,r]
decomposeSeqP avoid (R n) (SequenceLiteralP _ ps)
  | length ps >= n
  , (pl, pr) <- splitAt (length ps - n) ps
  = Cover $ SequenceLiteralP u <$> [pl,pr]
  | otherwise = Disjoint
  where u = freshIn avoid $ typed Pattern

decomposeSeqP _ (DL n) (SequenceOpP _ l Concat _)
  | Just m <- seqPSize l , n == m = Disjoint
decomposeSeqP _ (DL n) (SequenceLiteralP _ ps)
  | length ps >= n = Disjoint

decomposeSeqP _ (DR n) (SequenceOpP _ _ Concat r)
  | Just m <- seqPSize r , n == m = Disjoint
decomposeSeqP _ (DR n) (SequenceLiteralP _ ps)
  | length ps >= n = Disjoint

decomposeSeqP _ _ _ = Overlap

splitRow
  :: Var v
  => v
  -> Int
  -> Int
  -> PatternRow v
  -> [([PatternP v], PatternRow v)]
splitRow v t nfields (PR (break ((==v).loc) -> (pl, sp : pr)) g b)
  = decomposePattern t nfields sp
      <&> \subs -> (subs, PR (pl ++ filter refutable subs ++ pr) g b)
splitRow _ _ _ row = [([],row)]

splitRowBuiltin
  :: Var v
  => v
  -> PatternRow v
  -> [(PatternP (), [([PatternP v], PatternRow v)])]
splitRowBuiltin v (PR (break ((==v).loc) -> (pl, sp : pr)) g b)
  | Just p <- matchBuiltin sp = [(p, [([], PR (pl ++ pr) g b)])]
  | otherwise = []
splitRowBuiltin _ r = [(UnboundP (), [([], r)])]

splitRowSeq
  :: Var v
  => v
  -> SeqMatch
  -> PatternRow v
  -> [([PatternP v], PatternRow v)]
splitRowSeq v m r@(PR (break ((==v).loc) -> (pl, sp : pr)) g b)
  = case decomposeSeqP avoid m sp of
      Cover sps -> 
        [(sps, PR (pl ++ filter refutable sps ++ pr) g b)]
      Disjoint -> []
      Overlap -> [([], r)]
  where avoid = maybe mempty freeVars g <> freeVars b
splitRowSeq _ _ r = [([], r)]

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
  => [([PatternP v], PatternRow v)]
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
  => v
  -> PatternMatrix v
  -> [(PatternP (), [(v,Reference)], PatternMatrix v)]
splitMatrixBuiltin v (PM rs)
  = fmap (\(a,(b,c)) -> (a,b,c))
  . toList
  . fmap buildMatrix
  . fromListWith (++)
  $ splitRowBuiltin v =<< rs

matchPattern :: [(v,Reference)] -> SeqMatch -> PatternP ()
matchPattern vrs = \case
    E -> sz 0
    C -> SequenceOpP () vr Cons vr
    S -> SequenceOpP () vr Snoc vr
    L n -> SequenceOpP () (sz n) Concat (VarP ())
    R n -> SequenceOpP () (VarP ()) Concat (sz n)
    DL _ -> UnboundP ()
    DR _ -> UnboundP ()
  where
  vr | [] <- vrs = UnboundP () | otherwise = VarP ()
  sz n = SequenceLiteralP () . replicate n $ UnboundP ()

splitMatrixSeq
  :: Var v
  => v
  -> PatternMatrix v
  -> [(PatternP (), [(v,Reference)], PatternMatrix v)]
splitMatrixSeq v (PM rs)
  = cases
  where
  ms = decideSeqPat $ take 1 . dropWhile ((/=v).loc) . matches =<< rs
  hint m vrs
    | m `elem` [E,C,S] = vrs
    | otherwise = (fmap.fmap) (const Rf.vectorRef) vrs
  cases = ms <&> \m ->
    let frs = rs >>= splitRowSeq v m
        (vrs, pm)
          | null frs = ([], PM [])
          | otherwise = buildMatrix frs
    in (matchPattern vrs m, hint m vrs, pm)

splitMatrix
  :: Var v
  => v
  -> Either Cons Cons
  -> PatternMatrix v
  -> [(Int, [(v,Reference)], PatternMatrix v)]
splitMatrix v econs (PM rs)
  = fmap (\(a, (b, c)) -> (a,b,c)) . (fmap.fmap) buildMatrix $ mmap
  where
  cons = either (((-1,1):) . zip [0..]) (zip [0..]) econs
  mmap = fmap (\(t,fs) -> (t, splitRow v t fs =<< rs)) cons

type PPM v a = State (Word64, [v], Map v v) a

freshVar :: Var v => PPM v v
freshVar = state $ \(fw, vs, rn) ->
  let v = freshenId fw $ typed Pattern
  in (v, (fw+1, vs, rn))

useVar :: PPM v v
useVar = state $ \(avoid, v:vs, rn) -> (v, (avoid, vs, rn))

renameTo :: Var v => v -> v -> PPM v ()
renameTo to from
  = modify $ \(avoid, vs, rn) ->
      ( avoid, vs
      , insertWith (error "renameTo: duplicate rename") from to rn
      )

normalizeSeqP :: PatternP a -> PatternP a
normalizeSeqP (AsP a p) = AsP a (normalizeSeqP p)
normalizeSeqP (EffectPureP a p) = EffectPureP a $ normalizeSeqP p
normalizeSeqP (EffectBindP a r i ps k)
  = EffectBindP a r i (normalizeSeqP <$> ps) (normalizeSeqP k)
normalizeSeqP (ConstructorP a r i ps)
  = ConstructorP a r i $ normalizeSeqP <$> ps
normalizeSeqP (SequenceLiteralP a ps)
  = SequenceLiteralP a $ normalizeSeqP <$> ps
normalizeSeqP (SequenceOpP a p0 op q0)
  = case (op, normalizeSeqP p0, normalizeSeqP q0) of
      (Cons, p, SequenceLiteralP _ ps)
        -> SequenceLiteralP a (p:ps)
      (Snoc, SequenceLiteralP _ ps, p)
        -> SequenceLiteralP a (ps ++ [p])
      (Concat, SequenceLiteralP _ ps, SequenceLiteralP _ qs)
        -> SequenceLiteralP a (ps ++ qs)
      (Concat, SequenceLiteralP _ ps, q)
        -> foldr (\p r -> SequenceOpP a p Cons r) q ps
      (Concat, p, SequenceLiteralP _ qs)
        -> foldl (\r q -> SequenceOpP a r Snoc q) p qs
      (op, p, q) -> SequenceOpP a p op q
normalizeSeqP p = p

prepareAs :: Var v => PatternP a -> v -> PPM v (PatternP v)
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

preparePattern :: Var v => PatternP a -> PPM v (PatternP v)
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

compile :: Var v => DataSpec -> Ctx v -> PatternMatrix v -> Term v
compile _ _ (PM []) = blank ()
compile spec ctx m@(PM (r:rs))
  | rowIrrefutable r
  = case guard r of
      Nothing -> body r
      Just g -> iff mempty g (body r) $ compile spec ctx (PM rs)
  | rf == Rf.vectorRef
  = match () (var () v)
      $ buildCaseBuiltin spec ctx
     <$> splitMatrixSeq v m
  | rf `member` builtinCase
  = match () (var () v)
      $ buildCaseBuiltin spec ctx
     <$> splitMatrixBuiltin v m
  | otherwise
  = case Map.lookup rf spec of
      Just cons ->
        match () (var () v)
          $ buildCase spec rf cons ctx
         <$> splitMatrix v cons m
      Nothing -> error $ "unknown data reference: " ++ show r
  where
  v = choose heuristics m
  rf = Map.findWithDefault defaultRef v ctx

buildCaseBuiltin
  :: Var v
  => DataSpec
  -> Ctx v
  -> (PatternP (), [(v,Reference)], PatternMatrix v)
  -> MatchCase () (Term v)
buildCaseBuiltin spec ctx0 (p, vrs, m)
  = MatchCase p Nothing . absChain' vs $ compile spec ctx m
  where
  vs = ((),) . fst <$> vrs
  ctx = Map.fromList vrs <> ctx0

buildCase
  :: Var v
  => DataSpec
  -> Reference
  -> Either Cons Cons
  -> Ctx v
  -> (Int, [(v,Reference)], PatternMatrix v)
  -> MatchCase () (Term v)
buildCase spec r econs ctx0 (t, vrs, m)
  = MatchCase pat Nothing . absChain' vs $ compile spec ctx m
  where
  (eff, cons) = either (True,) (False,) econs
  pat = buildPattern eff r t vs $ cons !! t
  vs = ((),) . fst <$> vrs
  ctx = Map.fromList vrs <> ctx0

mkRow
  :: Var v
  => v
  -> MatchCase a (Term v)
  -> State Word64 (PatternRow v)
mkRow sv (MatchCase (normalizeSeqP -> p0) g0 (AbsN' vs b))
  = state $ \w -> case runState (prepareAs p0 sv) (w, vs, mempty) of
      (p, (w, [], rn)) -> (,w)
        $ PR (filter refutable [p])
             (changeVars rn <$> g)
             (changeVars rn b)
      _ -> error "mkRow: not all variables used"
  where
  g = case g0 of
        Just (AbsN' us g)
          | us == vs -> Just g
          | otherwise -> error "mkRow: guard variables do not match body"
        Nothing -> Nothing
        _ -> error "mkRow: impossible"
mkRow _ _ = error "mkRow: impossible"

initialize
  :: Var v
  => Reference
  -> Term v
  -> [MatchCase () (Term v)]
  -> (Maybe v, (v, Reference), PatternMatrix v)
initialize r sc cs
  = ( lv
    , (sv, r)
    , PM $ evalState (traverse (mkRow sv) cs) 1
    )
  where
  (lv, sv) | Var' v <- sc = (Nothing, v)
           | pv <- freshenId 0 $ typed Pattern
           = (Just pv, pv)

splitPatterns :: Var v => DataSpec -> Term v -> Term v
splitPatterns spec0 = visitPure $ \case
  Match' sc0 cs0
    | r <- determineType $ p <$> cs0
    , (lv, scrut, pm) <- initialize r sc cs
    , body <- compile spec (uncurry Map.singleton scrut) pm
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

determineType :: Show a => [PatternP a] -> Reference
determineType ps = fromMaybe defaultRef . foldr ((<|>) . f) Nothing $ ps
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
