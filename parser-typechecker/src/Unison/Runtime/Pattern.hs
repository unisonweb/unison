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
import qualified Unison.Pattern as P
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
  { matches :: [Pattern v]
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

refutable :: P.Pattern a -> Bool
refutable (P.Unbound _) = False
refutable (P.Var _) = False
refutable _ = True

rowIrrefutable :: PatternRow v -> Bool
rowIrrefutable (PR ps _ _) = null ps

firstRow :: ([P.Pattern v] -> Maybe v) -> Heuristic v
firstRow f (PM (r:_)) = f $ matches r
firstRow _ _ = Nothing

heuristics :: [Heuristic v]
heuristics = [firstRow $ fmap loc . listToMaybe]

extractVar :: Var v => P.Pattern v -> Maybe v
extractVar p
  | P.Unbound{} <- p = Nothing
  | otherwise = Just (loc p)

extractVars :: Var v => [P.Pattern v] -> [v]
extractVars = catMaybes . fmap extractVar

decomposePattern
  :: Var v
  => Int -> Int -> P.Pattern v
  -> [[P.Pattern v]]
decomposePattern t nfields p@(P.Constructor _ _ u ps)
  | t == u
  = if length ps == nfields
    then [ps]
    else error err
  where
  err = "decomposePattern: wrong number of constructor fields: "
     ++ show (nfields, p)
decomposePattern t nfields p@(P.EffectBind _ _ u ps pk)
  | t == u
  = if length ps == nfields
    then [ps ++ [pk]]
    else error err
  where
  err = "decomposePattern: wrong number of ability fields: "
     ++ show (nfields, p)
decomposePattern t _ (P.EffectPure _ p)
  | t == -1 = [[p]]
decomposePattern _ nfields (P.Var _)
  = [replicate nfields (P.Unbound (typed Pattern))]
decomposePattern _ nfields (P.Unbound _)
  = [replicate nfields (P.Unbound (typed Pattern))]
decomposePattern _ _ (P.SequenceLiteral _ _)
  = error "decomposePattern: sequence literal"
decomposePattern _ _ _ = []

matchBuiltin :: P.Pattern a -> Maybe (P.Pattern ())
matchBuiltin (P.Var _) = Just $ P.Unbound ()
matchBuiltin (P.Unbound _) = Just $ P.Unbound ()
matchBuiltin (P.Nat _ n) = Just $ P.Nat () n
matchBuiltin (P.Int _ n) = Just $ P.Int () n
matchBuiltin (P.Text _ t) = Just $ P.Text () t
matchBuiltin (P.Char _ c) = Just $ P.Char () c
matchBuiltin (P.Float _ d) = Just $ P.Float () d
matchBuiltin _ = Nothing

data SeqMatch = E | C | S | L Int | R Int | DL Int | DR Int
  deriving (Eq,Ord,Show)

seqPSize :: P.Pattern v -> Maybe Int
seqPSize (P.SequenceLiteral _ l) = Just $ length l
seqPSize (P.SequenceOp _ _ Cons r) = (1+) <$> seqPSize r
seqPSize (P.SequenceOp _ l Snoc _) = (1+) <$> seqPSize l
seqPSize (P.SequenceOp _ l Concat r) = (+) <$> seqPSize l <*> seqPSize r
seqPSize _ = Nothing

decideSeqPat :: [P.Pattern v] -> [SeqMatch]
decideSeqPat = go False
  where
  go _ [] = [E,C]
  go _ (P.SequenceLiteral{} : ps) = go True ps
  go _ (P.SequenceOp _ _ Snoc _ : _) = [E,S]
  go _ (P.SequenceOp _ _ Cons _ : _) = [E,C]

  go guard (P.SequenceOp _ l Concat r : _)
    | guard = [E,C] -- prefer prior literals
    | Just n <- seqPSize l = [L n, DL n]
    | Just n <- seqPSize r = [R n, DR n]
  go b (P.Unbound _ : ps) = go b ps
  go b (P.Var _ : ps) = go b ps
  go _ (p:_)
    = error $ "Cannot process sequence pattern: " ++ show p

data SeqCover v
  = Cover [P.Pattern v]
  | Disjoint
  | Overlap

decomposeSeqP :: Var v => Set v -> SeqMatch -> P.Pattern v -> SeqCover v
decomposeSeqP _ E (P.SequenceLiteral _ []) = Cover []
decomposeSeqP _ E _ = Disjoint

decomposeSeqP _ C (P.SequenceOp _ l Cons r) = Cover [l,r]
decomposeSeqP _ C (P.SequenceOp _ _ Concat _) = Overlap
decomposeSeqP _ C (P.SequenceLiteral _ []) = Disjoint
decomposeSeqP avoid C (P.SequenceLiteral _ (p:ps))
  = Cover [p, P.SequenceLiteral u ps]
  where u = freshIn avoid $ typed Pattern

decomposeSeqP _ S (P.SequenceOp _ l Snoc r) = Cover [l,r]
decomposeSeqP _ S (P.SequenceOp _ _ Concat _) = Overlap
decomposeSeqP _ S (P.SequenceLiteral _ []) = Disjoint
decomposeSeqP avoid S (P.SequenceLiteral _ ps)
  = Cover [P.SequenceLiteral u (init ps), last ps]
  where u = freshIn avoid $ typed Pattern

decomposeSeqP _ (L n) (P.SequenceOp _ l Concat r)
  | Just m <- seqPSize l
  , n == m
  = Cover [l,r]
  | otherwise = Overlap
decomposeSeqP avoid (L n) (P.SequenceLiteral _ ps)
  | length ps >= n
  , (pl, pr) <- splitAt n ps
  = Cover $ P.SequenceLiteral u <$> [pl,pr]
  | otherwise = Disjoint
  where u = freshIn avoid $ typed Pattern

decomposeSeqP _ (R n) (P.SequenceOp _ l Concat r)
  | Just m <- seqPSize r
  , n == m
  = Cover [l,r]
decomposeSeqP avoid (R n) (P.SequenceLiteral _ ps)
  | length ps >= n
  , (pl, pr) <- splitAt (length ps - n) ps
  = Cover $ P.SequenceLiteral u <$> [pl,pr]
  | otherwise = Disjoint
  where u = freshIn avoid $ typed Pattern

decomposeSeqP _ (DL n) (P.SequenceOp _ l Concat _)
  | Just m <- seqPSize l , n == m = Disjoint
decomposeSeqP _ (DL n) (P.SequenceLiteral _ ps)
  | length ps >= n = Disjoint

decomposeSeqP _ (DR n) (P.SequenceOp _ _ Concat r)
  | Just m <- seqPSize r , n == m = Disjoint
decomposeSeqP _ (DR n) (P.SequenceLiteral _ ps)
  | length ps >= n = Disjoint

decomposeSeqP _ _ _ = Overlap

splitRow
  :: Var v
  => v
  -> Int
  -> Int
  -> PatternRow v
  -> [([P.Pattern v], PatternRow v)]
splitRow v t nfields (PR (break ((==v).loc) -> (pl, sp : pr)) g b)
  = decomposePattern t nfields sp
      <&> \subs -> (subs, PR (pl ++ filter refutable subs ++ pr) g b)
splitRow _ _ _ row = [([],row)]

splitRowBuiltin
  :: Var v
  => v
  -> PatternRow v
  -> [(P.Pattern (), [([P.Pattern v], PatternRow v)])]
splitRowBuiltin v (PR (break ((==v).loc) -> (pl, sp : pr)) g b)
  | Just p <- matchBuiltin sp = [(p, [([], PR (pl ++ pr) g b)])]
  | otherwise = []
splitRowBuiltin _ r = [(P.Unbound (), [([], r)])]

splitRowSeq
  :: Var v
  => v
  -> SeqMatch
  -> PatternRow v
  -> [([P.Pattern v], PatternRow v)]
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

chooseVars :: Var v => [[P.Pattern v]] -> [v]
chooseVars [] = []
chooseVars ([]:rs) = chooseVars rs
chooseVars ((P.Unbound{} : _) : rs) = chooseVars rs
chooseVars (r : _) = extractVars r

buildMatrix
  :: Var v
  => [([P.Pattern v], PatternRow v)]
  -> ([(v,Reference)], PatternMatrix v)
buildMatrix [] = ([], PM [])
buildMatrix vrs = (zip cvs rs, PM $ fixRow <$> vrs)
  where
  rs = fmap determineType . transpose . fmap fst $ vrs
  cvs = chooseVars $ fst <$> vrs
  fixRow (extractVars -> rvs, pr)
    = renameRow (fromListWith const . zip rvs $ cvs) pr

splitMatrixBuiltin
  :: Var v
  => v
  -> PatternMatrix v
  -> [(P.Pattern (), [(v,Reference)], PatternMatrix v)]
splitMatrixBuiltin v (PM rs)
  = fmap (\(a,(b,c)) -> (a,b,c))
  . toList
  . fmap buildMatrix
  . fromListWith (++)
  $ splitRowBuiltin v =<< rs

matchPattern :: [(v,Reference)] -> SeqMatch -> P.Pattern ()
matchPattern vrs = \case
    E -> sz 0
    C -> P.SequenceOp () vr Cons vr
    S -> P.SequenceOp () vr Snoc vr
    L n -> P.SequenceOp () (sz n) Concat (P.Var ())
    R n -> P.SequenceOp () (P.Var ()) Concat (sz n)
    DL _ -> P.Unbound ()
    DR _ -> P.Unbound ()
  where
  vr | [] <- vrs = P.Unbound () | otherwise = P.Var ()
  sz n = P.SequenceLiteral () . replicate n $ P.Unbound ()

splitMatrixSeq
  :: Var v
  => v
  -> PatternMatrix v
  -> [(P.Pattern (), [(v,Reference)], PatternMatrix v)]
splitMatrixSeq v (PM rs)
  = cases
  where
  ms = decideSeqPat $ take 1 . dropWhile ((/=v).loc) . matches =<< rs
  hint m vrs
    | m `elem` [E,C,S] = vrs
    | otherwise = (fmap.fmap) (const Rf.vectorRef) vrs
  cases = ms <&> \m ->
    let frs = rs >>= splitRowSeq v m
        (vrs, pm) = buildMatrix frs
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

normalizeSeqP :: P.Pattern a -> P.Pattern a
normalizeSeqP (P.As a p) = P.As a (normalizeSeqP p)
normalizeSeqP (P.EffectPure a p) = P.EffectPure a $ normalizeSeqP p
normalizeSeqP (P.EffectBind a r i ps k)
  = P.EffectBind a r i (normalizeSeqP <$> ps) (normalizeSeqP k)
normalizeSeqP (P.Constructor a r i ps)
  = P.Constructor a r i $ normalizeSeqP <$> ps
normalizeSeqP (P.SequenceLiteral a ps)
  = P.SequenceLiteral a $ normalizeSeqP <$> ps
normalizeSeqP (P.SequenceOp a p0 op q0)
  = case (op, normalizeSeqP p0, normalizeSeqP q0) of
      (Cons, p, P.SequenceLiteral _ ps)
        -> P.SequenceLiteral a (p:ps)
      (Snoc, P.SequenceLiteral _ ps, p)
        -> P.SequenceLiteral a (ps ++ [p])
      (Concat, P.SequenceLiteral _ ps, P.SequenceLiteral _ qs)
        -> P.SequenceLiteral a (ps ++ qs)
      (Concat, P.SequenceLiteral _ ps, q)
        -> foldr (\p r -> P.SequenceOp a p Cons r) q ps
      (Concat, p, P.SequenceLiteral _ qs)
        -> foldl (\r q -> P.SequenceOp a r Snoc q) p qs
      (op, p, q) -> P.SequenceOp a p op q
normalizeSeqP p = p

prepareAs :: Var v => P.Pattern a -> v -> PPM v (P.Pattern v)
prepareAs (P.Unbound _) u = pure $ P.Var u
prepareAs (P.As _ p) u = prepareAs p u <* (renameTo u =<< useVar)
prepareAs (P.Var _) u = P.Var u <$ (renameTo u =<< useVar)
prepareAs (P.Constructor _ r i ps) u = do
  P.Constructor u r i <$> traverse preparePattern ps
prepareAs (P.EffectPure _ p) u = do
  P.EffectPure u <$> preparePattern p
prepareAs (P.EffectBind _ r i ps k) u = do
  P.EffectBind u r i
    <$> traverse preparePattern ps
    <*> preparePattern k
prepareAs (P.SequenceLiteral _ ps) u = do
  P.SequenceLiteral u <$> traverse preparePattern ps
prepareAs (P.SequenceOp _ p op q) u = do
  flip (P.SequenceOp u) op
    <$> preparePattern p
    <*> preparePattern q
prepareAs p u = pure $ u <$ p

preparePattern :: Var v => P.Pattern a -> PPM v (P.Pattern v)
preparePattern (P.Unbound _) = P.Var <$> freshVar
preparePattern (P.Var _) = P.Var <$> useVar
preparePattern (P.As _ p) = prepareAs p =<< useVar
preparePattern p = prepareAs p =<< freshVar

buildPattern :: Bool -> Reference -> Int -> [v] -> Int -> P.Pattern ()
buildPattern ef r t vs nfields =
  case ef of
    False -> P.Constructor () r t vps
    True | t == -1 -> P.EffectPure () (P.Var ())
         | otherwise -> P.EffectBind () r t aps kp
     where
     (aps,kp) | [] <- vps = error "too few patterns for effect bind"
              | otherwise = (init vps, last vps)
  where
  vps | length vs < nfields
      = replicate nfields $ P.Unbound ()
      | otherwise
      = P.Var () <$ vs

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
  -> (P.Pattern (), [(v,Reference)], PatternMatrix v)
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

determineType :: Show a => [P.Pattern a] -> Reference
determineType ps = fromMaybe defaultRef . foldr ((<|>) . f) Nothing $ ps
  where
  f (P.As _ p) = f p
  f P.Int{} = Just Rf.intRef
  f P.Nat{} = Just Rf.natRef
  f P.Float{} = Just Rf.floatRef
  f P.Boolean{} = Just Rf.booleanRef
  f P.Text{} = Just Rf.textRef
  f P.Char{} = Just Rf.charRef
  f P.SequenceLiteral{} = Just Rf.vectorRef
  f P.SequenceOp{} = Just Rf.vectorRef
  f (P.Constructor _ r _ _) = Just r
  f (P.EffectBind _ r _ _ _) = Just r
  f _ = Nothing
