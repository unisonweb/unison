{-# LANGUAGE PatternSynonyms #-}

-- | Loop state exploded into parameters.
--
-- A function of a batch whose every saturated call in the batch passes,
-- at some argument, a product the caller built for the call (a let of a
-- constructor application or a record literal, @(acc, i)@ in
-- @go (acc, i)@, or a record update of such a value) and whose body only
-- takes that argument apart takes its fields as parameters instead: a
-- worker, a second combinator of the group at the lambda's index with
-- 'explodedBit' set, whose parameters are the fields and whose body is the
-- lambda's with each match on the argument replaced by the matching arm's
-- body and each update of it by a literal of the fields. The lambda's own
-- index becomes the entry for callers the batch cannot see (a later batch,
-- a function value, a partial application): it matches the argument and
-- calls the worker, one unpack per entry from outside. A call in the batch
-- goes to the worker with the field variables, which are in scope where
-- the product was built, and the let that built it is dropped when nothing
-- else uses it ('deadCons'), so a loop whose state is a tuple or a record
-- allocates nothing per round. Fields that are products built for the call
-- in turn explode as well, which is what a tuple's nesting needs. This is
-- the argument half of the worker/wrapper transformation, done for the
-- call sites the loader can see ('explosions', run over each batch
-- 'Unison.Runtime.Machine.cacheAdd' loads) and without any change to how
-- values are represented: the worker's parameters are ordinary slots, so
-- nothing here depends on the wide convention (Unison.Runtime.Wide) or
-- changes how a frame is written out ('Unison.Runtime.MCode.FrameLayout').
--
-- The argument is taken apart only where the body consumes it: an
-- argument the body passes on, stores or returns on some path stays a
-- plain parameter ('usable'), so nothing is ever rebuilt. A site that
-- passes anything else at an exploded position, or a different product,
-- makes the position plain for everyone ('meet'); sites in a later batch
-- are not seen and go through the entry.
--
-- What it does not change: the groups the cache stores and hashes (the
-- rewrite is a function of a batch's optimized code, like inlining, run
-- again at every load), a compiled program's file (its MCode has the
-- workers in it), and the serialized form of a continuation (a worker's
-- frame holds plain slots; its index is a combinator of its group like any
-- other). A continuation captured inside a worker names the worker's
-- index, so a process that loads the same code with the rewrite off
-- cannot resume it, as with any difference in the optimizer between two
-- versions.
--
-- @UNISON_EXPLODE=off@ switches it off ('explodeOn'), for measuring and
-- for a process that has to agree with one that ran without it.
module Unison.Runtime.MCode.Explode
  ( explodeOn,
    explodedBit,
    Shape (..),
    Explosions,
    explosions,
    Split (..),
    split,
    Built (..),
    ConLets (..),
    conLets,
    uniqueBinders,
    isBuiltLet,
    deadCons,
    explodedArgs,
  )
where

import Control.Monad.State.Strict (evalState, get, put)
import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Word (Word64)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Unison.ABT.Normalized (pattern TAbss)
import Unison.ABT.Normalized qualified as ABTN
import Unison.Reference (Reference)
import Unison.Runtime.ANF
import Unison.Runtime.ANF qualified as ANF
import Unison.Util.EnumContainers qualified as EC
import Unison.Var (Var)
import Unison.Var qualified as Var

-- | Whether loop state is exploded in this process: on unless
-- @UNISON_EXPLODE=off@. Read once.
explodeOn :: Bool
explodeOn = unsafePerformIO $ do
  v <- lookupEnv "UNISON_EXPLODE"
  pure $ (map toLower <$> v) /= Just "off"
{-# NOINLINE explodeOn #-}

-- | The combinator index of a lambda's worker, from the lambda's own: a
-- bit no let index reaches (the lets of a combinator are numbered from 1
-- in its low sixteen bits, a group's local functions in the high bits).
-- The JIT treats a worker like the entry point it stands for (Compile's
-- @isEntry@).
explodedBit :: Word64
explodedBit = 0x8000

-- | How an argument position is taken apart: left as one slot, or a
-- product, a data constructor or a record (@Fields@, its fields in the schema's
-- order, which is the field names' order), whose fields are taken apart
-- in turn.
data Shape = Plain | Con !Reference !CTag [Shape] | Fields !RecordSchema [Shape]
  deriving (Eq, Ord, Show)

isPlain :: Shape -> Bool
isPlain Plain = True
isPlain _ = False

subShapes :: Shape -> [Shape]
subShapes Plain = []
subShapes (Con _ _ ss) = ss
subShapes (Fields _ ss) = ss

withSubShapes :: Shape -> [Shape] -> Shape
withSubShapes Plain _ = Plain
withSubShapes (Con r t _) ss = Con r t ss
withSubShapes (Fields rs _) ss = Fields rs ss

-- | Whether two shapes are the same product at the top.
sameProduct :: Shape -> Shape -> Bool
sameProduct (Con r t as) (Con r' t' bs) = r == r' && t == t' && length as == length bs
sameProduct (Fields rs as) (Fields rs' bs) = rs == rs' && length as == length bs
sameProduct _ _ = False

-- | The shapes two sites agree on: the same product at each node, else
-- plain.
meet :: Shape -> Shape -> Shape
meet a b
  | sameProduct a b = withSubShapes a (zipWith meet (subShapes a) (subShapes b))
  | otherwise = Plain

-- | The shapes of the arguments of each exploded function of a batch, by
-- its reference: present only for a function with at least one exploded
-- position.
type Explosions = M.Map Reference [Shape]

-- | The body with a name of its own for every binder. Normalization reuses
-- a name under shadowing (the blank variables), and 'ABTN.freshen' gives
-- a name back once the binder's scope is over, while everything here maps
-- variables by name; so a body is renamed, each binder to a name no other
-- in the body has, before it is analysed, rewritten or emitted with the
-- explosion on.
uniqueBinders :: (Var v) => ANormal Reference v -> ANormal Reference v
uniqueBinders bd = evalState (go M.empty bd) (Set.unions (ABTN.freeVars bd : map boundVars (universe bd)))
  where
    go rn (ABTN.Term _ (ABTN.Abs u body)) = do
      used <- get
      let u' = Var.freshIn used u
      put (Set.insert u' used)
      ABTN.TAbs u' <$> go (M.insert u u' rn) body
    go rn (ABTN.Term _ (ABTN.Tm f)) =
      ABTN.TTm <$> traverse (go rn) (first (\v -> M.findWithDefault v v rn) f)

-- | A product a let built: which one, and its field variables, in the
-- shape's field order.
data Built v = BuiltCon Reference CTag [v] | BuiltRec RecordSchema [v]

builtFields :: Built v -> [v]
builtFields (BuiltCon _ _ fs) = fs
builtFields (BuiltRec _ fs) = fs

builtShape :: Built v -> [Shape] -> Shape
builtShape (BuiltCon r t _) = Con r t
builtShape (BuiltRec rs _) = Fields rs

matchesBuilt :: Shape -> Built v -> Bool
matchesBuilt (Con r t ss) (BuiltCon r' t' fs) = r == r' && t == t' && length ss == length fs
matchesBuilt (Fields rs ss) (BuiltRec rs' fs) = rs == rs' && length ss == length fs
matchesBuilt _ _ = False

-- | The product lets of a lambda: for each variable bound to a constructor
-- application, a record literal, or a record update of a variable whose
-- fields are known, the product and its field variables ('builtOf'); for
-- an update, the variable it updated ('baseOf'), whose use the update is;
-- and for an update of one of the lambda's own parameters, or of such an
-- update, the parameter's position and the fields replaced ('rootedOf'):
-- a loop stepping its record state. Binders are unique ('uniqueBinders').
data ConLets v = ConLets
  { builtOf :: M.Map v (Built v),
    baseOf :: M.Map v v,
    rootedOf :: M.Map v (Int, [(FieldName, v)])
  }

conLets :: (Var v) => [v] -> ANormal Reference v -> ConLets v
conLets params = go (ConLets M.empty M.empty M.empty)
  where
    go acc tm = case tm of
      TLets _ [u] _ bn bo
        | TApp (FCon r t) args <- bn -> go (bind u (BuiltCon r t args) acc) bo
        | TApp (FRec rs) args <- bn -> go (bind u (BuiltRec rs args) acc) bo
        | TApp (FRecSet f) [base, x] <- bn,
          Just (BuiltRec rs fs) <- M.lookup base (builtOf acc),
          Just j <- elemIndex f (schemaFields rs) ->
            go (bind u (BuiltRec rs (take j fs ++ x : drop (j + 1) fs)) acc {baseOf = M.insert u base (baseOf acc)}) bo
        | TApp (FRecSet f) [base, x] <- bn,
          Just (j, repls) <- rootOf base ->
            go acc {rootedOf = M.insert u (j, repls ++ [(f, x)]) (rootedOf acc)} bo
      _ -> foldl' go acc (children tm)
      where
        rootOf base
          | Just j <- elemIndex base params = Just (j, [])
          | otherwise = M.lookup base (rootedOf acc)
    bind u b acc = acc {builtOf = M.insert u b (builtOf acc)}

-- | Whether a let binds a product ('conLets' records it), so that its
-- instruction can be left out when the product is dead.
isBuiltLet :: (Var v) => ANormal Reference v -> Bool
isBuiltLet = \case
  TApp (FCon _ _) _ -> True
  TApp (FRec _) _ -> True
  TApp (FRecSet _) _ -> True
  _ -> False

schemaFields :: RecordSchema -> [FieldName]
schemaFields (RecordSchema fields) = Set.toAscList fields

-- | The shape a call site offers at an argument: the product the variable
-- was built from, its fields' shapes below, to a depth that bounds the
-- worker's arity. An update of a parameter of the calling lambda offers
-- the parameter's own shape with the replaced fields' shapes in it, which
-- is known only from the round before ('explosions'): Nothing until then,
-- and plain once the parameter is.
siteShape :: (Var v) => (Int -> Maybe Shape) -> ConLets v -> Int -> v -> Maybe Shape
siteShape paramShape cons depth a
  | depth >= maxDepth = Just Plain
  | Just b <- M.lookup a (builtOf cons) = Just (builtShape b (map sub (builtFields b)))
  | Just (j, repls) <- M.lookup a (rootedOf cons) = case paramShape j of
      Just (Fields rs _) -> Just (Fields rs [maybe Plain sub (lookup f (reverse repls)) | f <- schemaFields rs])
      Just _ -> Just Plain
      Nothing -> Nothing
  | otherwise = Just Plain
  where
    maxDepth = 8
    sub x = fromMaybe Plain (siteShape paramShape cons (depth + 1) x)

-- | The explosions of a batch of groups: for each group's entry point, the
-- meet of the shapes its saturated calls in the batch offer, narrowed to
-- what its body consumes. A site that passes an updated parameter offers
-- the parameter's shape, so the result is a fixpoint: a first round
-- without those sites, then rounds with them until nothing changes (a
-- shape only ever becomes plainer).
explosions :: (Var v) => M.Map Reference (SuperGroup Reference v) -> Explosions
explosions groups
  | not explodeOn = M.empty
  | otherwise = settle (roundWith Nothing)
  where
    settle guess
      | next == guess = guess
      | otherwise = settle next
      where
        next = roundWith (Just guess)
    arityOf r = case ANF.arities <$> M.lookup r groups of
      Just (n : _) -> Just n
      _ -> Nothing
    roundWith guess = M.mapMaybeWithKey narrow (M.mapMaybe sequence (offeredWith guess))
    offeredWith guess =
      M.fromListWith (zipWith meetMaybe) $
        [ (r, map (siteShape (paramShape guess gr vs) cons 0) args)
          | (gr, g) <- M.toList groups,
            lam@(Lambda _ (TAbss vs _)) <- lambdas g,
            let bd = uniqueBinders (lambdaBody lam),
            let cons = conLets vs bd,
            TApp (FComb r) args <- universe bd,
            Just n <- [arityOf r],
            n == length args
        ]
    -- the shape of a lambda's parameter by the last round: only a group's
    -- entry point has one; a group left out of the guess is all plain
    paramShape guess gr vs j
      | Just (Rec _ (Lambda _ (TAbss evs _))) <- M.lookup gr groups, evs == vs = case guess of
          Nothing -> Nothing
          Just m -> Just (maybe Plain (\shapes -> if j < length shapes then shapes !! j else Plain) (M.lookup gr m))
      | otherwise = Just Plain
    meetMaybe (Just a) (Just b) = Just (meet a b)
    meetMaybe a Nothing = a
    meetMaybe Nothing b = b
    narrow r shapes
      | Just (Rec _ (Lambda _ (TAbss vs bd0))) <- M.lookup r groups,
        length vs == length shapes,
        let bd = uniqueBinders bd0,
        let final = zipWith (\s v -> usable s v bd) shapes vs,
        not (all isPlain final) =
          Just final
      | otherwise = Nothing

-- | The part of a shape the body consumes: a product node stays when every
-- occurrence of the variable is a data match with an arm for the
-- constructor, a record match (of any of its fields), or a record update,
-- and its fields are what the arms' bodies consume of them; an update
-- passes every field on into the new record, so it leaves the fields
-- plain. An argument used any other way, or matched without an arm for
-- the constructor it is built from, is left plain.
usable :: (Var v) => Shape -> v -> ANormal Reference v -> Shape
usable Plain _ _ = Plain
usable s v bd
  | occurrences v bd /= length matches + length updates = Plain
  | any null arms = Plain
  | not (null updates) = withSubShapes s (map (const Plain) subs)
  | otherwise = withSubShapes s [foldl' meet sub [usable sub f bo | (f, bo) <- fieldArms j] | (j, sub) <- zip [0 ..] subs]
  where
    subs = subShapes s
    matches = [bs | TMatch u bs <- universe bd, u == v]
    updates = [() | TLets _ [_] _ (TApp (FRecSet f) [base, _]) _ <- universe bd, base == v, Fields rs _ <- [s], f `elem` schemaFields rs]
    -- an arm as the fields it binds, each with its position in the product
    arms = map armFor matches
    armFor bs = case (s, bs) of
      (Con r t _, MatchData r' cs _)
        | r' == r,
          Just (_, TAbss fs bo) <- EC.lookup t cs,
          length fs == length subs ->
            [(zip [0 ..] fs, bo)]
      (Fields rs _, MatchRec sch (TAbss fs bo))
        | Just js <- traverse (`elemIndex` schemaFields rs) (schemaFields sch),
          length fs == length js ->
            [(zip js fs, bo)]
      _ -> []
    fieldArms j = [(f, bo) | (jfs, bo) <- concat arms, (j', f) <- jfs, j' == j]

-- | A lambda with exploded arguments as its two combinators: the entry,
-- which takes the arguments as before, matches the exploded ones and
-- calls the worker through the local function variable 'splitWorker', and
-- the worker, whose parameters are the plain arguments and the fields of
-- the exploded ones.
data Split v = Split
  { splitWorker :: v,
    splitEntry :: SuperNormal Reference v,
    splitWorkerLambda :: SuperNormal Reference v
  }

split :: (Var v) => [Shape] -> SuperNormal Reference v -> Split v
split shapes (Lambda ccs (TAbss vs bd0)) = Split w entry worker
  where
    bd = uniqueBinders bd0
    avoid0 = Set.unions (ABTN.freeVars bd : Set.fromList vs : map boundVars (universe bd))
    (w, avoid1) = freshVar avoid0
    -- the parameters of the worker: a plain argument as itself, an
    -- exploded one as its leaves, each a fresh name; and the arms of the
    -- entry's matches, product fields getting fresh names of their own
    (_, params) = foldl' step (avoid1, []) (zip3 vs ccs shapes)
    step (avoid, acc) (v, cc, s) = case s of
      Plain -> (avoid, acc ++ [(v, cc, Leaf v)])
      _ ->
        let (tree, avoid') = treeOf avoid s
         in (avoid', acc ++ [(v, cc, tree)])
    workerParams = concat [leaves tree | (_, _, tree) <- params]
    worker = Lambda (map snd workerParams) (TAbss (map fst workerParams) body)
    body = foldl' (\e (v, _, tree) -> rewrite v tree e) bd params
    entry = Lambda ccs (TAbss vs (foldr unpack call params))
    call = TApp (FVar w) (map fst workerParams)
    unpack (v, _, tree) inner = unpackTree v tree inner

-- | A tree of fresh names for an exploded argument: a leaf is a worker
-- parameter, a node a product whose fields are below it, named for the
-- entry's match to bind it by.
data Tree v = Leaf v | Node v Shape [Tree v]

treeOf :: (Var v) => Set.Set v -> Shape -> (Tree v, Set.Set v)
treeOf avoid Plain = first Leaf (freshVar avoid)
treeOf avoid s = let (u, avoid') = freshVar avoid in first (Node u s) (go avoid' (subShapes s))
  where
    go av [] = ([], av)
    go av (sub : ss) = let (tr, av') = treeOf av sub; (trs, av'') = go av' ss in (tr : trs, av'')

leaves :: Tree v -> [(v, Mem)]
leaves (Leaf v) = [(v, BX)]
leaves (Node _ _ ts) = concatMap leaves ts

nameOf :: Tree v -> v
nameOf (Leaf u) = u
nameOf (Node u _ _) = u

-- | The entry's match of one argument down to the leaves, around the call.
unpackTree :: (Var v) => v -> Tree v -> ANormal Reference v -> ANormal Reference v
unpackTree _ (Leaf _) inner = inner
unpackTree v (Node _ s ts) inner = TMatch v branches
  where
    names = map nameOf ts
    body = TAbss names (foldr (\tr e -> unpackTree (nameOf tr) tr e) inner ts)
    branches = case s of
      Con r t _ -> MatchData r (EC.mapSingleton t (map (const BX) ts, body)) Nothing
      Fields rs _ -> MatchRec rs body
      Plain -> MatchEmpty

-- | The body with every use of the variable rewritten to its fields: a
-- match becomes the arm for its product, a leaf field's arm variable
-- renamed to the parameter and a nested field's arm variable rewritten in
-- turn; a record update becomes a record literal of the parameters with
-- one replaced.
rewrite :: (Var v) => v -> Tree v -> ANormal Reference v -> ANormal Reference v
rewrite _ (Leaf _) e = e
rewrite v (Node _ s ts) e = go e
  where
    fields = case s of
      Fields rs _ -> schemaFields rs
      _ -> []
    go tm = case tm of
      TMatch u (MatchData r' cs _)
        | u == v,
          Con r t _ <- s,
          r' == r,
          Just (_, TAbss fs bo) <- EC.lookup t cs,
          length fs == length ts ->
            arm (zip fs ts) bo
      TMatch u (MatchRec sch (TAbss fs bo))
        | u == v,
          Fields _ _ <- s,
          Just js <- traverse (`elemIndex` fields) (schemaFields sch),
          length fs == length js ->
            arm [(f, ts !! j) | (f, j) <- zip fs js] bo
      TLets d [u] ms (TApp (FRecSet f) [base, x]) bo
        | base == v,
          Fields rs _ <- s,
          Just j <- elemIndex f fields,
          let ls = map nameOf ts ->
            TLets d [u] ms (TApp (FRec rs) (take j ls ++ x : drop (j + 1) ls)) (go bo)
      ABTN.TAbs u b -> ABTN.TAbs u (go b)
      ABTN.TTm f -> ABTN.TTm (fmap go f)
    arm pairs bo =
      let renaming = M.fromList [(f, l) | (f, Leaf l) <- pairs]
          bo' = ABTN.renames renaming bo
       in go (foldl' (\acc (f, tr) -> rewrite f tr acc) bo' pairs)

freshVar :: (Var v) => Set.Set v -> (v, Set.Set v)
freshVar avoid = (u, Set.insert u avoid)
  where
    u = Var.freshIn avoid (Var.typed Var.Eta)

-- | The product lets of a lambda that nothing uses but the exploded calls
-- they feed (as the argument itself or as a field of one) and the updates
-- of them that are dead in turn: their instruction is not emitted.
deadCons :: (Var v) => (Reference -> Maybe [Shape]) -> ConLets v -> ANormal Reference v -> Set.Set v
deadCons shapesOf cons bd = go (dead consumed)
  where
    dead counts = M.keysSet (M.filterWithKey (\u n -> M.member u (builtOf cons) && occurrences u bd == n) counts)
    -- a dead update is the one use of the variable it updated
    go ds =
      let ds' = dead (M.unionWith (+) consumed (M.fromListWith (+) [(base, 1) | (u, base) <- M.toList (baseOf cons), Set.member u ds]))
       in if ds' == ds then ds else go ds'
    consumed =
      M.fromListWith (+) $
        [ (u, 1 :: Int)
          | TApp (FComb r) args <- universe bd,
            Just shapes <- [shapesOf r],
            length shapes == length args,
            (s, a) <- zip shapes args,
            u <- consumedBy s a
        ]
    consumedBy Plain _ = []
    consumedBy s a
      | Just b <- M.lookup a (builtOf cons), matchesBuilt s b = a : concat (zipWith consumedBy (subShapes s) (builtFields b))
      | otherwise = []

-- | The variables a call passes to a worker, in the worker's parameter
-- order: a plain argument as itself, an exploded one as the field
-- variables of the product let it names, down to the leaves. Nothing when
-- the site does not offer what the shapes ask (it cannot, for a site the
-- shapes were computed from, but the emitter checks).
explodedArgs :: (Var v) => ConLets v -> [Shape] -> [v] -> Maybe [v]
explodedArgs cons shapes args
  | length shapes == length args = concat <$> traverse (uncurry go) (zip shapes args)
  | otherwise = Nothing
  where
    go Plain a = Just [a]
    go s a
      | Just b <- M.lookup a (builtOf cons), matchesBuilt s b = concat <$> traverse (uncurry go) (zip (subShapes s) (builtFields b))
      | otherwise = Nothing

-- The boring helpers.

lambdas :: SuperGroup Reference v -> [SuperNormal Reference v]
lambdas (Rec bs e) = e : map snd bs

lambdaBody :: (Var v) => SuperNormal Reference v -> ANormal Reference v
lambdaBody (Lambda _ (TAbss _ bd)) = bd

-- | A term and every term inside it.
universe :: ANormal Reference v -> [ANormal Reference v]
universe t = t : concatMap universe (children t)

children :: ANormal Reference v -> [ANormal Reference v]
children (ABTN.Term _ (ABTN.Abs _ b)) = [b]
children (ABTN.Term _ (ABTN.Tm f)) = toList f

boundVars :: (Ord v) => ANormal Reference v -> Set.Set v
boundVars (ABTN.Term _ (ABTN.Abs u _)) = Set.singleton u
boundVars _ = Set.empty

-- | How many times a variable is used: its free occurrences, in every
-- position a variable can take.
occurrences :: (Var v) => v -> ANormal Reference v -> Int
occurrences v = length . filter (== v) . concatMap nodeVars . universe
  where
    nodeVars (ABTN.Term _ (ABTN.Tm f)) = case f of
      AVar u -> [u]
      AApp fn args -> funcVars fn ++ args
      AName (Right u) as _ -> u : as
      AName (Left _) as _ -> as
      AMatch u _ -> [u]
      AHnd _ nh ah _ -> nh : toList ah
      AFrc u -> [u]
      ADiscard u -> [u]
      ALocal u _ -> [u]
      AUpdate _ u x -> [u, x]
      ALet {} -> []
      AShift {} -> []
      ALit _ -> []
      ABLit _ -> []
    nodeVars _ = []
    funcVars = \case
      FVar u -> [u]
      FCont u -> [u]
      _ -> []
