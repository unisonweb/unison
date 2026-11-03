{-# LANGUAGE PatternSynonyms #-}

-- | Loop state exploded into parameters: call-pattern specialisation.
--
-- A function of a batch that some saturated call in the batch passes a
-- product or constructor built for the call (a let of a constructor
-- application or a record literal, @(acc, i)@ in @go (acc, i)@, @Left x@
-- in @go (Left x)@, or a record update of such a value) gets a
-- specialisation for that call pattern: a worker, a combinator of the
-- group at an index of its own ('specIndex'), whose parameters are the
-- pattern's fields and whose body is the lambda's with each match on the
-- argument replaced by the arm for the pattern's constructor and each
-- update of it by a literal of the fields. Call sites that build the
-- pattern call the worker with the field variables, which are in scope
-- where the value was built, and the let that built it is dropped when
-- nothing else uses it ('dropDead'); so a loop whose state is a tuple or
-- a record allocates nothing per round, and a loop whose state is a sum
-- runs as one worker per constructor, mutually recursive through the
-- sites, with no tag anywhere. The lambda's own index keeps the original
-- body, for callers the batch cannot see (a later batch, a function
-- value, a partial application) and for sites that pass a value they did
-- not build. Fields that are products built for the call explode in
-- turn, which is what a tuple's nesting needs. This is GHC's SpecConstr
-- (Peyton Jones, "Call-pattern specialisation for Haskell programs",
-- 2007), done for the call sites the loader can see ('explosions', over
-- each batch 'Unison.Runtime.Machine.cacheAdd' loads) and without any
-- change to how values are represented: a worker's parameters are
-- ordinary slots, so nothing here depends on the wide convention
-- (Unison.Runtime.Wide). A continuation captured in rewritten code is
-- written out as the unoptimized code's frames, which is what 'Sources'
-- and 'Prepared' are for ('Unison.Runtime.MCode.FrameLayout', design.md
-- "Continuations across processes").
--
-- A pattern is taken apart only as far as the body consumes it: an
-- argument the body passes on, stores or returns on some path stays a
-- plain parameter ('usable'), so nothing is ever rebuilt. The patterns of
-- a function are a closure: the ones its callers' bodies build, then the
-- ones each worker's own body builds, up to a cap per function
-- ('maxSpecs'). A sum built in the branches of a match and passed at once,
-- @go (if p then Left x else Right y)@, is first rewritten so that each
-- branch makes the call ('sinkCalls'), which is what gives each branch its
-- worker.
--
-- The return side is the same idea for results ('returns'): a function
-- whose every return builds a product, through its own tail calls, gets a
-- /product-return worker/, a combinator of its group at 'retIndex' whose
-- tails yield the product's leaves as several values instead of packing
-- them ('returnWorker'), while the entry keeps the original body. A caller
-- in the batch that takes the result apart at once calls the worker with
-- a let of the leaves ('useReturns'), so a loop and its step function
-- exchange a tuple with nothing allocated in either direction; a result
-- that escapes keeps the entry, and one that is only passed on to an
-- exploded position of another worker is rebuilt in the caller and
-- dropped like any other product built for such a call.
--
-- Everything here is a rewrite of the ANF, and the result says what it
-- means: a call of a worker is @TApp (FWorker r i) args@, the combinator
-- at index @i@ of the group of @r@ with exactly the arguments it takes,
-- and a tail that returns several values is @TApp FYield vs@. The emitter
-- (Unison.Runtime.MCode) turns each into one instruction and checks the
-- counts ('Unison.Runtime.MCode.resultCount'); it decides nothing. The
-- two forms exist only in a batch's code after it is loaded, never in
-- what the cache stores or a continuation names, so the serializers
-- refuse them.
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
-- Two rewrites of a body on its own come first ('knownCons',
-- 'splitJoins'; design.md "Known constructors and case of case"): a let
-- binding a constructor application or a record literal has every match
-- on it replaced by the arm, the arm's variables renamed to the fields
-- (the argument side's 'rewrite'), so that @match (a, b) with ..@, which
-- builds the tuple and takes it apart at once, allocates nothing and
-- tests no tag; and a let whose binding is a match (or anything whose
-- every tail builds a constructor of the matched type) and whose body is
-- a match on it (the let's body, or the binding of the next let) has
-- the outer match moved into those tails, where the constructor is
-- known, an outer arm that several tails reach becoming a /join point/,
-- a combinator of the group at 'joinIndex' that the tails call with the
-- arm's fields and the variables the arm uses, so that an arm's code is
-- not copied into every tail (an arm that is a single expression, or
-- that one tail reaches, is copied instead), and so does the rest of the
-- body when the match's value was let-bound. The join points of a lambda are made last,
-- after the other rewrites, with their parameters taken from the
-- unoptimized arm ('caseSites'), so that every variable the unoptimized
-- arm reads is in the join point's frame and a continuation captured
-- inside it can be written out ('Join').
--
-- @UNISON_OPTIMIZATIONS=-explode_args@ and @-explode_returns@ switch the
-- two sides off, and @-known_constructor@ the two rewrites above
-- ('explodeArgsOn', 'explodeReturnsOn', 'knownConstructorOn';
-- Unison.Runtime.Optimizations), for measuring and for a process that has
-- to agree with one that ran without them.
module Unison.Runtime.MCode.Explode
  ( explodeArgsOn,
    explodeReturnsOn,
    knownConstructorOn,
    rewritesOn,
    specIndex,
    retIndex,
    joinIndex,
    isJoinIndex,
    Shape (..),
    Pattern,
    shapeLeaves,
    Batch (..),
    Explosions,
    explosions,
    Returns,
    returns,
    prepare,
    specialise,
    returnWorker,
    Prepared (..),
    Join (..),
    JoinSetting (..),
    Sources,
    Built (..),
    builtFields,
    conLets,
    uniqueBody,
    universe,
  )
where

import Control.Monad (foldM)
import Control.Monad.State.Strict (State, evalState, get, modify, put, runState, state)
import Data.Bifunctor (first)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.List (elemIndex)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Set qualified as Set
import Data.Word (Word16, Word64)
import Unison.ABT.Normalized (pattern TAbss)
import Unison.ABT.Normalized qualified as ABTN
import Unison.Reference (Reference)
import Unison.Runtime.ANF
import Unison.Runtime.ANF qualified as ANF
import Unison.Runtime.InternalError (internalBug)
import Unison.Runtime.Optimizations (Optimization (..), optimizationSetting)
import Unison.Util.EnumContainers qualified as EC
import Unison.Var (Var)
import Unison.Var qualified as Var

-- | Whether loop state is exploded in this process (the argument side:
-- 'explosions', 'specialise'): on unless @UNISON_OPTIMIZATIONS@ says
-- @-explode_args@.
explodeArgsOn :: Bool
explodeArgsOn = fromMaybe True (optimizationSetting ExplodeArgs)

-- | Whether product results are exploded (the return side: 'returns',
-- 'returnWorker'): on unless @UNISON_OPTIMIZATIONS@ says @-explode_returns@.
explodeReturnsOn :: Bool
explodeReturnsOn = fromMaybe True (optimizationSetting ExplodeReturns)

-- | Whether a product or sum built and matched in the same body is
-- matched where it is built, and a match on the value of a match moved
-- into its tails ('knownCons', 'splitJoins'): on unless
-- @UNISON_OPTIMIZATIONS@ says @-known_constructor@.
knownConstructorOn :: Bool
knownConstructorOn = fromMaybe True (optimizationSetting KnownConstructor)

-- | Whether any rewrite of the ANF is on: with none, every lambda is
-- emitted as its unoptimized form and no section gets a deoptimization
-- table (the emitter checks this).
rewritesOn :: Bool
rewritesOn = explodeArgsOn || explodeReturnsOn || knownConstructorOn

-- | The combinator index of a group's k-th specialisation (k from 1): in
-- the high bits like a local function's, with a bit no let index reaches
-- (the lets of a combinator are numbered from 1 in its low sixteen bits)
-- and no local function has. The JIT treats a worker like the entry point
-- it stands for (Compile's @isEntry@).
specIndex :: Int -> Word64
specIndex k = (fromIntegral k `shiftL` 16) .|. 0x8000

-- | The combinator index of a group's product-return worker ('returns'):
-- the specialisation slot no call pattern takes, since those are numbered
-- from 1.
retIndex :: Word64
retIndex = specIndex 0

-- | The combinator index of the k-th join point (k from 1) of the lambda
-- at an index of the group ('splitJoins'): the lambda's index with k in
-- the bits above any a local function's or a specialisation's number
-- uses, so that the low fifteen bits are the owner's and the JIT compiles
-- it as it does a worker (Compile's @isEntry@), and the lets of the join
-- point are numbered under it like any combinator's.
joinIndex :: Word64 -> Int -> Word64
joinIndex owner k = owner .|. (fromIntegral k `shiftL` 48)

isJoinIndex :: Word64 -> Bool
isJoinIndex i = i `shiftR` 48 /= 0

-- | The most specialisations a function gets.
maxSpecs :: Int
maxSpecs = 8

-- | The most join points a lambda gets (what 'joinIndex' has bits for,
-- with room to spare).
maxJoins :: Int
maxJoins = 15

-- | The most leaves a returned product may have.
maxLeaves :: Int
maxLeaves = 8

-- | How an argument position is taken apart: left as one slot, or a
-- product, a data constructor or a record (@Fields@, its fields in the
-- schema's order, which is the field names' order), whose fields are
-- taken apart in turn.
data Shape = Plain | Con !Reference !CTag [Shape] | Fields !RecordSchema [Shape]
  deriving (Eq, Ord, Show)

-- | A call pattern: a shape per argument.
type Pattern = [Shape]

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

-- | Whether a site's shape offers everything a pattern's shape takes
-- apart.
refines :: Shape -> Shape -> Bool
refines _ Plain = True
refines s p = sameProduct s p && and (zipWith refines (subShapes s) (subShapes p))

-- | The values a shape is taken apart into: one for a plain position, the
-- leaves of its fields for a product.
shapeLeaves :: Shape -> Int
shapeLeaves Plain = 1
shapeLeaves s = sum (map shapeLeaves (subShapes s))

-- | What two shapes agree on: the product where both have the same one,
-- with the meet of their fields, and plain anywhere else.
meetShape :: Shape -> Shape -> Shape
meetShape a b
  | sameProduct a b = withSubShapes a (zipWith meetShape (subShapes a) (subShapes b))
  | otherwise = Plain

-- | What the rewrites know about the batch, each by a function's
-- reference and Nothing for a function outside the batch or without one:
-- the call patterns it has a specialisation for, in worker order
-- ('explosions'), the product it returns on every path ('returns'), and
-- its arity. The analyses run with the parts not yet known empty.
data Batch = Batch
  { batchSpecs :: Reference -> Maybe [Pattern],
    batchRets :: Reference -> Maybe Shape,
    batchArity :: Reference -> Maybe Int
  }

-- | The specialisations of each function of a batch, by its reference:
-- the patterns, in the order their workers are numbered ('specIndex').
type Explosions = M.Map Reference [Pattern]

-- | A product a let built: which one, and its field variables, in the
-- shape's field order.
data Built v = BuiltCon Reference CTag [v] | BuiltRec RecordSchema [v]
  deriving (Show)

-- | Where a variable of the unoptimized body went, for the frames of a
-- continuation captured in the optimized code to be written out as the
-- unoptimized code's (design.md, "Continuations across processes";
-- 'Unison.Runtime.MCode.FrameLayout'): renamed to another variable (a
-- match arm's field variable to the leaf or product that stands for it, a
-- field read's let to the parameter it reads), or replaced by a product
-- the optimized body no longer names, given as the product with the
-- names of its fields in the optimized body (an exploded parameter, a
-- result the leaves replace, a product of either that a match took
-- apart in turn). A variable with no entry has the same name in both.
type Sources v = M.Map v (Either v (Built v))

-- | A lambda as the emitter sees it, with what its rewrite changed: the
-- sources of the unoptimized body's variables, the binders of each let
-- the rewrite changed, mapped to the binders of the unoptimized let it
-- stands for (a let of a result's leaves, to the let of the result), and
-- the variables the rewrite found dead wherever a frame could hold them
-- (the let of a site 'splitJoins' rewrote, used only by the match that
-- is gone), and its join points, the k-th at 'joinIndex' k of the
-- lambda's index.
data Prepared v = Prepared
  { preparedLambda :: SuperNormal Reference v,
    preparedSources :: Sources v,
    preparedLets :: M.Map [v] [v],
    preparedDead :: Set.Set v,
    preparedJoins :: [Join v]
  }

-- | A join point of a lambda ('splitJoins'): an arm of a match on the
-- value of a match, as a combinator of the group whose parameters are
-- the arm's variables and then the variables the arm uses; the binders
-- of the unoptimized let whose binding the match was in, if any, which is
-- where the unoptimized frames of a continuation captured inside the
-- join point stop (the emitter's 'deoptOf'); and the source of the
-- match's variable, the constructor this arm is for over the arm's
-- variables (none for the default arm, whose frames never read it).
data Join v = Join
  { joinLambda :: SuperNormal Reference v,
    joinCut :: Maybe [v],
    joinSources :: Sources v
  }

-- | Where a lambda's join points go: the lambda's index in its group,
-- the group's reference, and the names of the group's local functions,
-- which a body refers to as variables but which no frame holds.
data JoinSetting v = JoinSetting
  { jsOwner :: Word64,
    jsGroup :: Reference,
    jsLocals :: Set.Set v
  }

-- | What 'splitJoins' knows of the unoptimized body: the setting, its
-- sites ('caseSites'), the products it built ('conLets'), and the
-- sources the rewrites before it recorded.
data Joining v = Joining (JoinSetting v) (Sites v) (ConLets v) (Sources v)

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
-- fields are known, the product and its field variables. Binders are
-- unique ('prepare').
type ConLets v = M.Map v (Built v)

conLets :: (Var v) => ANormal Reference v -> ConLets v
conLets = go M.empty
  where
    go acc tm = case tm of
      TLets _ [u] _ bn bo
        | TApp (FCon r t) args <- bn -> go (M.insert u (BuiltCon r t args) acc) bo
        | TApp (FRec rs) args <- bn -> go (M.insert u (BuiltRec rs args) acc) bo
        | TApp (FRecSet f) [base, x] <- bn,
          Just (BuiltRec rs fs) <- M.lookup base acc,
          Just j <- elemIndex f (schemaFields rs) ->
            go (M.insert u (BuiltRec rs (take j fs ++ x : drop (j + 1) fs)) acc) bo
      _ -> foldl' go acc (children tm)

-- | Whether a let binds a product ('conLets' records it): a binding with
-- no effect, so that its let can be left out when nothing uses it
-- ('dropDead').
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
-- worker's arity.
siteShape :: (Var v) => ConLets v -> Int -> v -> Shape
siteShape cons depth a
  | depth < maxDepth, Just b <- M.lookup a cons = builtShape b (map (siteShape cons (depth + 1)) (builtFields b))
  | otherwise = Plain
  where
    maxDepth = 8

-- | The saturated calls of a body to functions of the batch, each with the
-- pattern its arguments offer, where some argument offers a product. A
-- call already rewritten to a worker's is not one.
sites :: (Var v) => Batch -> ANormal Reference v -> [(Reference, Pattern)]
sites b bd =
  [ (r, pat)
  | TApp (FComb r) args <- universe bd,
    Just n <- [batchArity b r],
    n == length args,
    let pat = map (siteShape cons 0) args,
    not (all isPlain pat)
  ]
  where
    cons = conLets bd

-- | The specialisations of a batch of groups: for each group's entry
-- point, the patterns its callers in the batch build, each narrowed to
-- what its body consumes, and then the patterns each worker's own body
-- builds, until there are no new ones.
explosions :: (Var v) => Returns -> M.Map Reference (SuperGroup Reference v) -> Explosions
explosions rets groups
  | not explodeArgsOn = M.empty
  | otherwise = close M.empty initial
  where
    b = Batch (const Nothing) (`M.lookup` rets) (groupArity groups)
    prep bd = let (bd', _, _, _, _) = prepareBody b Nothing Nothing bd in bd'
    initial = concat [sites b (prep (lambdaBody lam)) | g <- M.elems groups, lam <- lambdas g]
    close specs [] = specs
    close specs ((r, site) : rest)
      | Just (Rec _ ent@(Lambda _ (TAbss vs _))) <- M.lookup r groups,
        length vs == length site,
        let pat = zipWith (\s v -> usable s v (prep (lambdaBody ent))) site vs,
        not (all isPlain pat),
        let have = M.findWithDefault [] r specs,
        pat `notElem` have,
        length have < maxSpecs =
          close (M.insert r (have ++ [pat]) specs) (rest ++ sites b (lambdaBody (preparedLambda (specialise b Nothing pat ent))))
      | otherwise = close specs rest

-- | The arity of a group's entry point.
groupArity :: M.Map Reference (SuperGroup Reference v) -> Reference -> Maybe Int
groupArity groups r = case ANF.arities <$> M.lookup r groups of
  Just (n : _) -> Just n
  _ -> Nothing

-- | The product each function of a batch returns on every path, by its
-- reference: the shape every tail of its body builds (a constructor
-- application or a record literal, or a variable bound to one, its fields
-- taken apart in turn where they are built for it), met over the tails,
-- where the other tails are saturated calls of functions of the batch that
-- return the same product. A closure from the shapes the tails build: a
-- callee's shape narrows its callers' until nothing changes, and a
-- function with a tail that returns anything else, or calls a function
-- that does, is dropped. Products of one leaf are left to the wide
-- convention and products of none are nothing to return; a function of no
-- arguments is a value evaluated once when it loads, so it keeps its entry.
type Returns = M.Map Reference Shape

returns :: (Var v) => M.Map Reference (SuperGroup Reference v) -> Returns
returns groups
  | not explodeReturnsOn = M.empty
  | otherwise = M.map fst (settle (M.mapMaybeWithKey candidate groups))
  where
    b = Batch (const Nothing) (const Nothing) (groupArity groups)
    candidate r (Rec _ (Lambda _ (TAbss vs bd0)))
      | null vs = Nothing
      | Just n <- batchArity b r, n /= length vs = Nothing
      | Just ts <- sequence (tails b cons bd),
        built@(s0 : _) <- [s | Left s <- ts],
        let s = foldl' meetShape s0 built,
        returnable s =
          Just (s, [r' | Right r' <- ts])
      | otherwise = Nothing
      where
        (bd, _, _, _, _) = prepareBody b Nothing Nothing bd0
        cons = conLets bd
    settle cs
      | cs' == cs = cs
      | otherwise = settle cs'
      where
        cs' = M.mapMaybe step cs
        step (s, calls) = do
          ss <- traverse (\r -> fst <$> M.lookup r cs) calls
          let s' = foldl' meetShape s ss
          if returnable s' then Just (s', calls) else Nothing
    returnable s = not (isPlain s) && shapeLeaves s >= 2 && shapeLeaves s <= maxLeaves

-- | The tails of a body: the shape each builds (Left), the function of the
-- batch each calls saturated (Right), or Nothing for any other tail.
-- 'yieldLeaves' rewrites exactly these.
tails :: (Var v) => Batch -> ConLets v -> ANormal Reference v -> [Maybe (Either Shape Reference)]
tails b cons = go
  where
    go tm = case tm of
      TLets _ _ _ _ bo -> go bo
      TName _ _ _ bo -> go bo
      TLocal _ bo -> go bo
      TMatch _ bs -> concatMap (\(TAbss _ bo) -> go bo) (toList bs)
      TVar u | Just _ <- M.lookup u cons -> [Just (Left (siteShape cons 0 u))]
      TApp (FCon r t) args -> [Just (Left (Con r t (map (siteShape cons 1) args)))]
      TApp (FRec rs) args -> [Just (Left (Fields rs (map (siteShape cons 1) args)))]
      TApp (FComb r) args | batchArity b r == Just (length args) -> [Just (Right r)]
      _ -> [Nothing]

-- | The values a product-return worker yields for a tail that builds a
-- product: the leaves of the shape, a field built for the tail taken apart
-- in turn, any other field as itself.
returnLeaves :: (Var v) => ConLets v -> Shape -> Built v -> [v]
returnLeaves cons s b = concat (zipWith leaf (subShapes s) (builtFields b))
  where
    leaf Plain a = [a]
    leaf sub a
      | Just b' <- M.lookup a cons, matchesBuilt sub b' = returnLeaves cons sub b'
      | otherwise = [a]

-- | The body of a product-return worker for a shape: each tail that builds
-- the product (the constructor application or record literal itself, or a
-- variable bound to one) yields its leaves as that many values, a field
-- built for the tail taken apart in turn ('returnLeaves'), and each tail
-- call of a function with a worker for the same product calls that
-- worker. Only the tails change: a product built in a let's binding, or
-- under a handler or a shift, is packed as before. The analysis
-- ('returns', through 'tails') chose the shape from these tails, so a tail
-- of any other form is a bug.
yieldLeaves :: (Var v) => Batch -> Shape -> ANormal Reference v -> ANormal Reference v
yieldLeaves b s bd = go bd
  where
    cons = conLets bd
    go tm = case tm of
      TLets d us ms bn bo -> TLets d us ms bn (go bo)
      TName v f as bo -> TName v f as (go bo)
      TLocal v bo -> TLocal v (go bo)
      TMatch v bs -> TMatch v (fmap go bs)
      ABTN.Term _ (ABTN.Abs u e) -> ABTN.TAbs u (go e)
      TVar u | Just bt <- M.lookup u cons, matchesBuilt s bt -> yield (returnLeaves cons s bt)
      TApp (FCon r t) args | matchesBuilt s (BuiltCon r t args) -> yield (returnLeaves cons s (BuiltCon r t args))
      TApp (FRec rs) args | matchesBuilt s (BuiltRec rs args) -> yield (returnLeaves cons s (BuiltRec rs args))
      TApp (FComb r) args | Just _ <- batchRets b r, batchArity b r == Just (length args) -> TApp (FWorker r retIndex) args
      _ -> internalBug [] $ "yieldLeaves: a tail of a product-return worker that does not build " ++ show s ++ ": " ++ show tm
    yield leaves
      | length leaves == shapeLeaves s = TApp FYield leaves
      | otherwise = internalBug [] $ "yieldLeaves: a tail building " ++ show (length leaves) ++ " leaves for a product of " ++ show (shapeLeaves s)

-- | A body with each let of a saturated call of a function that has a
-- product-return worker rewritten to a let of the result's leaves over a
-- call of the worker, when the body takes the result apart (every match
-- on it, and on the fields the shape takes apart, replaced by its arm:
-- 'rewrite') and what is left of it is only passed to saturated calls of
-- functions of the batch, where an exploded position may take it; the
-- product, and any field the body still uses as a whole, is rebuilt from
-- the leaves before the body ('rebuild'). A result used any other way
-- keeps the call of the entry.
useReturns :: (Var v) => Batch -> ANormal Reference v -> (ANormal Reference v, Sources v, M.Map [v] [v])
useReturns b bd = (bd', srcs, lets)
  where
    (bd', (_, srcs, lets)) = runState (go bd) (Set.unions (ABTN.freeVars bd : map boundVars (universe bd)), M.empty, M.empty)
    go tm = case tm of
      TLets d [u] [BX] bn@(TApp (FComb r) args) bo
        | Just s <- batchRets b r,
          batchArity b r == Just (length args) -> do
            (avoid, srcs0, lets0) <- get
            let (tree0, avoid') = treeOf avoid s
                tree = renameRoot u tree0
                (rewritten, armSrcs) = runState (rewrite u tree bo) M.empty
                bo' = rebuild tree rewritten
            if occurrences u bo' == callUses u bo'
              then do
                put (avoid', M.unions [srcs0, armSrcs, treeSources u tree], M.insert (leafNames tree) [u] lets0)
                TLets d (leafNames tree) (map (const BX) (leafNames tree)) (TApp (FWorker r retIndex) args) <$> go bo'
              else do
                put (avoid', srcs0, lets0)
                TLets d [u] [BX] bn <$> go bo
      ABTN.Term _ (ABTN.Abs v e) -> ABTN.TAbs v <$> go e
      ABTN.Term _ (ABTN.Tm f) -> ABTN.TTm <$> traverse go f
    callUses u e = length [() | TApp (FComb r) args <- universe e, batchArity b r == Just (length args), a <- args, a == u]
    renameRoot u = \case
      Node _ sh ts -> Node u sh ts
      t -> t

-- | A body with a let rebuilding each product of the tree that the body
-- still names, from its fields, an inner product before the one that
-- holds it.
rebuild :: (Var v) => Tree v -> ANormal Reference v -> ANormal Reference v
rebuild (Leaf _) e = e
rebuild (Node v sh ts) e = foldr rebuild withNode ts
  where
    withNode
      | occurrences v e > 0 = TLets Direct [v] [BX] (packOf sh (map treeName ts)) e
      | otherwise = e
    packOf (Con r t _) fs = TApp (FCon r t) fs
    packOf (Fields rs _) fs = TApp (FRec rs) fs
    packOf Plain _ = TVar v

-- | A body with each saturated call of a function of the batch that builds
-- one of its call patterns rewritten to a call of that specialisation with
-- the fields of what it built ('siteSpec', 'explodedArgs').
useSpecs :: (Var v) => Batch -> ANormal Reference v -> ANormal Reference v
useSpecs b bd = go bd
  where
    cons = conLets bd
    go tm = case tm of
      TApp (FComb r) args
        | Just pats <- batchSpecs b r,
          Just (k, pat) <- siteSpec cons pats args,
          Just fields <- explodedArgs cons pat args ->
            TApp (FWorker r (specIndex k)) fields
      ABTN.Term _ (ABTN.Abs u e) -> ABTN.TAbs u (go e)
      ABTN.Term _ (ABTN.Tm f) -> ABTN.TTm (fmap go f)

-- | The body without the product lets nothing uses: what was built for a
-- call that takes its fields instead, or for a tail that yields its
-- leaves, and, once such a let is gone, a field only it held, or the
-- record only an update of it updated. A product let has no effect, so
-- one with no use is left out whatever it was built for. The body is
-- walked from the inside out, so that a let is judged once the lets in
-- its body are settled; binders are unique ('prepare'), so a use is a
-- use by name.
dropDead :: (Var v) => ANormal Reference v -> ANormal Reference v
dropDead tm = case tm of
  TLets d us ms bn bo
    | [u] <- us, isBuiltLet bn, occurrences u bo' == 0 -> bo'
    | otherwise -> TLets d us ms (dropDead bn) bo'
    where
      bo' = dropDead bo
  ABTN.Term _ (ABTN.Abs u e) -> ABTN.TAbs u (dropDead e)
  ABTN.Term _ (ABTN.Tm f) -> ABTN.TTm (fmap dropDead f)

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
    meet a b
      | sameProduct a b = withSubShapes a (zipWith meet (subShapes a) (subShapes b))
      | otherwise = Plain

-- | The worker of a lambda for a pattern: its parameters are the plain
-- arguments and the fields of the exploded ones, each a fresh name, and
-- its body is the lambda's with the exploded arguments rewritten to their
-- fields, prepared again afterwards, since a record update the rewrite
-- turned into a literal is a product built for whatever it is passed to.
-- The join points are made by the second pass, from the sites of the
-- unoptimized body.
specialise :: (Var v) => Batch -> Maybe (JoinSetting v) -> Pattern -> SuperNormal Reference v -> Prepared v
specialise b js pat (Lambda ccs (TAbss vs bd0)) =
  Prepared
    (Lambda (map snd params) (TAbss (map fst params) body))
    (M.unions [srcs0, treeSrcs, armSrcs, srcs1])
    (lets0 <> lets1)
    dead
    joins
  where
    (bd, srcs0, lets0, _, _) = prepareBody b Nothing Nothing bd0
    treeSrcs = M.unions [treeSources v tree | (v, _, tree) <- trees]
    avoid0 = Set.unions (ABTN.freeVars bd : Set.fromList vs : map boundVars (universe bd))
    (_, trees) = foldl' step (avoid0, []) (zip3 vs ccs pat)
    step (avoid, acc) (v, cc, s) = case s of
      Plain -> (avoid, acc ++ [(v, cc, Leaf v)])
      _ ->
        let (tree, avoid') = treeOf avoid s
         in (avoid', acc ++ [(v, cc, tree)])
    params = concat [leaves tree | (_, _, tree) <- trees]
    (rewritten, armSrcs) = runState (foldM (\e (v, _, tree) -> rewrite v tree e) bd trees) M.empty
    -- the second pass rewrites only lets the first left (a let of a
    -- call of a worker is not a let of a call), so its sources and lets
    -- add to the first's
    (body, srcs1, lets1, dead, joins) = prepareBody b Nothing (joiningOf js bd0 (M.unions [srcs0, treeSrcs, armSrcs])) rewritten

-- | The product-return worker of a lambda that returns a product of the
-- shape on every path: the lambda with its tails yielding the leaves. It
-- gets no join points: its tails are what yield the leaves.
returnWorker :: (Var v) => Batch -> Shape -> SuperNormal Reference v -> Prepared v
returnWorker b s (Lambda ccs (TAbss vs bd)) = Prepared (Lambda ccs (TAbss vs bd')) srcs lets Set.empty []
  where
    (bd', srcs, lets, _, _) = prepareBody b (Just s) Nothing bd

-- | A tree of fresh names for an exploded argument or result: a leaf is a
-- worker parameter (or a yielded value), a node a product whose fields are
-- below it, named so that a body can refer to the product as a whole
-- ('rebuild').
data Tree v = Leaf v | Node v Shape [Tree v]

treeOf :: (Var v) => Set.Set v -> Shape -> (Tree v, Set.Set v)
treeOf avoid Plain = first Leaf (freshVar avoid)
treeOf avoid s = let (nv, avoid') = freshVar avoid in first (Node nv s) (go avoid' (subShapes s))
  where
    go av [] = ([], av)
    go av (sub : ss) = let (tr, av') = treeOf av sub; (trs, av'') = go av' ss in (tr : trs, av'')

treeName :: Tree v -> v
treeName (Leaf v) = v
treeName (Node v _ _) = v

leaves :: Tree v -> [(v, Mem)]
leaves (Leaf v) = [(v, BX)]
leaves (Node _ _ ts) = concatMap leaves ts

leafNames :: Tree v -> [v]
leafNames = map fst . leaves

-- | The sources ('Sources') of a variable a tree replaces and of the
-- tree's inner products: each is the product of its children's names.
-- A leaf is a name of its own and needs none.
treeSources :: (Ord v) => v -> Tree v -> Sources v
treeSources _ (Leaf _) = M.empty
treeSources v (Node _ sh ts) =
  M.insert v (Right (packOf sh (map treeName ts))) (M.unions [treeSources (treeName t) t | t <- ts])
  where
    packOf (Con r t _) fs = BuiltCon r t fs
    packOf (Fields rs _) fs = BuiltRec rs fs
    packOf Plain _ = internalBug [] "treeSources: a product node of plain shape"

-- | The body with every use of the variable rewritten to its fields: a
-- match becomes the arm for its product, a leaf field's arm variable
-- renamed to the parameter and a nested field's arm variable rewritten in
-- turn; a record update becomes a record literal of the parameters with
-- one replaced (the fields of an updated record are leaves: 'usable').
rewrite :: (Var v) => v -> Tree v -> ANormal Reference v -> State (Sources v) (ANormal Reference v)
rewrite _ (Leaf _) e = pure e
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
          Just ls <- traverse leafName ts ->
            TLets d [u] ms (TApp (FRec rs) (take j ls ++ x : drop (j + 1) ls)) <$> go bo
      -- a let whose binding was a field read of the variable binds a
      -- parameter now: its uses take the parameter, with no let to run
      TLets d [x] ms bn bo ->
        go bn >>= \case
          TVar u -> do
            modify (M.insert x (Left u))
            go (ABTN.renames (M.singleton x u) bo)
          bn' -> TLets d [x] ms bn' <$> go bo
      ABTN.TAbs u b -> ABTN.TAbs u <$> go b
      ABTN.TTm f -> ABTN.TTm <$> traverse go f
    -- an arm's variables take the tree's names, so that a product the
    -- body still names after the rewrite has one name wherever it was
    -- matched ('rebuild'); each is recorded as a source of the tree's
    -- name
    arm pairs bo = do
      let renaming = M.fromList [(f, treeName tr) | (f, tr) <- pairs]
      modify (M.union (M.map Left renaming))
      bo' <- foldM (\acc (_, tr) -> rewrite (treeName tr) tr acc) (ABTN.renames renaming bo) pairs
      go bo'
    leafName (Leaf l) = Just l
    leafName _ = Nothing

freshVar :: (Var v) => Set.Set v -> (v, Set.Set v)
freshVar avoid = (u, Set.insert u avoid)
  where
    u = Var.freshIn avoid (Var.typed Var.Eta)

-- | A lambda as the emitter sees it ('prepareBody' on its body). Applied to
-- every lambda of a batch, so that the sites the analyses saw are the
-- sites emitted; the analyses pass no setting and get no join points.
prepare :: (Var v) => Batch -> Maybe (JoinSetting v) -> SuperNormal Reference v -> Prepared v
prepare b js (Lambda ccs (TAbss vs bd)) = Prepared (Lambda ccs (TAbss vs bd')) srcs lets dead joins
  where
    (bd', srcs, lets, dead, joins) = prepareBody b Nothing (joiningOf js bd M.empty) bd

-- | What 'splitJoins' needs of an unoptimized body, given the setting and
-- the sources recorded before the body is prepared again (the second
-- pass of 'specialise').
joiningOf :: (Var v) => Maybe (JoinSetting v) -> ANormal Reference v -> Sources v -> Maybe (Joining v)
joiningOf js bd srcs = (\setting -> Joining setting (caseSites cons (jsLocals setting) base) cons srcs) <$> js
  where
    base = uniqueBody bd
    cons = conLets base

-- | A body as the analyses and the emitter see it, or as it is while
-- every rewrite is off: every binder with a name of its own
-- ('uniqueBinders'); each match on a product or sum built in the body
-- made its arm ('knownCons'); calls sunk into the branches that build
-- their argument ('sinkCalls'); each let of a call of a function with a
-- product-return worker that takes the result apart made a let of the
-- leaves over a call of the worker ('useReturns'); for the body of a
-- product-return worker, given its shape, each tail that builds the
-- product made a yield of the leaves ('yieldLeaves'); each call that
-- builds one of its callee's patterns made a call of that specialisation
-- with the fields ('useSpecs'); the product lets nothing uses any more
-- left out ('dropDead'); and last, given a setting, each match on the
-- value of a match moved into its tails, with its arms as join points
-- ('splitJoins'). The tails go before the calls, so that a tail call of a
-- function with both kinds of worker takes the one that yields the
-- leaves, which is what the worker's own count needs; the join points go
-- last so that every other rewrite sees one body. With the body come the
-- sources of its variables, the lets the rewrite changed ('Prepared')
-- and the join points.
prepareBody :: (Var v) => Batch -> Maybe Shape -> Maybe (Joining v) -> ANormal Reference v -> (ANormal Reference v, Sources v, M.Map [v] [v], Set.Set v, [Join v])
prepareBody b ms mj bd
  | not rewritesOn = (uniqueBody bd, M.empty, M.empty, Set.empty, [])
  | not (null loose) =
      internalBug [] $
        "prepareBody: the rewrites left "
          ++ show loose
          ++ " unbound; unoptimized body:\n"
          ++ show (uniqueBody bd)
          ++ "\nrewritten body:\n"
          ++ show main
          ++ concat ["\njoin point:\n" ++ show (joinLambda jn) | jn <- joins]
  | otherwise = (freshLets main, M.unions [srcsK, srcs, srcsJ], lets, dead, map freshJoin joins)
  where
    -- a rewritten body, or a join point under its parameters, is closed
    -- where the unoptimized body is
    scope = ABTN.freeVars bd
    loose =
      Set.toList . Set.unions $
        (ABTN.freeVars main `Set.difference` scope)
          : [ ABTN.freeVars jb `Set.difference` (scope <> Set.fromList js)
            | jn <- joins,
              Lambda _ (TAbss js jb) <- [joinLambda jn]
            ]
    (known, srcsK)
      | knownConstructorOn = runState (knownCons (uniqueBody bd)) M.empty
      | otherwise = (uniqueBody bd, M.empty)
    (rets, srcs, lets) = useReturns b (sinkCalls known)
    body = dropDead (useSpecs b (maybe id (yieldLeaves b) ms rets))
    (main, joins, srcsJ, dead) = case mj of
      Just j | knownConstructorOn, Nothing <- ms -> splitJoins j (srcsK <> srcs) body
      _ -> (body, [], M.empty, Set.empty)

-- | The body with a number of its own for every indirect let: a let
-- 'splitJoins' copied into several tails carries its number into each
-- copy, and two sections of a combinator cannot share one, so every copy
-- after the first gets a number above the body's. The first keeps the
-- unoptimized number, which is what pairs sections when nothing else
-- changed ('deoptOf').
freshLets :: (Var v) => ANormal Reference v -> ANormal Reference v
freshLets bd = evalState (go bd) (Set.empty, top + 1)
  where
    top = maximum (0 : [w | TLets (Indirect w) _ _ _ _ <- universe bd])
    go (ABTN.Term _ (ABTN.Abs u b)) = ABTN.TAbs u <$> go b
    go (ABTN.Term _ (ABTN.Tm f)) = case f of
      ALet (Indirect w) ccs bn bo -> do
        (seen, next) <- get
        w' <-
          if Set.member w seen
            then next <$ put (seen, next + 1)
            else w <$ put (Set.insert w seen, next)
        ABTN.TTm <$> (ALet (Indirect w') ccs <$> go bn <*> go bo)
      _ -> ABTN.TTm <$> traverse go f

freshJoin :: (Var v) => Join v -> Join v
freshJoin (Join (Lambda ccs (TAbss vs bd)) cut srcs) = Join (Lambda ccs (TAbss vs (freshLets bd))) cut srcs

-- | The body with every match on a variable bound to a constructor
-- application or a record literal replaced by its arm, the arm's
-- variables renamed to the fields and recorded as their sources (the
-- argument side's 'rewrite', over a tree whose leaves are the fields);
-- a record update of such a record becomes a literal. The let stays
-- until 'dropDead' finds nothing else uses it. A match whose arms do not
-- include the constructor (a default arm would take it) is left as it is.
-- The body of a let is done before the let, since the arm of a later
-- product's match is what names an earlier product's variable (the
-- pair inside a tuple).
knownCons :: (Var v) => ANormal Reference v -> State (Sources v) (ANormal Reference v)
knownCons = go
  where
    go tm = case tm of
      TLets d [u] ms bn bo
        | TApp (FCon r t) args <- bn -> TLets d [u] ms bn <$> (rewrite u (Node u (Con r t (Plain <$ args)) (map Leaf args)) =<< go bo)
        | TApp (FRec rs) args <- bn -> TLets d [u] ms bn <$> (rewrite u (Node u (Fields rs (Plain <$ args)) (map Leaf args)) =<< go bo)
      ABTN.Term _ (ABTN.Abs v e) -> ABTN.TAbs v <$> go e
      ABTN.Term _ (ABTN.Tm f) -> ABTN.TTm <$> traverse go f

-- | The sites of a body where a match on the value of a match can be
-- moved into the tails that build the value ('site'), by the let's
-- binder: for each arm of the outer match (by constructor number, Nothing
-- for the default arm), the variables its body uses that are bound
-- outside it, and the same for what follows the match when its value is
-- let-bound, the group's local functions aside. Recorded on the
-- unoptimized body, so that a join point's parameters cover every
-- variable the unoptimized code reads, which is what lets a continuation
-- captured inside the join point be written out as the unoptimized
-- frames: a variable the join point's frame cannot rebuild is then one
-- the unoptimized code never reads there ('splitJoins', 'deoptOf').
type Sites v = M.Map v (M.Map (Maybe Int) (Set.Set v), Set.Set v)

caseSites :: (Var v) => ConLets v -> Set.Set v -> ANormal Reference v -> Sites v
caseSites cons locals bd = M.fromList [(p, (arms cs df, restFree kont)) | Just (Site p _ _ cs df kont) <- map (site cons) (universe bd)]
  where
    arms cs df =
      M.fromList $
        [(Just (EC.keyToInt t), free fs bo) | (t, (_, TAbss fs bo)) <- EC.mapToList cs]
          ++ [(Nothing, free [] d) | Just d <- [df]]
    restFree = maybe Set.empty (\(_, x, _, rest) -> free [x] rest)
    free fs bo = (ABTN.freeVars bo `Set.difference` Set.fromList fs) `Set.difference` locals

-- | A let whose binding's every tail builds a constructor of one type
-- ('tailCon') and whose body is a data match on the let's variable, used
-- nowhere else: the variable, the binding, the type, the arms and the
-- default arm, and what takes the match's value: nothing when the match
-- is the let's body, so its value is the enclosing section's, or the let
-- binding it (its direction, variable and convention) and the rest of
-- the body, which is how normalization leaves a block whose last
-- expression is the match.
data Site v = Site v (ANormal Reference v) Reference (EC.EnumMap CTag ([Mem], ANormal Reference v)) (Maybe (ANormal Reference v)) (Maybe (Direction Word16, v, [Mem], ANormal Reference v))

site :: (Var v) => ConLets v -> ANormal Reference v -> Maybe (Site v)
site cons tm = case tm of
  TLets _ [p] _ bn bo
    | Just (r, cs, df, kont) <- outer p bo,
      occurrences p bo == 1,
      all (isJust . tailCon cons r) (tailsOf bn) ->
        Just (Site p bn r cs df kont)
  _ -> Nothing
  where
    outer p = \case
      TMatch p' (MatchData r cs df) | p' == p -> Just (r, cs, df, Nothing)
      TLets d [x] ms (TMatch p' (MatchData r cs df)) rest | p' == p -> Just (r, cs, df, Just (d, x, ms, rest))
      _ -> Nothing

-- | The tails of a binding, through lets, names and the arms of matches
-- (what 'sinkCalls' sees through too).
tailsOf :: (Var v) => ANormal Reference v -> [ANormal Reference v]
tailsOf = \case
  TMatch _ bs -> concatMap tailsOf (toList bs)
  TLets _ _ _ _ bo -> tailsOf bo
  TName _ _ _ bo -> tailsOf bo
  ABTN.Term _ (ABTN.Abs _ b) -> tailsOf b
  tm -> [tm]

-- | The body with a function applied at each tail.
sinkTails :: (Var v) => (ANormal Reference v -> ANormal Reference v) -> ANormal Reference v -> ANormal Reference v
sinkTails k = \case
  TMatch v bs -> TMatch v (fmap (sinkTails k) bs)
  TLets d us ms bn bo -> TLets d us ms bn (sinkTails k bo)
  TName v f as bo -> TName v f as (sinkTails k bo)
  ABTN.Term _ (ABTN.Abs u b) -> ABTN.TAbs u (sinkTails k b)
  tm -> k tm

-- | A tail that builds a constructor of the type, or names a variable
-- bound to one: its tag and its fields.
tailCon :: (Var v) => ConLets v -> Reference -> ANormal Reference v -> Maybe (CTag, [v])
tailCon cons r = \case
  TApp (FCon r' t) args | r' == r -> Just (t, args)
  TVar u | Just (BuiltCon r' t fs) <- M.lookup u cons, r' == r -> Just (t, fs)
  _ -> Nothing

-- | The body with each site ('site') that the unoptimized body had
-- ('caseSites') rewritten: the let is gone, and each tail of its binding
-- becomes the outer arm for the constructor it builds, with the arm's
-- variables bound to the fields, followed, when the match's value was
-- let-bound, by a let of that variable over the rest of the body. An arm
-- that is a single expression, or that only one tail builds the
-- constructor of, is copied into the tail (the argument side's 'rewrite'
-- does the renaming and records the arm's variables' sources, as for an
-- exploded parameter); any other arm, and the rest when it is more than
-- a single expression, becomes a join point, made once, that the tail
-- calls with the fields (the match's value, for the rest) and the other
-- variables it uses: the variables the unoptimized code used there, each
-- as what stands for it now (renamed, or the fields of a product the body
-- no longer names, through the sources and the unoptimized products), and
-- the variables it uses now. The let's variable is dead everywhere its
-- frame could hold it, and is recorded so. A site whose tails would need
-- more join points than the lambda has room for is left as it is. The
-- join points come out in the order they are made, the k-th at
-- 'joinIndex' k; a site inside an arm or the rest makes its join points
-- before the arm does. With the body come the sources the copied arms
-- recorded and the dead variables.
splitJoins :: (Var v) => Joining v -> Sources v -> ANormal Reference v -> (ANormal Reference v, [Join v], Sources v, Set.Set v)
splitJoins (Joining (JoinSetting owner grpr locals) sites bcons srcs0) srcs1 bd = (bd', reverse joins, srcsOut, dead)
  where
    cons = conLets bd
    srcs = srcs0 <> srcs1
    inScope = Set.unions (ABTN.freeVars bd : map boundVars (universe bd))
    (bd', (joins, srcsOut, dead)) = runState (go Nothing bd) ([], M.empty, Set.empty)
    go cut tm = case tm of
      TLets d [p] ms bn bo
        | Just (Site _ _ r cs df kont) <- site cons tm,
          Just (arms, restFree) <- M.lookup p sites,
          let built = [t | tl <- tailsOf bn, Just (t, _) <- [tailCon cons r tl]]
              tags = nubOrd built
              armOf t = case EC.lookup t cs of
                Just (_, TAbss fs ab) -> Just (Just (EC.keyToInt t), fs, ab)
                Nothing -> (\ab -> (Nothing, [], ab)) <$> df
              -- an arm is copied when it is a leaf, or when one tail
              -- reaches it (the default arm is reached by every
              -- constructor without an arm of its own)
              tailsTo k = length [() | t <- built, Just (k', _, _) <- [armOf t], k' == k]
              copied k ab = leaf ab || tailsTo k == 1
              -- the arms of the outer match are inside the let that takes
              -- its value, when there is one
              armCut = maybe cut (\(_, x, _, _) -> Just [x]) kont
              restJoin = case kont of
                Just (_, _, _, rest) | not (leaf rest) -> 1
                _ -> 0,
          Just targets <- traverse armOf tags -> do
            (made, _, _) <- get
            let needed = length (nubOrd [k | (k, _, ab) <- targets, not (copied k ab)]) + restJoin
            if length made + needed > maxJoins
              then fallback
              else do
                modify (\(js, ss, ds) -> (js, ss, Set.insert p ds))
                -- the rest of the body as a join point taking the match's
                -- value, unless it is a single expression, copied
                restCode <- case kont of
                  Just (_, x, _, rest)
                    | not (leaf rest) -> do
                        rest' <- go cut rest
                        j <- newJoin cut M.empty [x] restFree rest'
                        pure (Just (TApp (FWorker grpr (joinIndex owner (fst j))) (x : snd j)))
                    | otherwise -> pure (Just rest)
                  Nothing -> pure Nothing
                -- one join point per arm that is neither a leaf nor built
                -- by one tail only
                numbered <- foldM (joinFor p r arms armCut) M.empty [tg | tg@(k, _, ab) <- targets, not (copied k ab)]
                -- an arm copied into its one tail, renamed by 'rewrite'
                -- (the default arm binds nothing and is copied as it is)
                let copyArm t args = \case
                      Just _ -> do
                        let (code, armSrcs) = runState (rewrite p (Node p (Con r t (Plain <$ args)) (map Leaf args)) (TMatch p (MatchData r cs df))) M.empty
                        modify (\(js, ss, ds) -> (js, ss <> armSrcs, ds))
                        pure code
                      Nothing -> pure (fromMaybe (internalBug [] "splitJoins: no default arm") df)
                onceCodes <-
                  M.fromList
                    <$> sequence
                      [ (,) t <$> copyArm t args k
                      | (t, (k, _, ab)) <- zip tags targets,
                        copied k ab,
                        not (leaf ab),
                        tl <- take 1 [tl | tl <- tailsOf bn, Just (t', _) <- [tailCon cons r tl], t' == t],
                        Just (_, args) <- [tailCon cons r tl]
                      ]
                let at tl
                      | Just (t, args) <- tailCon cons r tl,
                        Just (k, fs, ab) <- armOf t =
                          let arm
                                | leaf ab = ABTN.renames (M.fromList (zip fs args)) ab
                                | copied k ab = fromMaybe (internalBug [] "splitJoins: an arm without its copy") (M.lookup t onceCodes)
                                | otherwise = case M.lookup k numbered of
                                    Just (j, extra) -> TApp (FWorker grpr (joinIndex owner j)) (args ++ extra)
                                    Nothing -> internalBug [] "splitJoins: an arm without a join point"
                           in case (kont, restCode) of
                                (Just (d', x, ms', _), Just rest) -> TLets d' [x] ms' arm rest
                                _ -> arm
                      | otherwise = tl
                -- the binding takes the let's place, so its lets are inside
                -- whatever the let was inside, and the arms it now ends in
                -- are walked for their own sites
                go cut (sinkTails at bn)
        where
          fallback = TLets d [p] ms <$> go (cutFor d [p] cut) bn <*> go cut bo
      TLets d us ms bn bo -> TLets d us ms <$> go (cutFor d us cut) bn <*> go cut bo
      TShift {} -> pure tm
      ABTN.Term _ (ABTN.Abs u e) -> ABTN.TAbs u <$> go cut e
      ABTN.Term _ (ABTN.Tm f) -> ABTN.TTm <$> traverse (go cut) f
    -- the binding of an indirect let has the let's frame around it
    cutFor d us cut = case d of
      Indirect _ -> Just us
      Direct -> cut
    leaf ab = null (children ab)
    -- the join point for an arm, unless already made: numbered when its
    -- body, with the join points of the sites inside it, is done
    joinFor p r arms cut acc (k, fs, ab)
      | M.member k acc = pure acc
      | otherwise = do
          ab' <- go cut ab
          let psrcs = case k of
                Just t -> M.singleton p (Right (BuiltCon r (EC.intToKey t) fs))
                Nothing -> M.empty
          j <- newJoin cut psrcs fs (M.findWithDefault Set.empty k arms) ab'
          pure (M.insert k j acc)
    -- a join point whose parameters are the given variables and then what
    -- stands for the unoptimized variables and what the body uses now
    newJoin cut psrcs fs base body = do
      (js, ss, ds) <- get
      let extra =
            Set.toAscList $
              (Set.unions (map (expand (srcs <> ss)) (Set.toList base)) <> ABTN.freeVars body)
                `Set.difference` Set.fromList fs
                `Set.difference` locals
          params = fs ++ extra
      put (Join (Lambda (map (const BX) params) (TAbss params body)) cut psrcs : js, ss, ds)
      pure (length js + 1, extra)
    -- what stands for a variable of the unoptimized code: itself, what it
    -- was renamed to, or the fields of the product it was; through the
    -- sources given and those recorded so far, since an arm copied into
    -- its one tail at an enclosing site renames its variables, and a
    -- site inside it is reached after
    expand ss w = Set.fromList (ex ss (0 :: Int) w)
    ex ss depth w
      | depth > 64 = internalBug [] $ "splitJoins: the source of " ++ show w ++ " refers back to itself"
      | Set.member w dead0 = []
      | Just (Left u) <- M.lookup w ss = ex ss (depth + 1) u
      | Just (Right b) <- M.lookup w ss = concatMap (ex ss (depth + 1)) (builtFields b)
      | Just b <- M.lookup w bcons = [w | Set.member w inScope] ++ concatMap (ex ss (depth + 1)) (builtFields b)
      | Set.member w inScope = [w]
      | otherwise = internalBug [] $ "splitJoins: nothing stands for " ++ show w ++ " of the unoptimized code"
    -- the variables of sites, dead wherever a frame could hold them
    dead0 = M.keysSet sites

-- | The body with a name of its own for every binder and an index of its
-- own for every indirect let. Normalization reuses a name under
-- shadowing (the blank variables), and 'ABTN.freshen' gives a name back
-- once the binder's scope is over, while everything here maps variables
-- by name. Only a binder already bound elsewhere in the body is renamed,
-- so that a body whose binders are already unique comes back as it is:
-- the unoptimized body is renamed the same way, and the names are what
-- pairs its lets with the optimized body's ('Prepared'). The let indices
-- number the sections of the emitted combinator, and the ones
-- normalization assigned repeat once the ANF optimizer has inlined a
-- body whose lets carry their own ('uniqueLets'), which is also why the
-- renaming is applied with every rewrite off.
uniqueBody :: (Var v) => ANormal Reference v -> ANormal Reference v
uniqueBody bd = uniqueLets (evalState (uniqueBinders M.empty bd) (Set.unions (ABTN.freeVars bd : map boundVars (universe bd)), Set.empty))

-- | The indirect lets numbered 1, 2, .. in the order they appear, binding
-- before body, so that the same body always gets the same numbers: a
-- continuation names the sections it resumes by them, in this process
-- and in any other.
uniqueLets :: (Var v) => ANormal Reference v -> ANormal Reference v
uniqueLets bd = evalState (go bd) (1 :: Word16)
  where
    go (ABTN.Term _ (ABTN.Abs u b)) = ABTN.TAbs u <$> go b
    go (ABTN.Term _ (ABTN.Tm f)) = case f of
      ALet (Indirect _) ccs bn bo -> do
        w <- state (\n -> (n, n + 1))
        ABTN.TTm <$> (ALet (Indirect w) ccs <$> go bn <*> go bo)
      _ -> ABTN.TTm <$> traverse go f

-- | The state is the names in use, and the binders seen so far.
uniqueBinders :: (Var v) => M.Map v v -> ANormal Reference v -> State (Set.Set v, Set.Set v) (ANormal Reference v)
uniqueBinders rn (ABTN.Term _ (ABTN.Abs u body)) = do
  u' <- fresh u
  ABTN.TAbs u' <$> uniqueBinders (M.insert u u' rn) body
uniqueBinders rn (ABTN.Term _ (ABTN.Tm f)) =
  ABTN.TTm <$> traverse (uniqueBinders rn) (first (\v -> M.findWithDefault v v rn) f)

fresh :: (Var v) => v -> State (Set.Set v, Set.Set v) v
fresh u = do
  (used, seen) <- get
  if Set.member u seen
    then do
      let u' = Var.freshIn used u
      put (Set.insert u' used, Set.insert u' seen)
      pure u'
    else do
      put (used, Set.insert u seen)
      pure u

-- | A call whose argument is the value of a match, @let s = match c with
-- arms in f .. s ..@ with @s@ used nowhere else, moved into the arms, so
-- that the branch that builds a constructor calls with it: the let's body
-- is the call, so each arm's tail becomes the call with its tail variable
-- for @s@. Only when every tail is a variable (a built constructor's let
-- ends that way): a tail that is an expression would need a let of its
-- own, with an index, to name it.
sinkCalls :: (Var v) => ANormal Reference v -> ANormal Reference v
sinkCalls tm = case tm of
  TLets _ [s] _ bn@(TMatch _ _) call@(TApp f args)
    | s `elem` args,
      occurrences s call == 1,
      Just sunk <- sink bn (\u -> TApp f (map (\a -> if a == s then u else a) args)) ->
        sinkCalls sunk
  ABTN.Term _ (ABTN.Abs u b) -> ABTN.TAbs u (sinkCalls b)
  ABTN.Term _ (ABTN.Tm f) -> ABTN.TTm (fmap sinkCalls f)
  where
    sink e k = case e of
      TMatch v bs -> TMatch v <$> traverse (`sink` k) bs
      TLets d us ms bn bo -> TLets d us ms bn <$> sink bo k
      TName v fn as bo -> TName v fn as <$> sink bo k
      TVar u -> Just (k u)
      ABTN.Term _ (ABTN.Abs u b) -> ABTN.TAbs u <$> sink b k
      _ -> Nothing

-- | The specialisation a call site goes to, with its number: the first
-- pattern the site's arguments offer everything of.
siteSpec :: (Var v) => ConLets v -> [Pattern] -> [v] -> Maybe (Int, Pattern)
siteSpec cons pats args = listToMaybe [(k, pat) | (k, pat) <- zip [1 ..] pats, length pat == length args, and (zipWith refines site pat)]
  where
    site = map (siteShape cons 0) args

-- | The variables a call passes to a worker, in the worker's parameter
-- order: a plain argument as itself, an exploded one as the field
-- variables of the product let it names, down to the leaves. Nothing when
-- the site does not offer what the pattern asks ('siteSpec' chose it, so
-- it does).
explodedArgs :: (Var v) => ConLets v -> Pattern -> [v] -> Maybe [v]
explodedArgs cons pat args
  | length pat == length args = concat <$> traverse (uncurry go) (zip pat args)
  | otherwise = Nothing
  where
    go Plain a = Just [a]
    go s a
      | Just b <- M.lookup a cons, matchesBuilt s b = concat <$> traverse (uncurry go) (zip (subShapes s) (builtFields b))
      | otherwise = Nothing

-- The boring helpers.

lambdas :: SuperGroup Reference v -> [SuperNormal Reference v]
lambdas (Rec bs e) = e : map snd bs

lambdaBody :: (Var v) => SuperNormal Reference v -> ANormal Reference v
lambdaBody (Lambda _ (TAbss _ bd)) = bd

-- | A term and every term inside it.
-- | Every subterm, the term first; built with an accumulator, since the
-- analyses walk it once per let of a body and the nested appends of a
-- 'concatMap' cost the body's size times its depth per walk (a doc
-- literal of a few thousand lets took minutes, 2026-11-03).
universe :: ANormal Reference v -> [ANormal Reference v]
universe t0 = go t0 []
  where
    go t acc = t : foldr go acc (children t)

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
