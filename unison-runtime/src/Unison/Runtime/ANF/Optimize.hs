{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- Various optimizations that may be applied to ANF terms. Many of
-- these are able to be run after intermediate code generation and
-- before interpreter code generation, to improve the code without
-- affecting the communication protocol.
module Unison.Runtime.ANF.Optimize
  ( optimize,
    inline,
    InlineInfo (..),
    buildInlineMap,
    optimizeHandler,
  ) where

import Control.Monad.Writer (MonadWriter (..), WriterT (..), tell, runWriter, Writer)
import Data.Map qualified as Map
import Data.Monoid (Any (..))
import Data.Set qualified as Set
import Unison.ABT.Normalized qualified as ABTN
import Unison.Prelude
import Unison.Reference (Reference, Reference' (Builtin))
import Unison.Runtime.ANF
import Unison.Var (Var)
import Unison.Var qualified as Var

data InlineInfo v = InlInfo
  { inlArity :: Int
  , inlExpr :: ANormal v
    -- inlines that should only be performed in tail position, as they
    -- might change stack contents.
  , _tailOnly :: Bool
  }
  deriving (Show)

-- Checks a SuperGroup makes it eligible to be inlined.
-- Unfortunately we need to be quite conservative about this.
--
-- The heuristic implemented below is as follows:
--
--   1. There are no local bindings, so only the 'entry point'
--      matters.
--   2. The entry point body is just a single expression, that is,
--      an application, variable or literal.
--
-- The first condition ensures that there isn't any need to jump
-- into a non-entrypoint from outside a group. These should be rare
-- anyway, because the local bindings are no longer used for
-- (unison-level) local function definitions (those are lifted
-- out). The second condition ensures that inlining the body should
-- have no effect on the runtime stack of of the function we're
-- inlining into, because the combinator is just a wrapper around
-- the simple expression.
--
-- Fortunately, it should be possible to make _most_ builtins have
-- this form, so that their instructions can be inlined directly
-- into the call sites when saturated.
--
-- The result of this function is the information necessary to
-- inline the combinator—an arity and the body expression with
-- bound variables. This should allow checking if the call is
-- saturated and make it possible to locally substitute for an
-- inlined expression.
--
-- The `Reference` argument allows us to check if the body is a
-- direct recursive call to the same function, which would result
-- in infinite inlining. This isn't the only such scenario, but
-- it's one we can opportunistically rule out.
inlineInfo ::
  (Var v) => Reference -> SuperGroup v -> Maybe (InlineInfo v)
inlineInfo r (Rec [] (Lambda ccs body@(ABTN.TAbss vs e)))
  | isInlinable r e = Just $ InlInfo arity body False
  | isThunkApp e = Just $ InlInfo arity body True
  | Just opt <- matchHandlerApp e =
      Just $ InlInfo arity (ABTN.TAbss vs opt) True
  where
    arity = length ccs
inlineInfo _ _ = Nothing

-- This is a special inlining available for handlers, inlining the
-- entry point of the handler into the actual implementation. This
-- improves recursive handlers slightly, enables other optimizations,
-- and makes it easier to recognize affine handlers.
entryInfo :: Var v => SuperGroup v -> Maybe (InlineInfo v)
entryInfo (Rec [(him, _)] (Lambda ccs body@(ABTN.TAbss vs e)))
  | req:_ <- shiftArgs vs,
    isHandlerEntry him req e =
      Just $ InlInfo (length ccs) body True
entryInfo _ = Nothing

-- Rewriting
-- ---------
--
-- These functions allow rewriting terms while keeping track of some
-- pertinent information. They use a technique to recognize whether
-- any rewriting has been applied on subterms, to e.g. just use the
-- original term structure if not (to preserve more sharing).

type Memo = MonadWriter Any

memo :: Memo m => a -> m a -> m a
memo orig mod =
  listen mod <&> \(new, Any changed) ->
    if changed then new else orig

dirty :: Memo m => m ()
dirty = tell $ Any True

runMemo :: Writer Any a -> a
runMemo = fst . runWriter

descend ::
  (Memo m, Var v) =>
  (Bool -> ANormal v -> m (ANormal v)) ->
  Bool -> ANormal v -> m (ANormal v)
descend rec tail tm = memo tm $ case tm of
  TLet d v ccs bn bd ->
    TLet d v ccs <$> rec False bn <*> rec tail bd
  TName v f vs bd ->
    TName v f vs <$> rec tail bd
  TMatch v bs ->
    TMatch v <$> traverse (rec tail) bs
  TShift r v bd ->
    TShift r v <$> rec tail bd
  THnd rs hn ha bd ->
    THnd rs hn ha <$> rec tail bd
  TLocal v bd ->
    TLocal v <$> rec tail bd
  ABTN.TAbs v (ABTN.TAbss vs bd) ->
    ABTN.TAbss (v:vs) <$> rec tail bd
  _ -> pure tm

-- Rewrites a term from the top down, first applying the step
-- transform given, then descending to children.
rewriteDown ::
  (Memo m, Var v) =>
  (Bool -> ANormal v -> m (ANormal v)) ->
  ANormal v ->
  m (ANormal v)
rewriteDown step = go True
  where
    go tail tm = step tail tm >>= descend go tail

rewriteUp ::
  (Memo m, Var v) =>
  (Bool -> ANormal v -> m (ANormal v)) ->
  ANormal v ->
  m (ANormal v)
rewriteUp step = go True
  where
    go tail tm = memo tm (descend go tail tm) >>= step tail

-- Performs inlining on a `SuperGroup` using the inlining information
-- in the map. The map can be created from typical `SuperGroup` data
-- using the `buildInlineMap` function.
--
-- Inlining is capped at 30 iterations per site to avoid infinite
-- loops on recursive inlining situations that were not detected by
-- `builtInlineMap`.
inline ::
  (Memo m, Var v) =>
  Reference ->
  Map Reference (InlineInfo v) ->
  SuperGroup v ->
  m (SuperGroup v)
inline self inls0 grp@(Rec bs entry) =
  memo grp $ Rec <$> (traverse . traverse) go0 bs <*> go0 entry
  where
    inls = maybe id (Map.insert self) (entryInfo grp) inls0

    go0 nrm@(Lambda ccs body) =
      memo nrm $ Lambda ccs <$> go (30 :: Int) body

    go n tm
      | n <= 0 = pure tm
      | otherwise = rewriteUp (step n) tm

    step n tail (TApp (FComb r) args)
      | Just new <- findInline tail r args =
          dirty *> go (n-1) new
    step _ _tail tm = pure tm

    findInline tail r args =
      tweak tail args =<< Map.lookup r inls

    tweak isTail args (InlInfo arity (ABTN.TAbss vs body) tailOnly)
      | tailOnly && not isTail = Nothing
      -- exactly saturated
      | length args == arity,
        rn <- Map.fromList (zip vs args) =
          Just $ ABTN.renames rn body
      -- oversaturated, only makes sense if body is a call
      | length args > arity,
        (pre, post) <- splitAt arity args,
        rn <- Map.fromList (zip vs pre),
        TApp f pre <- ABTN.renames rn body =
          Just $ TApp f (pre ++ post)
      | otherwise = Nothing

-- Performs peephole optimizations on a `SuperGroup`. The term is
-- traversed and certain patterns are looked for, and replaced with
-- more optimized patterns. Some patterns rely on inlining to have
-- already happened to work, so the former should be called first, and
-- it may be beneficial to repeat the process after peephole
-- optimization runs in case more inlining became available.
peephole ::
  (Memo m, Var v) =>
  Map Reference (InlineInfo v) ->
  SuperGroup v ->
  m (SuperGroup v)
peephole inls grp@(Rec bs entry) =
  memo grp $ Rec <$> (traverse . traverse) go0 bs <*> go0 entry
  where
    go0 nrm@(Lambda ccs body) =
      memo nrm $ Lambda ccs <$> go body

    go = rewriteDown \_tail -> \case
      HandlerApp rw -> rw <$ dirty
      HandlerResume _ f as lh h bs rs -> do
        dirty
        pure . TName lh h bs . THnd rs lh Nothing $ TApp (Nameable f) as
      HandledThunk r n expr
        | Just info <- Map.lookup r inls,
          n < inlArity info ->
            expr <$ dirty
      tm -> pure tm

optimize ::
  (Var v) =>
  Reference ->
  Map Reference (InlineInfo v) ->
  SuperGroup v ->
  SuperGroup v
optimize self inls g0 = peep . runMemo $ inline self inls g0
  where
    inl g = case runWriter $ inline self inls g of
      (g, Any changed)
        | changed -> peep g
        | otherwise -> g

    peep g = case runWriter $ peephole inls g of
      (g, Any changed)
        | changed -> inl g
        | otherwise -> g

-- Optimizes a generated affine handler based on knowledge about
-- contexts it will be used in.
--
-- For instance, affine handlers never capture stacks, and are never
-- used in a context where their stacks may be captured. So, we may
-- freely drop unused values without causing problems.
affineOptimize :: (Var v) => ANormal v -> ANormal v
affineOptimize = runMemo . rewriteUp \_tail -> \case
  TLetD v _ bn bd
    | v `Set.notMember` ABTN.freeVars bd, effectless bn ->
        bd <$ dirty
  tm -> pure tm

  where
    effectless (TCon {}) = True
    effectless (TLit {}) = True
    effectless (TBLit {}) = True
    effectless (TVar {}) = True
    effectless _ = False

-- Recognize Func that can be used in a `TName`
nameable :: Func v -> Maybe (Either Reference v)
nameable (FVar v) = Just $ Right v
nameable (FComb r) = Just $ Left r
nameable _ = Nothing

pattern Nameable e <- (nameable -> Just e)
  where
  Nameable e = either FComb FVar e

-- Recognize a handler call with a given delayed value and handler for
-- it, yielding the specified handled references
handlerResumption :: Var v => v -> v -> ANormal v -> Maybe [Reference]
handlerResumption lz0 lh0 (THnd rs lh1 Nothing (TFrc lz1)) =
  rs <$ guard (lz0 == lz1 && lh0 == lh1)
handlerResumption _ _ _ = Nothing

-- Tail of a handler implementation resuming itself. This is the
-- unoptimized version from initial code generation.
--
--   lazy lz := f as
--   lazy lh := h bs
--   handle{rs} !lz with lh
pattern HandlerResume lz f as lh h bs rs
  <- TName lz f as (TName lh h bs (handlerResumption lz lh -> Just rs))

-- Recognize a possibly underapplied combinator that is used at the
-- end of a handler block. The result is the combinator reference, the
-- length of the variables it's applied to, and a rewritten expression
-- that inlines the application to the single use site.
matchHandledThunk ::
  Var v => ANormal v -> Maybe (Reference, Int, ANormal v)
matchHandledThunk (TLet _ th _ (TCom r vs) bd) =
  (r, length vs,) <$> prefix bd
  where
    -- Some by-name values may be defined before the handle
    prefix (TName v g bs bd)
      | v /= th, all (/= th) g, all (/= th) bs =
          TName v g bs <$> prefix bd

    prefix (THnd rs nh ah bd)
      | nh /= th, all (/= th) ah =
          THnd rs nh ah <$> suffix bd

    prefix _ = Nothing

    -- Some values may be bound before the thunk call as long as
    -- they're 'direct' calls that can't capture stacks and reveal
    -- that we've changed the convention.
    suffix (TLetD v cc bn bd)
      | v /= th, th `Set.notMember` ABTN.freeVars bn =
          TLetD v cc bn <$> suffix bd

    -- final expression in handle body is a call to the thunk.
    suffix (TApv h us)
      | h == th, all (/= th) us =
          Just . TCom r $ vs ++ us

    suffix _ = Nothing

matchHandledThunk _ = Nothing

--  th = f <vs> -- undersaturated
--  lazy v := ...
--  ...
--  handle
--    w = Con ...
--    x = Lit
--    ...
--    th w x ...
--  with ...
--
--  ==>
--
--  lazy v := ...
--  ...
--  handle
--    ...
--    f <vs> w x ...
--  with ...
pattern HandledThunk ref ar expr <-
  (matchHandledThunk -> Just (ref, ar, expr))

-- Builds inlining information from a collection of SuperGroups.
-- They are all tested for inlinability, and the result map
-- contains only the information for groups that are able to be
-- inlined.
buildInlineMap ::
  (Var v) =>
  Map Reference (SuperGroup v) ->
  Map Reference (InlineInfo v)
buildInlineMap =
  runIdentity
    . Map.traverseMaybeWithKey (\r g -> Identity $ inlineInfo r g)

-- Checks the body of a SuperGroup makes it eligible for inlining.
-- See below for the discussion.
isInlinable :: (Var v) => Reference -> ANormal v -> Bool
isInlinable r (TCom s _) = r /= s
isInlinable _ TApp {} = True
isInlinable _ TBLit {} = True
isInlinable _ TVar {} = True
isInlinable _ _ = False

-- Recognizes the form resulting from certain `handle` calls in the
-- surface syntax. The recognized pattern is:
--
--   h = hh <us>
--   lazy lz = th <vs>
--   h lz
--
-- We take the opportunity to rearrange the call a bit, making it:
--
--   lazy lz = th <vs>
--   hh <us> lz
--
-- This can be used both for inlining and improving the tail of a
-- handler.
matchHandlerApp :: Var v => ANormal v -> Maybe (ANormal v)
matchHandlerApp tm
  | TLet _ h0 _ (TCom r us) bd <- tm,
    TName lz0 th vs bd <- bd,
    TApv h1 [lz1] <- bd,
    h0 == h1, lz0 == lz1,
    -- binding/shadowing
    all (/= h0) th, all (/= h0) vs, all (/= lz0) us =
      Just . TName lz0 th vs $ TCom r (us ++ [lz1])
  | otherwise = Nothing

pattern HandlerApp rw <- (matchHandlerApp -> Just rw)

-- Recognizes the entry point of a handler, for inlining into the
-- actual handler if applicable.
isHandlerEntry :: Var v => v -> v -> ANormal v -> Bool
isHandlerEntry him0 req0 tm
  | TName lzh0 (Right him1) _ tm <- tm,
    THnd _ lzh1 Nothing (TFrc req1) <- tm =
      lzh0 == lzh1 && him0 == him1 && req0 == req1
  | otherwise = False

isThunkApp :: Var v => ANormal v -> Bool
isThunkApp tm
  | TLet _ un0 _ (TCon _ _ []) bd <- tm,
    TApv _ (shiftArgs -> un1:_) <- bd =
      un0 == un1
  | otherwise = False

-- If the provided SuperGroup is recognized as a handler, applies
-- optimizations to improve it, like adding better code for affine
-- handlers.
optimizeHandler :: Var v => Reference -> SuperGroup v -> SuperGroup v
optimizeHandler self group =
  fromMaybe group $ augmentHandler self group

-- moves the last value of a list to the start, for easier matching
shiftArgs :: [v] -> [v]
shiftArgs vs = case reverse vs of
  v : vs -> v : reverse vs
  [] -> []

-- Checks if the group represents a handler, and if so, tries to add
-- optimized affine code.
augmentHandler ::
  (Var v) => Reference -> SuperGroup v -> Maybe (SuperGroup v)
augmentHandler _self group
  | Rec [(mv0, matcher)] entry <- group,
    Lambda ccs (ABTN.TAbss args body) <- entry,
    thunk : vs <- shiftArgs args,
    Just body <- augmentHandlerEntry vs thunk mv0 ah body,
    Just amatcher <- translateHandlerMatch mv0 ah matcher =
      Just .
        Rec [(mv0, matcher), (ah, amatcher)] .
        Lambda ccs $
          ABTN.TAbss args body

  | otherwise = Nothing
  where
    ah = freshAff 0

-- Recognizes the matching portion of a handler, and produces an
-- optimized affine version if possible.
translateHandlerMatch
  :: Var v => v -> v -> SuperNormal v -> Maybe (SuperNormal v)
translateHandlerMatch self ah (Lambda ccs (ABTN.TAbss args body))
  | v : vs <- shiftArgs args,
    TMatch u branches <- body, u == v,
    MatchRequest cs df <- branches,
    args <- vs ++ [ar, v],
    ccs <- ccs ++ [BX] =
      Lambda ccs .
      ABTN.TAbss args .
      affineOptimize .
      TMatch u .
      flip MatchRequest df <$>
        traverse3 (affineHandlerCase self vs ah) cs

  | otherwise = Nothing

  where
    ar = freshAff 2
    traverse3 = traverse . traverse . traverse

-- Recognizes the entry combinator of a compiled handler. If it is
-- one, then the result is a modified version with an affine handler
-- filled in.
augmentHandlerEntry ::
  Var v => [v] -> v -> v -> v -> ANormal v -> Maybe (ANormal v)
augmentHandlerEntry vs thunk0 mv0 ah body
  | TName hv (Right mv1) us body <- body,
    THnd rs nh Nothing (TFrc thunk1) <- body,
    mv0 == mv1, nh == hv, thunk0 == thunk1,
    Prelude.and (zipWith (==) us vs) =
      Just .
        TName hv (Right mv1) us .
        TName ahp (Right ah) us $
          THnd rs nh (Just ahp) (TFrc thunk1)

  | otherwise = Nothing
  where
    ahp = freshAff 1

-- Recognizes an affine handler case, yielding a translated efficient
-- version if it is one.
affineHandlerCase ::
  Var v => v -> [v] -> v -> ANormal v -> Maybe (ANormal v)
affineHandlerCase self vs rec br
  | ABTN.TAbss us body <- br,
    TShift _ kf0 body <- body,
    TName kf (Left (Builtin "jumpCont")) [kf1] body <- body,
    kf0 == kf1 =
      ABTN.TAbss us <$>
        affinePreBranch self Set.empty vs rec ar kf body

  | otherwise = Nothing
  where
    ar = freshAff 2

-- Allows for having multiple branches that differ in the exact type
-- of affine handler recognized.
--
-- If the entire term doesn't use the continuation, then an irrelevant
-- handler is generated.
--
-- If the immediate term is a match, then we delay the choice of which
-- type of handler to generate into each branch.
--
-- If neither of the above cases hold, then we look for a linear case.
affinePreBranch ::
  Var v =>
  v ->
  Set v ->
  [v] ->
  v ->
  v ->
  v ->
  ANormal v ->
  Maybe (ANormal v)
affinePreBranch self bound vs rec ar kf bd
  | Just it <- irrelevantTail ar kf bd = Just it

  | TMatch v bs <- bd =
      TMatch v <$>
        for bs \case
          ABTN.TAbss us bd ->
            ABTN.TAbss us <$>
              affinePreBranch self bound' vs rec ar kf bd
            where
              bound' = Set.union (Set.fromList us) bound

  | otherwise =
      localize <$>
        runWriterT (translateLinear self bound vs rec ar kf bd)
  where
    localize (tm, Any True) = TLocal ar tm
    localize (tm, Any False) = tm

translateLinear ::
  Var v =>
  v ->
  Set v ->
  [v] ->
  v ->
  v ->
  v ->
  ANormal v ->
  WriterT Any Maybe (ANormal v)
translateLinear self bound0 vs rec ar kf = go bound0
  where
  go bound body
    | Just lt <- linearTail self vs bound rec ar kf body =
        lt <$ tell (Any True)

    | Just it <- irrelevantTail ar kf body = pure it

    | TLet d v cc e body <- body,
      kf `Set.notMember` ABTN.freeVars e =
        TLet d v cc e <$> go (Set.insert v bound) body

    | TName v f us body <- body,
      all (kf /=) f, all (kf /=) us =
        TName v f us <$> go (Set.insert v bound) body

    | TMatch v bs <- body =
        TMatch v <$>
          for bs \case
            ABTN.TAbss us bd ->
              ABTN.TAbss us <$>
                go (Set.fromList us `Set.union` bound) bd

    | otherwise = mzero

-- Recognizes the tail of a linear handler case, where the
-- continuation is called once in tail position. Returns a transformed
-- version if a match is found.
--
-- Arguments:
--   self: Reference to handler combinator
--   bound: arguments bound since header
--   vs: arguments to handler combinator
--   rec: local variable for affine handler
--   ar: argument variable for affine handler info
--   kf0: continuation variable
--   tm: term to transform
--
-- Note: this relies on inlining into the thunked continuation call to
-- avoid see exactly what the `k result` call is, rather than it
-- having multiple forms depending on the variable order.
linearTail ::
  Var v => v -> [v] -> Set v -> v -> v -> v -> ANormal v -> Maybe (ANormal v)
linearTail self vs bound rec ar kf0 tm
  -- | TLet _ hr0 _ (TCom r us) tm <- tm, -- recursive handler call
  --   TName thunk0 (Right kf1) [result] tm <- tm, -- lazy cont resume
  --   TApv hr1 [thunk1] <- tm, -- apply handler to thunk
  --   r == self, kf0 == kf1, thunk0 == thunk1, hr0 == hr1 =
  --     Just . update hr0 us $ TVar result

  -- | TName thunk0 (Right kf1) [result] tm <- tm, -- lazy cont resume
  --   TCom r (shiftArgs -> thunk1 : us) <- tm, -- apply handler to thunk
  --   r == self, thunk0 == thunk1, kf0 == kf1 =
  --     Just . update kf1 us $ TVar result

  | TName rh (Right f) as tm <- tm, f == self, -- recursive handler call
    all (/= kf0) as, rh /= kf0, -- no shadowing or non-linearity
    THnd _rs hh Nothing bd <- tm, rh == hh, -- handle recursively
    SimpleBody pre shad free kf1 result <- bd, -- simple enough body
    kf0 `Set.notMember` shad, -- kf is not shadowed in body
    kf0 `Set.notMember` free, -- kf is not free in `pre`
    kf0 == kf1 = -- continuation is called in tail position
      Just . update rh as . pre $ TVar result

  | otherwise = Nothing

  where
    update huv us
      -- recursive call with identical, non-shadowed variables;
      -- no need to update
      | Prelude.and (zipWith (==) us vs),
        all (`Set.notMember` bound) us = id
      -- repurpose hr0 variable for update call
      | otherwise =
          TName huv (Right rec) (us ++ [ar]) .
          TLets Direct [] [] (TUpdate ar huv)

parseSimpleHandlerBody ::
  Var v =>
  ANormal v ->
  Maybe (ANormal v -> ANormal v, Set v, Set v, v, v)
parseSimpleHandlerBody = \case
  TLet d u cc bn@(TCon {}) bd ->
    tweak u (ABTN.freeVars bn) (TLet d u cc bn) <$>
      parseSimpleHandlerBody bd
  TApv u [result] ->
    Just (id, mempty, mempty, u, result)
  _ -> Nothing
  where
    tweak w fvs1 f (g, sh, fvs0, u, v) =
      (f . g, Set.insert w sh, fvs, u, v)
      where
        fvs = fvs1 `Set.union` Set.delete w fvs0

pattern SimpleBody head shad free kf result <-
  (parseSimpleHandlerBody -> Just (head, shad, free, kf, result))

irrelevantTail :: Var v => v -> v -> ANormal v -> Maybe (ANormal v)
irrelevantTail ar kf tm
  | kf `Set.notMember` ABTN.freeVars tm =
      Just $ TLets Direct [] [] (TDiscard ar) tm
  | otherwise = Nothing

freshAff :: (Var v) => Word64 -> v
freshAff fr = Var.freshenId fr $ Var.typed Var.AffBlank

