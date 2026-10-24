{-# LANGUAGE LambdaCase #-}

-- | Deciding, before compiling a function, whether native code for it
-- would be faster than the interpreter. See design.md, "What isn't
-- compiled".
--
-- What compiling saves is the interpreter's overhead (dispatch and stack
-- traffic), a few nanoseconds per instruction; the instruction's own work
-- costs the same either way. What it can cost is exits: leaving native
-- code for something only the interpreter can do, and coming back, is a
-- round trip worth several instructions' savings. So a function that does
-- one foreign call and little else is slower compiled.
--
-- The estimate walks the function's MCode and computes, for an average
-- path from entry to return, the saving (in units of one simple
-- instruction's overhead) and the number of exits that are certain: taken
-- every time the code runs. Exits that only happen on a slow path (a fast
-- path's miss, the poll, stack growth) don't count.
module Unison.Runtime.JIT.Estimate
  ( Estimate (..),
    judge,
  )
where

import Control.Monad (unless, when)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Foreign.Ptr (Ptr)
import Unison.Runtime.JIT.Codegen (Switches (..), callOutWorthwhile, instrNativeIn, startsSupported, switches)
import Unison.Runtime.JIT.Config (Config, entryCost, exitCost)
import Unison.Runtime.MCode hiding (Env)
import Unison.Runtime.MCode qualified as MCode (GRef (Env))
import Unison.Runtime.Stack (MSection, Val)
import Unison.Util.EnumContainers qualified as EC

-- | For an average path through a function: the interpreter overhead that
-- native code avoids, and the exits it is certain to take.
data Estimate = Estimate
  { esSaving :: !Double,
    esExits :: !Double,
    -- | the path calls the function itself: it is a loop or a recursion
    esRec :: !Bool
  }

plus :: Estimate -> Estimate -> Estimate
plus (Estimate a b l) (Estimate c d m) = Estimate (a + c) (b + d) (l || m)

-- | The interpreter's overhead for an instruction or a branch, and for a
-- call or a let (a frame pushed, arguments moved).
simple, call :: Estimate
simple = Estimate 1 0 False
call = Estimate 3 0 False

-- | A certain exit. The node saves nothing: the interpreter does its work.
exit :: Estimate
exit = Estimate 0 1 False

worth :: Config -> Maybe Estimate -> Bool
worth config = \case
  Nothing -> True -- never returns; it doesn't matter
  Just (Estimate saving exits _) -> not (exits * exitCost config > saving)

-- | How likely a call of the function is to exit, as far as is known: its
-- certain exits on an average path, at most one.
exiting :: Maybe Estimate -> Double
exiting = maybe 0 (min 1 . esExits)

-- | What is recorded in the cell (see 'readNativeVerdict'). The low two
-- bits: 2 not worth compiling, 1 worth it, 3 worth it only for native
-- callers, because a call saves less than entering native code from the
-- interpreter costs. Above them, 'exiting' in hundredths.
verdictOf :: Config -> Maybe Estimate -> Int
verdictOf config path = code + 4 * round (100 * exiting path)
  where
    code
      | not (worth config path) = 2
      | Just (Estimate saving _ False) <- path, saving < entryCost config = 3
      | otherwise = 1

data Cx = Cx
  { cxConfig :: Config,
    cxSwitches :: Switches,
    cxSelf :: Ptr NativeCell,
    -- | how likely a call of the function itself is to exit (from a first
    -- pass that assumed it doesn't)
    cxSelfExits :: Double,
    -- | the functions being judged further up: a cycle is assumed native
    cxBusy :: Set.Set (Ptr NativeCell),
    -- | set when the result relied on that assumption
    cxTainted :: IORef Bool,
    -- | what was found out during this walk, final or not: whether each
    -- callee is worth compiling, and how likely a call of it is to exit
    cxMemo :: IORef (Map.Map (Ptr NativeCell) (Bool, Double))
  }

-- | Whether the function in this cell, with this body, is worth compiling,
-- and the estimate that says so (Nothing for code that never returns).
-- The verdict is recorded in the cell. Callees that have not been judged
-- are judged on the way, and their verdicts recorded too, so the answer
-- doesn't depend on the order in which functions get hot.
judge :: Config -> Ptr NativeCell -> MSection -> IO (Bool, Maybe Estimate)
judge config cell sect = do
  memo <- newIORef Map.empty
  tainted <- newIORef False
  path <- function (Cx config (switches config) cell 0 Set.empty tainted memo) sect
  unless (cell == noNativeCell) (writeNativeVerdict cell (verdictOf config path))
  pure (worth config path, path)

-- | The estimate for a whole function. A function that calls itself is
-- walked twice: the first walk finds how likely a call of it is to exit,
-- and the second counts that against each of its calls to itself.
function :: Cx -> MSection -> IO (Maybe Estimate)
function cx sect = do
  first <- walk cx {cxSelfExits = 0} sect
  case first of
    Just (Estimate _ exits True) | exits > 0 -> walk cx {cxSelfExits = min 1 exits} sect
    _ -> pure first

-- | Whether a call to this combinator stays in native code, and if it
-- does, how likely the callee is to exit before it returns.
native :: Cx -> RComb Val -> IO (Bool, Double)
native cx comb = case unRComb comb of
  CachedVal {} -> pure (False, 0)
  Comb (LamI _ _ entry cell)
    -- code not loaded through the cache: never compiled
    | cell == noNativeCell -> pure (False, 0)
    | cell == cxSelf cx -> pure (True, cxSelfExits cx)
    | Set.member cell (cxBusy cx) -> writeIORef (cxTainted cx) True >> pure (True, 0)
    | otherwise -> do
        verdict <- readNativeVerdict cell
        known <- Map.lookup cell <$> readIORef (cxMemo cx)
        case (known, verdict) of
          (Just v, _) -> pure v
          (_, 0) -> do
            tainted <- newIORef False
            path <- function cx {cxSelf = cell, cxBusy = Set.insert (cxSelf cx) (cxBusy cx), cxTainted = tainted} entry
            let v = (worth (cxConfig cx) path, exiting path)
            t <- readIORef tainted
            when t (writeIORef (cxTainted cx) True)
            unless t (writeNativeVerdict cell (verdictOf (cxConfig cx) path))
            modifyIORef' (cxMemo cx) (Map.insert cell v)
            pure v
          _ -> pure (verdict `mod` 4 /= 2, fromIntegral (verdict `div` 4) / 100)

-- | The estimate for the paths through a section, following what the code
-- generator does with each node ('genSection'). Nothing: no path returns.
walk :: Cx -> MSection -> IO (Maybe Estimate)
walk cx = \case
  Yield _ -> pure (Just simple)
  Die _ -> pure Nothing
  Exit -> pure Nothing
  Ins i rest
    | instrNativeIn (cxSwitches cx) i rest -> fmap (plus simple) <$> walk cx rest
    -- a call-out: the interpreter runs the instruction, native code goes on
    | Just _ <- pushCount i, callOutWorthwhile i rest -> fmap (plus exit) <$> walk cx rest
    -- the interpreter takes the rest of the function
    | otherwise -> pure (Just exit)
  Match _ br -> branch br
  NMatch _ _ br -> branch br
  DMatch _ _ _ br -> branch br
  Call _ _ comb args
    | constant comb args -> pure (Just (plus simple simple))
    | otherwise -> tailCall comb
  App _ (MCode.Env _ comb) args
    | constant comb args -> pure (Just (plus simple simple))
    | otherwise -> tailCall comb
  App _ (Stk _) _ -> pure (Just call) -- a function value: nothing is known about it
  -- a request: a call of the handler, a function value found at run time
  App _ (Dyn _) _ | swRequests (cxSwitches cx) -> pure (Just call)
  -- a request match: the pure arm and every ability's arms
  RMatch _ pu _ br
    | swRequests (cxSwitches cx), Just arms <- traverse armsOf (map snd (EC.mapToList br)) -> weigh (pu : concat arms)
  Let b _ _ body _ ->
    binding b >>= \case
      Nothing -> pure Nothing
      Just e -> fmap (plus e) <$> walk cx body
  -- the interpreter takes these: a jump to a continuation, a request with
  -- requests disabled, a request match with requests disabled or with arms
  -- the generator has no shape for
  Jump {} -> pure (Just exit)
  App _ (Dyn _) _ -> pure (Just exit)
  RMatch {} -> pure (Just exit)
  where
    isSelf comb = case unRComb comb of
      Comb (LamI _ _ _ cell) -> cell == cxSelf cx && cell /= noNativeCell
      _ -> False
    -- A tail call leaves nothing of the caller behind, so what the callee
    -- does afterwards is not the caller's concern.
    tailCall comb = do
      (ok, _) <- native cx comb
      pure (Just (if ok then call {esRec = isSelf comb} else exit))
    -- A call that returns here. If the callee exits, this function's
    -- frame is unwound with it and the rest of the function is entered
    -- again through the trampoline: that is an exit for this function too.
    returningCall comb = do
      (ok, exits) <- native cx comb
      pure (Just (if ok then Estimate 3 exits (isSelf comb) else exit))
    -- a combinator used as a value is a constant, and so is a top-level
    -- value that was evaluated when it was loaded
    constant comb args = case (unRComb comb, args) of
      (Comb (LamI arity _ _ _), ZArgs) -> arity > 0
      (CachedVal {}, ZArgs) -> True
      _ -> False
    -- A binding that is a call is a native call if the callee is native,
    -- and otherwise the interpreter's; either way native code continues
    -- with the body. Other bindings are generated inline.
    binding = \case
      Call _ _ comb args
        | constant comb args -> pure (Just simple)
        | otherwise -> returningCall comb
      App _ (MCode.Env _ comb) args
        | constant comb args -> pure (Just simple)
        | otherwise -> returningCall comb
      App _ (Stk _) _ -> pure (Just call)
      App _ (Dyn _) _ | swRequests (cxSwitches cx) -> pure (Just call)
      b
        | startsSupported (cxSwitches cx) b -> fmap (plus call) <$> walk cx b
        | otherwise -> pure (Just exit)
    -- The arms weigh the same, with two exceptions: those that can't
    -- return don't count, and in a function that calls itself the arms
    -- that do (the loop, the recursion) weigh more than the arms that
    -- don't (the way out, the base cases).
    branch br = case armsOf br of
      Nothing -> pure (Just exit)
      Just sects -> weigh sects
    weigh sects = do
      arms <- catMaybes <$> mapM (walk cx) sects
      pure $ case arms of
        [] -> Nothing
        _ ->
          let weight e = if esRec e then recursiveWeight else 1
              total = sum (map weight arms)
              s = sum [weight e * esSaving e | e <- arms] / total
              x = sum [weight e * esExits e | e <- arms] / total
           in Just (plus simple (Estimate s x (any esRec arms)))
    armsOf = \case
      Test1 _ a d -> Just [a, d]
      Test2 _ a _ b d -> Just [a, b, d]
      TestW d m -> Just (d : map snd (EC.mapToList m))
      -- matches on text and other boxed values are the interpreter's
      TestT _ _ -> Nothing
      TestY _ _ -> Nothing

-- | How much more often a branch arm that loops or recurses is taken than
-- one that doesn't. A guess: a loop takes its way out once, a binary
-- recursion reaches a base case every other call.
recursiveWeight :: Double
recursiveWeight = 4
