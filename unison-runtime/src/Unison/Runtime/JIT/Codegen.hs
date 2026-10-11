{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | MCode to LLVM IR, for the subset the JIT supports. See design.md for
-- the conventions and internals.md ("The generator") for how this module
-- is organized.
--
-- Within a function, every Unison stack slot is an LLVM alloca (decision
-- D11): @%u<k>@ holds the unboxed word and @%b<k>@ the boxed pointer of the
-- slot at frame offset @k@, which is stack index @fp + k@. The frame depth
-- @d@ (the interpreter's @sp - fp@) is known statically at every point, so
-- MCode's "slot @i@ from the top" is frame offset @d - i@. The real stack
-- is touched only at entry, before a tail call, and on exit paths.
--
-- The generator is strict throughout (see "Unison.Runtime.JIT.Strict"):
-- its text is 'Text', its sequences are 'Deque's, its pairs are 'Pair's,
-- and every field of its state is strict, so nothing it builds is a thunk.
module Unison.Runtime.JIT.Codegen
  ( Env (..),
    Cells (..),
    CtxOffsets (..),
    RtsFacts (..),
    Function (..),
    Deferred (..),
    AuxKey,
    AuxMemo,
    genFunction,
    genDeferred,
    modulePrelude,
    pushCount,
    startsSupported,
    callOutWorthwhile,
    instrNative,
    instrNativeIn,
    requestsOn,
    unitForeign,
    workerShape,
  )
where

import Control.Monad (foldM, forM, forM_, unless, void, when)
import Control.Monad.State.Strict hiding (put)
import Control.Monad.State.Strict qualified as State
import Data.Char (ord)
import Data.Maybe (isJust, isNothing)
import Data.Bits (shiftL, shiftR)
import Data.Int (Int64)
import Data.Primitive.PrimArray (primArrayToList)
import Data.Word (Word64)
import Foreign.Ptr (Ptr, WordPtr (..), ptrToWordPtr)
import GHC.Float (castDoubleToWord64)
import Unison.Runtime.JIT.Config (config, disabled, exitCall)
import Unison.Runtime.JIT.Exits (Exit (..))
import Unison.Runtime.JIT.Frames (Frame (..))
import Unison.Runtime.JIT.Layout
import Unison.Runtime.JIT.Pool
import Unison.Reference (Reference)
import Unison.Runtime.ANF (PackedTag (..))
import Unison.Runtime.Foreign.Function.Type (ForeignFunc (..))
import Unison.Runtime.TypeTags qualified as TT
import Unison.Builtin.Decls qualified as Ty (eitherRef, optionalRef, pairRef, seqViewRef, unitRef)
import Data.Map.Strict qualified as Map
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Unison.Runtime.MCode hiding (Env)
import Unison.Runtime.MCode qualified as MCode (GRef (Env))
import Unison.Runtime.Machine.Types (MCombs, MRef, MSection)
import Unison.Runtime.JIT.Strict
import Unison.Runtime.Stack (Val (..))
import Unison.Util.Deque (Deque, pattern Empty, pattern (:<|), (<|), (><), (|>))
import Unison.Util.Deque qualified as D
import Unison.Util.EnumContainers qualified as EC
import Unison.Util.Text qualified as UT

-- | Byte offsets of the fields of the C @Ctx@, from @unison_jit_ctx_layout@.
data CtxOffsets = CtxOffsets
  { oUstk, oBstk, oPool, oStackSize, oHplim, oAp, oFp, oSp, oMaxSp, oStressPoll, oStressPollLeft, oStressCallee, oStressCalleeLeft, oFrames, oNFrames, oMaxFrames, oCStackLimit, oCap, oBudgetEnd, oHp, oHpLim, oHenv, oNLocals :: !Int
  }

-- | From unison_jit_rts_facts: the info pointers and layouts of the two
-- arrays a @Seg@ is made of.
data RtsFacts = RtsFacts
  { rArrWordsInfo, rArrPtrsInfo :: !Int,
    -- | ByteArray#: header words including the byte count, and the word index of the count
    rBytesHeader, rBytesCount :: !Int,
    -- | Array#: header words, word index of the element count and of the payload size
    rPtrsHeader, rPtrsCount, rPtrsSize :: !Int,
    -- | log2 of elements per card-table byte
    rCardBits :: !Int,
    -- | word index of a MutVar#'s content
    rMutVarVar :: !Int,
    -- | info pointer a mutable Array# gets when written
    rArrPtrsDirtyInfo :: !Int
  }

data Env = Env
  { envLayouts :: !Layouts,
    envCtx :: !CtxOffsets,
    -- | index of this module's first exit in the global table
    envExitBase :: !Int,
    -- | index of this module's first frame in the global frame table
    envFrameBase :: !Int,
    -- | emit the stress-mode poll countdown
    envStressPoll :: !Bool,
    -- | emit the stress-mode "callee not compiled" countdown
    envStressCallee :: !Bool,
    -- | exits write the frame back through a call (see 'joinCall')
    envExitCall :: !Bool,
    -- | the group being compiled, for the arity of Let body combinators
    envCombs :: !MCombs,
    -- | pool index of every constant this group uses
    envPool :: !(Map.Map PoolKey Int),
    envRts :: !RtsFacts,
    -- | constructor arities of the data types loaded so far
    envTypes :: !(Map.Map Reference (Deque Int)),
    -- | the same by type number, for request matches, which name the
    -- ability by its number
    envAbilities :: !(EC.EnumMap Word64 (Deque Int)),
    -- | cells for the auxiliary functions (re-entry points), taken in order
    envCells :: !Cells,
    -- | features turned off for debugging (see Config)
    envDisabled :: !(Deque Text),
    -- | leave the re-entry points that are only used when something exits
    -- (bodies of Lets inside bindings, slow paths of instructions with a
    -- native fast path) to be generated when they turn out to be used:
    -- each gets a cell and a 'Deferred', but no code
    envLazy :: !Bool,
    -- | the auxiliary functions already known for the function this one
    -- belongs to, generated or deferred: a re-entry function generated
    -- later reuses the cells its parent handed out
    envKnown :: !AuxMemo,
    -- | the functions defined in the module being generated, by cell: a
    -- call to one of them is a direct call to its symbol, which LLVM can
    -- inline, instead of a call through the cell
    envLocal :: !(Map.Map (Ptr NativeCell) Text),
    -- | the functions of this module that have a worker (see 'feWorker'),
    -- by cell: the worker's symbol and its arity
    envWorkers :: !(Map.Map (Ptr NativeCell) (Pair Text Int)),
    -- | the code is from a sandboxed code cache: Ref.cas and
    -- Ref.readForCas are left to the interpreter, which refuses them
    envSandboxed :: !Bool,
    -- | the function's own symbol gets internal linkage: it is a private
    -- copy of a callee (see Compile's 'uCopy'), which LLVM drops once it
    -- is inlined at every call. Its worker is internal already; its
    -- auxiliary functions stay external, since they are installed in cells
    envInternal :: !Bool
  }

-- | The cells left for auxiliary functions: a supply without end (the
-- first pass over a module, which only counts what each function needs),
-- or what remains of the module's block.
data Cells = Counting | Cells !(Deque (Ptr NativeCell))

-- | The next cell, if there is one.
takeCell :: Cells -> Maybe (Pair (Ptr NativeCell) Cells)
takeCell = \case
  Counting -> Just (Pair noNativeCell Counting)
  Cells (c :<| cs) -> Just (Pair c (Cells cs))
  Cells _ -> Nothing

-- | What an auxiliary function is generated from: the section (without its
-- combinator references, which have no Ord; their CombIx stays), the depth
-- it starts at, and the frame base.
type AuxKey = (GSection (), Int, Int)

type AuxMemo = Map.Map AuxKey (Pair Text (Ptr NativeCell))

-- | A re-entry function that was given a cell but not generated: all that
-- 'genDeferred' needs to generate it later.
data Deferred = Deferred
  { dName :: !Text,
    dCix :: !CombIx,
    -- | slots on the stack at entry
    dLoaded :: !Int,
    dFrameSize :: !Int,
    -- | the frame base (see 'feBase')
    dBase :: !Int,
    dBody :: !MSection,
    dCell :: !(Ptr NativeCell)
  }

data Function = Function
  { fnName :: !Text,
    -- | the LLVM text of the function and what comes with it (a module's
    -- IR can run to tens of megabytes)
    fnIR :: !Text,
    -- | in index order, starting at the module's base plus the count before this function
    fnExits :: !(Deque Exit),
    -- | likewise for the frame table
    fnFrames :: !(Deque Frame),
    fnCell :: !(Ptr NativeCell),
    -- | the auxiliary functions defined alongside (re-entry points after
    -- call-outs, bodies of Lets inside bindings) and their cells
    fnAux :: !(Deque (Pair Text (Ptr NativeCell))),
    -- | the re-entry functions given a cell but left for later
    fnDeferred :: !(Deque Deferred),
    -- | every auxiliary function known after this one was generated
    fnMemo :: !AuxMemo,
    -- | why parts of the function fell back to the interpreter
    fnNotes :: !(Deque Text)
  }

-- | Declarations every module needs.
modulePrelude :: Text
modulePrelude =
  unlinesT . D.fromList $
    [ "declare ptr @llvm.stacksave.p0()",
      -- floats: the libm functions compiled Haskell calls for the same operations,
      -- and the intrinsics for what it does with instructions (see genPrim1)
      "declare double @exp(double)",
      "declare double @log(double)",
      "declare double @pow(double, double)",
      "declare double @cos(double)",
      "declare double @sin(double)",
      "declare double @tan(double)",
      "declare double @cosh(double)",
      "declare double @sinh(double)",
      "declare double @tanh(double)",
      "declare double @acos(double)",
      "declare double @asin(double)",
      "declare double @atan(double)",
      "declare double @asinh(double)",
      "declare double @acosh(double)",
      "declare double @atanh(double)",
      "declare double @llvm.fabs.f64(double)",
      "declare double @llvm.sqrt.f64(double)",
      "declare double @llvm.ceil.f64(double)",
      "declare double @llvm.floor.f64(double)",
      "declare double @llvm.rint.f64(double)",
      "declare i64 @llvm.fptosi.sat.i64.f64(double)",
      "declare i64 @unison_jit_pow(i64, i64)",
      "declare double @unison_jit_atan2(double, double)",
      -- arrays and refs (see "Arrays and Refs" in jit_rt.c); the pair is (ok, value)
      "declare i64 @unison_jit_barray_copy(ptr, i64, ptr, i64, i64, i64)",
      "declare ptr @unison_jit_barray_freeze(ptr, ptr, i64, i64, i64)",
      "declare ptr @unison_jit_barray_to_bytes(ptr, ptr, i64, i64)",
      "declare ptr @unison_jit_barray_from_bytes(ptr, ptr)",
      "declare ptr @unison_jit_barray_new(ptr, i64, i64, i64)",
      "declare ptr @unison_jit_barray_contents(ptr, ptr)",
      "declare ptr @unison_jit_parray_new(ptr, i64, i64, i64, ptr)",
      "declare i64 @unison_jit_parray_copy(ptr, i64, ptr, i64, i64, i64)",
      "declare ptr @unison_jit_parray_freeze(ptr, ptr, i64, i64, i64)",
      "declare ptr @unison_jit_ref_new(ptr, i64, ptr)",
      "declare ptr @unison_jit_ref_read_for_cas(ptr, ptr)",
      "declare i64 @unison_jit_ref_cas(ptr, ptr, ptr, i64, ptr)",
      "declare { i64, i64 } @unison_jit_murmur(i64, ptr)",
      "declare ptr @unison_jit_resolve_dyn(ptr, i64)",
      "declare i64 @unison_jit_set_aff(ptr, ptr, ptr)",
      "declare ptr @unison_jit_local_env(ptr, ptr)",
      "declare ptr @unison_jit_alloc_words(ptr, i64)",
      "declare preserve_mostcc void @unison_jit_exit_frame(ptr, ptr, i64, i64, ...)",
      "declare void @unison_jit_write_mutvar(ptr, ptr, ptr)",
      "declare i64 @unison_jit_list_size(ptr)",
      "declare ptr @unison_jit_list_view(ptr, ptr, ptr, i64, i64)",
      "declare ptr @unison_jit_list_push(ptr, ptr, i64, ptr, i64)",
      "declare ptr @unison_jit_list_index(ptr, ptr, i64, ptr, i64)",
      "declare ptr @unison_jit_list_lit(ptr, ptr, i64, ptr)",
      "declare ptr @unison_jit_list_wrap(ptr, ptr)",
      "declare ptr @unison_jit_list_cut(ptr, ptr, i64, i64)",
      "declare ptr @unison_jit_list_split(ptr, ptr, i64, ptr, i64, i64)",
      "declare ptr @unison_jit_list_append(ptr, ptr, ptr)",
      "declare i64 @unison_jit_text_size(ptr)",
      "declare ptr @unison_jit_text_append(ptr, ptr, ptr)",
      "declare ptr @unison_jit_text_cut(ptr, ptr, i64, i64)",
      "declare i64 @unison_jit_text_eq(ptr, ptr)",
      "declare i64 @unison_jit_bytes_size(ptr)",
      "declare ptr @unison_jit_bytes_append(ptr, ptr, ptr)",
      "declare ptr @unison_jit_bytes_cut(ptr, ptr, i64, i64)",
      "declare i64 @unison_jit_foreign_eq(ptr, ptr, i64)",
      "declare ptr @unison_jit_bytes_index(ptr, ptr, i64, ptr, i64, ptr)",
      "declare ptr @unison_jit_bytes_flatten(ptr, ptr)",
      "declare ptr @unison_jit_text_uncons(ptr, ptr, ptr, i64, ptr, i64, ptr, ptr, i64)",
      "declare ptr @unison_jit_int_to_text(ptr, i64, i64)",
      "declare ptr @unison_jit_float_to_text(ptr, i64)",
      "declare ptr @unison_jit_text_to_num(ptr, ptr, ptr, i64, ptr, i64)",
      "declare ptr @unison_jit_text_pack(ptr, ptr, ptr)",
      "declare ptr @unison_jit_bytes_pack(ptr, ptr, ptr)",
      "declare ptr @unison_jit_text_unpack(ptr, ptr, ptr)",
      "declare ptr @unison_jit_bytes_unpack(ptr, ptr, ptr)",
      "declare ptr @unison_jit_text_index_of(ptr, ptr, ptr, ptr, i64, ptr)",
      "declare ptr @unison_jit_bytes_index_of(ptr, ptr, ptr, ptr, i64, ptr)",
      "declare i64 @unison_jit_text_cmp(ptr, ptr)",
      "declare i64 @unison_jit_foreign_cmp(ptr, ptr, i64)",
      "declare ptr @unison_jit_char_to_text(ptr, i64)",
      "declare ptr @unison_jit_text_repeat(ptr, i64, ptr)",
      "declare ptr @unison_jit_text_reverse(ptr, ptr)",
      "declare ptr @unison_jit_text_case(ptr, ptr, i64)",
      "declare ptr @unison_jit_text_to_utf8(ptr, ptr)",
      "declare ptr @unison_jit_text_from_utf8(ptr, ptr, ptr, i64)",
      "declare ptr @unison_jit_bytes_decode_nat(ptr, ptr, i64, i64, ptr, i64, ptr, i64, ptr, ptr)",
      "declare ptr @unison_jit_bytes_encode_nat(ptr, i64, i64, i64)",
      "declare i64 @unison_jit_bytes_read_ok(ptr, i64, i64)",
      "declare i64 @unison_jit_bytes_read_at(ptr, i64, i64, i64)",
      "declare ptr @unison_jit_bytes_to_base(ptr, ptr, i64)",
      "declare ptr @unison_jit_bytes_from_base(ptr, ptr, i64, ptr, i64)",
      "declare ptr @unison_jit_name(ptr, ptr, i64, i64, ptr, i64, ptr, i64, ptr, i64, ptr)",
      "declare i64 @llvm.ctlz.i64(i64, i1)",
      "declare i64 @llvm.cttz.i64(i64, i1)",
      "declare i64 @llvm.ctpop.i64(i64)",
      "declare i64 @llvm.bswap.i64(i64)",
      -- branch weights: the first target is the cold one, or the hot one
      "!0 = !{!\"branch_weights\", i32 1, i32 4000}",
      "!1 = !{!\"branch_weights\", i32 4000, i32 1}"
    ]

-- | Marks a conditional branch whose first target is rarely taken (an
-- exit, a slow path), or nearly always. Without these the register
-- allocator keeps what the exit paths need in callee-saved registers, and
-- every call of the function pays to save and restore them.
unlikely, likely :: Text
unlikely = ", !prof !0"
likely = ", !prof !1"

-- ---------------------------------------------------------------------------
-- The generator

-- The state is strict in every field, and what the fields hold is strict
-- too (a 'Text', a 'Deque', a 'Pair', a strict map), so 'modify'' and 'put'
-- leave nothing unevaluated behind.
data GS = GS
  { gsFresh :: !Int,
    -- | finished blocks, in order: label and instructions
    gsBlocks :: !(Deque (Pair Text (Deque Text))),
    -- | the block being written: label and instructions
    gsCur :: !(Pair Text (Deque Text)),
    -- | exits, in order
    gsExits :: !(Deque Exit),
    gsNExits :: !Int,
    -- | frames, in order
    gsFrames :: !(Deque Frame),
    gsNFrames :: !Int,
    -- | highest frame offset used
    gsMaxK :: !Int,
    -- | slots whose value is a boolean held as an i1 register, with no
    -- closure built yet (internals.md, "Slots"). Absent means the slot's
    -- allocas hold the value. Saved and restored around branch arms.
    gsKinds :: !(IM.IntMap Text),
    -- | set when the function can't be compiled after all
    gsFailed :: !(Maybe Text),
    -- | auxiliary functions generated so far: their IR, their names and
    -- cells, how many there are, and the cells left to hand out
    gsAuxText :: !(Deque Text),
    gsAuxCells :: !(Deque (Pair Text (Ptr NativeCell))),
    gsNAux :: !Int,
    gsCells :: !Cells,
    -- | why parts of the function fell back to the interpreter, for the log
    gsNotes :: !(Deque Text),
    -- | the captured segment of the closure being called: its arrays and
    -- element count, from 'closureCallee' for 'copyCaptured'
    gsCaptured :: !(Maybe Captured),
    -- | auxiliary functions already generated, by what they were generated
    -- from (section, depth, frame base). The same Let body or call-out
    -- continuation is met again wherever its enclosing code is generated
    -- more than once (inline after a binding and in an auxiliary function
    -- for the binding's body); without this the output grows exponentially
    -- with the nesting of bindings.
    -- The combinator references are dropped from the key (their CombIx
    -- stays), since they have no Ord.
    gsAuxMemo :: !AuxMemo,
    -- | highest pool index the function being generated uses
    gsMaxPool :: !Int,
    -- | re-entry functions left for later, in order
    gsDeferred :: !(Deque Deferred),
    -- | set when the function was being generated as a worker and turned
    -- out to return other than one value somewhere; it has to be
    -- generated in the uniform form instead (not undone by 'attempt')
    gsNotWorker :: !Bool,
    -- | the function being generated is a worker: it doesn't load the
    -- addresses of the Unison stacks at entry (it only needs them where
    -- it exits), so each use loads them from @Ctx@
    gsWorker :: !Bool,
    -- | the shared write-back blocks of the function being generated
    -- (see 'joinShared'), by what they write; generated at the end
    gsShared :: !(Map.Map SharedKey Shared),
    -- | the exit descriptors of the function being generated (see
    -- 'joinCall'): the global's name by its contents, and the globals
    gsDescs :: !(Map.Map Text Text),
    gsDescText :: !(Deque Text)
  }

-- | What a write-back block does, which is all that exits and unwinds
-- differ in besides the status they return: the depth (an exit records
-- the stack pointer from it), the slots written (the live ones, see
-- 'liveAt'), whether the stack pointers are recorded in @Ctx@ (an exit)
-- or not (an unwind, where the callee that exited has recorded them),
-- the frame record for the binding being unwound (its index and depth),
-- and the enclosing inline bindings (index and base of each).
data SharedKey = SharedKey !Int ![Int] !Bool !(Maybe (Int, Int)) ![(Int, Int)]
  deriving (Eq, Ord)

-- | A shared write-back block: its label, the environment it is generated
-- in (its enclosing bindings are those of the key), and the blocks that
-- branch to it, each with the status it returns.
data Shared = Shared !Text !FnEnv !(Deque (Pair Text Text))

-- | The registers 'closureCallee' leaves for 'copyCaptured': the two
-- segment arrays and the element count.
data Captured = Captured !Text !Text !Text

type Gen = State GS

-- | 'State.put' with the state evaluated first, as 'modify'' does.
put :: GS -> Gen ()
put s = s `seq` State.put s

-- | The state for a fresh function: nothing generated, the given cells to
-- hand out, the given auxiliary functions known.
initialState :: Int -> Cells -> AuxMemo -> Bool -> GS
initialState maxK cells known worker =
  GS
    { gsFresh = 0,
      gsBlocks = D.empty,
      gsCur = Pair "head" D.empty,
      gsExits = D.empty,
      gsNExits = 0,
      gsFrames = D.empty,
      gsNFrames = 0,
      gsMaxK = maxK,
      gsKinds = IM.empty,
      gsFailed = Nothing,
      gsAuxText = D.empty,
      gsAuxCells = D.empty,
      gsNAux = 0,
      gsCells = cells,
      gsNotes = D.empty,
      gsCaptured = Nothing,
      gsAuxMemo = known,
      gsMaxPool = -1,
      gsDeferred = D.empty,
      gsNotWorker = False,
      gsWorker = worker,
      gsShared = Map.empty,
      gsDescs = Map.empty,
      gsDescText = D.empty
    }

-- | Gives up on the function (or on what 'attempt' is trying).
failWith :: Text -> Gen ()
failWith why = modify' (\s -> s {gsFailed = Just $! why})

fresh :: Text -> Gen Text
fresh base = do
  n <- gets gsFresh
  modify' (\s -> s {gsFresh = n + 1})
  pure ("%" <> base <> "." <> tshow n)

freshLabel :: Text -> Gen Text
freshLabel base = do
  n <- gets gsFresh
  modify' (\s -> s {gsFresh = n + 1})
  pure (base <> "." <> tshow n)

emit :: Text -> Gen ()
emit i = modify' (\s -> let !(Pair l is) = gsCur s in s {gsCur = Pair l (is |> i)})

-- | The label of the block being written.
curLabel :: Gen Text
curLabel = gets (\s -> let !(Pair l _) = gsCur s in l)

-- | Ends the current block (which must have been terminated) and starts another.
startBlock :: Text -> Gen ()
startBlock label = modify' $ \s -> s {gsBlocks = gsBlocks s |> gsCur s, gsCur = Pair label D.empty}

-- | Generates a block off to the side, then returns to the current one.
sideBlock :: Text -> Gen () -> Gen Text
sideBlock base body = do
  label <- if base == "grow" || base == "stale" then pure base else freshLabel base
  sideBlockNamed label body
  pure label

sideBlockNamed :: Text -> Gen () -> Gen ()
sideBlockNamed label body = do
  cur <- gets gsCur
  modify' (\s -> s {gsCur = Pair label D.empty})
  body
  modify' $ \s -> s {gsBlocks = gsBlocks s |> gsCur s, gsCur = cur}

useK :: Int -> Gen ()
useK k
  | k < 0 = failWith ("refers to a slot below the frame (offset " <> tshow k <> ")")
  | otherwise = modify' (\s -> s {gsMaxK = max (gsMaxK s) k})

uSlot, bSlot :: Int -> Text
uSlot k = "%u" <> tshow k
bSlot k = "%b" <> tshow k

-- | Slot offsets start at 1; offset 0 and below belong to the caller.
useSlot :: Int -> Gen ()
useSlot k
  | k < 1 = failWith ("refers to a slot below the frame (offset " <> tshow k <> ")")
  | otherwise = useK k

slotKind :: Int -> Gen (Maybe Text)
slotKind k = gets (IM.lookup k . gsKinds)

-- | Marks slot @k@ as holding the boolean @c@ (an i1) with no closure.
setBoolKind :: Int -> Text -> Gen ()
setBoolKind k c = useSlot k >> modify' (\s -> s {gsKinds = IM.insert k c (gsKinds s)})

clearKind :: Int -> Gen ()
clearKind k = modify' (\s -> s {gsKinds = IM.delete k (gsKinds s)})

-- | Runs a generator and puts the slot kinds back afterwards: for code
-- (a branch arm, an inline binding) after which control rejoins other paths.
withKinds :: Gen a -> Gen a
withKinds g = do
  ks <- gets gsKinds
  r <- g
  modify' (\s -> s {gsKinds = ks})
  pure r

loadU :: Int -> Gen Text
loadU k = do
  useSlot k
  slotKind k >>= \case
    Just _ -> pure "-1" -- a boxed value's word
    Nothing -> do
      v <- fresh "u"
      emit (v <> " = load i64, ptr " <> uSlot k)
      pure v

-- | The closure in slot @k@; a boolean held as an i1 is materialized here.
loadB :: Int -> Gen Text
loadB k = do
  useSlot k
  slotKind k >>= \case
    Just c -> do
      -- the two closures are loaded here, not at entry: most functions
      -- only need them where they exit
      -- (and the pool's address with them, so that it needn't be kept
      -- across calls for this)
      pool <- fresh "pool"
      emit (pool <> " = load ptr, ptr %pool.a")
      ta <- fresh "true.a"
      emit (ta <> " = getelementptr ptr, ptr " <> pool <> ", i64 " <> tshow poolIndexTrue)
      t <- fresh "true"
      emit (t <> " = load ptr, ptr " <> ta)
      fa <- fresh "false.a"
      emit (fa <> " = getelementptr ptr, ptr " <> pool <> ", i64 " <> tshow poolIndexFalse)
      f <- fresh "false"
      emit (f <> " = load ptr, ptr " <> fa)
      p <- fresh "bool"
      emit (p <> " = select i1 " <> c <> ", ptr " <> t <> ", ptr " <> f)
      pure p
    Nothing -> do
      v <- fresh "b"
      emit (v <> " = load ptr, ptr " <> bSlot k)
      pure v

storeU :: Int -> Text -> Gen ()
storeU k v = useSlot k >> clearKind k >> emit ("store i64 " <> v <> ", ptr " <> uSlot k)

storeB :: Int -> Text -> Gen ()
storeB k v = useSlot k >> clearKind k >> emit ("store ptr " <> v <> ", ptr " <> bSlot k)

-- | Address of stack index @fp + k@ in the unboxed or boxed stack.
stackAddrU, stackAddrB :: Int -> Gen Text
stackAddrU k = do
  f <- fpPlus k
  stk <- stackReg "%ustk"
  a <- fresh "ua"
  emit (a <> " = getelementptr i64, ptr " <> stk <> ", i64 " <> f)
  pure a
stackAddrB k = do
  f <- fpPlus k
  stk <- stackReg "%bstk"
  a <- fresh "ba"
  emit (a <> " = getelementptr ptr, ptr " <> stk <> ", i64 " <> f)
  pure a

-- | The register holding the address of a Unison stack (@%ustk@ or
-- @%bstk@). Loaded at entry, except in a worker, which loads it from
-- @Ctx@ wherever it is used (see 'gsWorker').
stackReg :: Text -> Gen Text
stackReg name = do
  lazy <- gets gsWorker
  if lazy
    then do
      r <- fresh (UT.drop 1 name)
      emit (r <> " = load ptr, ptr " <> name <> ".a")
      pure r
    else pure name

-- | A register holding @fp + k@, computed here. (The entry block has
-- its own, for the argument loads and the stack check. Sharing those
-- would keep them alive across every call for the sake of the exit
-- paths, which are where most uses are, and the register allocator would
-- rather save them than compute them again.) Using a slot offset records
-- it, so that the stack check covers it.
fpPlus :: Int -> Gen Text
fpPlus k = do
  useK k
  r <- fresh "fpk"
  emit (r <> " = add i64 %fp, " <> tshow k)
  pure r

ctxField :: Env -> (CtxOffsets -> Int) -> Gen Text
ctxField env f = do
  a <- fresh "ctx"
  emit (a <> " = getelementptr i8, ptr %ctx, i64 " <> tshow (f (envCtx env)))
  pure a

-- | Room for @words@ words of heap. The fast path bumps the context's copy
-- of the current allocation block's free pointer against a limit, as
-- compiled Haskell does with Hp and HpLim; the limit is the nearer of the
-- block's end and the allocation budget's end, so the budget costs nothing
-- here. When the object doesn't fit (or there is no block yet, or the budget
-- is used up) the slow path calls the allocator, which also moves the
-- context on to the new block. See "Allocation" in jit_rt.c. The caller
-- writes every word of the object before anything else can happen.
allocWords :: Env -> Int -> Gen Text
allocWords env words = do
  hpA <- ctxField env oHp
  hp <- fresh "hp"
  emit (hp <> " = load ptr, ptr " <> hpA)
  hp' <- fresh "hp.new"
  emit (hp' <> " = getelementptr i64, ptr " <> hp <> ", i64 " <> tshow words)
  limA <- ctxField env oHpLim
  lim <- fresh "hplim"
  emit (lim <> " = load ptr, ptr " <> limA)
  full <- fresh "full"
  emit (full <> " = icmp ugt ptr " <> hp' <> ", " <> lim)
  fastL <- freshLabel "alloc.fast"
  slowL <- freshLabel "alloc.slow"
  joinL <- freshLabel "alloc.join"
  emit ("br i1 " <> full <> ", label %" <> slowL <> ", label %" <> fastL <> unlikely)
  startBlock fastL
  emit ("store ptr " <> hp' <> ", ptr " <> hpA)
  emit ("br label %" <> joinL)
  startBlock slowL
  slowObj <- fresh "obj.slow"
  emit (slowObj <> " = call ptr @unison_jit_alloc_words(ptr %ctx, i64 " <> tshow words <> ")")
  emit ("br label %" <> joinL)
  startBlock joinL
  obj <- fresh "obj"
  emit (obj <> " = phi ptr [ " <> hp <> ", %" <> fastL <> " ], [ " <> slowObj <> ", %" <> slowL <> " ]")
  pure obj

-- ---------------------------------------------------------------------------
-- Exits

data FnEnv = FnEnv
  { feEnv :: !Env,
    -- | the LLVM function's name; auxiliary functions are named after it
    feName :: !Text,
    feCix :: !CombIx,
    -- | slots loaded at entry: the combinator's arity, or for an auxiliary
    -- function the depth it starts at
    feArity :: !Int,
    feFrameSize :: !Int,
    feCell :: !(Ptr NativeCell),
    -- | the loop head, for self tail calls; an auxiliary function has none
    feHead :: !(Maybe Text),
    -- | the frame base: the frame offset the interpreter's @fp@ points at
    -- when it enters this function. Zero for a combinator. An auxiliary
    -- function generated inside an inline binding has the binding's base,
    -- and the interpreter holds a @Push@ frame for the binding's body. Slot
    -- offsets stay relative to the combinator's frame throughout.
    feBase :: !Int,
    -- | registers holding the type-tag and boolean closures, loaded at entry
    feTagChar, feTagFloat, feTagInt, feTagNat :: !Text,
    -- | the inline @Let@ bindings the code being generated is inside of,
    -- innermost first
    feEnclosing :: !(Deque Enclosing),
    -- | The function is being generated as a /worker/ (design.md,
    -- "Workers"): its arguments arrive as LLVM parameters instead of on the
    -- Unison stack, and it returns @{status, u, b}@, its one result in
    -- registers. The Unison stack is written only when something exits.
    -- The function the cell points to is then a wrapper around it.
    feWorker :: !Bool,
    -- | Set while generating a worker's /fast entry/: the function its
    -- callers call, which holds only the paths that need nothing checked
    -- (no stack room, no poll: they can't exit, call or loop) and return
    -- at once, a base case typically. Any other path tail calls the full
    -- worker, named here, which starts again from the top.
    feFast :: !(Maybe Text),
    -- | the symbol of the LLVM function being generated
    feSym :: !Text,
    -- | the function gets internal linkage (see 'envInternal'); only the
    -- function the cell points to, not its auxiliary functions
    feInternal :: !Bool,
    -- | an affine handler case's environment switch this code is inside
    -- of (see 'genInLocal'): the switch is in force until the case's
    -- @Yield@, and every exit until then writes a @Local@ frame record
    feLocal :: !(Maybe Local)
  }

-- | An environment switch in force (a native @InLocal@): the register
-- holding the environment to switch back to, and how many inline bindings
-- the code was inside of when the switch was made, which says where its
-- @Local@ frame goes among the bindings' records and which @Yield@ ends it.
data Local = Local
  { loHenv0 :: !Text,
    loDepth :: !Int
  }

-- | Whether a @Yield@ here ends the environment switch in force: it is at
-- the depth the switch was made at. A tail call here would leave the
-- switch in force with nothing to end it, so tail calls are exits then.
localEnds :: FnEnv -> Bool
localEnds fe = case feLocal fe of
  Just lo -> loDepth lo == D.size (feEnclosing fe)
  Nothing -> False

-- | An inline @Let@ binding being generated. Inside it, the interpreter's
-- view is a fresh frame starting at @enBase@ (its @ap = fp = sp0@), so
-- exits write a frame record for it, and a @Yield@ delivers the results
-- to the body instead of returning.
data Enclosing = Enclosing
  { -- | frame table index
    enIndex :: !Int,
    -- | frame offset of the binding's frame base
    enBase :: !Int,
    -- | label of the body block
    enBody :: !Text,
    -- | number of results the body expects
    enResults :: !Int,
    -- | the slots the body reads once it runs (see 'liveAt')
    enLive :: !(Maybe IS.IntSet)
  }

-- | The slots the bodies of the enclosing bindings read once they run,
-- which an exit or unwind inside a binding has to write back as well as
-- what the resumed code itself reads.
enclosingLive :: FnEnv -> Maybe IS.IntSet
enclosingLive fe = foldr (\e acc -> enLive e <+> acc) (Just IS.empty) (feEnclosing fe)

-- | The union of two slot sets, where Nothing (every slot) absorbs.
(<+>) :: Maybe IS.IntSet -> Maybe IS.IntSet -> Maybe IS.IntSet
a <+> b = IS.union <$> a <*> b

infixr 5 <+>

-- | The slots (frame offsets, 1 and up) the interpreter reads when it
-- runs @sect@ at depth @d@ with the frame base at offset @base@, and in
-- whatever of the same frame runs after it; Nothing when that can't be
-- told statically, and every slot then has to count. Only these need to
-- be on the Unison stack when native code exits there: the others are
-- never read again (a slot dead here is dead at every later point of the
-- same path; a continuation captured later copies the frame as it is,
-- dead slots included, and never reads them either). Depths follow
-- 'genSection': MCode's "slot @i@ from the top" is offset @d - i@, an
-- instruction pushes 'pushCount' values, a @Let@'s body starts at its
-- combinator's arity, a data match arm pushes the constructor's fields.
-- An offset of 0 or below is the caller's (a pending argument), not ours.
liveAt :: Env -> Int -> Int -> MSection -> Maybe IS.IntSet
liveAt env base = go
  where
    go d = \case
      App _ r args -> refReads d r <+> argReads base d args
      Call _ _ _ args -> argReads base d args
      Jump i args -> slotAt d i <+> argReads base d args
      Match i br -> slotAt d i <+> arms d br
      NMatch _ i br -> slotAt d i <+> arms d br
      DMatch mr i br -> slotAt d i <+> dataArms d (mr >>= \r -> Map.lookup r (envTypes env)) br
      -- the pure request has one field, the value; a request arm pushes
      -- its constructor's fields
      RMatch i pu br ->
        slotAt d i
          <+> go (d + 1) pu
          <+> foldr (\(e, gb) acc -> dataArms d (EC.lookup e (envAbilities env)) gb <+> acc) (Just IS.empty) (EC.mapToList br)
      Yield args -> argReads base d args
      Ins i rest -> instrReads base d i <+> (pushCount i >>= \n -> go (d + n) rest)
      Let b (CIx _ _ w) _ body _ -> case EC.lookup w (envCombs env) of
        Just (Comb (LamI bodyArity _ _ _)) | bodyArity >= d -> go d b <+> go bodyArity body
        _ -> Nothing
      Die _ -> Just IS.empty
      Exit -> Just IS.empty
    refReads d = \case
      Stk i -> slotAt d i
      _ -> Just IS.empty
    -- the arms of a match that pushes nothing
    arms d = \case
      Test1 _ a b -> go d a <+> go d b
      Test2 _ a _ b c -> go d a <+> go d b <+> go d c
      TestW df m -> foldr (\(_, a) acc -> go d a <+> acc) (go d df) (EC.mapToList m)
      TestT df m -> foldr (\a acc -> go d a <+> acc) (go d df) (Map.elems m)
      TestY df m -> foldr (\a acc -> go d a <+> acc) (go d df) (Map.elems m)
    -- the arms of a data match: constructor u's arm runs with u's fields
    -- pushed, the default arm with nothing pushed
    dataArms d arities = \case
      Test1 u a df -> ctor d arities u a <+> go d df
      Test2 u a v b df -> ctor d arities u a <+> ctor d arities v b <+> go d df
      TestW df m -> foldr (\(u, a) acc -> ctor d arities u a <+> acc) (go d df) (EC.mapToList m)
      _ -> Nothing
    ctor d arities u a = case arities of
      Just as | Just n <- D.lookup (fromIntegral u) as -> go (d + n) a
      _ -> Nothing

-- | The slots an instruction reads (see 'liveAt').
instrReads :: Int -> Int -> GInstr comb -> Maybe IS.IntSet
instrReads base d = \case
  Prim1 _ i -> slotAt d i
  Prim2 _ i j -> slotAt d i <+> slotAt d j
  RefCAS i j k -> slotAt d i <+> slotAt d j <+> slotAt d k
  ForeignCall _ _ args -> argReads base d args
  DLLCall -> Nothing
  SetAff _ i j -> slotAt d i <+> slotAt d j
  Capture _ -> Nothing
  Discard _ -> Nothing
  Name r args -> (case r of Stk i -> slotAt d i; _ -> Just IS.empty) <+> argReads base d args
  Info _ -> Just IS.empty
  Pack _ _ args -> argReads base d args
  Lit _ -> Just IS.empty
  Print i -> slotAt d i
  Reset _ i mi -> slotAt d i <+> maybe (Just IS.empty) (slotAt d) mi
  InLocal i -> slotAt d i
  Fork i -> slotAt d i
  Atomically i -> slotAt d i
  Seq args -> argReads base d args
  TryForce i -> slotAt d i
  SandboxingFailure _ -> Nothing
  KeepAlive i -> slotAt d i
  NewForeignPtr i j -> slotAt d i <+> slotAt d j
  AddFinalizer i j -> slotAt d i <+> slotAt d j

-- | The slots an argument list reads, as 'argSources' resolves them.
argReads :: Int -> Int -> Args -> Maybe IS.IntSet
argReads base d = \case
  ZArgs -> Just IS.empty
  VArg1 i -> slotAt d i
  VArg2 i j -> slotAt d i <+> slotAt d j
  VArgR i l -> Just (IS.fromList [d - i - k | k <- [0 .. l - 1], d - i - k >= 1])
  VArgN v -> Just (IS.fromList [d - i | i <- primArrayToList v, d - i >= 1])
  VArgV i -> Just (IS.fromList [d - k | k <- [0 .. (d - base) - i - 1], d - k >= 1])

-- | Slot @i@ from the top at depth @d@, if it is one of ours.
slotAt :: Int -> Int -> Maybe IS.IntSet
slotAt d i
  | d - i >= 1 = Just (IS.singleton (d - i))
  | otherwise = Just IS.empty

-- | The slots that have to be on the Unison stack when this exit is
-- taken at depth @d@: what the interpreter reads when it carries on from
-- there, and what the enclosing bindings' bodies read afterwards.
-- Nothing for an exit that calls the function again from its entry
-- (every slot).
exitLive :: FnEnv -> Int -> Exit -> Maybe IS.IntSet
exitLive fe d = \case
  Resume _ sect -> liveAt env base d sect <+> enclosingLive fe
  CallOut _ instr rest n _ -> instrReads base d instr <+> liveAt env base (d + n) rest <+> enclosingLive fe
  Named _ e -> exitLive fe d e
  _ -> Nothing
  where
    env = feEnv fe
    base = currentBase fe

-- | The frame base the interpreter would see: @fp@ for the function's own
-- frame, or the innermost inline binding's base.
frameBase :: FnEnv -> Gen Text
frameBase fe = fpPlus (currentBase fe)

-- | The frame offset of the interpreter's current frame base.
currentBase :: FnEnv -> Int
currentBase fe = case feEnclosing fe of
  Empty -> feBase fe
  e :<| _ -> enBase e

-- | Writes the frame records for every enclosing inline binding,
-- innermost first (the order a chain of native callers would write them).
unwindEnclosing :: FnEnv -> Gen ()
unwindEnclosing fe = go (feEnclosing fe)
  where
    go Empty = pure ()
    go (e :<| outer) = do
      let !(Pair fsz asz) = case outer of
            Empty -> Pair (enBase e - feBase fe) Nothing
            o :<| _ -> Pair (enBase e - enBase o) (Just "0")
      writeRecord (feEnv fe) (enIndex e) fsz asz
      go outer

-- | Writes one frame record. The pending-argument count is @fp - ap@
-- unless given.
writeRecord :: Env -> Int -> Int -> Maybe Text -> Gen ()
writeRecord env ix fsz masz = do
  fr <- ctxField env oFrames
  frp <- fresh "frames"
  emit (frp <> " = load ptr, ptr " <> fr)
  nfa <- ctxField env oNFrames
  nf <- fresh "nf"
  emit (nf <> " = load i64, ptr " <> nfa)
  off <- fresh "off"
  emit (off <> " = mul i64 " <> nf <> ", 3")
  rec0 <- fresh "rec"
  emit (rec0 <> " = getelementptr i64, ptr " <> frp <> ", i64 " <> off)
  emit ("store i64 " <> tshow ix <> ", ptr " <> rec0)
  rec1 <- fresh "rec"
  emit (rec1 <> " = getelementptr i64, ptr " <> rec0 <> ", i64 1")
  emit ("store i64 " <> tshow fsz <> ", ptr " <> rec1)
  rec2 <- fresh "rec"
  emit (rec2 <> " = getelementptr i64, ptr " <> rec0 <> ", i64 2")
  asz <- case masz of
    Just a -> pure a
    Nothing -> do
      a <- fresh "asz"
      emit (a <> " = sub i64 %fpb, %ap")
      pure a
  emit ("store i64 " <> asz <> ", ptr " <> rec2)
  nf' <- fresh "nf"
  emit (nf' <> " = add i64 " <> nf <> ", 1")
  emit ("store i64 " <> nf' <> ", ptr " <> nfa)

-- | Adds an exit and returns its global index.
addExit :: Env -> Exit -> Gen Int
addExit env e = do
  n <- gets gsNExits
  modify' (\s -> s {gsExits = gsExits s |> e, gsNExits = n + 1})
  pure (envExitBase env + n)

-- | Adds a frame table entry and returns its global index.
addFrame :: Env -> Frame -> Gen Int
addFrame env f = do
  n <- gets gsNFrames
  modify' (\s -> s {gsFrames = gsFrames s |> f, gsNFrames = n + 1})
  pure (envFrameBase env + n)

-- | Writes the given slots back to the Unison stack.
writeSlots :: [Int] -> Gen ()
writeSlots [] = pure ()
writeSlots slots = do
  -- the frame's address on each stack once, then constant offsets
  f <- fpPlus 0
  ustk <- stackReg "%ustk"
  ub <- fresh "ufr"
  emit (ub <> " = getelementptr i64, ptr " <> ustk <> ", i64 " <> f)
  bstk <- stackReg "%bstk"
  bb <- fresh "bfr"
  emit (bb <> " = getelementptr ptr, ptr " <> bstk <> ", i64 " <> f)
  forM_ slots $ \k -> do
    useK k
    u <- loadU k
    ua <- fresh "ua"
    emit (ua <> " = getelementptr i64, ptr " <> ub <> ", i64 " <> tshow k)
    emit ("store i64 " <> u <> ", ptr " <> ua)
    b <- loadB k
    ba <- fresh "ba"
    emit (ba <> " = getelementptr ptr, ptr " <> bb <> ", i64 " <> tshow k)
    emit ("store ptr " <> b <> ", ptr " <> ba)

-- | A stress-mode countdown on a pair of Ctx fields; gives an i1 that is
-- true every Nth time.
stressFire :: Env -> (CtxOffsets -> Int) -> (CtxOffsets -> Int) -> Gen Text
stressFire env oLeft oEvery = do
  left <- ctxField env oLeft
  n <- fresh "left"
  emit (n <> " = load i64, ptr " <> left)
  n' <- fresh "left"
  emit (n' <> " = sub i64 " <> n <> ", 1")
  fire <- fresh "fire"
  emit (fire <> " = icmp sle i64 " <> n' <> ", 0")
  every <- ctxField env oEvery
  ev <- fresh "every"
  emit (ev <> " = load i64, ptr " <> every)
  reset <- fresh "reset"
  emit (reset <> " = select i1 " <> fire <> ", i64 " <> ev <> ", i64 " <> n')
  emit ("store i64 " <> reset <> ", ptr " <> left)
  pure fire

-- | Loads the callee's code pointer from its cell. Gives the pointer and
-- an i1 saying whether the callee must be treated as not compiled. A
-- callee defined in this module is named directly, and is always there
-- (except under the callee stress mode, which keeps the cell path tested).
loadCallee :: Env -> Ptr NativeCell -> Gen (Pair Text Text)
loadCallee env cell
  | not (envStressCallee env), Just name <- Map.lookup cell (envLocal env) = pure (Pair ("@" <> name) "false")
loadCallee env cell = do
  let WordPtr addr = ptrToWordPtr cell
  fnp <- fresh "fn"
  emit (fnp <> " = load ptr, ptr inttoptr (i64 " <> tshow addr <> " to ptr)")
  isNull <- fresh "isnull"
  emit (isNull <> " = icmp eq ptr " <> fnp <> ", null")
  if envStressCallee env
    then do
      fire <- stressFire env oStressCalleeLeft oStressCallee
      skip <- fresh "skip"
      emit (skip <> " = or i1 " <> isNull <> ", " <> fire)
      pure (Pair fnp skip)
    else pure (Pair fnp isNull)

-- | A block that writes the frame back to the Unison stack, records the
-- stack pointers in @Ctx@, and returns the exit's index. @d@ is the frame
-- depth at the exit point; every slot 1..d is written back.
exitBlock :: FnEnv -> Int -> Exit -> Gen Text
exitBlock = exitBlockNamed "exit"

exitBlockNamed :: Text -> FnEnv -> Int -> Exit -> Gen Text
exitBlockNamed base fe d e = do
  ix <- addExit (feEnv fe) e
  sideBlock base (joinShared fe d (exitLive fe d e) True Nothing (tshow ix))

-- | Ends the current block by branching to the function's write-back
-- block for the given key (see 'SharedKey'), which returns @status@ for
-- this path. One such block serves every exit and unwind that writes the
-- same thing, with the status as a phi: the write-back is most of a
-- function's code otherwise (a slot is a load, an address and a store on
-- each stack, for every slot at every exit; 72% of the lines of the
-- suite's largest module on 2026-10-09). LLVM's mem2reg turns the slot
-- loads of the shared block into phis of their own.
--
-- Only the slots in @live@ are written (every slot up to @d@ when it is
-- Nothing): the rest are never read again, see 'liveAt'.
--
-- A slot holding a boolean as an i1 register has no value in its
-- allocas; the shared block can't know which paths those are, so the
-- closure is built and stored here, on the cold path, before the branch.
joinShared :: FnEnv -> Int -> Maybe IS.IntSet -> Bool -> Maybe (Pair Int Int) -> Text -> Gen ()
joinShared fe d live isExit ownFrame status
  | envExitCall (feEnv fe) = joinCall fe d live isExit ownFrame status
  | otherwise = joinBlock fe d live isExit ownFrame status

-- | The slots a write-back writes: the live ones up to the depth.
liveSlots :: Int -> Maybe IS.IntSet -> [Int]
liveSlots d live = maybe [1 .. d] (filter (<= d) . IS.toAscList) live

-- | Ends the current block with the write-back as one call to the C
-- routine @unison_jit_exit_frame@ (jit_rt.c), the live slots as its
-- variadic arguments and the rest (what to do with them: the offsets, the
-- depth, the frame records) as a constant descriptor of the module, then
-- returns the status. The site is then one instruction
-- where the blocks of 'joinBlock' are six per slot; LLVM spills each
-- argument to the outgoing area, the same stores the blocks made, and
-- nothing else of the write-back goes through the optimizer or the
-- backend. The routine has the @preserve_most@ convention: a callee's
-- exit inlined into its caller is followed by the caller's unwind, two
-- calls in a row with the caller's live values across the first, and
-- under the C convention those would need callee-saved registers, which
-- the function then saves at every entry (Collatz 4% slower). The
-- default since 2026-10-09 (@UNISON_JIT_EXITS@).
joinCall :: FnEnv -> Int -> Maybe IS.IntSet -> Bool -> Maybe (Pair Int Int) -> Text -> Gen ()
joinCall fe d live isExit ownFrame status = do
  let slots = liveSlots d live
      base = feBase fe
      es = D.toList (feEnclosing fe)
      -- the records, as 'flushShared' writes them: the binding being
      -- unwound, then the enclosing bindings innermost first, each with
      -- its frame size relative to the next binding out, and the Local
      -- frame of an environment switch in force (see 'genInLocal') where
      -- the switch was made, after the bindings entered since. The pending
      -- count of the outermost record is -1 ("fpb - ap", computed by the
      -- routine) and 0 for the rest.
      own = case ownFrame of
        Nothing -> []
        Just (Pair ix fdepth) -> [[ix, fdepth - currentBase fe]]
      enclosing = zipWith (\e outer -> [enIndex e, enBase e - maybe base enBase outer]) es (map Just (drop 1 es) ++ [Nothing])
      ordered = case feLocal fe of
        Nothing -> own ++ enclosing
        Just lo -> let inner = length es - loDepth lo in own ++ take inner enclosing ++ [[-1, 0]] ++ drop inner enclosing
      recs = zipWith (\r pending -> r ++ [pending]) ordered (replicate (length ordered - 1) 0 ++ [-1 :: Int])
      flags = (if isExit then 1 else 0) + (if null es && isNothing (feLocal fe) then 2 else 0) :: Int
      header = [flags, currentBase fe, base, d, length slots, length recs]
      contents = intercalateT ", " (D.fromList (map (("i64 " <>) . tshow) (header ++ slots ++ concat recs)))
      -- the environment a Local record switches back to, one more argument
      localArg = maybe "" (\lo -> ", ptr " <> loHenv0 lo) (feLocal fe)
  when isExit (useK d)
  vals <- forM slots $ \k -> useK k >> (Pair <$> loadU k <*> loadB k)
  descs <- gets gsDescs
  name <- case Map.lookup contents descs of
    Just n -> pure n
    Nothing -> do
      let n = "@" <> feSym fe <> ".x" <> tshow (Map.size descs)
          n' = tshow (length header + length slots + 3 * length recs)
      modify' $ \s ->
        s
          { gsDescs = Map.insert contents n descs,
            gsDescText = gsDescText s |> (n <> " = private unnamed_addr constant [" <> n' <> " x i64] [" <> contents <> "]")
          }
      pure n
  -- the routine doesn't hand the status back: returning what was passed
  -- keeps it a constant here, so that LLVM still knows the function's
  -- range of statuses (a caller folds its checks of them with it)
  emit ("call preserve_mostcc void (ptr, ptr, i64, i64, ...) @unison_jit_exit_frame(ptr %ctx, ptr " <> name <> ", i64 %fp, i64 %ap" <> argList (D.fromList vals) <> localArg <> ")")
  retStatus fe status

-- | 'joinShared' with generated write-back blocks (@UNISON_JIT_EXITS=blocks@).
joinBlock :: FnEnv -> Int -> Maybe IS.IntSet -> Bool -> Maybe (Pair Int Int) -> Text -> Gen ()
joinBlock fe d live isExit ownFrame status = do
  let slots = liveSlots d live
  -- (the observing InLocal isn't generated in this mode, see 'localObservingOn')
  when (isJust (feLocal fe)) $ failWith "an environment switch with generated write-back blocks"
  kinds <- gets gsKinds
  forM_ (IM.keys kinds) $ \k -> when (k `elem` slots) $ do
    b <- loadB k
    emit ("store i64 -1, ptr " <> uSlot k)
    emit ("store ptr " <> b <> ", ptr " <> bSlot k)
  let key = SharedKey d slots isExit (fmap (\(Pair ix fd) -> (ix, fd)) ownFrame) [(enIndex e, enBase e) | e <- D.toList (feEnclosing fe)]
  from <- curLabel
  shared <- gets gsShared
  label <- case Map.lookup key shared of
    Just (Shared l _ _) -> pure l
    Nothing -> freshLabel "wb"
  let pred_ = Pair status from
      entry = case Map.lookup key shared of
        Just (Shared l fe0 preds) -> Shared l fe0 (preds |> pred_)
        Nothing -> Shared label fe (D.singleton pred_)
  modify' (\s -> s {gsShared = Map.insert key entry (gsShared s)})
  emit ("br label %" <> label)

-- | Generates the function's shared write-back blocks (see 'joinShared').
flushShared :: Gen ()
flushShared = do
  shared <- gets gsShared
  modify' (\s -> s {gsKinds = IM.empty})
  forM_ (Map.toList shared) $ \(SharedKey d slots isExit ownFrame _, Shared label fe preds) ->
    sideBlockNamed label $ do
      let env = feEnv fe
      st <- fresh "st"
      emit (st <> " = phi i64 " <> intercalateT ", " (fmap (\(Pair v from) -> "[ " <> v <> ", %" <> from <> " ]") preds))
      writeSlots slots
      when isExit $ do
        b <- frameBase fe
        ap <- ctxField env oAp
        emit ("store i64 " <> (if null (feEnclosing fe) then "%ap" else b) <> ", ptr " <> ap)
        fp <- ctxField env oFp
        emit ("store i64 " <> b <> ", ptr " <> fp)
        sp <- ctxField env oSp
        f <- fpPlus d
        emit ("store i64 " <> f <> ", ptr " <> sp)
        -- a worker's entry doesn't raise the high-water mark; its exits do
        when (feWorker fe) (markWritten env f)
      forM_ ownFrame $ \(ix, fdepth) -> case feEnclosing fe of
        Empty -> writeRecord env ix (fdepth - feBase fe) Nothing
        e :<| _ -> writeRecord env ix (fdepth - enBase e) (Just "0")
      unwindEnclosing fe
      retStatus fe st

-- | Terminates the current block with a resume exit at this section.
exitResume :: FnEnv -> Int -> MSection -> Gen ()
exitResume fe d sect = case feFast fe of
  -- a fast entry can't exit (it has checked nothing): the full worker can
  Just full -> tailToFull fe full
  Nothing -> do
    l <- exitBlock fe d (Resume (feCix fe) sect)
    emit ("br label %" <> l)

-- | In a fast entry: hands the call over to the full worker, with the
-- arguments it was called with (see 'feFast').
tailToFull :: FnEnv -> Text -> Gen ()
tailToFull fe full = do
  vals <- loadSources (D.fromList [1 .. feArity fe])
  r <- fresh "r"
  emit (r <> " = musttail call tailcc " <> workerRet <> " @" <> full <> "(ptr %ctx, i64 %fp.in" <> argList vals <> ")")
  emit ("ret " <> workerRet <> " " <> r)

-- | Arguments as a worker takes them, each after a comma.
argList :: Deque (Pair Text Text) -> Text
argList = foldMap (\(Pair u b) -> ", i64 " <> u <> ", ptr " <> b)

-- | What a worker returns: the status, and when it is OK the result's
-- unboxed word and boxed pointer. With status 'statusTailCall' the other
-- two are instead the code pointer of a function to call and the stack
-- pointer to call it with.
workerRet :: Text
workerRet = "{ i64, i64, ptr }"

-- | The status a worker returns to say "call this function for me, as a
-- tail call": its arguments are on the Unison stack above the worker's
-- frame base. A worker can't make the call itself, since the callee has
-- the uniform signature and a different return type; the wrapper at the
-- bottom makes it as a real tail call, and a worker that called this one
-- with a plain call makes it as a plain call.
statusTailCall :: Int
statusTailCall = -2

-- | Returns a status from the function being generated, in its own
-- return type.
retStatus :: FnEnv -> Text -> Gen ()
retStatus fe st
  | feWorker fe = do
      r <- fresh "ret"
      emit (r <> " = insertvalue " <> workerRet <> " undef, i64 " <> st <> ", 0")
      emit ("ret " <> workerRet <> " " <> r)
  | otherwise = emit ("ret i64 " <> st)

-- | Returns a worker's three values.
retWorker :: Text -> Text -> Text -> Gen ()
retWorker st u b = do
  r0 <- fresh "ret"
  emit (r0 <> " = insertvalue " <> workerRet <> " { i64 " <> st <> ", i64 undef, ptr undef }, i64 " <> u <> ", 1")
  r1 <- fresh "ret"
  emit (r1 <> " = insertvalue " <> workerRet <> " " <> r0 <> ", ptr " <> b <> ", 2")
  emit ("ret " <> workerRet <> " " <> r1)

-- | Raises the high-water mark of slots written to the boxed stack (for
-- marking its cards on return) to the given stack index.
markWritten :: Env -> Text -> Gen ()
markWritten env top = do
  maxA <- ctxField env oMaxSp
  old <- fresh "maxsp"
  emit (old <> " = load i64, ptr " <> maxA)
  gt <- fresh "maxsp.gt"
  emit (gt <> " = icmp sgt i64 " <> top <> ", " <> old)
  new <- fresh "maxsp"
  emit (new <> " = select i1 " <> gt <> ", i64 " <> top <> ", i64 " <> old)
  emit ("store i64 " <> new <> ", ptr " <> maxA)

-- | Whether the code builds anything in the heap itself.
allocates :: MSection -> Bool
allocates = anyInstr $ \case
  Pack _ _ ZArgs -> False
  Pack {} -> True
  Prim2 REFW _ _ -> True
  ForeignCall _ MutableArray_write _ -> True
  RefCAS {} -> True
  -- the list helpers allocate, and charge the budget themselves
  Prim1 op _ -> op `elem` [VWLS, VWRS, FLTB, UCNS, USNC, ITOT, NTOT, FTOT, TTOI, TTON, TTOF, PAKT, UPKT, PAKB, UPKB, REFN, RRFC]
  Prim2 op _ _ -> op `elem` [CONS, SNOC, IDXS, CATS, TAKS, DRPS, SPLL, SPLR, CATT, TAKT, DRPT, CATB, TAKB, DRPB, IDXB, IXOT, IXOB]
  ForeignCall _ f _ -> f `elem` (textForeign <> bytesForeign) || isJust (arrayForeign f)
  Seq _ -> True
  Name {} -> True
  _ -> False

-- | Whether the code makes a call that returns to it.
makesCalls :: MSection -> Bool
makesCalls = \case
  Let {} -> True
  Ins _ rest -> makesCalls rest
  Match _ br -> anyArm makesCalls br
  DMatch _ _ br -> anyArm makesCalls br
  NMatch _ _ br -> anyArm makesCalls br
  _ -> False

anyInstr :: (GInstr (RComb Val) -> Bool) -> MSection -> Bool
anyInstr p = go
  where
    go = \case
      Ins i rest -> p i || go rest
      Let b _ _ body _ -> go b || go body
      Match _ br -> anyArm go br
      DMatch _ _ br -> anyArm go br
      NMatch _ _ br -> anyArm go br
      _ -> False

anyArm :: (MSection -> Bool) -> GBranch (RComb Val) -> Bool
anyArm p = \case
  Test1 _ a d -> p a || p d
  Test2 _ a _ b d -> p a || p b || p d
  TestW d m -> p d || any (p . snd) (EC.mapToList m)
  TestT d m -> p d || any p (Map.elems m)
  TestY d m -> p d || any p (Map.elems m)

-- | Whether a combinator can have a worker: every return it makes itself
-- yields exactly one value. (Its tail calls return whatever their callees
-- do; a caller that expects one value from this function gets one.) This
-- is a first sieve: how many values a @Yield (VArgV i)@ returns depends on
-- the frame depth, which only generating the code finds out, and
-- generating a worker fails if it turns out not to be one.
workerShape :: MSection -> Bool
workerShape = \case
  Yield args -> case args of
    VArg1 _ -> True
    VArgV _ -> True
    _ -> False
  Ins _ rest -> workerShape rest
  Match _ br -> branches br
  DMatch _ _ br -> branches br
  NMatch _ _ br -> branches br
  Let _ _ _ body _ -> workerShape body
  -- calls, and whatever exits
  _ -> True
  where
    branches = \case
      Test1 _ a d -> workerShape a && workerShape d
      Test2 _ a _ b d -> workerShape a && workerShape b && workerShape d
      TestW d m -> workerShape d && all (workerShape . snd) (EC.mapToList m)
      TestT d m -> workerShape d && all workerShape (Map.elems m)
      TestY d m -> workerShape d && all workerShape (Map.elems m)

-- ---------------------------------------------------------------------------
-- Functions

-- | Compiles one combinator, or says why it can't be.
genFunction :: Env -> Text -> CombIx -> Int -> Int -> MSection -> Ptr NativeCell -> Either Text Function
genFunction env name cix arity frameSize body cell =
  runFunction env name cix arity frameSize cell (Just "head") 0 (Map.member cell (envWorkers env)) body

-- | Generates a re-entry function that was left for later.
genDeferred :: Env -> Deferred -> Either Text Function
genDeferred env d = runFunction env (dName d) (dCix d) (dLoaded d) (dFrameSize d) (dCell d) Nothing (dBase d) False (dBody d)

runFunction :: Env -> Text -> CombIx -> Int -> Int -> Ptr NativeCell -> Maybe Text -> Int -> Bool -> MSection -> Either Text Function
runFunction env name cix arity frameSize cell headL base worker body =
  let -- a worker whose callers call a fast entry is itself named apart
      fast = worker && freePath body
      sym
        | fast = name <> "_wf"
        | worker = workerName name
        | otherwise = name
      fe = FnEnv env name cix arity frameSize cell headL base "%tag.char" "%tag.float" "%tag.int" "%tag.nat" D.empty worker Nothing sym (envInternal env) Nothing
      gs0 = initialState arity (envCells env) (envKnown env) worker
      -- a worker comes with the function the cell points to, the wrapper,
      -- and maybe with a fast entry
      !(Pair text extra, gs) = flip runState gs0 $ do
        t <- genFunctionText fe body
        f <- if fast then D.singleton <$> genFastEntry fe {feFast = Just sym, feSym = workerName name, feHead = Nothing} body else pure D.empty
        w <- if worker then D.singleton <$> genWrapper fe body else pure D.empty
        pure (Pair t (f <> w))
   in case gsFailed gs of
        _ | gsNotWorker gs -> Left "a worker would return other than one value"
        Just why -> Left why
        Nothing ->
          Right $!
            Function
              name
              (unlinesT (text <| (extra <> gsAuxText gs)))
              (gsExits gs)
              (gsFrames gs)
              cell
              (gsAuxCells gs)
              (gsDeferred gs)
              (gsAuxMemo gs)
              (gsNotes gs)

-- | The text of one LLVM function for @body@, generated in the current
-- state (which must hold no blocks yet). Its exits and frames join the
-- state's lists.
genFunctionText :: FnEnv -> MSection -> Gen Text
genFunctionText fe body = do
  let env = feEnv fe
      arity = feArity fe
  growIx <- gets gsNExits
  case feFast fe of
    Nothing -> genHead fe body
    Just _ -> genSection fe arity body
  startBlock "unreachable"
  flushShared
  gs <- get
  let block (Pair l is)
        | l == "unreachable" = D.empty
        | otherwise = (l <> ":") <| fmap ("  " <>) is
      maxK = max (gsMaxK gs) (arity + feFrameSize fe)
      entry = entryBlock env (feWorker fe) (feFast fe == Nothing) arity (feBase fe) maxK (gsMaxPool gs)
      -- the grow exit, the first this function added, asks for what the
      -- entry check demanded
      fixGrow e
        | GrowStack _ c <- e = GrowStack (maxK - arity) c
        | otherwise = e
  modify' $ \s -> case D.splitAt growIx (gsExits s) of
    (before, e :<| after) -> s {gsExits = (before |> fixGrow e) >< after}
    _ -> s
  let header
        | feWorker fe =
            "define internal tailcc " <> workerRet <> " @" <> feSym fe <> "(ptr %ctx, i64 %fp.in"
              <> foldMap (\k -> ", i64 %a.u" <> tshow k <> ", ptr %a.b" <> tshow k) [1 .. arity]
              <> ") {"
        | otherwise = "define " <> (if feInternal fe then "internal " else "") <> "i64 @" <> feSym fe <> "(ptr %ctx, i64 %ap, i64 %fp.in, i64 %sp) {"
  pure . unlinesT $
    gsDescText gs <> (header <| entry) <> foldMap block (gsBlocks gs) |> "}"

-- | Generates a worker's fast entry (see 'feFast') as a function of its
-- own. Its exits (there are none that can be taken) join the function's.
genFastEntry :: FnEnv -> MSection -> Gen Text
genFastEntry fe body = do
  s <- get
  let gs0 = s {gsFresh = 0, gsBlocks = D.empty, gsCur = Pair "head" D.empty, gsMaxK = feArity fe, gsKinds = IM.empty, gsCaptured = Nothing, gsMaxPool = -1, gsShared = Map.empty, gsDescs = Map.empty, gsDescText = D.empty}
      !(text, gs) = runState (genFunctionText fe body) gs0
  put
    s
      { gsExits = gsExits gs,
        gsNExits = gsNExits gs,
        gsFrames = gsFrames gs,
        gsNFrames = gsNFrames gs,
        gsNotes = gsNotes gs,
        gsFailed = maybe (gsFailed gs) Just (gsFailed s),
        gsNotWorker = gsNotWorker s || gsNotWorker gs
      }
  pure text

-- | Whether some path through the code returns without needing anything
-- checked: only instructions that can't exit, branches on words, and a
-- return. (Statically a match on a data value counts; whether it really
-- is a match on a boolean still in a register is known when the code is
-- generated, see 'checkFree'.)
freePath :: MSection -> Bool
freePath = \case
  Yield _ -> True
  Ins i rest -> exitFree i && freePath rest
  Match _ br -> plainBranch br && anyArm freePath br
  NMatch _ _ br -> plainBranch br && anyArm freePath br
  DMatch _ _ br -> plainBranch br && anyArm freePath br
  _ -> False

-- | Instructions whose native code has no exit.
exitFree :: GInstr (RComb Val) -> Bool
exitFree = \case
  Lit l -> litSupported l
  Prim1 op _ -> op `elem` [DECI, DECN, INCI, INCN, NEGI, COMN, COMI, TRNC, SGNI, LZRO, TZRO, POPC]
  Prim2 op _ _ ->
    op
      `elem` [ ADDI, SUBI, MULI, EQLI, NEQI, LEQI, LESI, ANDI, IORI, XORI,
               ADDN, SUBN, MULN, EQLN, NEQN, LEQN, LESN, ANDN, IORN, XORN, DRPN
             ]
  _ -> False

plainBranch :: GBranch (RComb Val) -> Bool
plainBranch = \case
  Test1 {} -> True
  Test2 {} -> True
  TestW {} -> True
  _ -> False

-- | Whether the code for this node, at this depth, can run before the
-- function's entry checks (see 'feFast').
checkFree :: Int -> MSection -> Gen Bool
checkFree d = \case
  Yield _ -> pure True
  Ins i _ -> pure (exitFree i)
  Match _ br -> pure (plainBranch br)
  NMatch _ _ br -> pure (plainBranch br)
  -- a boolean still in a register: one branch, no closure to look at
  DMatch _ i br -> do
    kind <- slotKind (d - i)
    pure (kind /= Nothing && plainBranch br)
  _ -> pure False

-- | The symbol of a function's worker.
workerName :: Text -> Text
workerName name = name <> "_w"

-- | The function a worker's cell points to. It has the uniform signature:
-- it loads the arguments from the Unison stack, calls the worker, and on
-- OK stores the result and the stack pointers as a @Yield@ does. If the
-- worker asks for a tail call it makes it; any other status is an exit
-- the worker has already prepared, and is passed on.
--
-- Pending arguments (an over-application: @ap@ differs from @fp@) are the
-- interpreter's business when the function returns, so the wrapper hands
-- such a call to the interpreter straight away, and workers never see one.
genWrapper :: FnEnv -> MSection -> Gen Text
genWrapper fe body = do
  let env = feEnv fe
      name = feName fe
      arity = feArity fe
      field reg f = reg <> " = getelementptr i8, ptr %ctx, i64 " <> tshow (f (envCtx env))
      indent = fmap ("  " <>)
      lines = D.fromList
  -- the exit for pending arguments: the interpreter resumes at the top
  pendingIx <- addExit env (Resume (feCix fe) body)
  pure . unlinesT $
    lines
      [ "define " <> (if feInternal fe then "internal " else "") <> "i64 @" <> name <> "(ptr %ctx, i64 %ap, i64 %fp.in, i64 %sp) {",
        "  %pending = icmp ne i64 %ap, %fp.in",
        "  br i1 %pending, label %pending.exit, label %enter" <> unlikely,
        "enter:"
      ]
      <> indent
        ( lines
            [ field "%ustk.a" oUstk,
              "%ustk = load ptr, ptr %ustk.a",
              field "%bstk.a" oBstk,
              "%bstk = load ptr, ptr %bstk.a"
            ]
            <> foldMap
              ( \k' ->
                  let k = tshow k'
                   in lines
                        [ "%ix" <> k <> " = add i64 %fp.in, " <> k,
                          "%u" <> k <> ".a = getelementptr i64, ptr %ustk, i64 %ix" <> k,
                          "%u" <> k <> " = load i64, ptr %u" <> k <> ".a",
                          "%b" <> k <> ".a = getelementptr ptr, ptr %bstk, i64 %ix" <> k,
                          "%b" <> k <> " = load ptr, ptr %b" <> k <> ".a"
                        ]
              )
              [1 .. arity]
            <> lines
              [ "%r = call tailcc " <> workerRet <> " @" <> workerName name <> "(ptr %ctx, i64 %fp.in"
                  <> foldMap (\k -> ", i64 %u" <> tshow k <> ", ptr %b" <> tshow k) [1 .. arity]
                  <> ")",
                "%st = extractvalue " <> workerRet <> " %r, 0",
                "%r.u = extractvalue " <> workerRet <> " %r, 1",
                "%r.b = extractvalue " <> workerRet <> " %r, 2",
                "switch i64 %st, label %exit [ i64 0, label %ok  i64 " <> tshow statusTailCall <> ", label %tail ]"
              ]
        )
      <> lines ["ok:"]
      <> indent
        ( lines
            [ -- the worker may have run native code that moved nothing, but
              -- the stacks' addresses are the same for the whole native run
              "%res = add i64 %fp.in, 1",
              "%res.u = getelementptr i64, ptr %ustk, i64 %res",
              "store i64 %r.u, ptr %res.u",
              "%res.b = getelementptr ptr, ptr %bstk, i64 %res",
              "store ptr %r.b, ptr %res.b",
              field "%ap.a" oAp,
              "store i64 %ap, ptr %ap.a",
              field "%fp.a" oFp,
              "store i64 %ap, ptr %fp.a",
              field "%sp.a" oSp,
              "store i64 %res, ptr %sp.a",
              field "%maxsp.a" oMaxSp,
              "%maxsp.old = load i64, ptr %maxsp.a",
              "%maxsp.gt = icmp sgt i64 %res, %maxsp.old",
              "%maxsp.new = select i1 %maxsp.gt, i64 %res, i64 %maxsp.old",
              "store i64 %maxsp.new, ptr %maxsp.a",
              "ret i64 0"
            ]
        )
      <> lines ["tail:"]
      <> indent
        ( lines
            [ "%fn = inttoptr i64 %r.u to ptr",
              "%top = ptrtoint ptr %r.b to i64",
              "%t = musttail call i64 %fn(ptr %ctx, i64 %ap, i64 %fp.in, i64 %top)",
              "ret i64 %t"
            ]
        )
      <> lines ["exit:", "  ret i64 %st"]
      -- the frame is as the caller left it: only the stack pointers go back
      <> lines ["pending.exit:"]
      <> indent
        ( lines
            [ field "%p.ap.a" oAp,
              "store i64 %ap, ptr %p.ap.a",
              field "%p.fp.a" oFp,
              "store i64 %fp.in, ptr %p.fp.a",
              field "%p.sp.a" oSp,
              "store i64 %sp, ptr %p.sp.a",
              "ret i64 " <> tshow pendingIx
            ]
        )
      <> lines ["}"]

-- | Generates an auxiliary function in this module: the code for @body@
-- starting at depth @loaded@ (that many slots are on the stack), entered
-- by the interpreter with its frame pointer at frame offset @base@. Gives
-- its name and cell, or Nothing if it can't be compiled or there are no
-- cells left. Its exits and frames join this module's tables.
--
-- When @later@ is set and the module is generated lazily, the function
-- only gets its name and cell, and a 'Deferred' to generate it from when
-- the interpreter has found the cell empty often enough.
genAuxFunction :: Bool -> FnEnv -> Int -> Int -> MSection -> Gen (Maybe (Pair Text (Ptr NativeCell)))
genAuxFunction later fe loaded base body = do
  s <- get
  let key = (void body, loaded, base)
  case (Map.lookup key (gsAuxMemo s), takeCell (gsCells s)) of
    (Just known, _) -> pure (Just known)
    (_, Nothing) -> pure Nothing
    (_, Just (Pair cell cells))
      | later && envLazy (feEnv fe) -> do
          let name = feName fe <> "_r" <> tshow (gsNAux s)
              known = Pair name cell
          put
            s
              { gsCells = cells,
                gsNAux = gsNAux s + 1,
                gsDeferred = gsDeferred s |> Deferred name (feCix fe) loaded (feFrameSize fe) base body cell,
                gsAuxMemo = Map.insert key known (gsAuxMemo s)
              }
          pure (Just known)
    (_, Just (Pair cell cells)) -> do
      let name = feName fe <> "_r" <> tshow (gsNAux s)
          known = Pair name cell
          -- (an environment switch in force at the exit this re-enters after has become a Local frame in K)
          fe' = fe {feName = name, feArity = loaded, feCell = cell, feHead = Nothing, feBase = base, feEnclosing = D.empty, feWorker = False, feFast = Nothing, feSym = name, feInternal = False, feLocal = Nothing}
          gs0 = s {gsFresh = 0, gsBlocks = D.empty, gsCur = Pair "head" D.empty, gsMaxK = loaded, gsKinds = IM.empty, gsFailed = Nothing, gsCells = cells, gsNAux = gsNAux s + 1, gsCaptured = Nothing, gsMaxPool = -1, gsWorker = False, gsShared = Map.empty, gsDescs = Map.empty, gsDescText = D.empty}
          !(text, gs) = runState (genFunctionText fe' body) gs0
      case gsFailed gs of
        Just why -> put s {gsNotes = gsNotes gs |> (name <> ": " <> why)} >> pure Nothing
        Nothing -> do
          put
            s
              { gsNotes = gsNotes gs,
                gsExits = gsExits gs,
                gsNExits = gsNExits gs,
                gsFrames = gsFrames gs,
                gsNFrames = gsNFrames gs,
                gsAuxText = gsAuxText gs |> text,
                gsAuxCells = gsAuxCells gs |> known,
                gsNAux = gsNAux gs,
                gsCells = gsCells gs,
                gsDeferred = gsDeferred gs,
                gsAuxMemo = Map.insert key known (gsAuxMemo gs)
              }
          pure (Just known)

-- | Whether a @Let@ binding is generated inline: its first node must be
-- one the generator has code for. A binding that would exit straight away
-- is better left to the interpreter along with its @Let@, which saves the
-- binding's frame record. (Whether a whole function is worth compiling is
-- decided before this, in JIT.Estimate.)
startsSupported :: MSection -> Bool
startsSupported = \case
  Ins i rest -> instrNativeIn i rest || callOutWorthwhile i rest
  Match {} -> True
  DMatch {} -> True
  NMatch {} -> True
  RMatch {} -> requestsOn
  Call {} -> True
  App _ (MCode.Env _ _) _ -> True
  App _ (Stk _) _ -> True
  App _ (Dyn _) _ -> requestsOn
  Yield {} -> True
  Let b _ _ _ _ -> startsSupported b
  _ -> False

-- | Whether an instruction the generator can't compile can be a call-out
-- with native code after it.
callOutWorthwhile :: GInstr (RComb Val) -> MSection -> Bool
callOutWorthwhile i rest = case (pushCount i, rest) of
  (Just _, Yield (VArgV _)) -> False
  (Just _, _) -> True
  _ -> False

-- | The entry block: allocas, addresses from @Ctx@, argument loads, the
-- stack check, then a branch to the loop head. @maxK@ is the highest
-- frame offset the function touches, which is at least the frame size and
-- covers the arguments of every call it makes.
entryBlock :: Env -> Bool -> Bool -> Int -> Int -> Int -> Int -> Deque Text
entryBlock env worker checks arity base maxK maxPool =
  fmap ("  " <>) $
    -- %fp is the combinator's frame pointer, %fpb the interpreter's (the
    -- one passed in); they differ by the frame base
    lines
      [ "%fp = sub i64 %fp.in, " <> tshow base,
        "%fpb = add i64 %fp, " <> tshow base
      ]
      -- A worker isn't passed the stack pointer: its arguments, were they
      -- on the stack, would end there. Nor is it passed @ap@: a worker
      -- never has pending arguments (its wrapper sees to that, and native
      -- callers don't pass any), so @ap@ is the frame pointer.
      <> (if worker then lines ["%sp = add i64 %fp.in, " <> tshow arity, "%ap = add i64 %fp.in, 0"] else D.empty)
      <> each [1 .. maxK] (\k -> lines ["%u" <> tshow k <> " = alloca i64"])
      <> each [1 .. maxK] (\k -> lines ["%b" <> tshow k <> " = alloca ptr"])
      <> each [0 .. maxK] (\k -> lines ["%fpk" <> tshow k <> " = add i64 %fp, " <> tshow k])
      <> lines
        [ (if worker then ctxAddr else ctxLoad "ptr") "%ustk" oUstk,
          (if worker then ctxAddr else ctxLoad "ptr") "%bstk" oBstk,
          ctxLoad "ptr" "%pool" oPool,
          ctxLoad "ptr" "%hplim.p" oHplim,
          ctxLoad "i64" "%stack.size" oStackSize
        ]
      <> poolLoad "%tag.char" poolIndexCharTag
      <> poolLoad "%tag.float" poolIndexFloatTag
      <> poolLoad "%tag.int" poolIndexIntTag
      <> poolLoad "%tag.nat" poolIndexNatTag
      <> each
        [1 .. arity]
        ( \k' ->
            let k = tshow k'
             in if worker
                  then
                    lines
                      [ "store i64 %a.u" <> k <> ", ptr %u" <> k,
                        "store ptr %a.b" <> k <> ", ptr %b" <> k
                      ]
                  else
                    lines
                      [ "%arg.u" <> k <> ".a = getelementptr i64, ptr %ustk, i64 %fpk" <> k,
                        "%arg.u" <> k <> " = load i64, ptr %arg.u" <> k <> ".a",
                        "store i64 %arg.u" <> k <> ", ptr %u" <> k,
                        "%arg.b" <> k <> ".a = getelementptr ptr, ptr %bstk, i64 %fpk" <> k,
                        "%arg.b" <> k <> " = load ptr, ptr %arg.b" <> k <> ".a",
                        "store ptr %arg.b" <> k <> ", ptr %b" <> k
                      ]
        )
      -- the high-water mark of slots this function may write, for marking
      -- bstk on return. A worker writes the stack only where it exits or
      -- calls through the stack, and raises the mark there.
      <> ( if worker
             then D.empty
             else
               lines
                 [ "%maxsp.a = getelementptr i8, ptr %ctx, i64 " <> tshow (oMaxSp (envCtx env)),
                   "%maxsp.old = load i64, ptr %maxsp.a",
                   "%maxsp.gt = icmp sgt i64 %fpk" <> tshow maxK <> ", %maxsp.old",
                   "%maxsp.new = select i1 %maxsp.gt, i64 %fpk" <> tshow maxK <> ", i64 %maxsp.old",
                   "store i64 %maxsp.new, ptr %maxsp.a"
                 ]
         )
      -- A constant past the pool's first array may not be in the array this
      -- run was entered with, if this function was installed after the run
      -- began (see Pool). Then exit, to be entered again with the current one.
      <> ( if maxPool < poolStableSize || not checks
             then D.empty
             else
               lines
                 [ "%pool.n.a = getelementptr i64, ptr %pool, i64 " <> tshow (rPtrsCount (envRts env) - rPtrsHeader (envRts env)),
                   "%pool.n = load i64, ptr %pool.n.a",
                   "%pool.ok = icmp ugt i64 %pool.n, " <> tshow maxPool,
                   "br i1 %pool.ok, label %entry.room, label %stale" <> likely,
                   "entry.room:"
                 ]
         )
      -- the interpreter's check: sp + size + 1 < stack size, else grow
      <> ( if checks
             then
               lines
                 [ "%need = add i64 %sp, " <> tshow (maxK - arity + 1),
                   "%room = icmp slt i64 %need, %stack.size",
                   "br i1 %room, label %head, label %grow" <> likely
                 ]
             else lines ["br label %head"]
         )
  where
    lines = D.fromList
    each ks f = foldMap f (ks :: [Int])
    ctxAddr name f = name <> ".a = getelementptr i8, ptr %ctx, i64 " <> tshow (f (envCtx env))
    ctxLoad ty name f =
      name <> ".a = getelementptr i8, ptr %ctx, i64 " <> tshow (f (envCtx env)) <> "\n  " <> name <> " = load " <> ty <> ", ptr " <> name <> ".a"
    poolLoad name ix =
      lines
        [ name <> ".a = getelementptr ptr, ptr %pool, i64 " <> tshow ix,
          name <> " = load ptr, ptr " <> name <> ".a"
        ]

-- | The loop head: the high-water mark, the poll, then the body.
genHead :: FnEnv -> MSection -> Gen ()
genHead fe body = do
  let env = feEnv fe
      d = feArity fe
  -- the entry block's stack check branches to %grow when the frame doesn't
  -- fit; the size asked for is fixed up in genFunction once it is known
  _ <- exitBlockNamed "grow" fe d (GrowStack (feFrameSize fe) (feCell fe))
  -- likewise for the entry block's check of the constant pool
  _ <- exitBlockNamed "stale" fe d (Named "stale constant pool" (Reenter (feCell fe)))
  -- poll
  -- The load is volatile: another thread sets HpLim, and without volatile
  -- LLVM would hoist the load out of the loop and the poll would never fire.
  hp <- fresh "hplim"
  emit (hp <> " = load volatile ptr, ptr %hplim.p")
  stopHp <- fresh "stop"
  emit (stopHp <> " = icmp eq ptr " <> hp <> ", null")
  -- The allocation budget: exhausted once the bump pointer has passed its
  -- end. Only code that allocates needs to look (a callee that allocates
  -- looks itself).
  stopAlloc <-
    if allocates body
      then do
        hpA <- ctxField env oHp
        hp <- fresh "hp"
        emit (hp <> " = load ptr, ptr " <> hpA)
        endA <- ctxField env oBudgetEnd
        end <- fresh "budget.end"
        emit (end <> " = load ptr, ptr " <> endA)
        over <- fresh "over"
        emit (over <> " = icmp ugt ptr " <> hp <> ", " <> end)
        stop <- fresh "stop"
        emit (stop <> " = or i1 " <> stopHp <> ", " <> over)
        pure stop
      else pure stopHp
  -- A worker that makes calls checks the C stack here, once, instead of
  -- before each call. If the budget for native calls is used up, it goes
  -- back to the trampoline (its callers unwinding as for any exit) and is
  -- entered again at the bottom of the C stack.
  stop <-
    if feWorker fe && makesCalls body
      then do
        csp <- fresh "csp"
        emit (csp <> " = call ptr @llvm.stacksave.p0()")
        cspi <- fresh "csp"
        emit (cspi <> " = ptrtoint ptr " <> csp <> " to i64")
        lim <- ctxField env oCStackLimit
        limv <- fresh "lim"
        emit (limv <> " = load i64, ptr " <> lim)
        deep <- fresh "deep"
        emit (deep <> " = icmp ult i64 " <> cspi <> ", " <> limv)
        stop <- fresh "stop"
        emit (stop <> " = or i1 " <> stopAlloc <> ", " <> deep)
        pure stop
      else pure stopAlloc
  reenter <- exitBlock fe d (Reenter (feCell fe))
  bodyLabel <- freshLabel "body"
  when (envStressPoll env) $ do
    left <- ctxField env oStressPollLeft
    n <- fresh "left"
    emit (n <> " = load i64, ptr " <> left)
    n' <- fresh "left"
    emit (n' <> " = sub i64 " <> n <> ", 1")
    fire <- fresh "fire"
    emit (fire <> " = icmp sle i64 " <> n' <> ", 0")
    every <- ctxField env oStressPoll
    ev <- fresh "every"
    emit (ev <> " = load i64, ptr " <> every)
    reset <- fresh "reset"
    emit (reset <> " = select i1 " <> fire <> ", i64 " <> ev <> ", i64 " <> n')
    emit ("store i64 " <> reset <> ", ptr " <> left)
    stop' <- fresh "stop"
    emit (stop' <> " = or i1 " <> stop <> ", " <> fire)
    emit ("br i1 " <> stop' <> ", label %" <> reenter <> ", label %" <> bodyLabel <> unlikely)
  unless (envStressPoll env) $
    emit ("br i1 " <> stop <> ", label %" <> reenter <> ", label %" <> bodyLabel <> unlikely)
  startBlock bodyLabel
  genSection fe d body

-- | Generates a section at frame depth @d@. Every path ends in a terminator.
genSection :: FnEnv -> Int -> MSection -> Gen ()
genSection fe d sect = case feFast fe of
  Nothing -> genSectionFull fe d sect
  -- the fast entry: what can run unchecked runs here, and anything else
  -- is the full worker's, called with the arguments this was called with
  Just full -> do
    free <- checkFree d sect
    if free then genSectionFull fe d sect else tailToFull fe full

genSectionFull :: FnEnv -> Int -> MSection -> Gen ()
genSectionFull fe d sect = case sect of
  -- an affine handler's switch to its own environment, in the form the
  -- rest of the case can observe (the other form is a no-op, in 'genInstr')
  Ins (InLocal i) rest
    | on "requests", localObservingOn, not (localUnobserved rest) -> genInLocal fe d i rest sect
  Ins i rest -> genInstr fe d i sect (\d' -> genSection fe d' rest)
  Yield args -> genYield fe d args sect
  -- a tail call would leave an environment switch in force (see 'localEnds')
  Call {} | localEnds fe -> exitResume fe d sect
  App {} | localEnds fe -> exitResume fe d sect
  Call _ cix comb args -> genCall fe d cix comb args sect
  App _ r args | on "app" -> genApp fe d r args sect
  Match i br -> do
    x <- loadU (d - i)
    genBranch fe d x br sect
  DMatch mr i br -> genDMatch fe d i mr br sect
  NMatch _ i br -> do
    x <- loadU (d - i)
    genBranch fe d x br sect
  Let binding bcix f body cell -> genLet fe d binding bcix f body cell sect
  RMatch i pu br | on "requests" -> genRMatch fe d i pu br sect
  _ -> exitResume fe d sect
  where
    on = enabled fe

-- | Whether a feature is on (see Config's @disabled@).
enabled :: FnEnv -> Text -> Bool
enabled fe name = name `notElem` envDisabled (feEnv fe)

-- | A @Let@. A binding that is a call to a known function becomes a native
-- call. Other bindings are generated inline: their exits write a frame
-- record for this @Let@, and their @Yield@ delivers the results to the
-- body. If neither works, the interpreter takes the @Let@, and pushes the
-- frame itself.
genLet :: FnEnv -> Int -> MSection -> CombIx -> Int -> MSection -> Ptr NativeCell -> MSection -> Gen ()
genLet fe d binding bcix@(CIx _ _ w) f body cell sect = case EC.lookup w (envCombs env) of
  Just (Comb (LamI bodyArity _ _ _))
    | m <- bodyArity - d,
      m >= 0 -> case binding of
        Call _ ccix comb ZArgs
          | m == 1,
            Just (Pair u ix) <- cachedConstant fe ccix comb -> do
              pushCached d u ix
              genSection fe (d + 1) body
        App _ (MCode.Env ccix comb) ZArgs
          | m == 1,
            Just (Pair u ix) <- cachedConstant fe ccix comb -> do
              pushCached d u ix
              genSection fe (d + 1) body
        Call _ _ comb args
          | Comb (LamI arity _ _ ccell) <- unRComb comb,
            let srcs = argSources fe d args,
            D.size srcs == arity -> do
              bcell <- bodyCell
              ix <- addFrame env (Frame bcix f body bcell)
              genNonTailCall fe d d ccell srcs m sect (Just (Pair ix d)) $
                genSection fe (d + m) body
        App _ r@(MCode.Env ccix comb) args
          | enabled fe "app",
            Comb (LamI arity _ _ ccell) <- unRComb comb,
            let srcs = argSources fe d args,
            D.size srcs == arity -> do
              bcell <- bodyCell
              ix <- addFrame env (Frame bcix f body bcell)
              genNonTailCall fe d d ccell srcs m sect (Just (Pair ix d)) $
                genSection fe (d + m) body
          -- Fewer arguments than the function takes: the value of the binding
          -- is a partial application, built here (see "Partial applications"
          -- in jit_rt.c) from the function's closure in the pool.
          | enabled fe "name",
            Nothing <- feFast fe,
            m == 1,
            Comb info@(LamI arity _ _ _) <- unRComb comb,
            let srcs = argSources fe d args,
            not (D.null srcs),
            D.size srcs < arity,
            D.size srcs <= 4,
            Just pix <- Map.lookup (KeyComb ccix info) (envPool env) -> do
              slow <- exitBlock fe d (Resume (feCix fe) sect)
              vals <- loadSources srcs
              fn <- poolValue pix
              listHelper d slow (nameCall fn vals)
              genSection fe (d + 1) body
          | enabled fe "app",
            ZArgs <- args,
            m == 1,
            Just ix <- combConstant fe r -> do
              poolConstant fe d ix
              genSection fe (d + 1) body
          | otherwise -> exitResume fe d sect
        App _ (Stk i) args | enabled fe "app" -> do
          bcell <- bodyCell
          ix <- addFrame env (Frame bcix f body bcell)
          genClosureCall fe d d (d - i) (argSources fe d args) m sect (Just (Pair ix d)) $
            genSection fe (d + m) body
        _ | startsSupported binding -> do
              bcell <- bodyCell
              ix <- addFrame env (Frame bcix f body bcell)
              bodyL <- freshLabel "body"
              let fe' = fe {feEnclosing = Enclosing ix d bodyL m (liveAt env (currentBase fe) (d + m) body) <| feEnclosing fe}
              ok <- attempt (withKinds (genSection fe' d binding))
              if ok
                then do
                  startBlock bodyL
                  genSection fe (d + m) body
                else exitResume fe d sect
        _ -> exitResume fe d sect
  _ -> exitResume fe d sect
  where
    env = feEnv fe
    -- A Let inside a binding carries no cell (see attachNativeCells): its
    -- body combinator's arity counts the enclosing function's slots, so
    -- the interpreter can't enter it. An auxiliary function generated
    -- here, with the current frame base, can be.
    bodyCell
      | cell /= noNativeCell = pure cell
      | otherwise = case EC.lookup w (envCombs env) of
          Just (Comb (LamI bodyArity _ _ _)) ->
            maybe noNativeCell (\(Pair _ c) -> c) <$> genAuxFunction True fe bodyArity (currentBase fe) body
          _ -> pure noNativeCell

-- | Loads @m@ results left on the stack above offset @base@ into their slots.
loadResults :: Int -> Int -> Gen ()
loadResults base m =
  forM_ [1 .. m] $ \j -> do
    ua <- stackAddrU (base + j)
    u <- fresh "u"
    emit (u <> " = load i64, ptr " <> ua)
    storeU (base + j) u
    ba <- stackAddrB (base + j)
    b <- fresh "b"
    emit (b <> " = load ptr, ptr " <> ba)
    storeB (base + j) b

-- | A native call that returns here. The callee's frame starts at offset
-- @base@ (the current depth for a @Let@ binding, or an enclosing binding's
-- base for a tail call inside one); its arguments come from the slots
-- @srcs@ at the current depth @d@. On @OK@ the continuation runs with the
-- results on the stack above @base@. Otherwise the slots up to @base@ are
-- written back, the frame record for this @Let@ (if given: index and frame
-- size) and those of the enclosing bindings are written, and the status is
-- passed on. If the callee can't be called, the interpreter resumes at
-- @sect@.
-- | The slots to write back when a call made at @base@ unwinds: what the
-- body that gets the callee's @m@ results reads, and the enclosing
-- bindings' bodies after it (see 'liveAt').
bodyLive :: FnEnv -> Int -> Int -> MSection -> Maybe IS.IntSet
bodyLive fe base m body = liveAt (feEnv fe) (currentBase fe) (base + m) body <+> enclosingLive fe

genNonTailCall :: FnEnv -> Int -> Int -> Ptr NativeCell -> Deque Int -> Int -> MSection -> Maybe (Pair Int Int) -> Gen () -> Gen ()
genNonTailCall fe d base ccell srcs m sect ownFrame continue
  | m == 1,
    Just (Pair wname arity) <- Map.lookup ccell (envWorkers (feEnv fe)),
    arity == D.size srcs =
      genWorkerCall fe d base wname srcs sect ownFrame continue
  | otherwise =
      genNonTailCallWith fe d base (loadCallee (feEnv fe) ccell) (fpPlus (base + D.size srcs)) srcs m sect ownFrame continue

-- | 'genNonTailCall' with the callee given by a generator (the code
-- pointer and an i1 saying the call can't be made), and the callee's
-- stack pointer given by another, run after the arguments are in place
-- (a closure call copies its captured arguments there). The callee's @m@
-- results are loaded from the stack into their slots before the
-- continuation runs.
genNonTailCallWith :: FnEnv -> Int -> Int -> Gen (Pair Text Text) -> Gen Text -> Deque Int -> Int -> MSection -> Maybe (Pair Int Int) -> Gen () -> Gen ()
genNonTailCallWith fe d base getCallee getTop srcs m sect ownFrame continue = do
  let n = D.size srcs
  Pair fnp skip <- getCallee
  slow <- exitBlock fe d (Resume (feCix fe) sect)
  guardL <- freshLabel "guard"
  emit ("br i1 " <> skip <> ", label %" <> slow <> ", label %" <> guardL <> unlikely)
  startBlock guardL
  cStackGuard fe slow
  -- arguments go above the callee's base, as moveArgs would put them
  vals <- loadSources srcs
  iforM_ vals $ \j (Pair u b) -> do
    ua <- stackAddrU (base + n - j)
    emit ("store i64 " <> u <> ", ptr " <> ua)
    ba <- stackAddrB (base + n - j)
    emit ("store ptr " <> b <> ", ptr " <> ba)
  bp <- fpPlus base
  top <- getTop
  r <- fresh "r"
  emit (r <> " = call i64 " <> fnp <> "(ptr %ctx, i64 " <> bp <> ", i64 " <> bp <> ", i64 " <> top <> ")")
  ok <- fresh "ok"
  emit (ok <> " = icmp eq i64 " <> r <> ", 0")
  -- the callee is exiting: record the frames the interpreter would have
  -- pushed, and pass the status along
  unwind <- sideBlock "unwind" (unwindWith fe base (bodyLive fe base m sect) ownFrame r)
  contL <- freshLabel "cont"
  emit ("br i1 " <> ok <> ", label %" <> contL <> ", label %" <> unwind <> likely)
  startBlock contL
  loadResults base m
  continue

-- | The C stack guard: branches to @slow@ instead of going on to a call
-- when the budget for native non-tail calls is used up. A worker has
-- checked at entry (see 'genHead') and doesn't check again here. Every
-- cycle of calls still passes a check: an edge without one goes from a
-- worker to a function with the uniform signature, whose own calls check.
cStackGuard :: FnEnv -> Text -> Gen ()
cStackGuard fe _ | feWorker fe = pure ()
cStackGuard fe slow = do
  let env = feEnv fe
  csp <- fresh "csp"
  emit (csp <> " = call ptr @llvm.stacksave.p0()")
  cspi <- fresh "csp"
  emit (cspi <> " = ptrtoint ptr " <> csp <> " to i64")
  lim <- ctxField env oCStackLimit
  limv <- fresh "lim"
  emit (limv <> " = load i64, ptr " <> lim)
  deep <- fresh "deep"
  emit (deep <> " = icmp ult i64 " <> cspi <> ", " <> limv)
  callL <- freshLabel "call"
  emit ("br i1 " <> deep <> ", label %" <> slow <> ", label %" <> callL <> unlikely)
  startBlock callL

-- | What a caller does when its callee comes back with the status in
-- register @r@, which isn't OK: write the slots up to @base@ back to the
-- stack, write the frame record for this @Let@ (if given) and those of
-- the enclosing bindings, and return the status.
unwindWith :: FnEnv -> Int -> Maybe IS.IntSet -> Maybe (Pair Int Int) -> Text -> Gen ()
unwindWith fe base live ownFrame r = joinShared fe base live False ownFrame r

-- | A call to a function of this module that has a worker, expecting one
-- result: the arguments go in registers and the result comes back in
-- registers, so the Unison stack isn't touched unless the callee exits.
-- The callee's frame still has its place on the stack, above @base@, and
-- this function's stack check has made room for its arguments there
-- (the callee writes them if it exits before anything else).
--
-- The worker may come back asking for a tail call it couldn't make
-- itself ('statusTailCall'): its callee's arguments are on the stack
-- above @base@, and the call is made here, as a plain call through the
-- stack.
genWorkerCall :: FnEnv -> Int -> Int -> Text -> Deque Int -> MSection -> Maybe (Pair Int Int) -> Gen () -> Gen ()
genWorkerCall fe d base wname srcs sect ownFrame continue = do
  let env = feEnv fe
      n = D.size srcs
  slow <- exitBlock fe d (Resume (feCix fe) sect)
  -- the callee stress mode still takes the slow path now and then
  when (envStressCallee env) $ do
    fire <- stressFire env oStressCalleeLeft oStressCallee
    goL <- freshLabel "go"
    emit ("br i1 " <> fire <> ", label %" <> slow <> ", label %" <> goL <> unlikely)
    startBlock goL
  cStackGuard fe slow
  vals <- loadSources srcs
  useK (base + max n 1)
  bp <- fpPlus base
  r <- fresh "r"
  emit (r <> " = call tailcc " <> workerRet <> " @" <> wname <> "(ptr %ctx, i64 " <> bp <> argList (D.reverse vals) <> ")")
  st <- fresh "st"
  emit (st <> " = extractvalue " <> workerRet <> " " <> r <> ", 0")
  ru <- fresh "r.u"
  emit (ru <> " = extractvalue " <> workerRet <> " " <> r <> ", 1")
  rb <- fresh "r.b"
  emit (rb <> " = extractvalue " <> workerRet <> " " <> r <> ", 2")
  ok <- fresh "ok"
  emit (ok <> " = icmp eq i64 " <> st <> ", 0")
  callL <- curLabel
  contL <- freshLabel "cont"
  notOkL <- freshLabel "notok"
  tailL <- freshLabel "tailcall"
  tailOkL <- freshLabel "tailcall.ok"
  unwindL <- freshLabel "unwind"
  emit ("br i1 " <> ok <> ", label %" <> contL <> ", label %" <> notOkL <> likely)
  -- not OK: a tail call to make, or an exit
  r2 <- fresh "r"
  sideBlockNamed notOkL $ do
    tc <- fresh "tc"
    emit (tc <> " = icmp eq i64 " <> st <> ", " <> tshow statusTailCall)
    emit ("br i1 " <> tc <> ", label %" <> tailL <> ", label %" <> unwindL <> unlikely)
  u2 <- fresh "u"
  b2 <- fresh "b"
  sideBlockNamed tailL $ do
    fn <- fresh "fn"
    emit (fn <> " = inttoptr i64 " <> ru <> " to ptr")
    top <- fresh "top"
    emit (top <> " = ptrtoint ptr " <> rb <> " to i64")
    emit (r2 <> " = call i64 " <> fn <> "(ptr %ctx, i64 " <> bp <> ", i64 " <> bp <> ", i64 " <> top <> ")")
    ok2 <- fresh "ok"
    emit (ok2 <> " = icmp eq i64 " <> r2 <> ", 0")
    emit ("br i1 " <> ok2 <> ", label %" <> tailOkL <> ", label %" <> unwindL <> likely)
  sideBlockNamed tailOkL $ do
    ua <- stackAddrU (base + 1)
    emit (u2 <> " = load i64, ptr " <> ua)
    ba <- stackAddrB (base + 1)
    emit (b2 <> " = load ptr, ptr " <> ba)
    emit ("br label %" <> contL)
  sideBlockNamed unwindL $ do
    status <- fresh "status"
    emit (status <> " = phi i64 [ " <> st <> ", %" <> notOkL <> " ], [ " <> r2 <> ", %" <> tailL <> " ]")
    unwindWith fe base (bodyLive fe base 1 sect) ownFrame status
  startBlock contL
  u <- fresh "u"
  emit (u <> " = phi i64 [ " <> ru <> ", %" <> callL <> " ], [ " <> u2 <> ", %" <> tailOkL <> " ]")
  b <- fresh "b"
  emit (b <> " = phi ptr [ " <> rb <> ", %" <> callL <> " ], [ " <> b2 <> ", %" <> tailOkL <> " ]")
  storeU (base + 1) u
  storeB (base + 1) b
  continue

-- | Runs a generator that may fail. On failure the state is rolled back
-- (except the name counter) and False is returned; nothing was emitted.
attempt :: Gen () -> Gen Bool
attempt g = do
  before <- get
  g
  after <- get
  case gsFailed after of
    Nothing -> pure True
    Just why -> do
      put before {gsFresh = gsFresh after, gsNotes = gsNotes after |> why, gsNotWorker = gsNotWorker after}
      pure False

-- | Branch on an i64 value. @arm@ generates one arm at depth @d@; if it
-- can't (a data match arm that needs fields the native path doesn't
-- push), that arm exits at the whole section instead.
genBranch :: FnEnv -> Int -> Text -> GBranch (RComb Val) -> MSection -> Gen ()
genBranch fe d x br sect = genBranchWith fe d x br sect (\_ -> genSection fe d)

-- | 'genBranch' with the code for an arm supplied: it gets the case's
-- constructor tag (or word) and the arm's section.
genBranchWith :: FnEnv -> Int -> Text -> GBranch (RComb Val) -> MSection -> (Word64 -> MSection -> Gen ()) -> Gen ()
genBranchWith fe d x br sect armCode = case br of
  Test1 u y n -> do
    c <- fresh "is"
    emit (c <> " = icmp eq i64 " <> x <> ", " <> signed u)
    ly <- arm "yes" (Just u) y
    ln <- arm "no" Nothing n
    emit ("br i1 " <> c <> ", label %" <> ly <> ", label %" <> ln)
  Test2 u cu v cv e -> do
    lu <- arm "case" (Just u) cu
    lv <- arm "case" (Just v) cv
    le <- arm "default" Nothing e
    emit ("switch i64 " <> x <> ", label %" <> le <> " [ i64 " <> signed u <> ", label %" <> lu <> "  i64 " <> signed v <> ", label %" <> lv <> " ]")
  TestW df cs -> do
    ldf <- arm "default" Nothing df
    arms <- foldM (\acc (w, s) -> (\l -> acc |> ("i64 " <> signed w <> ", label %" <> l)) <$> arm "case" (Just w) s) D.empty (EC.mapToList cs)
    emit ("switch i64 " <> x <> ", label %" <> ldf <> " [ " <> intercalateT " " arms <> " ]")
  _ -> exitResume fe d sect
  where
    -- the default arm has no known constructor, so it runs at depth d
    -- with nothing pushed (the interpreter's dataBranch does the same)
    arm base mu s = do
      label <- freshLabel base
      let code = case mu of
            Just u -> armCode u s
            Nothing -> genSection fe d s
      ok <- attempt (withKinds (sideBlockNamed label code))
      unless ok $ sideBlockNamed label (exitResume fe d sect)
      pure label

signed :: Word64 -> Text
signed w = tshow (fromIntegral w :: Int64)

-- | Branch on the constructor of a data value. Only enumerations (no
-- fields) are handled natively so far; anything else exits.
genDMatch :: FnEnv -> Int -> Int -> Maybe Reference -> GBranch (RComb Val) -> MSection -> Gen ()
genDMatch fe d i mr br sect =
  slotKind (d - i) >>= \case
    Just c -> genBoolBranch fe d c br sect
    Nothing -> genDMatchClosure fe d i mr br sect

-- | A match on a boolean that is still an i1: one branch. False is
-- constructor 0, true is 1.
genBoolBranch :: FnEnv -> Int -> Text -> GBranch (RComb Val) -> MSection -> Gen ()
genBoolBranch fe d c br sect = case br of
  Test1 u y n -> two (if u == 1 then Pair y n else Pair n y)
  Test2 u cu v cv e -> two (Pair (pick 1 u cu v cv e) (pick 0 u cu v cv e))
  TestW df cs -> two (Pair (maybe df id (EC.lookup 1 cs)) (maybe df id (EC.lookup 0 cs)))
  _ -> exitResume fe d sect
  where
    pick t u cu v cv e
      | u == t = cu
      | v == t = cv
      | otherwise = e
    two (Pair t f) = do
      lt <- arm "true" t
      lf <- arm "false" f
      emit ("br i1 " <> c <> ", label %" <> lt <> ", label %" <> lf)
    arm base s = do
      label <- freshLabel base
      ok <- attempt (withKinds (sideBlockNamed label (genSection fe d s)))
      unless ok $ sideBlockNamed label (exitResume fe d sect)
      pure label

-- | A match on a data closure. The pointer tag says which of the four
-- constructor closures it is; each keeps the constructor tag at a
-- different offset, so four small blocks load it and meet at a switch.
-- Each arm then pushes the fields as @dataBranch@ does: the first field
-- on top. How many fields a constructor has comes from the data type's
-- declaration ('envTypes'); one with three or more is a @DataG@ holding
-- two arrays.
genDMatchClosure :: FnEnv -> Int -> Int -> Maybe Reference -> GBranch (RComb Val) -> MSection -> Gen ()
genDMatchClosure fe d i mr br sect = do
  let env = feEnv fe
      ls = envLayouts env
      arities = mr >>= \r -> Map.lookup r (envTypes env)
      -- a type whose arities aren't known (Boolean and the other builtin
      -- references) can still be matched when the value is an enumeration,
      -- which the pointer tag says; the other kinds exit
      kinds = case arities of
        Just _ -> D.fromList [lEnum ls, lData1 ls, lData2 ls, lDataG ls]
        Nothing -> D.singleton (lEnum ls)
  p <- loadB (d - i)
  raw <- fresh "raw"
  emit (raw <> " = ptrtoint ptr " <> p <> " to i64")
  tagBits <- fresh "ptrtag"
  emit (tagBits <> " = and i64 " <> raw <> ", 7")
  base <- fresh "base"
  emit (base <> " = sub i64 " <> raw <> ", " <> tagBits)
  other <- exitBlock fe d (Resume (feCix fe) sect)
  dispatch <- freshLabel "dispatch"
  -- one block per closure kind, loading the packed tag from its offset
  loads <- forM kinds $ \layout -> do
    l <- freshLabel "kind"
    packed <- fresh "packed"
    sideBlockNamed l $ do
      addr <- fresh "tag.a"
      emit (addr <> " = add i64 " <> base <> ", " <> tshow (lFieldOffset layout (lPtrs layout)))
      ptr <- fresh "tag.p"
      emit (ptr <> " = inttoptr i64 " <> addr <> " to ptr")
      emit (packed <> " = load i64, ptr " <> ptr)
      emit ("br label %" <> dispatch)
    pure (Triple layout l packed)
  emit ("switch i64 " <> tagBits <> ", label %" <> other <> " [ " <> intercalateT " " (fmap (\(Triple layout l _) -> " i64 " <> tshow (lPtrTag layout) <> ", label %" <> l) loads) <> " ]")
  startBlock dispatch
  packed <- fresh "packed"
  emit (packed <> " = phi i64 " <> intercalateT ", " (fmap (\(Triple _ l v) -> "[ " <> v <> ", %" <> l <> " ]") loads))
  tag <- fresh "tag"
  emit (tag <> " = and i64 " <> packed <> ", 65535") -- maskTags
  -- an arm for constructor u knows its field count, so it knows the kind
  let arm u body = case arities of
        Nothing -> genSection fe d body
        Just as -> case D.lookup (fromIntegral u) as of
          Nothing -> exitResume fe d sect
          Just 0 -> genSection fe d body
          Just n -> do
            pushFields fe d base n other
            genSection fe (d + n) body
  genBranchWith fe d tag br sect arm

-- | A match on a request closure (an affine handler's @RMatch@). The
-- closure is a data constructor like any other; its packed tag is
-- @ability << 16 | constructor@, and zero for the pure request, @Data1
-- effectRef 0 v@, whose one field is the handled computation's value. The
-- branches are keyed by ability number, then constructor, and an arm
-- pushes its constructor's fields as a data match does; their counts come
-- from the ability's declaration ('envAbilities'). An ability or
-- constructor without an arm, or a closure of another kind, is the
-- interpreter's (it dies with "unhandled ability").
genRMatch :: FnEnv -> Int -> Int -> MSection -> EC.EnumMap Word64 (GBranch (RComb Val)) -> MSection -> Gen ()
genRMatch fe d i pu br sect = do
  let env = feEnv fe
      ls = envLayouts env
      kinds = D.fromList [lEnum ls, lData1 ls, lData2 ls, lDataG ls]
  p <- loadB (d - i)
  raw <- fresh "raw"
  emit (raw <> " = ptrtoint ptr " <> p <> " to i64")
  tagBits <- fresh "ptrtag"
  emit (tagBits <> " = and i64 " <> raw <> ", 7")
  base <- fresh "base"
  emit (base <> " = sub i64 " <> raw <> ", " <> tagBits)
  other <- exitBlock fe d (Resume (feCix fe) sect)
  dispatch <- freshLabel "dispatch"
  loads <- forM kinds $ \layout -> do
    l <- freshLabel "kind"
    packed <- fresh "packed"
    sideBlockNamed l $ do
      addr <- fresh "tag.a"
      emit (addr <> " = add i64 " <> base <> ", " <> tshow (lFieldOffset layout (lPtrs layout)))
      ptr <- fresh "tag.p"
      emit (ptr <> " = inttoptr i64 " <> addr <> " to ptr")
      emit (packed <> " = load i64, ptr " <> ptr)
      emit ("br label %" <> dispatch)
    pure (Triple layout l packed)
  emit ("switch i64 " <> tagBits <> ", label %" <> other <> " [ " <> intercalateT " " (fmap (\(Triple layout l _) -> " i64 " <> tshow (lPtrTag layout) <> ", label %" <> l) loads) <> " ]")
  startBlock dispatch
  packed <- fresh "packed"
  emit (packed <> " = phi i64 " <> intercalateT ", " (fmap (\(Triple _ l v) -> "[ " <> v <> ", %" <> l <> " ]") loads))
  let PackedTag pureTag = TT.pureEffectTag
  isPure <- fresh "pure"
  emit (isPure <> " = icmp eq i64 " <> packed <> ", " <> tshow pureTag)
  pureL <- freshLabel "pure"
  reqL <- freshLabel "request"
  emit ("br i1 " <> isPure <> ", label %" <> pureL <> ", label %" <> reqL)
  okPure <- attempt (withKinds (sideBlockNamed pureL (pushFields fe d base 1 other >> genSection fe (d + 1) pu)))
  unless okPure $ sideBlockNamed pureL (exitResume fe d sect)
  startBlock reqL
  ability <- fresh "ability"
  emit (ability <> " = lshr i64 " <> packed <> ", 16")
  ctor <- fresh "ctor"
  emit (ctor <> " = and i64 " <> packed <> ", 65535") -- maskTags
  arms <- forM (EC.mapToList br) $ \(e, gb) -> do
    l <- freshLabel "ability"
    let arities = EC.lookup e (envAbilities env)
        arm u body = case arities >>= D.lookup (fromIntegral u) of
          Nothing -> exitResume fe d sect
          Just 0 -> genSection fe d body
          Just n -> pushFields fe d base n other >> genSection fe (d + n) body
    sideBlockNamed l (genBranchWith fe d ctor gb sect arm)
    pure (Pair e l)
  emit ("switch i64 " <> ability <> ", label %" <> other <> " [ " <> intercalateT " " (D.fromList (map (\(Pair e l) -> "i64 " <> tshow e <> ", label %" <> l) arms)) <> " ]")

-- | Pushes the @n@ fields of the constructor closure at untagged address
-- @base@ onto slots d+1..d+n, first field on top. A @DataG@ whose
-- segment boxes aren't evaluated branches to @slow@.
pushFields :: FnEnv -> Int -> Text -> Int -> Text -> Gen ()
pushFields fe d base n slow = do
  let ls = envLayouts (feEnv fe)
      rts = envRts (feEnv fe)
      loadFrom from what ty off = do
        a <- fresh (what <> ".a")
        emit (a <> " = add i64 " <> from <> ", " <> tshow off)
        pp <- fresh (what <> ".p")
        emit (pp <> " = inttoptr i64 " <> a <> " to ptr")
        v <- fresh what
        emit (v <> " = load " <> ty <> ", ptr " <> pp)
        pure v
      loadAt = loadFrom base
      -- field j of Data1/Data2: pointer j+1 and non-pointer j+1
      field layout j = do
        b <- loadAt "fb" "ptr" (lFieldOffset layout (j + 1))
        u <- loadAt "fu" "i64" (lFieldOffset layout (lPtrs layout + j + 1))
        pure (Pair u b)
  case n of
    1 -> do
      Pair u b <- field (lData1 ls) 0
      storeU (d + 1) u >> storeB (d + 1) b
    2 -> do
      Pair u0 b0 <- field (lData2 ls) 0
      Pair u1 b1 <- field (lData2 ls) 1
      storeU (d + 2) u0 >> storeB (d + 2) b0
      storeU (d + 1) u1 >> storeB (d + 1) b1
    _ -> do
      -- DataG: the Seg's two fields are lifted boxes (ByteArray, Array)
      -- around the arrays, since a tuple's fields can't be unpacked. Each
      -- box is an evaluated single-constructor object: untag it and read
      -- its one field, the array. The arrays hold the fields in reverse,
      -- so element j goes to slot d+1+j.
      let layout = lDataG ls
          unbox what off = do
            box <- loadAt what "ptr" (lFieldOffset layout off)
            boxI <- fresh (what <> ".box")
            emit (boxI <> " = ptrtoint ptr " <> box <> " to i64")
            boxTag <- fresh (what <> ".tag")
            emit (boxTag <> " = and i64 " <> boxI <> ", 7")
            boxBad <- fresh (what <> ".lazy")
            emit (boxBad <> " = icmp ne i64 " <> boxTag <> ", 1")
            branchIf what boxBad slow
            untagged <- fresh (what <> ".un")
            emit (untagged <> " = and i64 " <> boxI <> ", -8")
            loadFrom untagged what "i64" (lHeaderBytes ls)
      usegI <- unbox "useg" 1
      bsegI <- unbox "bseg" 2
      forM_ [0 .. n - 1] $ \j -> do
        u <- loadFrom usegI "fu" "i64" (8 * (rBytesHeader rts + j))
        b <- loadFrom bsegI "fb" "ptr" (8 * (rPtrsHeader rts + j))
        storeU (d + 1 + j) u >> storeB (d + 1 + j) b
-- | The slots that an argument list selects, top first, as frame offsets.
-- @VArgV@ means "everything in the frame above index i", and the frame
-- the interpreter sees is the innermost inline binding's, if any.
argSources :: FnEnv -> Int -> Args -> Deque Int
argSources fe d = \case
  ZArgs -> D.empty
  VArg1 i -> D.singleton (d - i)
  VArg2 i j -> D.fromList [d - i, d - j]
  VArgR i l -> D.fromList [d - i - k | k <- [0 .. l - 1]]
  VArgN v -> D.fromList [d - i | i <- primArrayToList v]
  VArgV i -> D.fromList [d - k | k <- [0 .. (d - base) - i - 1]]
  where
    base = currentBase fe

-- | Loads the selected values into registers (a parallel move must read everything first).
loadSources :: Deque Int -> Gen (Deque (Pair Text Text))
loadSources = traverse (\k -> Pair <$> loadU k <*> loadB k)

-- | Return: move the results into place as @moveArgs@ then @frameArgs@ would,
-- and hand them to the continuation.
genYield :: FnEnv -> Int -> Args -> MSection -> Gen ()
genYield fe d args _sect
  | e :<| _ <- feEnclosing fe = do
      -- inside an inline binding: the results go to the body
      let srcs = argSources fe d args
          n = D.size srcs
      if n /= enResults e
        then failWith "binding yields the wrong number of values"
        else do
          when (localEnds fe) (endLocal fe)
          vals <- loadSources srcs
          iforM_ vals $ \j (Pair u b) -> do
            storeU (enBase e + n - j) u
            storeB (enBase e + n - j) b
          emit ("br label %" <> enBody e)
genYield fe d args sect = do
  let base = feBase fe
  -- pending arguments (fp /= ap) mean over-application; leave that to the interpreter
  pending <- fresh "pending"
  emit (pending <> " = icmp ne i64 %ap, %fpb")
  slow <- exitBlock fe d (Resume (feCix fe) sect)
  fast <- freshLabel "yield"
  emit ("br i1 " <> pending <> ", label %" <> slow <> ", label %" <> fast <> unlikely)
  startBlock fast
  when (localEnds fe) (endLocal fe)
  let srcs = argSources fe d args
      n = D.size srcs
  vals <- loadSources srcs
  -- a worker hands its one result back in registers
  if feWorker fe
    then case vals of
      Pair u b :<| Empty -> retWorker "0" u b
      _ -> modify' (\s -> s {gsFailed = Just "a worker yields other than one value", gsNotWorker = True})
    else genYieldStack fe base n vals

-- | The rest of a return through the stack: the results go above the
-- frame base, and the stack pointers into @Ctx@.
genYieldStack :: FnEnv -> Int -> Int -> Deque (Pair Text Text) -> Gen ()
genYieldStack fe base n vals = do
  let env = feEnv fe
  iforM_ vals $ \j (Pair u b) -> do
    ua <- stackAddrU (base + n - j)
    emit ("store i64 " <> u <> ", ptr " <> ua)
    ba <- stackAddrB (base + n - j)
    emit ("store ptr " <> b <> ", ptr " <> ba)
  ap <- ctxField env oAp
  emit ("store i64 %ap, ptr " <> ap)
  fp <- ctxField env oFp
  emit ("store i64 %ap, ptr " <> fp) -- frameArgs: fp = ap
  sp <- ctxField env oSp
  f <- fpPlus (base + n)
  emit ("store i64 " <> f <> ", ptr " <> sp)
  emit "ret i64 0"

-- | A tail call: to this function (a loop) or to another one through its cell.
genCall :: FnEnv -> Int -> CombIx -> RComb Val -> Args -> MSection -> Gen ()
genCall fe d cix comb args sect
  -- a top-level value, evaluated when it was loaded: a constant, returned
  | ZArgs <- args,
    Just (Pair u ix) <- cachedConstant fe cix comb = do
      pushCached d u ix
      genYield fe (d + 1) (VArg1 0) sect
  | e :<| _ <- feEnclosing fe = case unRComb comb of
      -- a tail call inside an inline binding is a call that returns to the body
      Comb (LamI arity _ _ ccell)
        | let srcs = argSources fe d args,
          D.size srcs == arity ->
            genNonTailCall fe d (enBase e) ccell srcs (enResults e) sect Nothing $
              emit ("br label %" <> enBody e)
      _ -> exitResume fe d sect
  | cix == feCix fe,
    Just headL <- feHead fe = do
      let srcs = argSources fe d args
          n = D.size srcs
      if n /= feArity fe
        then exitResume fe d sect
        else do
          vals <- loadSources srcs
          iforM_ vals $ \j (Pair u b) -> do
            storeU (n - j) u
            storeB (n - j) b
          emit ("br label %" <> headL)
  | otherwise = case unRComb comb of
      Comb (LamI arity _ _ cell) -> do
        let srcs = argSources fe d args
            n = D.size srcs
            base = feBase fe
        if n /= arity
          then exitResume fe d sect
          else case Map.lookup cell (envWorkers env) of
            -- worker to worker: a real tail call with the arguments in registers
            Just (Pair wname _) | feWorker fe -> do
              when (envStressCallee env) $ do
                fire <- stressFire env oStressCalleeLeft oStressCallee
                slow <- exitBlock fe d (Resume (feCix fe) sect)
                goL <- freshLabel "tail"
                emit ("br i1 " <> fire <> ", label %" <> slow <> ", label %" <> goL <> unlikely)
                startBlock goL
              vals <- loadSources srcs
              r <- fresh "r"
              emit (r <> " = musttail call tailcc " <> workerRet <> " @" <> wname <> "(ptr %ctx, i64 %fpb" <> argList (D.reverse vals) <> ")")
              emit ("ret " <> workerRet <> " " <> r)
            _ -> do
              Pair fnp isNull <- loadCallee env cell
              slow <- exitBlock fe d (Resume (feCix fe) sect)
              go <- freshLabel "tail"
              emit ("br i1 " <> isNull <> ", label %" <> slow <> ", label %" <> go <> unlikely)
              startBlock go
              vals <- loadSources srcs
              iforM_ vals $ \j (Pair u b) -> do
                ua <- stackAddrU (base + n - j)
                emit ("store i64 " <> u <> ", ptr " <> ua)
                ba <- stackAddrB (base + n - j)
                emit ("store ptr " <> b <> ", ptr " <> ba)
              f <- fpPlus (base + n)
              tailCallThroughStack fe fnp f
      _ -> exitResume fe d sect
  where
    env = feEnv fe

-- | A tail call to a function with the uniform signature, its arguments
-- already on the stack above the frame base, with the stack pointer in
-- @top@. A worker can't make it (the return types differ): it returns
-- 'statusTailCall' with the code pointer and stack pointer, and whoever
-- called it makes the call.
tailCallThroughStack :: FnEnv -> Text -> Text -> Gen ()
tailCallThroughStack fe fnp top
  | feWorker fe = do
      -- the callee's entry raises the high-water mark over its arguments
      fni <- fresh "fn"
      emit (fni <> " = ptrtoint ptr " <> fnp <> " to i64")
      tp <- fresh "top"
      emit (tp <> " = inttoptr i64 " <> top <> " to ptr")
      retWorker (tshow statusTailCall) fni tp
  | otherwise = do
      r <- fresh "r"
      emit (r <> " = musttail call i64 " <> fnp <> "(ptr %ctx, i64 %ap, i64 %fpb, i64 " <> top <> ")")
      emit ("ret i64 " <> r)

-- | A call to a function value (@App@). A known combinator used as a value
-- (@Env@) with the right number of arguments is a plain call. A value on
-- the stack (@Stk@) is called through its closure, see 'genClosureCall'.
-- Anything else (a dynamic-scope reference, a mismatched arity) is left
-- to the interpreter.
genApp :: FnEnv -> Int -> MRef -> Args -> MSection -> Gen ()
genApp fe d r args sect = case r of
  MCode.Env cix comb
    | CachedVal {} <- unRComb comb, ZArgs <- args -> genCall fe d cix comb args sect
    | Comb (LamI arity _ _ _) <- unRComb comb,
      D.size (argSources fe d args) == arity ->
        genCall fe d cix comb args sect
    -- the combinator as a value: a constant closure, returned
    | ZArgs <- args,
      Just ix <- combConstant fe r -> do
        poolConstant fe d ix
        genYield fe (d + 1) (VArg1 0) sect
  Stk i
    | e :<| _ <- feEnclosing fe ->
        genClosureCall fe d (enBase e) (d - i) srcs (enResults e) sect Nothing $
          emit ("br label %" <> enBody e)
    | otherwise -> genTailClosureCall fe d (closureCallee fe d (d - i) n) srcs sect
  -- An ability request: the handler is the closure the environment maps
  -- the ability's number to (see "Ability requests" in internals.md), and
  -- it is called like any other function value, with the request as its
  -- argument and this frame as its continuation. A number the environment
  -- doesn't map to an affine handler is the interpreter's.
  Dyn key
    | enabled fe "requests", e :<| _ <- feEnclosing fe ->
        genNonTailCallWith fe d (enBase e) (dynCallee fe key n) (copyCaptured fe (enBase e + n)) srcs (enResults e) sect Nothing $
          emit ("br label %" <> enBody e)
    | enabled fe "requests" -> genTailClosureCall fe d (dynCallee fe key n) srcs sect
  _ -> exitResume fe d sect
  where
    srcs = argSources fe d args
    n = D.size srcs

-- | A tail call to a function value whose callee is given by @getCallee@
-- (see 'closureCallee'): the arguments go above the frame base, the
-- captured ones above them, and the call goes through the stack.
genTailClosureCall :: FnEnv -> Int -> Gen (Pair Text Text) -> Deque Int -> MSection -> Gen ()
genTailClosureCall fe d getCallee srcs sect = do
  let base = feBase fe
      n = D.size srcs
  -- pending arguments would be applied to the result; the interpreter's job
  pending <- fresh "pending"
  emit (pending <> " = icmp ne i64 %ap, %fpb")
  Pair fnp skip0 <- getCallee
  skip <- fresh "skip"
  emit (skip <> " = or i1 " <> skip0 <> ", " <> pending)
  slow <- exitBlock fe d (Resume (feCix fe) sect)
  go <- freshLabel "tail"
  emit ("br i1 " <> skip <> ", label %" <> slow <> ", label %" <> go <> unlikely)
  startBlock go
  vals <- loadSources srcs
  iforM_ vals $ \j (Pair u b) -> do
    ua <- stackAddrU (base + n - j)
    emit ("store i64 " <> u <> ", ptr " <> ua)
    ba <- stackAddrB (base + n - j)
    emit ("store ptr " <> b <> ", ptr " <> ba)
  top <- copyCaptured fe (base + n)
  tailCallThroughStack fe fnp top

-- | The callee of an ability request: the handler the environment in the
-- context maps the ability number to, examined like a closure in a slot
-- (null when it isn't an affine handler's, which fails the closure check).
dynCallee :: FnEnv -> Word64 -> Int -> Gen (Pair Text Text)
dynCallee fe key n = do
  ha <- ctxField (feEnv fe) oHenv
  henv <- fresh "henv"
  emit (henv <> " = load ptr, ptr " <> ha)
  h <- fresh "handler"
  emit (h <> " = call ptr @unison_jit_resolve_dyn(ptr " <> henv <> ", i64 " <> tshow key <> ")")
  closureCalleeFrom fe h n

-- | A top-level value that was evaluated when it was loaded: its unboxed
-- word and the pool index of its boxed part.
cachedConstant :: FnEnv -> CombIx -> RComb Val -> Maybe (Pair Int Int)
cachedConstant fe cix comb = case unRComb comb of
  CachedVal _ v -> Pair (getUnboxedVal v) <$> Map.lookup (KeyCached cix (getBoxedVal v)) (envPool (feEnv fe))
  _ -> Nothing

-- | Pushes a cached top-level value.
pushCached :: Int -> Int -> Int -> Gen ()
pushCached d u ix = do
  usePool ix
  a <- fresh "pool.a"
  emit (a <> " = getelementptr ptr, ptr %pool, i64 " <> tshow ix)
  v <- fresh "const"
  emit (v <> " = load ptr, ptr " <> a)
  storeU (d + 1) (tshow u)
  storeB (d + 1) v

-- | The pool index of a known combinator's closure (an @App@ with no
-- arguments of a combinator with some arity), if it has one.
combConstant :: FnEnv -> MRef -> Maybe Int
combConstant fe = \case
  MCode.Env cix comb
    | Comb info@(LamI arity _ _ _) <- unRComb comb,
      arity > 0 ->
        Map.lookup (KeyComb cix info) (envPool (feEnv fe))
  _ -> Nothing

-- | A non-tail call to the closure in slot @k@ with the given argument
-- slots; the callee's frame starts at @base@. See 'genNonTailCall'.
genClosureCall :: FnEnv -> Int -> Int -> Int -> Deque Int -> Int -> MSection -> Maybe (Pair Int Int) -> Gen () -> Gen ()
genClosureCall fe d base k srcs m sect ownFrame continue =
  genNonTailCallWith fe d base (closureCallee fe d k (D.size srcs)) (copyCaptured fe (base + D.size srcs)) srcs m sect ownFrame continue

-- | Examines the closure in slot @k@ for a call with @n@ supplied
-- arguments: it must be a @PAp@ whose arity is @n@ plus its captured
-- arguments, with compiled code. Gives the code pointer and an i1 that is
-- true when the call can't be made natively. Leaves the captured count in
-- @%cap.n@ and the segment arrays in @%cap.u@ and @%cap.b@ for
-- 'copyCaptured' (registers named per call site through the suffix).
closureCallee :: FnEnv -> Int -> Int -> Int -> Gen (Pair Text Text)
closureCallee fe _d k n = loadB k >>= \p -> closureCalleeFrom fe p n

-- | 'closureCallee' for the closure pointer in register @p@.
closureCalleeFrom :: FnEnv -> Text -> Int -> Gen (Pair Text Text)
closureCalleeFrom fe p n = do
  let env = feEnv fe
      ls = envLayouts env
      layout = lPAp ls
      rts = envRts env
      loadFrom from what ty off = do
        a <- fresh (what <> ".a")
        emit (a <> " = add i64 " <> from <> ", " <> tshow off)
        pp <- fresh (what <> ".p")
        emit (pp <> " = inttoptr i64 " <> a <> " to ptr")
        v <- fresh what
        emit (v <> " = load " <> ty <> ", ptr " <> pp)
        pure v
  raw <- fresh "raw"
  emit (raw <> " = ptrtoint ptr " <> p <> " to i64")
  tagBits <- fresh "ptrtag"
  emit (tagBits <> " = and i64 " <> raw <> ", 7")
  notPAp <- fresh "notpap"
  emit (notPAp <> " = icmp ne i64 " <> tagBits <> ", " <> tshow (lPtrTag layout))
  base <- fresh "pap"
  emit (base <> " = and i64 " <> raw <> ", -8")
  -- The loads below are only valid for a PAp, but any closure has at
  -- least the header, and the payload words read are within the
  -- allocation of a PAp; for another kind they may read past its
  -- payload, so they are guarded by a branch.
  okL <- freshLabel "pap"
  joinL <- freshLabel "pap.join"
  cur <- curLabel
  emit ("br i1 " <> notPAp <> ", label %" <> joinL <> ", label %" <> okL)
  startBlock okL
  arity <- loadFrom base "arity" "i64" (lFieldOffset layout (lPtrs layout + 2))
  cellA <- loadFrom base "cell" "i64" (lFieldOffset layout (lPtrs layout + 4))
  cellP <- fresh "cell.p"
  emit (cellP <> " = inttoptr i64 " <> cellA <> " to ptr")
  fnp0 <- fresh "fn"
  emit (fnp0 <> " = load ptr, ptr " <> cellP)
  isNull <- fresh "isnull"
  emit (isNull <> " = icmp eq ptr " <> fnp0 <> ", null")
  -- the segment's boxes must be evaluated (pointer tag 1) to be read
  -- through; a lazily built one (a thunk, whose payload is not an array
  -- pointer) is left to the interpreter, so nothing is loaded through a
  -- box before its tag has been checked
  let boxOf what off = do
        box <- loadFrom base what "ptr" (lFieldOffset layout off)
        boxI <- fresh (what <> ".box")
        emit (boxI <> " = ptrtoint ptr " <> box <> " to i64")
        boxTag <- fresh (what <> ".tag")
        emit (boxTag <> " = and i64 " <> boxI <> ", 7")
        boxBad <- fresh (what <> ".lazy")
        emit (boxBad <> " = icmp ne i64 " <> boxTag <> ", 1")
        untagged <- fresh (what <> ".un")
        emit (untagged <> " = and i64 " <> boxI <> ", -8")
        pure (Pair untagged boxBad)
  Pair usegBox usegBad <- boxOf "useg" 2
  Pair bsegBox bsegBad <- boxOf "bseg" 3
  boxesBad <- fresh "lazy"
  emit (boxesBad <> " = or i1 " <> usegBad <> ", " <> bsegBad)
  segL <- freshLabel "pap.seg"
  emit ("br i1 " <> boxesBad <> ", label %" <> joinL <> ", label %" <> segL <> unlikely)
  startBlock segL
  useg <- loadFrom usegBox "useg" "i64" (lHeaderBytes ls)
  bseg <- loadFrom bsegBox "bseg" "i64" (lHeaderBytes ls)
  count <- loadFrom bseg "count" "i64" (8 * rPtrsCount rts)
  need <- fresh "need"
  emit (need <> " = add i64 " <> count <> ", " <> tshow n)
  wrong <- fresh "wrong"
  emit (wrong <> " = icmp ne i64 " <> arity <> ", " <> need)
  -- the captured arguments must fit on the stack (the callee checks its own frame)
  top <- fresh "top"
  emit (top <> " = add i64 %fpb, " <> need)
  limit <- fresh "limit"
  emit (limit <> " = add i64 " <> top <> ", 1")
  noRoom <- fresh "noroom"
  emit (noRoom <> " = icmp sge i64 " <> limit <> ", %stack.size")
  bad0 <- fresh "bad"
  emit (bad0 <> " = or i1 " <> isNull <> ", " <> wrong)
  bad <- fresh "bad"
  emit (bad <> " = or i1 " <> bad0 <> ", " <> noRoom)
  emit ("br label %" <> joinL)
  startBlock joinL
  skip0 <- fresh "skip"
  emit (skip0 <> " = phi i1 [ true, %" <> cur <> " ], [ true, %" <> okL <> " ], [ " <> bad <> ", %" <> segL <> " ]")
  fnp <- fresh "fn"
  emit (fnp <> " = phi ptr [ null, %" <> cur <> " ], [ null, %" <> okL <> " ], [ " <> fnp0 <> ", %" <> segL <> " ]")
  usegR <- fresh "cap.u"
  emit (usegR <> " = phi i64 [ 0, %" <> cur <> " ], [ 0, %" <> okL <> " ], [ " <> useg <> ", %" <> segL <> " ]")
  bsegR <- fresh "cap.b"
  emit (bsegR <> " = phi i64 [ 0, %" <> cur <> " ], [ 0, %" <> okL <> " ], [ " <> bseg <> ", %" <> segL <> " ]")
  countR <- fresh "cap.n"
  emit (countR <> " = phi i64 [ 0, %" <> cur <> " ], [ 0, %" <> okL <> " ], [ " <> count <> ", %" <> segL <> " ]")
  modify' (\st -> st {gsCaptured = Just $! Captured usegR bsegR countR})
  skip <-
    if envStressCallee env
      then do
        fire <- stressFire env oStressCalleeLeft oStressCallee
        sk <- fresh "skip"
        emit (sk <> " = or i1 " <> skip0 <> ", " <> fire)
        pure sk
      else pure skip0
  pure (Pair fnp skip)

-- | Copies the captured arguments left by 'closureCallee' to the slots
-- above frame offset @from@, as @dumpSeg@ does, and gives the register
-- holding the resulting stack pointer. Marks the high-water mark, since
-- the count isn't static.
copyCaptured :: FnEnv -> Int -> Gen Text
copyCaptured fe from = do
  let env = feEnv fe
      rts = envRts env
  Captured useg bseg count <- gets gsCaptured >>= \case
    Just c -> pure c
    Nothing -> error "copyCaptured: no closure examined"
  modify' (\st -> st {gsCaptured = Nothing})
  cur <- curLabel
  headL <- freshLabel "cp.head"
  bodyL <- freshLabel "cp.body"
  endL <- freshLabel "cp.end"
  fromR <- fpPlus from
  emit ("br label %" <> headL)
  startBlock headL
  k <- fresh "k"
  k1 <- fresh "k"
  emit (k <> " = phi i64 [ 0, %" <> cur <> " ], [ " <> k1 <> ", %" <> bodyL <> " ]")
  done <- fresh "done"
  emit (done <> " = icmp eq i64 " <> k <> ", " <> count)
  emit ("br i1 " <> done <> ", label %" <> endL <> ", label %" <> bodyL)
  startBlock bodyL
  -- element k of each array goes to slot from + 1 + k
  ui <- fresh "ui"
  emit (ui <> " = add i64 " <> k <> ", " <> tshow (rBytesHeader rts))
  ua <- fresh "ua"
  emit (ua <> " = inttoptr i64 " <> useg <> " to ptr")
  up <- fresh "up"
  emit (up <> " = getelementptr i64, ptr " <> ua <> ", i64 " <> ui)
  u <- fresh "u"
  emit (u <> " = load i64, ptr " <> up)
  bi <- fresh "bi"
  emit (bi <> " = add i64 " <> k <> ", " <> tshow (rPtrsHeader rts))
  ba <- fresh "ba"
  emit (ba <> " = inttoptr i64 " <> bseg <> " to ptr")
  bp <- fresh "bp"
  emit (bp <> " = getelementptr ptr, ptr " <> ba <> ", i64 " <> bi)
  b <- fresh "b"
  emit (b <> " = load ptr, ptr " <> bp)
  slot <- fresh "slot"
  emit (slot <> " = add i64 " <> fromR <> ", " <> k)
  slot1 <- fresh "slot"
  emit (slot1 <> " = add i64 " <> slot <> ", 1")
  dstU <- fresh "ua"
  ustk <- stackReg "%ustk"
  emit (dstU <> " = getelementptr i64, ptr " <> ustk <> ", i64 " <> slot1)
  emit ("store i64 " <> u <> ", ptr " <> dstU)
  dstB <- fresh "ba"
  bstk <- stackReg "%bstk"
  emit (dstB <> " = getelementptr ptr, ptr " <> bstk <> ", i64 " <> slot1)
  emit ("store ptr " <> b <> ", ptr " <> dstB)
  emit (k1 <> " = add i64 " <> k <> ", 1")
  emit ("br label %" <> headL)
  startBlock endL
  top <- fresh "top"
  emit (top <> " = add i64 " <> fromR <> ", " <> count)
  -- the high-water mark for card marking on return
  markWritten env top
  pure top

-- ---------------------------------------------------------------------------
-- Instructions

litSupported :: MLit -> Bool
litSupported = \case
  MI _ -> True
  MN _ -> True
  MC _ -> True
  MD _ -> True
  _ -> False

prim1Supported :: Prim1 -> Bool
prim1Supported op =
  op
    `elem` [ DECI, DECN, INCI, INCN, NEGI, COMN, COMI, TRNC, SGNI, LZRO, TZRO, POPC,
             ITOF, NTOF, ABSF, CEIL, FLOR, TRNF, RNDF, EXPF, LOGF, SQRT,
             COSF, SINF, TANF, COSH, SINH, TANH, ACOS, ASIN, ATAN, ASNH, ACSH, ATNH
           ]

prim2Supported :: Prim2 -> Bool
prim2Supported op =
  op
    `elem` [ ADDI, SUBI, MULI, DIVI, MODI, EQLI, NEQI, LEQI, LESI, ANDI, IORI, XORI, SHLI, SHRI,
             ADDN, SUBN, MULN, DIVN, MODN, EQLN, NEQN, LEQN, LESN, ANDN, IORN, XORN, SHLN, SHRN, DRPN,
             POWI, POWN, CAST,
             ADDF, SUBF, MULF, DIVF, EQLF, NEQF, LEQF, LESF, MINF, MAXF, POWF, LOGB, ATN2
           ]

-- | Whether 'genInstr' has native code for an instruction (at least a fast
-- path), as opposed to calling out for it every time. Must agree with the
-- cases of 'genInstr'; the estimate of what compiling a function saves
-- relies on it (see JIT.Estimate).
-- | The Text and Bytes foreign functions with native versions (see "The
-- rest of Text and Bytes" in jit_rt.c).
textForeign, bytesForeign :: [ForeignFunc]
textForeign = [Text_repeat, Text_reverse, Text_toUppercase, Text_toLowercase, Text_toUtf8, Text_fromUtf8_impl_v3, Char_toText]
bytesForeign =
  [ Bytes_decodeNat16be, Bytes_decodeNat16le, Bytes_decodeNat32be, Bytes_decodeNat32le, Bytes_decodeNat64be, Bytes_decodeNat64le,
    Bytes_encodeNat16be, Bytes_encodeNat16le, Bytes_encodeNat32be, Bytes_encodeNat32le, Bytes_encodeNat64be, Bytes_encodeNat64le,
    Bytes_read, Bytes_read16be, Bytes_read16le, Bytes_read32be, Bytes_read32le, Bytes_read64be, Bytes_read64le,
    Bytes_toBase16, Bytes_toBase32, Bytes_toBase64, Bytes_toBase64UrlUnpadded,
    Bytes_fromBase16, Bytes_fromBase32, Bytes_fromBase64, Bytes_fromBase64UrlUnpadded
  ]

-- | The width in bytes and endianness (1 for big) of the Bytes number functions.
decodeNatKind, encodeNatKind, readKind :: ForeignFunc -> Maybe (Pair Int Int)
decodeNatKind = \case
  Bytes_decodeNat16be -> Just (Pair 2 1)
  Bytes_decodeNat16le -> Just (Pair 2 0)
  Bytes_decodeNat32be -> Just (Pair 4 1)
  Bytes_decodeNat32le -> Just (Pair 4 0)
  Bytes_decodeNat64be -> Just (Pair 8 1)
  Bytes_decodeNat64le -> Just (Pair 8 0)
  _ -> Nothing
encodeNatKind = \case
  Bytes_encodeNat16be -> Just (Pair 2 1)
  Bytes_encodeNat16le -> Just (Pair 2 0)
  Bytes_encodeNat32be -> Just (Pair 4 1)
  Bytes_encodeNat32le -> Just (Pair 4 0)
  Bytes_encodeNat64be -> Just (Pair 8 1)
  Bytes_encodeNat64le -> Just (Pair 8 0)
  _ -> Nothing
readKind = \case
  Bytes_read -> Just (Pair 1 1)
  Bytes_read16be -> Just (Pair 2 1)
  Bytes_read16le -> Just (Pair 2 0)
  Bytes_read32be -> Just (Pair 4 1)
  Bytes_read32le -> Just (Pair 4 0)
  Bytes_read64be -> Just (Pair 8 1)
  Bytes_read64le -> Just (Pair 8 0)
  _ -> Nothing

-- | The base of the Bytes encoding functions: 16, 32, 64, or 65 for base 64
-- with the URL alphabet and no padding.
toBaseKind, fromBaseKind :: ForeignFunc -> Maybe Int
toBaseKind = \case
  Bytes_toBase16 -> Just 16
  Bytes_toBase32 -> Just 32
  Bytes_toBase64 -> Just 64
  Bytes_toBase64UrlUnpadded -> Just 65
  _ -> Nothing
fromBaseKind = \case
  Bytes_fromBase16 -> Just 16
  Bytes_fromBase32 -> Just 32
  Bytes_fromBase64 -> Just 64
  Bytes_fromBase64UrlUnpadded -> Just 65
  _ -> Nothing

instrNative :: GInstr comb -> Bool
instrNative = \case
  Lit _ -> True
  Pack {} -> True
  -- VALU is native only as the first half of Universal.murmurHashUntyped
  -- (see genInstr); on its own it is a call-out, which the estimate accepts
  Prim1 op _ -> prim1Supported op || op `elem` [REFR, REFN, TIKR, RRFC, VALU, NOTB, SIZS, VWLS, VWRS, SIZT, SIZB, FLTB, UCNS, USNC, ITOT, NTOT, FTOT, TTOI, TTON, TTOF, PAKT, UPKT, PAKB, UPKB]
  Prim2 op _ _ -> prim2Supported op || op `elem` [REFW, EQLU, LEQU, LESU, CMPU, ANDB, IORB, CONS, SNOC, IDXS, CATS, TAKS, DRPS, SPLL, SPLR, CATT, TAKT, DRPT, EQLT, CATB, TAKB, DRPB, IDXB, IXOT, IXOB, LEQT, LEST]
  Seq _ -> True
  RefCAS {} -> True
  ForeignCall _ f _ -> f `elem` ([MutableArray_size, MutableArray_read, MutableArray_write, ImmutableArray_size, ImmutableArray_read, Universal_murmurHashUntyped] <> textForeign <> bytesForeign) || isJust (arrayForeign f)
  -- (up to four arguments; more is rare, and then it is a call-out)
  Name r _ -> case r of Dyn _ -> False; _ -> True
  -- storing an affine handler's next state (see 'genInstr'); with the
  -- environment update it is the interpreter's
  SetAff False _ _ -> requestsOn
  _ -> False

-- | 'instrNative' for an instruction in its section: an affine handler's
-- @InLocal@ is native (a no-op) when the rest of its case can't tell the
-- environment wasn't switched, see 'localUnobserved'.
instrNativeIn :: GInstr (RComb Val) -> MSection -> Bool
instrNativeIn i rest =
  instrNative i || case i of
    InLocal _ -> requestsOn && (localUnobserved rest || localObservingOn)
    _ -> False

-- | The observing form of @InLocal@ is generated (see 'genInLocal'): its
-- Local frame record is written by the exit routine, which the generated
-- write-back blocks of @UNISON_JIT_EXITS=blocks@ don't have.
localObservingOn :: Bool
localObservingOn = requestsOn && exitCall config

-- | Ability requests and the affine handler instructions are generated
-- (off under @UNISON_JIT_DISABLE=requests@).
requestsOn :: Bool
requestsOn = "requests" `notElem` disabled config

-- | Whether a section, the rest of an affine handler's case after its
-- @InLocal@, can run without the environment switch the instruction makes
-- in the interpreter. The switch is observed by requests, @Reset@,
-- @Capture@, @Discard@, @SetAff@ with the environment update, raising an
-- exception (a failing foreign call, @THRO@), and by any exit that resumes
-- the interpreter ahead of one of those. So: straight-line code to the
-- @Yield@, made of literals, constructors, partial applications of known
-- functions, the handler's own state update, and the native primitives,
-- none of which look at the environment. Even an exit in such a body (a
-- lazy field found by a @Pack@, say) is harmless: the interpreter finishes
-- the case in the requester's environment, which nothing left in it reads,
-- and the @Yield@ pops the requester's frame, which is what the @Local@
-- frame would have left it with.
localUnobserved :: MSection -> Bool
localUnobserved = \case
  Yield _ -> True
  Ins i rest -> harmless i && localUnobserved rest
  _ -> False
  where
    harmless = \case
      Lit _ -> True
      Pack {} -> True
      Name r _ -> case r of Dyn _ -> False; _ -> True
      SetAff False _ _ -> True
      Prim1 op _ -> prim1Supported op
      Prim2 op _ _ -> prim2Supported op
      _ -> False

-- | An affine handler's @InLocal@ when the rest of the case observes the
-- environment (a request of its own, a call): the switch the interpreter
-- makes, reproduced. The helper builds the case's environment (@HEnv@ of
-- the @Affine@ closure's @aenv@ and the empty @denv@); the old one goes
-- in a register and the new one in @Ctx@, where every request site reads
-- it, and the switch count goes up. The rest of the case is generated
-- with the switch in force ('feLocal'): its @Yield@ switches back
-- ('endLocal'), and every exit or unwind before that writes a @Local@
-- frame record with the old environment ('joinCall'), so the interpreter
-- continues as it would after its own @InLocal@, with the current
-- environment it reads back from the trampoline. The slow path is a
-- call-out: the interpreter runs the instruction and the rest of the case
-- re-enters with the @Local@ frame in @K@. Taken when the helper refuses
-- the closure, or when as many switches are in force as the out-area that
-- an exit hands their environments back in has room for.
genInLocal :: FnEnv -> Int -> Int -> MSection -> MSection -> Gen ()
genInLocal fe d i rest sect = do
  let env = feEnv fe
  slow <- callOutExit True fe d (InLocal i) sect
  nla <- ctxField env oNLocals
  nl <- fresh "nlocals"
  emit (nl <> " = load i64, ptr " <> nla)
  full <- fresh "full"
  emit (full <> " = icmp sge i64 " <> nl <> ", " <> tshow nativeLocals)
  branchIf "local" full slow
  aff <- loadB (d - i)
  h <- fresh "local"
  emit (h <> " = call ptr @unison_jit_local_env(ptr %ctx, ptr " <> aff <> ")")
  miss <- fresh "miss"
  emit (miss <> " = icmp eq ptr " <> h <> ", null")
  branchIf "local" miss slow
  ha <- ctxField env oHenv
  h0 <- fresh "henv0"
  emit (h0 <> " = load ptr, ptr " <> ha)
  emit ("store ptr " <> h <> ", ptr " <> ha)
  nl' <- fresh "nlocals"
  emit (nl' <> " = add i64 " <> nl <> ", 1")
  emit ("store i64 " <> nl' <> ", ptr " <> nla)
  genSection fe {feLocal = Just (Local h0 (D.size (feEnclosing fe)))} d rest

-- | Switches in force at once that an exit can hand back; must match
-- UNISON_JIT_LOCALS in jit_rt.h.
nativeLocals :: Int
nativeLocals = 8

-- | The end of an environment switch, at the case's @Yield@: the old
-- environment goes back into @Ctx@ and the switch count down.
endLocal :: FnEnv -> Gen ()
endLocal fe = forM_ (feLocal fe) $ \lo -> do
  let env = feEnv fe
  ha <- ctxField env oHenv
  emit ("store ptr " <> loHenv0 lo <> ", ptr " <> ha)
  nla <- ctxField env oNLocals
  nl <- fresh "nlocals"
  emit (nl <> " = load i64, ptr " <> nla)
  nl' <- fresh "nlocals"
  emit (nl' <> " = add i64 " <> nl <> ", -1")
  emit ("store i64 " <> nl' <> ", ptr " <> nla)

-- | An instruction at depth @d@; continues with the depth after it.
genInstr :: FnEnv -> Int -> GInstr (RComb Val) -> MSection -> (Int -> Gen ()) -> Gen ()
genInstr fe d instr sect k = case instr of
  Lit l | litSupported l -> do
    let !(Pair u tag) = case l of
          MI i -> Pair (tshow i) (feTagInt fe)
          MN n -> Pair (tshow (fromIntegral n :: Int64)) (feTagNat fe)
          MC c -> Pair (tshow (ord c)) (feTagChar fe)
          MD x -> Pair (tshow (fromIntegral (castDoubleToWord64 x) :: Int64)) (feTagFloat fe)
          _ -> error "unreachable"
    storeU (d + 1) u
    storeB (d + 1) tag
    k (d + 1)
  -- boxed literals and constructors without fields are pool constants
  Lit l | Just ix <- Map.lookup (KeyLit l) (envPool (feEnv fe)) -> do
    poolConstant fe d ix
    k (d + 1)
  Pack r t ZArgs | Just ix <- Map.lookup (KeyEnum r t) (envPool (feEnv fe)) -> do
    poolConstant fe d ix
    k (d + 1)
  Pack r t args
    | Just refIx <- Map.lookup (KeyEnum r (PackedTag 0)) (envPool (feEnv fe)),
      Just fields <- packFields fe d args -> do
        slow <- exitBlock fe d (Resume (feCix fe) sect)
        genPack fe d refIx t fields slow
        k (d + 1)
  -- An affine handler's switch to its own environment: nothing left in
  -- the case can tell (see 'localUnobserved'), so nothing is done; the
  -- interpreter's Local frame would only restore what was never changed.
  InLocal _ | enabled fe "requests", Ins _ rest <- sect, localUnobserved rest -> k d
  -- The handler's next state goes into the ref inside the Affine closure
  -- in slot i, through the runtime's barrier; the helper refuses anything
  -- that isn't an Affine closure, and the interpreter runs it then.
  SetAff False i j | enabled fe "requests" -> do
    slow <- callOutExit True fe d instr sect
    aff <- loadB (d - i)
    v <- loadB (d - j)
    r <- fresh "setaff"
    emit (r <> " = call i64 @unison_jit_set_aff(ptr %ctx, ptr " <> aff <> ", ptr " <> v <> ")")
    miss <- fresh "miss"
    emit (miss <> " = icmp eq i64 " <> r <> ", 0")
    branchIf "setaff" miss slow
    k d
  Prim1 op i | prim1Supported op -> do
    x <- loadU (d - i)
    genPrim1 fe d op x sect
    k (d + 1)
  Prim1 NOTB i -> do
    slow <- callOutExit True fe d instr sect
    c <- loadBoolean fe (d - i) slow
    r <- fresh "not"
    emit (r <> " = xor i1 " <> c <> ", true")
    resultBool fe d r
    k (d + 1)
  Prim2 op i j | op `elem` [ANDB, IORB] -> do
    slow <- callOutExit True fe d instr sect
    x <- loadBoolean fe (d - i) slow
    y <- loadBoolean fe (d - j) slow
    r <- fresh "bool"
    emit (r <> " = " <> (if op == ANDB then "and" else "or") <> " i1 " <> x <> ", " <> y)
    resultBool fe d r
    k (d + 1)
  Prim1 REFR i | enabled fe "ref" -> do
    genRefRead fe d (d - i) instr sect
    k (d + 1)
  -- A partial application: a copy of the function's closure with the
  -- arguments added (a C helper; see "Partial applications" in jit_rt.c).
  Name r args
    | enabled fe "name",
      srcs <- argSources fe d args,
      D.size srcs <= 4,
      Just closure <- nameClosure r -> do
        slow <- callOutExit True fe d instr sect
        vals <- loadSources srcs
        f <- closure
        listHelper d slow (nameCall f vals)
        k (d + 1)
    where
      nameClosure = \case
        Stk i -> Just (loadB (d - i))
        MCode.Env cix comb
          | Comb info <- unRComb comb,
            Just ix <- Map.lookup (KeyComb cix info) (envPool (feEnv fe)) ->
              Just (poolValue ix)
        _ -> Nothing
  -- Lists: C helpers that are ports of the Haskell operations (see "Lists"
  -- in jit_rt.c). They handle every list; the slow path is only for a
  -- closure that isn't one.
  Prim1 SIZS i | enabled fe "list" -> do
    slow <- callOutExit True fe d instr sect
    l <- loadB (d - i)
    n <- fresh "size"
    emit (n <> " = call i64 @unison_jit_list_size(ptr " <> l <> ")")
    miss <- fresh "miss"
    emit (miss <> " = icmp slt i64 " <> n <> ", 0")
    branchIf "list" miss slow
    result fe d (feTagNat fe) n
    k (d + 1)
  Prim1 op i
    | enabled fe "list",
      op == VWLS || op == VWRS,
      Just emptyIx <- Map.lookup (KeyEnum Ty.seqViewRef TT.seqViewEmptyTag) (envPool (feEnv fe)) -> do
        let PackedTag elemTag = TT.seqViewElemTag
        slow <- callOutExit True fe d instr sect
        l <- loadB (d - i)
        e <- poolValue emptyIx
        listHelper d slow ("@unison_jit_list_view(ptr %ctx, ptr " <> l <> ", ptr " <> e <> ", i64 " <> tshow elemTag <> ", i64 " <> (if op == VWLS then "1" else "0") <> ")")
        k (d + 1)
  Prim2 op i j | enabled fe "list", op == CONS || op == SNOC -> do
    -- cons takes the element first, snoc the list
    let !(Pair kl kx) = if op == CONS then Pair (d - j) (d - i) else Pair (d - i) (d - j)
    slow <- callOutExit True fe d instr sect
    l <- loadB kl
    u <- loadU kx
    b <- loadB kx
    listHelper d slow ("@unison_jit_list_push(ptr %ctx, ptr " <> l <> ", i64 " <> u <> ", ptr " <> b <> ", i64 " <> (if op == CONS then "1" else "0") <> ")")
    k (d + 1)
  Prim2 op i j | enabled fe "list", op == TAKS || op == DRPS -> do
    slow <- callOutExit True fe d instr sect
    n <- loadU (d - i)
    l <- loadB (d - j)
    listHelper d slow ("@unison_jit_list_cut(ptr %ctx, ptr " <> l <> ", i64 " <> n <> ", i64 " <> (if op == TAKS then "1" else "0") <> ")")
    k (d + 1)
  Prim2 op i j
    | enabled fe "list",
      op == SPLL || op == SPLR,
      Just emptyIx <- Map.lookup (KeyEnum Ty.seqViewRef TT.seqViewEmptyTag) (envPool (feEnv fe)) -> do
        let PackedTag elemTag = TT.seqViewElemTag
        slow <- callOutExit True fe d instr sect
        n <- loadU (d - i)
        l <- loadB (d - j)
        e <- poolValue emptyIx
        listHelper d slow ("@unison_jit_list_split(ptr %ctx, ptr " <> l <> ", i64 " <> n <> ", ptr " <> e <> ", i64 " <> tshow elemTag <> ", i64 " <> (if op == SPLL then "1" else "0") <> ")")
        k (d + 1)
  Prim2 CATS i j | enabled fe "list" -> do
    slow <- callOutExit True fe d instr sect
    x <- loadB (d - i)
    y <- loadB (d - j)
    listHelper d slow ("@unison_jit_list_append(ptr %ctx, ptr " <> x <> ", ptr " <> y <> ")")
    k (d + 1)
  -- A list literal: the elements are added one at a time to a deque that
  -- is wrapped at the end. The only slow path is an untagged element.
  Seq args | enabled fe "list" -> do
    vals <- loadSources (argSources fe d args)
    slow <- exitBlock fe d (Resume (feCix fe) sect)
    requireTagged slow (fmap (\(Pair _ b) -> b) vals)
    let add acc (Pair u b) = do
          r <- fresh "lit"
          emit (r <> " = call ptr @unison_jit_list_lit(ptr %ctx, ptr " <> acc <> ", i64 " <> u <> ", ptr " <> b <> ")")
          pure r
    acc <- foldM add "null" vals
    r <- fresh "list"
    emit (r <> " = call ptr @unison_jit_list_wrap(ptr %ctx, ptr " <> acc <> ")")
    storeU (d + 1) "-1"
    storeB (d + 1) r
    k (d + 1)
  -- Text: the same arrangement (see "Text" in jit_rt.c). The helpers handle
  -- every text; the slow path is only for a closure that isn't one.
  Prim1 SIZT i | enabled fe "text" -> do
    slow <- callOutExit True fe d instr sect
    t <- loadB (d - i)
    n <- fresh "size"
    emit (n <> " = call i64 @unison_jit_text_size(ptr " <> t <> ")")
    miss <- fresh "miss"
    emit (miss <> " = icmp slt i64 " <> n <> ", 0")
    branchIf "text" miss slow
    result fe d (feTagNat fe) n
    k (d + 1)
  Prim2 CATT i j | enabled fe "text" -> do
    slow <- callOutExit True fe d instr sect
    x <- loadB (d - i)
    y <- loadB (d - j)
    listHelper d slow ("@unison_jit_text_append(ptr %ctx, ptr " <> x <> ", ptr " <> y <> ")")
    k (d + 1)
  Prim2 op i j | enabled fe "text", op == TAKT || op == DRPT -> do
    slow <- callOutExit True fe d instr sect
    n <- loadU (d - i)
    t <- loadB (d - j)
    listHelper d slow ("@unison_jit_text_cut(ptr %ctx, ptr " <> t <> ", i64 " <> n <> ", i64 " <> (if op == TAKT then "1" else "0") <> ")")
    k (d + 1)
  Prim2 EQLT i j | enabled fe "text" -> do
    slow <- callOutExit True fe d instr sect
    x <- loadB (d - i)
    y <- loadB (d - j)
    r <- fresh "eq"
    emit (r <> " = call i64 @unison_jit_text_eq(ptr " <> x <> ", ptr " <> y <> ")")
    miss <- fresh "miss"
    emit (miss <> " = icmp slt i64 " <> r <> ", 0")
    branchIf "text" miss slow
    c <- fresh "bool"
    emit (c <> " = icmp ne i64 " <> r <> ", 0")
    resultBool fe d c
    k (d + 1)
  -- Bytes: the same rope with chunks of bytes and the same helpers (see
  -- "Ropes" in jit_rt.c), plus Bytes.at and Bytes.flatten.
  Prim1 SIZB i | enabled fe "bytes" -> do
    slow <- callOutExit True fe d instr sect
    b <- loadB (d - i)
    n <- fresh "size"
    emit (n <> " = call i64 @unison_jit_bytes_size(ptr " <> b <> ")")
    miss <- fresh "miss"
    emit (miss <> " = icmp slt i64 " <> n <> ", 0")
    branchIf "bytes" miss slow
    result fe d (feTagNat fe) n
    k (d + 1)
  Prim1 FLTB i | enabled fe "bytes" -> do
    slow <- callOutExit True fe d instr sect
    b <- loadB (d - i)
    listHelper d slow ("@unison_jit_bytes_flatten(ptr %ctx, ptr " <> b <> ")")
    k (d + 1)
  Prim2 CATB i j | enabled fe "bytes" -> do
    slow <- callOutExit True fe d instr sect
    x <- loadB (d - i)
    y <- loadB (d - j)
    listHelper d slow ("@unison_jit_bytes_append(ptr %ctx, ptr " <> x <> ", ptr " <> y <> ")")
    k (d + 1)
  Prim2 op i j | enabled fe "bytes", op == TAKB || op == DRPB -> do
    slow <- callOutExit True fe d instr sect
    n <- loadU (d - i)
    b <- loadB (d - j)
    listHelper d slow ("@unison_jit_bytes_cut(ptr %ctx, ptr " <> b <> ", i64 " <> n <> ", i64 " <> (if op == TAKB then "1" else "0") <> ")")
    k (d + 1)
  Prim2 IDXB i j
    | enabled fe "bytes",
      Just noneIx <- Map.lookup (KeyEnum Ty.optionalRef TT.noneTag) (envPool (feEnv fe)) -> do
        let PackedTag someTag = TT.someTag
        slow <- callOutExit True fe d instr sect
        ix <- loadU (d - i)
        b <- loadB (d - j)
        none <- poolValue noneIx
        listHelper d slow ("@unison_jit_bytes_index(ptr %ctx, ptr " <> b <> ", i64 " <> ix <> ", ptr " <> none <> ", i64 " <> tshow someTag <> ", ptr " <> feTagNat fe <> ")")
        k (d + 1)
  -- The rest of Text and Bytes (see that section of jit_rt.c): uncons and
  -- unsnoc, numbers to and from text, pack and unpack, indexOf, the
  -- orderings, and the foreign functions. A helper answers "not handled"
  -- for a form it doesn't decide, and the call-out does it.
  Prim1 op i
    | enabled fe "text",
      op == UCNS || op == USNC,
      Just noneIx <- Map.lookup (KeyEnum Ty.optionalRef TT.noneTag) (envPool (feEnv fe)),
      Just pairIx <- Map.lookup (KeyEnum Ty.pairRef (PackedTag 0)) (envPool (feEnv fe)),
      Just unitIx <- Map.lookup (KeyEnum Ty.unitRef TT.unitTag) (envPool (feEnv fe)) -> do
        let PackedTag someTag = TT.someTag
            PackedTag pairTag = TT.pairTag
        slow <- callOutExit True fe d instr sect
        t <- loadB (d - i)
        none <- poolValue noneIx
        pair <- poolValue pairIx
        unit <- poolValue unitIx
        listHelper d slow ("@unison_jit_text_uncons(ptr %ctx, ptr " <> t <> ", ptr " <> none <> ", i64 " <> tshow someTag <> ", ptr " <> pair <> ", i64 " <> tshow pairTag <> ", ptr " <> unit <> ", ptr " <> feTagChar fe <> ", i64 " <> (if op == UCNS then "1" else "0") <> ")")
        k (d + 1)
  Prim1 op i | enabled fe "text", op == ITOT || op == NTOT -> do
    n <- loadU (d - i)
    r <- fresh "text"
    emit (r <> " = call ptr @unison_jit_int_to_text(ptr %ctx, i64 " <> n <> ", i64 " <> (if op == ITOT then "1" else "0") <> ")")
    storeU (d + 1) "-1"
    storeB (d + 1) r
    k (d + 1)
  Prim1 FTOT i | enabled fe "text" -> do
    bits <- loadU (d - i)
    r <- fresh "text"
    emit (r <> " = call ptr @unison_jit_float_to_text(ptr %ctx, i64 " <> bits <> ")")
    storeU (d + 1) "-1"
    storeB (d + 1) r
    k (d + 1)
  Prim1 op i
    | enabled fe "text",
      op `elem` [TTOI, TTON, TTOF],
      Just noneIx <- Map.lookup (KeyEnum Ty.optionalRef TT.noneTag) (envPool (feEnv fe)) -> do
        let PackedTag someTag = TT.someTag
            !(Pair tag kind) = case op of
              TTOI -> Pair (feTagInt fe) "0"
              TTON -> Pair (feTagNat fe) "1"
              _ -> Pair (feTagFloat fe) "2"
        slow <- callOutExit True fe d instr sect
        t <- loadB (d - i)
        none <- poolValue noneIx
        listHelper d slow ("@unison_jit_text_to_num(ptr %ctx, ptr " <> t <> ", ptr " <> none <> ", i64 " <> tshow someTag <> ", ptr " <> tag <> ", i64 " <> kind <> ")")
        k (d + 1)
  Prim1 PAKT i | enabled fe "text" -> do
    slow <- callOutExit True fe d instr sect
    l <- loadB (d - i)
    listHelper d slow ("@unison_jit_text_pack(ptr %ctx, ptr " <> l <> ", ptr " <> feTagChar fe <> ")")
    k (d + 1)
  Prim1 UPKT i | enabled fe "text" -> do
    slow <- callOutExit True fe d instr sect
    t <- loadB (d - i)
    listHelper d slow ("@unison_jit_text_unpack(ptr %ctx, ptr " <> t <> ", ptr " <> feTagChar fe <> ")")
    k (d + 1)
  Prim2 IXOT i j
    | enabled fe "text",
      Just noneIx <- Map.lookup (KeyEnum Ty.optionalRef TT.noneTag) (envPool (feEnv fe)) -> do
        let PackedTag someTag = TT.someTag
        slow <- callOutExit True fe d instr sect
        x <- loadB (d - i)
        y <- loadB (d - j)
        none <- poolValue noneIx
        listHelper d slow ("@unison_jit_text_index_of(ptr %ctx, ptr " <> x <> ", ptr " <> y <> ", ptr " <> none <> ", i64 " <> tshow someTag <> ", ptr " <> feTagNat fe <> ")")
        k (d + 1)
  Prim2 op i j | enabled fe "text", op == LEQT || op == LEST -> do
    slow <- callOutExit True fe d instr sect
    x <- loadB (d - i)
    y <- loadB (d - j)
    r <- fresh "cmp"
    emit (r <> " = call i64 @unison_jit_text_cmp(ptr " <> x <> ", ptr " <> y <> ")")
    miss <- fresh "miss"
    emit (miss <> " = icmp eq i64 " <> r <> ", 2")
    branchIf "text" miss slow
    c <- fresh "bool"
    emit (c <> " = icmp " <> (if op == LEQT then "sle" else "slt") <> " i64 " <> r <> ", 0")
    resultBool fe d c
    k (d + 1)
  Prim1 PAKB i | enabled fe "bytes" -> do
    slow <- callOutExit True fe d instr sect
    l <- loadB (d - i)
    listHelper d slow ("@unison_jit_bytes_pack(ptr %ctx, ptr " <> l <> ", ptr " <> feTagNat fe <> ")")
    k (d + 1)
  Prim1 UPKB i | enabled fe "bytes" -> do
    slow <- callOutExit True fe d instr sect
    b <- loadB (d - i)
    listHelper d slow ("@unison_jit_bytes_unpack(ptr %ctx, ptr " <> b <> ", ptr " <> feTagNat fe <> ")")
    k (d + 1)
  Prim2 IXOB i j
    | enabled fe "bytes",
      Just noneIx <- Map.lookup (KeyEnum Ty.optionalRef TT.noneTag) (envPool (feEnv fe)) -> do
        let PackedTag someTag = TT.someTag
        slow <- callOutExit True fe d instr sect
        x <- loadB (d - i)
        y <- loadB (d - j)
        none <- poolValue noneIx
        listHelper d slow ("@unison_jit_bytes_index_of(ptr %ctx, ptr " <> x <> ", ptr " <> y <> ", ptr " <> none <> ", i64 " <> tshow someTag <> ", ptr " <> feTagNat fe <> ")")
        k (d + 1)
  ForeignCall _ Text_repeat args | enabled fe "text", kn :<| kt :<| Empty <- argSources fe d args -> do
    slow <- callOutExit True fe d instr sect
    n <- loadU kn
    t <- loadB kt
    listHelper d slow ("@unison_jit_text_repeat(ptr %ctx, i64 " <> n <> ", ptr " <> t <> ")")
    k (d + 1)
  ForeignCall _ f args
    | enabled fe "text",
      f `elem` [Text_reverse, Text_toUppercase, Text_toLowercase, Text_toUtf8],
      kt :<| Empty <- argSources fe d args -> do
        slow <- callOutExit True fe d instr sect
        t <- loadB kt
        let call = case f of
              Text_reverse -> "@unison_jit_text_reverse(ptr %ctx, ptr " <> t <> ")"
              Text_toUppercase -> "@unison_jit_text_case(ptr %ctx, ptr " <> t <> ", i64 1)"
              Text_toLowercase -> "@unison_jit_text_case(ptr %ctx, ptr " <> t <> ", i64 0)"
              _ -> "@unison_jit_text_to_utf8(ptr %ctx, ptr " <> t <> ")"
        listHelper d slow call
        k (d + 1)
  ForeignCall _ Text_fromUtf8_impl_v3 args
    | enabled fe "text",
      kb :<| Empty <- argSources fe d args,
      Just eitherIx <- Map.lookup (KeyEnum Ty.eitherRef (PackedTag 0)) (envPool (feEnv fe)) -> do
        let PackedTag rightTag = TT.rightTag
        slow <- callOutExit True fe d instr sect
        b <- loadB kb
        eith <- poolValue eitherIx
        listHelper d slow ("@unison_jit_text_from_utf8(ptr %ctx, ptr " <> b <> ", ptr " <> eith <> ", i64 " <> tshow rightTag <> ")")
        k (d + 1)
  ForeignCall _ Char_toText args | enabled fe "text", kc :<| Empty <- argSources fe d args -> do
    c <- loadU kc
    r <- fresh "text"
    emit (r <> " = call ptr @unison_jit_char_to_text(ptr %ctx, i64 " <> c <> ")")
    storeU (d + 1) "-1"
    storeB (d + 1) r
    k (d + 1)
  ForeignCall _ f args
    | enabled fe "bytes",
      Just (Pair width be) <- decodeNatKind f,
      kb :<| Empty <- argSources fe d args,
      Just noneIx <- Map.lookup (KeyEnum Ty.optionalRef TT.noneTag) (envPool (feEnv fe)),
      Just pairIx <- Map.lookup (KeyEnum Ty.pairRef (PackedTag 0)) (envPool (feEnv fe)),
      Just unitIx <- Map.lookup (KeyEnum Ty.unitRef TT.unitTag) (envPool (feEnv fe)) -> do
        let PackedTag someTag = TT.someTag
            PackedTag pairTag = TT.pairTag
        slow <- callOutExit True fe d instr sect
        b <- loadB kb
        none <- poolValue noneIx
        pair <- poolValue pairIx
        unit <- poolValue unitIx
        listHelper d slow ("@unison_jit_bytes_decode_nat(ptr %ctx, ptr " <> b <> ", i64 " <> tshow width <> ", i64 " <> tshow be <> ", ptr " <> none <> ", i64 " <> tshow someTag <> ", ptr " <> pair <> ", i64 " <> tshow pairTag <> ", ptr " <> unit <> ", ptr " <> feTagNat fe <> ")")
        k (d + 1)
  ForeignCall _ f args | enabled fe "bytes", Just (Pair width be) <- encodeNatKind f, kn :<| Empty <- argSources fe d args -> do
    n <- loadU kn
    r <- fresh "bytes"
    emit (r <> " = call ptr @unison_jit_bytes_encode_nat(ptr %ctx, i64 " <> n <> ", i64 " <> tshow width <> ", i64 " <> tshow be <> ")")
    storeU (d + 1) "-1"
    storeB (d + 1) r
    k (d + 1)
  ForeignCall _ f args | enabled fe "bytes", Just (Pair width be) <- readKind f, ki :<| kb :<| Empty <- argSources fe d args -> do
    slow <- callOutExit True fe d instr sect
    ix <- loadU ki
    b <- loadB kb
    ok <- fresh "ok"
    emit (ok <> " = call i64 @unison_jit_bytes_read_ok(ptr " <> b <> ", i64 " <> ix <> ", i64 " <> tshow width <> ")")
    miss <- fresh "miss"
    emit (miss <> " = icmp ne i64 " <> ok <> ", 1")
    branchIf "bytes" miss slow
    v <- fresh "read"
    emit (v <> " = call i64 @unison_jit_bytes_read_at(ptr " <> b <> ", i64 " <> ix <> ", i64 " <> tshow width <> ", i64 " <> tshow be <> ")")
    result fe d (feTagNat fe) v
    k (d + 1)
  ForeignCall _ f args | enabled fe "bytes", Just base <- toBaseKind f, kb :<| Empty <- argSources fe d args -> do
    slow <- callOutExit True fe d instr sect
    b <- loadB kb
    listHelper d slow ("@unison_jit_bytes_to_base(ptr %ctx, ptr " <> b <> ", i64 " <> tshow base <> ")")
    k (d + 1)
  ForeignCall _ f args
    | enabled fe "bytes",
      Just base <- fromBaseKind f,
      kb :<| Empty <- argSources fe d args,
      Just eitherIx <- Map.lookup (KeyEnum Ty.eitherRef (PackedTag 0)) (envPool (feEnv fe)) -> do
        let PackedTag rightTag = TT.rightTag
        slow <- callOutExit True fe d instr sect
        b <- loadB kb
        eith <- poolValue eitherIx
        listHelper d slow ("@unison_jit_bytes_from_base(ptr %ctx, ptr " <> b <> ", i64 " <> tshow base <> ", ptr " <> eith <> ", i64 " <> tshow rightTag <> ")")
        k (d + 1)
  Prim2 IDXS i j
    | enabled fe "list",
      Just noneIx <- Map.lookup (KeyEnum Ty.optionalRef TT.noneTag) (envPool (feEnv fe)) -> do
        let PackedTag someTag = TT.someTag
        slow <- callOutExit True fe d instr sect
        ix <- loadU (d - i)
        l <- loadB (d - j)
        none <- poolValue noneIx
        listHelper d slow ("@unison_jit_list_index(ptr %ctx, ptr " <> l <> ", i64 " <> ix <> ", ptr " <> none <> ", i64 " <> tshow someTag <> ")")
        k (d + 1)
  Prim2 REFW i j
    | enabled fe "ref",
      Just unitIx <- Map.lookup (KeyEnum Ty.unitRef TT.unitTag) (envPool (feEnv fe)) -> do
        genRefWrite fe d (d - i) (d - j) unitIx instr sect
        k (d + 1)
  Prim2 op i j | enabled fe "cmp", op `elem` [EQLU, LEQU, LESU, CMPU] -> do
    genUniversal fe d op (d - i) (d - j) instr sect
    k (d + 1)
  ForeignCall _ MutableArray_size args
    | enabled fe "array", ka :<| Empty <- argSources fe d args -> do
        genArrayOp fe d (mutableArrayWrap fe) ka Nothing Nothing instr sect
        k (d + 1)
  ForeignCall _ MutableArray_read args
    | enabled fe "array", ka :<| ki :<| Empty <- argSources fe d args -> do
        genArrayOp fe d (mutableArrayWrap fe) ka (Just ki) Nothing instr sect
        k (d + 1)
  ForeignCall _ MutableArray_write args
    | enabled fe "array",
      ka :<| ki :<| kv :<| Empty <- argSources fe d args,
      Just unitIx <- Map.lookup (KeyEnum Ty.unitRef TT.unitTag) (envPool (feEnv fe)) -> do
        genArrayOp fe d (mutableArrayWrap fe) ka (Just ki) (Just (Pair kv unitIx)) instr sect
        k (d + 1)
  ForeignCall _ ImmutableArray_size args
    | enabled fe "array", ka :<| Empty <- argSources fe d args -> do
        genArrayOp fe d (lArrayWrap (envLayouts (feEnv fe))) ka Nothing Nothing instr sect
        k (d + 1)
  ForeignCall _ ImmutableArray_read args
    | enabled fe "array", ka :<| ki :<| Empty <- argSources fe d args -> do
        genArrayOp fe d (lArrayWrap (envLayouts (feEnv fe))) ka (Just ki) Nothing instr sect
        k (d + 1)
  -- the rest of the array builtins: C helpers (see "Arrays and Refs" in jit_rt.c)
  ForeignCall _ f args
    | enabled fe "array", Just op <- arrayForeign f -> do
        genArrayForeign fe d op (argSources fe d args) instr sect
        k (d + 1)
  -- Universal.murmurHashUntyped is Value.value (VALU) followed by the
  -- foreign hash of the reflected value. The two together are one C helper
  -- that hashes the closures directly ("Universal.murmurHashUntyped" in
  -- jit_rt.c); the reflected value's slot gets the original value, which
  -- nothing reads. A value the helper doesn't handle goes to the call-out
  -- for VALU, after which the foreign call is a call-out of its own.
  Prim1 VALU i
    | enabled fe "hash",
      Ins _ (Ins (ForeignCall _ Universal_murmurHashUntyped (VArg1 0)) rest) <- sect -> do
        slow <- callOutExit True fe d instr sect
        u <- loadU (d - i)
        b <- loadB (d - i)
        r <- fresh "pair"
        emit (r <> " = call { i64, i64 } @unison_jit_murmur(i64 " <> u <> ", ptr " <> b <> ")")
        ok <- fresh "ok"
        emit (ok <> " = extractvalue { i64, i64 } " <> r <> ", 0")
        miss <- fresh "miss"
        emit (miss <> " = icmp eq i64 " <> ok <> ", 0")
        branchIf "hash" miss slow
        h <- fresh "hash"
        emit (h <> " = extractvalue { i64, i64 } " <> r <> ", 1")
        storeU (d + 1) u
        storeB (d + 1) b
        result fe (d + 1) (feTagNat fe) h
        genSection fe (d + 2) rest
  -- Refs: Scope.ref, Ref.readForCas, Ticket.read and Ref.cas
  Prim1 REFN i | enabled fe "ref" -> do
    slow <- callOutExit True fe d instr sect
    u <- loadU (d - i)
    b <- loadB (d - i)
    requireTagged slow (D.singleton b)
    listHelper d slow ("@unison_jit_ref_new(ptr %ctx, i64 " <> u <> ", ptr " <> b <> ")")
    k (d + 1)
  Prim1 RRFC i | enabled fe "ref", not (envSandboxed (feEnv fe)) -> do
    slow <- callOutExit True fe d instr sect
    r <- loadB (d - i)
    listHelper d slow ("@unison_jit_ref_read_for_cas(ptr %ctx, ptr " <> r <> ")")
    k (d + 1)
  Prim1 TIKR i | enabled fe "ref" -> do
    let ls = envLayouts (feEnv fe)
    slow <- callOutExit True fe d instr sect
    p <- loadB (d - i)
    raw <- fresh "raw"
    emit (raw <> " = ptrtoint ptr " <> p <> " to i64")
    fo <- taggedObject "foreign" raw (lForeignInfo ls) slow
    wrapped <- loadAt fo "wrap" "i64" (lHeaderBytes ls)
    w <- taggedObjectTag "ticket" wrapped (lTicketWrap ls) slow
    valp <- loadAt w "val" "i64" (lHeaderBytes ls)
    Pair u b <- valFields fe valp slow
    storeU (d + 1) u
    storeB (d + 1) b
    k (d + 1)
  RefCAS i j kv | enabled fe "ref", not (envSandboxed (feEnv fe)) -> do
    slow <- callOutExit True fe d instr sect
    r <- loadB (d - i)
    t <- loadB (d - j)
    u <- loadU (d - kv)
    b <- loadB (d - kv)
    requireTagged slow (D.singleton b)
    res <- fresh "cas"
    emit (res <> " = call i64 @unison_jit_ref_cas(ptr %ctx, ptr " <> r <> ", ptr " <> t <> ", i64 " <> u <> ", ptr " <> b <> ")")
    miss <- fresh "miss"
    emit (miss <> " = icmp slt i64 " <> res <> ", 0")
    branchIf "cas" miss slow
    c <- fresh "c"
    emit (c <> " = icmp eq i64 " <> res <> ", 1")
    resultBool fe d c
    k (d + 1)
  Prim2 op i j | prim2Supported op -> do
    x <- loadU (d - i)
    y <- loadU (d - j)
    genPrim2 fe d op x y sect
    k (d + 1)
  _ -> genCallOut fe d instr sect

-- | The boolean in slot @k@ as an i1. One still held as an i1 costs
-- nothing; otherwise the closure must be an enumeration (else @slow@),
-- and it is true unless its tag is the false constructor's, which is how
-- the interpreter reads one.
loadBoolean :: FnEnv -> Int -> Text -> Gen Text
loadBoolean fe k slow =
  slotKind k >>= \case
    Just c -> pure c
    Nothing -> do
      let layout = lEnum (envLayouts (feEnv fe))
          PackedTag false = TT.falseTag
      p <- loadB k
      raw <- fresh "raw"
      emit (raw <> " = ptrtoint ptr " <> p <> " to i64")
      tagBits <- fresh "ptrtag"
      emit (tagBits <> " = and i64 " <> raw <> ", 7")
      notEnum <- fresh "notenum"
      emit (notEnum <> " = icmp ne i64 " <> tagBits <> ", " <> tshow (lPtrTag layout))
      branchIf "enum" notEnum slow
      base <- fresh "base"
      emit (base <> " = and i64 " <> raw <> ", -8")
      packed <- loadAt base "tag" "i64" (lFieldOffset layout (lPtrs layout))
      c <- fresh "bool"
      emit (c <> " = icmp ne i64 " <> packed <> ", " <> tshow false)
      pure c

-- | An instruction the generator has no code for: the interpreter runs
-- it, and native code continues after it in an auxiliary function (a
-- re-entry point). Not worth it when the instruction's results are
-- yielded straight away, or when the instruction reshapes the stack; the
-- interpreter then takes the whole section.
genCallOut :: FnEnv -> Int -> GInstr (RComb Val) -> MSection -> Gen ()
genCallOut fe d instr sect = do
  l <- callOutExit False fe d instr sect
  emit ("br label %" <> l)

-- | The exit block for leaving an instruction to the interpreter: a
-- call-out when one is possible, else a resume at the section. Also the
-- slow path of instructions with a native fast path (@slowPath@): the
-- code after a slow path is only needed when the fast path misses, so its
-- re-entry function can be left for later.
callOutExit :: Bool -> FnEnv -> Int -> GInstr (RComb Val) -> MSection -> Gen Text
callOutExit slowPath fe d instr sect = case (pushCount instr, sect) of
  (Just n, Ins _ rest)
    | enabled fe "callout", callOutWorthwhile instr rest ->
        genAuxFunction slowPath fe (d + n) (currentBase fe) rest >>= \case
          Nothing -> resume
          Just (Pair _ cell) -> exitBlock fe d (CallOut (feCix fe) instr rest n cell)
  _ -> resume
  where
    resume = exitBlock fe d (Resume (feCix fe) sect)

-- | How many values an instruction leaves on the stack, or Nothing for
-- one that changes the stack in other ways (captures and discards cut it
-- back to a mark). MCode doesn't record this, so the trampoline checks
-- the stack pointer against it before re-entering native code; a wrong
-- entry here costs a resume, not correctness. Every constructor is
-- listed so that a new instruction fails to compile until it is added.
pushCount :: GInstr comb -> Maybe Int
pushCount = \case
  Prim1 LOAD _ -> Just 2 -- a tag and the value
  Prim1 _ _ -> Just 1
  Prim2 TRCE _ _ -> Just 0
  Prim2 THRO _ _ -> Just 0 -- never returns
  Prim2 _ _ _ -> Just 1
  RefCAS {} -> Just 1
  ForeignCall {} -> Just 1
  DLLCall -> Just 1
  SetAff {} -> Just 0
  Capture _ -> Nothing
  Discard _ -> Nothing
  Name _ _ -> Just 1
  Info _ -> Just 0
  Pack {} -> Just 1
  Lit _ -> Just 1
  Print _ -> Just 0
  Reset {} -> Just 0
  InLocal _ -> Just 0
  Fork _ -> Just 1
  Atomically _ -> Just 1
  Seq _ -> Just 1
  TryForce _ -> Just 1
  SandboxingFailure _ -> Nothing
  KeepAlive _ -> Just 0
  NewForeignPtr _ _ -> Just 1
  AddFinalizer _ _ -> Just 0

-- | The slots a @Pack@'s arguments come from, in field order, when the
-- code generator can build the constructor (one or two fields so far).
packFields :: FnEnv -> Int -> Args -> Maybe (Deque Int)
packFields fe d args = case args of
  ZArgs -> Nothing
  _ -> Just (argSources fe d args)

-- | Branches to @slow@ if any of the boxed values is an untagged pointer.
-- The interpreter can leave one in a stack slot: a top-level constant such
-- as @noneClo@ stored as itself puts the static closure's address there,
-- which is untagged whether or not the CAF has been forced (the bang in
-- @bpoke@ evaluates it but stores the same pointer). Native code must not copy
-- such a pointer into a strict field (a @Val@'s, a constructor's), because
-- GHC's optimized code reads a strict field without evaluating it and
-- would take the thunk's words for the constructor's fields. Reads through
-- a pointer are guarded by the tag switch in 'genDMatchClosure'; this
-- guards the writes. (Found 2026-10-03: a @None@ from a call-out packed into
-- a pair crashed the GC in eager mode on the optimized build.)
requireTagged :: Text -> Deque Text -> Gen ()
requireTagged slow bs = do
  zs <- forM bs $ \b -> do
    raw <- fresh "raw"
    emit (raw <> " = ptrtoint ptr " <> b <> " to i64")
    t <- fresh "ptrtag"
    emit (t <> " = and i64 " <> raw <> ", 7")
    z <- fresh "untagged"
    emit (z <> " = icmp eq i64 " <> t <> ", 0")
    pure z
  case zs of
    Empty -> pure ()
    z0 :<| rest -> do
      anyZ <- foldM (\acc z -> do r <- fresh "untagged"; emit (r <> " = or i1 " <> acc <> ", " <> z); pure r) z0 rest
      branchIf "tagged" anyZ slow

-- | Allocates a constructor with the given fields and pushes it. The
-- reference comes from the pool entry for the type's enumeration
-- constructor 0 (its first field). Each Pack allocates separately, since
-- a run of instructions can exit between two Packs and words taken for a
-- later one would be left uninitialized in the nursery, which the debug
-- RTS's heap walker rejects. An untagged field value goes to @slow@.
genPack :: FnEnv -> Int -> Int -> PackedTag -> Deque Int -> Text -> Gen ()
genPack fe d refIx (PackedTag t) fields slow = do
  let env = feEnv fe
      ls = envLayouts env
      rts = envRts env
      n = D.size fields
      !(Pair layout ptrTag) = case n of
        1 -> Pair (lData1 ls) 3
        2 -> Pair (lData2 ls) 4
        _ -> Pair (lDataG ls) 5
      conWords = 1 + lPtrs layout + lNptrs layout
      -- a DataG also gets its two arrays and their boxes, all in one chunk
      cardWords = (n + (1 `shiftL` rCardBits rts) - 1) `shiftR` rCardBits rts
      cardWords' = (cardWords + 7) `div` 8
      bytesWords = rBytesHeader rts + n
      ptrsWords = rPtrsHeader rts + n + cardWords'
      boxWords = 2
      words
        | n <= 2 = conWords
        | otherwise = conWords + 2 * boxWords + bytesWords + ptrsWords
  -- the fields, before the allocation call so nothing is held across it
  vals <- loadSources fields
  requireTagged slow (fmap (\(Pair _ b) -> b) vals)
  -- the Reference: first field of the pool's Enum for this type
  usePool refIx
  ea <- fresh "enum.a"
  emit (ea <> " = getelementptr ptr, ptr %pool, i64 " <> tshow refIx)
  ep <- fresh "enum"
  emit (ep <> " = load ptr, ptr " <> ea)
  ei <- fresh "enum"
  emit (ei <> " = ptrtoint ptr " <> ep <> " to i64")
  eb <- fresh "enum.base"
  emit (eb <> " = and i64 " <> ei <> ", -8")
  ra <- fresh "ref.a"
  emit (ra <> " = add i64 " <> eb <> ", " <> tshow (lFieldOffset (lEnum ls) 0))
  rp <- fresh "ref.p"
  emit (rp <> " = inttoptr i64 " <> ra <> " to ptr")
  ref <- fresh "ref"
  emit (ref <> " = load ptr, ptr " <> rp)
  -- charge the budget and allocate
  obj <- allocWords env words
  let word i = do
        a <- fresh "w"
        emit (a <> " = getelementptr i64, ptr " <> obj <> ", i64 " <> tshow i)
        pure a
      payload i = word (1 + i) -- after the header
  hdr <- word 0
  emit ("store i64 " <> tshow (lInfo layout) <> ", ptr " <> hdr)
  refA <- payload 0
  emit ("store ptr " <> ref <> ", ptr " <> refA)
  tagA <- payload (lPtrs layout)
  emit ("store i64 " <> tshow t <> ", ptr " <> tagA)
  if n <= 2
    then iforM_ vals $ \j (Pair u b) -> do
      ba <- payload (1 + j)
      emit ("store ptr " <> b <> ", ptr " <> ba)
      ua <- payload (lPtrs layout + 1 + j)
      emit ("store i64 " <> u <> ", ptr " <> ua)
    else do
      -- layout of the chunk after the constructor: ByteArray box, Array
      -- box, ByteArray#, Array#. The arrays hold the fields in reverse:
      -- element n-1-j is field j.
      let ubox = conWords
          bbox = ubox + boxWords
          ubytes = bbox + boxWords
          bptrs = ubytes + bytesWords
          storeAt i what = do
            a <- word i
            emit ("store " <> what <> ", ptr " <> a)
          addrOf i = do
            a <- word i
            v <- fresh "addr"
            emit (v <> " = ptrtoint ptr " <> a <> " to i64")
            pure v
          tagged i tg = do
            v <- addrOf i
            tv <- fresh "tagged"
            emit (tv <> " = or i64 " <> v <> ", " <> tshow (tg :: Int))
            tp <- fresh "tagged"
            emit (tp <> " = inttoptr i64 " <> tv <> " to ptr")
            pure tp
      -- the ByteArray#
      storeAt ubytes ("i64 " <> tshow (rArrWordsInfo rts))
      storeAt (ubytes + rBytesCount rts) ("i64 " <> tshow (8 * n))
      -- the Array#: element count, then payload size including the card table
      storeAt bptrs ("i64 " <> tshow (rArrPtrsInfo rts))
      storeAt (bptrs + rPtrsCount rts) ("i64 " <> tshow n)
      storeAt (bptrs + rPtrsSize rts) ("i64 " <> tshow (n + cardWords'))
      forM_ [0 .. cardWords' - 1] $ \c -> storeAt (bptrs + rPtrsHeader rts + n + c) "i64 0"
      iforM_ vals $ \j (Pair u b) -> do
        storeAt (ubytes + rBytesHeader rts + (n - 1 - j)) ("i64 " <> u)
        storeAt (bptrs + rPtrsHeader rts + (n - 1 - j)) ("ptr " <> b)
      -- the boxes, each pointing at its array
      ua <- word ubytes
      storeAt ubox ("i64 " <> tshow (lByteArrayBoxInfo ls))
      storeAt (ubox + 1) ("ptr " <> ua)
      ba <- word bptrs
      storeAt bbox ("i64 " <> tshow (lArrayBoxInfo ls))
      storeAt (bbox + 1) ("ptr " <> ba)
      -- and the constructor's two Seg fields point at the boxes (tag 1)
      ubp <- tagged ubox 1
      bbp <- tagged bbox 1
      storeAt (1 + 1) ("ptr " <> ubp)
      storeAt (1 + 2) ("ptr " <> bbp)
  -- the result is the tagged pointer
  oi <- fresh "obj"
  emit (oi <> " = ptrtoint ptr " <> obj <> " to i64")
  ti <- fresh "tagged"
  emit (ti <> " = or i64 " <> oi <> ", " <> tshow (ptrTag :: Int))
  tp <- fresh "tagged"
  emit (tp <> " = inttoptr i64 " <> ti <> " to ptr")
  storeU (d + 1) "-1"
  storeB (d + 1) tp

-- | Loads through a Ref: the closure in slot @k@ must be @Foreign
-- (WrapIORef ref)@, checked by info pointer, and the MutVar#'s content an
-- evaluated @Val@ (pointer tag 1). Anything else branches to @slow@.
-- Gives the MutVar# address and the Val's fields.
refFields :: FnEnv -> Int -> Text -> Gen (Triple Text Text Text)
refFields fe k slow = do
  let ls = envLayouts (feEnv fe)
      rts = envRts (feEnv fe)
      check what c = do
        l <- freshLabel what
        emit ("br i1 " <> c <> ", label %" <> slow <> ", label %" <> l <> unlikely)
        startBlock l
      loadFrom from what ty off = do
        a <- fresh (what <> ".a")
        emit (a <> " = add i64 " <> from <> ", " <> tshow off)
        pp <- fresh (what <> ".p")
        emit (pp <> " = inttoptr i64 " <> a <> " to ptr")
        v <- fresh what
        emit (v <> " = load " <> ty <> ", ptr " <> pp)
        pure v
      -- an object with pointer tag 7 and the given info pointer; gives its untagged address
      object what raw info = do
        tagBits <- fresh "ptrtag"
        emit (tagBits <> " = and i64 " <> raw <> ", 7")
        notTag <- fresh "nottag"
        emit (notTag <> " = icmp ne i64 " <> tagBits <> ", 7")
        check what notTag
        base <- fresh what
        emit (base <> " = and i64 " <> raw <> ", -8")
        i <- loadFrom base (what <> ".info") "i64" (0 :: Int)
        notInfo <- fresh "notinfo"
        emit (notInfo <> " = icmp ne i64 " <> i <> ", " <> tshow info)
        check what notInfo
        pure base
  p <- loadB k
  raw <- fresh "raw"
  emit (raw <> " = ptrtoint ptr " <> p <> " to i64")
  fo <- object "foreign" raw (lForeignInfo ls)
  wrapped <- loadFrom fo "wrap" "i64" (lHeaderBytes ls)
  w <- object "ioref" wrapped (lIORefInfo ls)
  mv <- loadFrom w "mutvar" "i64" (lHeaderBytes ls)
  valp <- loadFrom mv "val" "i64" (8 * rMutVarVar rts)
  vtag <- fresh "ptrtag"
  emit (vtag <> " = and i64 " <> valp <> ", 7")
  notVal <- fresh "notval"
  emit (notVal <> " = icmp ne i64 " <> vtag <> ", " <> tshow (lPtrTag (lVal ls)))
  check "val" notVal
  vbase <- fresh "val"
  emit (vbase <> " = and i64 " <> valp <> ", -8")
  b <- loadFrom vbase "vb" "ptr" (lFieldOffset (lVal ls) 0)
  u <- loadFrom vbase "vu" "i64" (lFieldOffset (lVal ls) 1)
  pure (Triple mv u b)

-- | @Ref.read@: the Val's fields go to the result slot.
genRefRead :: FnEnv -> Int -> Int -> GInstr (RComb Val) -> MSection -> Gen ()
genRefRead fe d k instr sect = do
  slow <- callOutExit True fe d instr sect
  Triple _ u b <- refFields fe k slow
  storeU (d + 1) u
  storeB (d + 1) b

-- | @Ref.write@: a fresh @Val@ holding the value, stored through the
-- runtime's barrier; the result is unit.
genRefWrite :: FnEnv -> Int -> Int -> Int -> Int -> GInstr (RComb Val) -> MSection -> Gen ()
genRefWrite fe d k kv unitIx instr sect = do
  let env = feEnv fe
      ls = envLayouts env
      layout = lVal ls
      words = 1 + lPtrs layout + lNptrs layout
  slow <- callOutExit True fe d instr sect
  -- the value, before anything else: the allocation call is a GC-safe
  -- point only for what is on the Unison stack, not for registers
  u <- loadU kv
  b <- loadB kv
  requireTagged slow (D.singleton b)
  Triple mv _ _ <- refFields fe k slow
  obj <- allocWords env words
  hdr <- fresh "w"
  emit (hdr <> " = getelementptr i64, ptr " <> obj <> ", i64 0")
  emit ("store i64 " <> tshow (lInfo layout) <> ", ptr " <> hdr)
  ba <- fresh "w"
  emit (ba <> " = getelementptr i64, ptr " <> obj <> ", i64 1")
  emit ("store ptr " <> b <> ", ptr " <> ba)
  ua <- fresh "w"
  emit (ua <> " = getelementptr i64, ptr " <> obj <> ", i64 2")
  emit ("store i64 " <> u <> ", ptr " <> ua)
  oi <- fresh "obj"
  emit (oi <> " = ptrtoint ptr " <> obj <> " to i64")
  ti <- fresh "tagged"
  emit (ti <> " = or i64 " <> oi <> ", " <> tshow (lPtrTag layout))
  tp <- fresh "tagged"
  emit (tp <> " = inttoptr i64 " <> ti <> " to ptr")
  mvp <- fresh "mutvar.p"
  emit (mvp <> " = inttoptr i64 " <> mv <> " to ptr")
  emit ("call void @unison_jit_write_mutvar(ptr %ctx, ptr " <> mvp <> ", ptr " <> tp <> ")")
  poolConstant fe d unitIx

-- | Universal comparison (@==@, @<=@, @<@, @compare@ on any type). The
-- fast path: both values unboxed with the same type-tag closure, which is
-- how @Val@'s instances compare them (a word comparison, signed for Int,
-- unsigned for Nat, bitwise for Float and Char). Anything else, including
-- an Int against a Nat, is left to the interpreter through the call-out.
genUniversal :: FnEnv -> Int -> Prim2 -> Int -> Int -> GInstr (RComb Val) -> MSection -> Gen ()
genUniversal fe d op ki kj instr sect = do
  slow <- callOutExit True fe d instr sect
  bi <- loadB ki
  bj <- loadB kj
  ui <- loadU ki
  uj <- loadU kj
  same <- fresh "same"
  emit (same <> " = icmp eq ptr " <> bi <> ", " <> bj)
  let isTag name = do
        c <- fresh "istag"
        emit (c <> " = icmp eq ptr " <> bi <> ", " <> name)
        pure c
  isNat <- isTag (feTagNat fe)
  isInt <- isTag (feTagInt fe)
  isOther <- case op of
    EQLU -> do
      c1 <- isTag (feTagChar fe)
      c2 <- isTag (feTagFloat fe)
      o <- fresh "istag"
      emit (o <> " = or i1 " <> c1 <> ", " <> c2)
      pure o
    _ -> pure "false"
  num <- fresh "isnum"
  emit (num <> " = or i1 " <> isNat <> ", " <> isInt)
  known <- fresh "known"
  emit (known <> " = or i1 " <> num <> ", " <> isOther)
  fast <- fresh "fast"
  emit (fast <> " = and i1 " <> same <> ", " <> known)
  go <- freshLabel "univ"
  -- which kinds of rope the helpers are to take: texts, bytes
  let kinds = (if enabled fe "text" then 1 else 0) + (if enabled fe "bytes" then 2 else 0) :: Int
      isCmp = op == CMPU
      deliver c = if isCmp then result fe d (feTagInt fe) c else resultBool fe d c
  if kinds /= 0
    then do
      -- two texts or two bytes are compared by a helper; anything else
      -- that isn't a pair of numbers goes to the interpreter
      feq <- freshLabel "feq"
      emit ("br i1 " <> fast <> ", label %" <> go <> ", label %" <> feq <> likely)
      startBlock feq
      r <- fresh "cmp"
      let !(Pair helper missVal) = if op == EQLU then Pair "unison_jit_foreign_eq" "-1" else Pair "unison_jit_foreign_cmp" "2"
      emit (r <> " = call i64 @" <> helper <> "(ptr " <> bi <> ", ptr " <> bj <> ", i64 " <> tshow kinds <> ")")
      miss <- fresh "miss"
      emit (miss <> " = icmp eq i64 " <> r <> ", " <> missVal)
      ok <- freshLabel "feq.ok"
      emit ("br i1 " <> miss <> ", label %" <> slow <> ", label %" <> ok <> unlikely)
      startBlock ok
      c2 <- case op of
        EQLU -> cmp "ne" r "0"
        LEQU -> cmp "sle" r "0"
        LESU -> cmp "slt" r "0"
        _ -> pure r
      join <- freshLabel "univ.join"
      emit ("br label %" <> join)
      startBlock go
      c1 <- genUniversalValue op isInt ui uj
      emit ("br label %" <> join)
      startBlock join
      c <- fresh "c"
      emit (c <> " = phi " <> (if isCmp then "i64" else "i1") <> " [ " <> c1 <> ", %" <> go <> " ], [ " <> c2 <> ", %" <> ok <> " ]")
      deliver c
    else do
      emit ("br i1 " <> fast <> ", label %" <> go <> ", label %" <> slow <> likely)
      startBlock go
      genUniversalValue op isInt ui uj >>= deliver

-- | The comparison of two words of the same numeric type: an i1 for the
-- boolean operations, the Int -1, 0 or 1 for compare.
genUniversalValue :: Prim2 -> Text -> Text -> Text -> Gen Text
genUniversalValue op isInt ui uj = do
  let signedUnsigned s u = do
        cs <- cmp s ui uj
        cu <- cmp u ui uj
        r <- fresh "c"
        emit (r <> " = select i1 " <> isInt <> ", i1 " <> cs <> ", i1 " <> cu)
        pure r
  case op of
    EQLU -> cmp "eq" ui uj
    LEQU -> signedUnsigned "sle" "ule"
    LESU -> signedUnsigned "slt" "ult"
    _ -> do
      lt <- signedUnsigned "slt" "ult"
      eq <- cmp "eq" ui uj
      a <- fresh "r"
      emit (a <> " = select i1 " <> eq <> ", i64 0, i64 1")
      r <- fresh "r"
      emit (r <> " = select i1 " <> lt <> ", i64 -1, i64 " <> a)
      pure r

-- | An object with pointer tag 7 and the given info pointer, else branch
-- to @slow@; gives the untagged address.
taggedObject :: Text -> Text -> Int -> Text -> Gen Text
taggedObject what raw info slow = taggedObjectTag what raw (Pair info 7) slow

-- | The wrapper of a mutable array: its info pointer, and pointer tag 7.
mutableArrayWrap :: FnEnv -> Pair Int Int
mutableArrayWrap fe = Pair (lMutableArrayInfo (envLayouts (feEnv fe))) 7

-- | As 'taggedObject', for a constructor with the given (info, pointer tag).
taggedObjectTag :: Text -> Text -> Pair Int Int -> Text -> Gen Text
taggedObjectTag what raw (Pair info tag) slow = do
  tagBits <- fresh "ptrtag"
  emit (tagBits <> " = and i64 " <> raw <> ", 7")
  notTag <- fresh "nottag"
  emit (notTag <> " = icmp ne i64 " <> tagBits <> ", " <> tshow tag)
  branchIf what notTag slow
  base <- fresh what
  emit (base <> " = and i64 " <> raw <> ", -8")
  ip <- fresh (what <> ".info.p")
  emit (ip <> " = inttoptr i64 " <> base <> " to ptr")
  i <- fresh (what <> ".info")
  emit (i <> " = load i64, ptr " <> ip)
  notInfo <- fresh "notinfo"
  emit (notInfo <> " = icmp ne i64 " <> i <> ", " <> tshow info)
  branchIf what notInfo slow
  pure base

-- | Branches to @slow@ if the condition holds, else continues in a new block.
branchIf :: Text -> Text -> Text -> Gen ()
branchIf what c slow = do
  l <- freshLabel what
  emit ("br i1 " <> c <> ", label %" <> slow <> ", label %" <> l <> unlikely)
  startBlock l

-- | A load of the given type at a byte offset from an address held in an i64.
loadAt :: Text -> Text -> Text -> Int -> Gen Text
loadAt from what ty off = do
  a <- fresh (what <> ".a")
  emit (a <> " = add i64 " <> from <> ", " <> tshow off)
  pp <- fresh (what <> ".p")
  emit (pp <> " = inttoptr i64 " <> a <> " to ptr")
  v <- fresh what
  emit (v <> " = load " <> ty <> ", ptr " <> pp)
  pure v

-- | The fields of the evaluated @Val@ at the tagged address (else @slow@).
valFields :: FnEnv -> Text -> Text -> Gen (Pair Text Text)
valFields fe valp slow = do
  let ls = envLayouts (feEnv fe)
  vtag <- fresh "ptrtag"
  emit (vtag <> " = and i64 " <> valp <> ", 7")
  notVal <- fresh "notval"
  emit (notVal <> " = icmp ne i64 " <> vtag <> ", " <> tshow (lPtrTag (lVal ls)))
  branchIf "val" notVal slow
  vbase <- fresh "val"
  emit (vbase <> " = and i64 " <> valp <> ", -8")
  b <- loadAt vbase "vb" "ptr" (lFieldOffset (lVal ls) 0)
  u <- loadAt vbase "vu" "i64" (lFieldOffset (lVal ls) 1)
  pure (Pair u b)

-- | Allocates a @Val@ holding the value in slot @kv@; gives its tagged
-- address. The value is read before the allocation call; an untagged
-- boxed value goes to @slow@.
allocVal :: FnEnv -> Int -> Text -> Gen Text
allocVal fe kv slow = do
  let env = feEnv fe
      layout = lVal (envLayouts env)
      words = 1 + lPtrs layout + lNptrs layout
  u <- loadU kv
  b <- loadB kv
  requireTagged slow (D.singleton b)
  obj <- allocWords env words
  hdr <- fresh "w"
  emit (hdr <> " = getelementptr i64, ptr " <> obj <> ", i64 0")
  emit ("store i64 " <> tshow (lInfo layout) <> ", ptr " <> hdr)
  ba <- fresh "w"
  emit (ba <> " = getelementptr i64, ptr " <> obj <> ", i64 1")
  emit ("store ptr " <> b <> ", ptr " <> ba)
  ua <- fresh "w"
  emit (ua <> " = getelementptr i64, ptr " <> obj <> ", i64 2")
  emit ("store i64 " <> u <> ", ptr " <> ua)
  oi <- fresh "obj"
  emit (oi <> " = ptrtoint ptr " <> obj <> " to i64")
  ti <- fresh "tagged"
  emit (ti <> " = or i64 " <> oi <> ", " <> tshow (lPtrTag layout))
  tp <- fresh "tagged"
  emit (tp <> " = inttoptr i64 " <> ti <> " to ptr")
  pure tp

-- | @MutableArray.size@, @read@ and @write@ (the last two given an index
-- slot, write also a value slot and the pool index of unit). The closure
-- must be @Foreign (WrapMutableArray arr)@ and the index in bounds;
-- otherwise the call-out runs the foreign call, which raises the error.
-- A write does what compiled Haskell does: store, set the dirty info
-- pointer, mark the card.
genArrayOp :: FnEnv -> Int -> Pair Int Int -> Int -> Maybe Int -> Maybe (Pair Int Int) -> GInstr (RComb Val) -> MSection -> Gen ()
genArrayOp fe d wrap ka mki mwrite instr sect = do
  let ls = envLayouts (feEnv fe)
      rts = envRts (feEnv fe)
  slow <- callOutExit True fe d instr sect
  -- a value to write is boxed up front, before anything is loaded from the heap
  newVal <- traverse (\(Pair kv _) -> allocVal fe kv slow) mwrite
  p <- loadB ka
  raw <- fresh "raw"
  emit (raw <> " = ptrtoint ptr " <> p <> " to i64")
  fo <- taggedObject "foreign" raw (lForeignInfo ls) slow
  wrapped <- loadAt fo "wrap" "i64" (lHeaderBytes ls)
  w <- taggedObjectTag "array" wrapped wrap slow
  arr <- loadAt w "arr" "i64" (lHeaderBytes ls)
  count <- loadAt arr "count" "i64" (8 * rPtrsCount rts)
  case mki of
    Nothing -> result fe d (feTagNat fe) count
    Just ki -> do
      ix <- loadU ki
      oob <- fresh "oob"
      emit (oob <> " = icmp uge i64 " <> ix <> ", " <> count)
      branchIf "inbounds" oob slow
      ea <- fresh "elem.a"
      emit (ea <> " = add i64 " <> ix <> ", " <> tshow (rPtrsHeader rts))
      arrP <- fresh "arr.p"
      emit (arrP <> " = inttoptr i64 " <> arr <> " to ptr")
      ep <- fresh "elem.p"
      emit (ep <> " = getelementptr i64, ptr " <> arrP <> ", i64 " <> ea)
      case (mwrite, newVal) of
        (Just (Pair _ unitIx), Just v) -> do
          emit ("store ptr " <> v <> ", ptr " <> ep)
          emit ("store i64 " <> tshow (rArrPtrsDirtyInfo rts) <> ", ptr " <> arrP)
          -- the card table follows the elements; one byte per 2^cardBits elements
          cardIx <- fresh "card"
          emit (cardIx <> " = lshr i64 " <> ix <> ", " <> tshow (rCardBits rts))
          cardsA <- fresh "cards"
          emit (cardsA <> " = add i64 " <> count <> ", " <> tshow (rPtrsHeader rts))
          cardsP <- fresh "cards.p"
          emit (cardsP <> " = getelementptr i64, ptr " <> arrP <> ", i64 " <> cardsA)
          cardP <- fresh "card.p"
          emit (cardP <> " = getelementptr i8, ptr " <> cardsP <> ", i64 " <> cardIx)
          emit ("store i8 1, ptr " <> cardP)
          poolConstant fe d unitIx
        _ -> do
          valp <- fresh "val"
          emit (valp <> " = load i64, ptr " <> ep)
          Pair u b <- valFields fe valp slow
          storeU (d + 1) u
          storeB (d + 1) b

-- | The array and ref builtins done by C helpers, and what each is.
data ArrayOp
  = -- | size of a byte array (kind 0 mutable, 1 immutable)
    BSize Int
  | -- | read of width bytes, big-endian?, from a byte array of this kind
    BRead Int Bool Int
  | -- | write of width bytes, big-endian?
    BWrite Int Bool
  | -- | copyTo! between pointer arrays (source kind) or byte arrays
    PCopy Int
  | BCopy Int
  | -- | freeze! (True) or freeze of a slice
    PFreeze Bool
  | BFreeze Bool
  | ToBytes
  | FromBytes
  | -- | a new pointer array, with an initial value or the empty one
    PNew Bool
  | -- | a new byte array: filled from an argument?, pinned?
    BNew Bool Bool
  | -- | PinnedByteArray.cast: the same wrapper
    Cast
  | -- | PinnedByteArray.contents
    Contents

arrayForeign :: ForeignFunc -> Maybe ArrayOp
arrayForeign = \case
  MutableByteArray_size -> Just (BSize 0)
  ImmutableByteArray_size -> Just (BSize 1)
  MutableByteArray_read8 -> Just (BRead 1 True 0)
  MutableByteArray_read16be -> Just (BRead 2 True 0)
  MutableByteArray_read24be -> Just (BRead 3 True 0)
  MutableByteArray_read32be -> Just (BRead 4 True 0)
  MutableByteArray_read40be -> Just (BRead 5 True 0)
  MutableByteArray_read64be -> Just (BRead 8 True 0)
  MutableByteArray_read16le -> Just (BRead 2 False 0)
  MutableByteArray_read24le -> Just (BRead 3 False 0)
  MutableByteArray_read32le -> Just (BRead 4 False 0)
  MutableByteArray_read40le -> Just (BRead 5 False 0)
  MutableByteArray_read64le -> Just (BRead 8 False 0)
  ImmutableByteArray_read8 -> Just (BRead 1 True 1)
  ImmutableByteArray_read16be -> Just (BRead 2 True 1)
  ImmutableByteArray_read24be -> Just (BRead 3 True 1)
  ImmutableByteArray_read32be -> Just (BRead 4 True 1)
  ImmutableByteArray_read40be -> Just (BRead 5 True 1)
  ImmutableByteArray_read64be -> Just (BRead 8 True 1)
  ImmutableByteArray_read16le -> Just (BRead 2 False 1)
  ImmutableByteArray_read24le -> Just (BRead 3 False 1)
  ImmutableByteArray_read32le -> Just (BRead 4 False 1)
  ImmutableByteArray_read40le -> Just (BRead 5 False 1)
  ImmutableByteArray_read64le -> Just (BRead 8 False 1)
  MutableByteArray_write8 -> Just (BWrite 1 True)
  MutableByteArray_write16be -> Just (BWrite 2 True)
  MutableByteArray_write32be -> Just (BWrite 4 True)
  MutableByteArray_write64be -> Just (BWrite 8 True)
  MutableByteArray_write16le -> Just (BWrite 2 False)
  MutableByteArray_write32le -> Just (BWrite 4 False)
  MutableByteArray_write64le -> Just (BWrite 8 False)
  MutableArray_copyTo_force -> Just (PCopy 0)
  ImmutableArray_copyTo_force -> Just (PCopy 1)
  MutableByteArray_copyTo_force -> Just (BCopy 0)
  ImmutableByteArray_copyTo_force -> Just (BCopy 1)
  MutableArray_freeze_force -> Just (PFreeze True)
  MutableArray_freeze -> Just (PFreeze False)
  MutableByteArray_freeze_force -> Just (BFreeze True)
  MutableByteArray_freeze -> Just (BFreeze False)
  ImmutableByteArray_toBytes -> Just ToBytes
  ImmutableByteArray_fromBytes -> Just FromBytes
  Scope_array -> Just (PNew False)
  IO_array -> Just (PNew False)
  Scope_arrayOf -> Just (PNew True)
  IO_arrayOf -> Just (PNew True)
  Scope_bytearray -> Just (BNew False False)
  IO_bytearray -> Just (BNew False False)
  Scope_bytearrayOf -> Just (BNew True False)
  IO_bytearrayOf -> Just (BNew True False)
  Scope_pinnedByteArray -> Just (BNew False True)
  IO_pinnedByteArray -> Just (BNew False True)
  Scope_pinnedByteArrayOf -> Just (BNew True True)
  IO_pinnedByteArrayOf -> Just (BNew True True)
  PinnedByteArray_cast -> Just Cast
  PinnedByteArray_contents -> Just Contents
  _ -> Nothing

-- | The foreign functions whose result is unit (they need the constant).
unitForeign :: [ForeignFunc]
unitForeign = [f | f <- [minBound .. maxBound], Just op <- [arrayForeign f], givesUnit op]
  where
    givesUnit = \case
      BWrite {} -> True
      PCopy _ -> True
      BCopy _ -> True
      _ -> False

-- | Generates one of the array builtins above from its argument slots. A
-- helper that says "not handled" (a wrong closure, an index out of bounds,
-- which the interpreter then reports) sends the code to the call-out.
genArrayForeign :: FnEnv -> Int -> ArrayOp -> Deque Int -> GInstr (RComb Val) -> MSection -> Gen ()
genArrayForeign fe d op srcs instr sect = do
  slow <- callOutExit True fe d instr sect
  let ls = envLayouts (feEnv fe)
      bool b = if b then "1" else "0"
      unitIx = Map.lookup (KeyEnum Ty.unitRef TT.unitTag) (envPool (feEnv fe))
      unitResult call = case unitIx of
        Just ix -> unitHelper fe d slow ix call
        Nothing -> emit ("br label %" <> slow) >> startBlock "unreachable.unit" >> pure ()
  case (op, srcs) of
    (BSize kind, ka :<| Empty) -> do
      Pair _ size <- byteArray fe slow ka kind
      result fe d (feTagNat fe) size
    (BRead width be kind, ka :<| ki :<| Empty) -> do
      Pair arr size <- byteArray fe slow ka kind
      i <- loadU ki
      p <- byteArrayElem fe slow arr size i width
      let ity = "i" <> tshow (8 * width)
      raw <- fresh "raw"
      emit (raw <> " = load " <> ity <> ", ptr " <> p <> ", align 1")
      v <-
        if width == 8
          then if be then bswap "i64" raw else pure raw
          else do
            -- an odd width has no bswap of its own: swap the 64-bit extension
            -- and shift the bytes down
            wide <- fresh "wide"
            emit (wide <> " = zext " <> ity <> " " <> raw <> " to i64")
            if not be
              then pure wide
              else do
                swapped <- bswap "i64" wide
                binop "lshr" swapped (tshow (64 - 8 * width))
      result fe d (feTagNat fe) v
    (BWrite width be, ka :<| ki :<| kv :<| Empty) | Just ux <- unitIx -> do
      Pair arr size <- byteArray fe slow ka 0
      i <- loadU ki
      v <- loadU kv
      p <- byteArrayElem fe slow arr size i width
      let ity = "i" <> tshow (8 * width)
          trunc x = do
            n <- fresh "narrow"
            emit (n <> " = trunc i64 " <> x <> " to " <> ity)
            pure n
      -- the low `width` bytes of the value, swapped to the front for big-endian
      narrow <- case (be, width) of
        (False, 8) -> pure v
        (False, _) -> trunc v
        (True, 8) -> bswap "i64" v
        (True, _) -> bswap "i64" v >>= \sw -> binop "lshr" sw (tshow (64 - 8 * width)) >>= trunc
      emit ("store " <> ity <> " " <> narrow <> ", ptr " <> p <> ", align 1")
      poolConstant fe d ux
    (PCopy kind, kd :<| kdo :<| ks :<| kso :<| kl :<| Empty) -> do
      dst <- loadB kd
      doff <- loadU kdo
      src <- loadB ks
      soff <- loadU kso
      l <- loadU kl
      unitResult ("@unison_jit_parray_copy(ptr " <> dst <> ", i64 " <> doff <> ", ptr " <> src <> ", i64 " <> soff <> ", i64 " <> l <> ", i64 " <> tshow kind <> ")")
    (BCopy kind, kd :<| kdo :<| ks :<| kso :<| kl :<| Empty) -> do
      dst <- loadB kd
      doff <- loadU kdo
      src <- loadB ks
      soff <- loadU kso
      l <- loadU kl
      unitResult ("@unison_jit_barray_copy(ptr " <> dst <> ", i64 " <> doff <> ", ptr " <> src <> ", i64 " <> soff <> ", i64 " <> l <> ", i64 " <> tshow kind <> ")")
    (PFreeze True, ka :<| Empty) -> do
      a <- loadB ka
      listHelper d slow ("@unison_jit_parray_freeze(ptr %ctx, ptr " <> a <> ", i64 0, i64 0, i64 1)")
    (PFreeze False, ka :<| ko :<| kl :<| Empty) -> do
      a <- loadB ka
      off <- loadU ko
      l <- loadU kl
      listHelper d slow ("@unison_jit_parray_freeze(ptr %ctx, ptr " <> a <> ", i64 " <> off <> ", i64 " <> l <> ", i64 0)")
    (BFreeze True, ka :<| Empty) -> do
      a <- loadB ka
      listHelper d slow ("@unison_jit_barray_freeze(ptr %ctx, ptr " <> a <> ", i64 0, i64 0, i64 1)")
    (BFreeze False, ka :<| ko :<| kl :<| Empty) -> do
      a <- loadB ka
      off <- loadU ko
      l <- loadU kl
      listHelper d slow ("@unison_jit_barray_freeze(ptr %ctx, ptr " <> a <> ", i64 " <> off <> ", i64 " <> l <> ", i64 0)")
    (ToBytes, ka :<| ko :<| kl :<| Empty) -> do
      a <- loadB ka
      off <- loadU ko
      l <- loadU kl
      listHelper d slow ("@unison_jit_barray_to_bytes(ptr %ctx, ptr " <> a <> ", i64 " <> off <> ", i64 " <> l <> ")")
    (FromBytes, kb :<| Empty) -> do
      b <- loadB kb
      listHelper d slow ("@unison_jit_barray_from_bytes(ptr %ctx, ptr " <> b <> ")")
    (PNew False, kn :<| Empty) -> do
      n <- loadU kn
      listHelper d slow ("@unison_jit_parray_new(ptr %ctx, i64 " <> n <> ", i64 0, i64 0, ptr null)")
    (PNew True, kv :<| kn :<| Empty) -> do
      u <- loadU kv
      b <- loadB kv
      requireTagged slow (D.singleton b)
      n <- loadU kn
      listHelper d slow ("@unison_jit_parray_new(ptr %ctx, i64 " <> n <> ", i64 1, i64 " <> u <> ", ptr " <> b <> ")")
    (BNew False pinned, kn :<| Empty) -> do
      n <- loadU kn
      listHelper d slow ("@unison_jit_barray_new(ptr %ctx, i64 " <> n <> ", i64 -1, i64 " <> bool pinned <> ")")
    (BNew True pinned, kf :<| kn :<| Empty) -> do
      f <- loadU kf
      -- the fill is a Word8: the low byte of the Nat
      byte <- fresh "fill"
      emit (byte <> " = and i64 " <> f <> ", 255")
      n <- loadU kn
      listHelper d slow ("@unison_jit_barray_new(ptr %ctx, i64 " <> n <> ", i64 " <> byte <> ", i64 " <> bool pinned <> ")")
    (Cast, ka :<| Empty) -> do
      -- the same closure, once it is known to be a mutable byte array
      p <- loadB ka
      raw <- fresh "raw"
      emit (raw <> " = ptrtoint ptr " <> p <> " to i64")
      fo <- taggedObject "foreign" raw (lForeignInfo ls) slow
      wrapped <- loadAt fo "wrap" "i64" (lHeaderBytes ls)
      _ <- taggedObjectTag "mbarray" wrapped (lMutableByteArrayWrap ls) slow
      storeU (d + 1) "-1"
      storeB (d + 1) p
    (Contents, ka :<| Empty) -> do
      a <- loadB ka
      listHelper d slow ("@unison_jit_barray_contents(ptr %ctx, ptr " <> a <> ")")
    _ -> do
      -- an argument shape the generator doesn't expect: the interpreter's
      emit ("br label %" <> slow)
      startBlock "unreachable.array"

-- | The @ByteArray#@ (kind 1) or @MutableByteArray#@ (kind 0) wrapped by the
-- closure in the slot, as its untagged address and its byte count; anything
-- else (a closure that isn't @Foreign (Wrap arr)@ for the right wrapper)
-- goes to @slow@. What the C helpers' @barray_of@ does, as generated code.
byteArray :: FnEnv -> Text -> Int -> Int -> Gen (Pair Text Text)
byteArray fe slow ka kind = do
  let ls = envLayouts (feEnv fe)
      rts = envRts (feEnv fe)
      wrap = if kind == 0 then lMutableByteArrayWrap ls else lByteArrayWrap ls
  p <- loadB ka
  raw <- fresh "raw"
  emit (raw <> " = ptrtoint ptr " <> p <> " to i64")
  fo <- taggedObject "foreign" raw (lForeignInfo ls) slow
  wrapped <- loadAt fo "wrap" "i64" (lHeaderBytes ls)
  w <- taggedObjectTag "barray" wrapped wrap slow
  arr <- loadAt w "arr" "i64" (lHeaderBytes ls)
  size <- loadAt arr "bytes" "i64" (8 * rBytesCount rts)
  pure (Pair arr size)

-- | A pointer to byte @i@ of the array, once @width@ bytes from there are
-- within its @size@ bytes (else @slow@). The check is the one the Haskell
-- primitives make: @i + width > size@ or, should that sum wrap, @i > size@.
byteArrayElem :: FnEnv -> Text -> Text -> Text -> Text -> Int -> Gen Text
byteArrayElem fe slow arr size i width = do
  let rts = envRts (feEnv fe)
  end <- binop "add" i (tshow width)
  past <- fresh "past"
  emit (past <> " = icmp ugt i64 " <> end <> ", " <> size)
  wrapped <- fresh "wrapped"
  emit (wrapped <> " = icmp ugt i64 " <> i <> ", " <> size)
  oob <- fresh "oob"
  emit (oob <> " = or i1 " <> past <> ", " <> wrapped)
  branchIf "inbounds" oob slow
  arrP <- fresh "arr.p"
  emit (arrP <> " = inttoptr i64 " <> arr <> " to ptr")
  off <- binop "add" i (tshow (8 * rBytesHeader rts))
  ep <- fresh "elem.p"
  emit (ep <> " = getelementptr i8, ptr " <> arrP <> ", i64 " <> off)
  pure ep

-- | @llvm.bswap@ of the value, at the given integer type.
bswap :: Text -> Text -> Gen Text
bswap ty v = do
  r <- fresh "swapped"
  emit (r <> " = call " <> ty <> " @llvm.bswap." <> ty <> "(" <> ty <> " " <> v <> ")")
  pure r

-- | Calls a helper that returns 1 when it did the job and gives unit; the
-- call-out otherwise.
unitHelper :: FnEnv -> Int -> Text -> Int -> Text -> Gen ()
unitHelper fe d slow unitIx call = do
  r <- fresh "ok"
  emit (r <> " = call i64 " <> call)
  miss <- fresh "miss"
  emit (miss <> " = icmp eq i64 " <> r <> ", 0")
  branchIf "helper" miss slow
  poolConstant fe d unitIx

-- | The call that builds a partial application of the closure @f@ with up
-- to four more arguments (given as unboxed and boxed halves).
nameCall :: Text -> Deque (Pair Text Text) -> Text
nameCall f vals =
  "@unison_jit_name(ptr %ctx, ptr " <> f <> ", i64 " <> tshow (D.size vals) <> ", " <> intercalateT ", " padded <> ")"
  where
    arg (Pair u b) = "i64 " <> u <> ", ptr " <> b
    padded = fmap arg vals <> D.fromList (replicate (4 - D.size vals) "i64 0, ptr null")

-- | Calls a list helper that returns a boxed result, or null for a case
-- it leaves to the interpreter (@slow@); pushes the result.
listHelper :: Int -> Text -> Text -> Gen ()
listHelper d slow call = do
  r <- fresh "list"
  emit (r <> " = call ptr " <> call)
  miss <- fresh "miss"
  emit (miss <> " = icmp eq ptr " <> r <> ", null")
  branchIf "list" miss slow
  storeU (d + 1) "-1"
  storeB (d + 1) r

-- | A pool entry, loaded.
poolValue :: Int -> Gen Text
poolValue ix = do
  usePool ix
  a <- fresh "pool.a"
  emit (a <> " = getelementptr ptr, ptr %pool, i64 " <> tshow ix)
  v <- fresh "const"
  emit (v <> " = load ptr, ptr " <> a)
  pure v

-- | Records that the function uses this pool index.
usePool :: Int -> Gen ()
usePool ix = modify' (\s -> s {gsMaxPool = max (gsMaxPool s) ix})

-- | Pushes a pool entry as a boxed value.
poolConstant :: FnEnv -> Int -> Int -> Gen ()
poolConstant _fe d ix = do
  usePool ix
  a <- fresh "pool.a"
  emit (a <> " = getelementptr ptr, ptr %pool, i64 " <> tshow ix)
  v <- fresh "const"
  emit (v <> " = load ptr, ptr " <> a)
  storeU (d + 1) "-1"
  storeB (d + 1) v

-- | Stores an unboxed result with its type tag at depth @d + 1@.
result :: FnEnv -> Int -> Text -> Text -> Gen ()
result _fe d tag v = storeU (d + 1) v >> storeB (d + 1) tag

-- | Stores a boolean result: a boxed enumeration closure.
resultBool :: FnEnv -> Int -> Text -> Gen ()
resultBool _fe d c = setBoolKind (d + 1) c

binop :: Text -> Text -> Text -> Gen Text
binop op x y = do
  r <- fresh "r"
  emit (r <> " = " <> op <> " i64 " <> x <> ", " <> y)
  pure r

cmp :: Text -> Text -> Text -> Gen Text
cmp op x y = do
  r <- fresh "c"
  emit (r <> " = icmp " <> op <> " i64 " <> x <> ", " <> y)
  pure r

-- | Branches to a resume exit if the condition holds, else continues.
exitIf :: FnEnv -> Int -> MSection -> Text -> Gen ()
exitIf fe d sect c = do
  slow <- exitBlock fe d (Resume (feCix fe) sect)
  ok <- freshLabel "ok"
  emit ("br i1 " <> c <> ", label %" <> slow <> ", label %" <> ok <> unlikely)
  startBlock ok

genPrim1 :: FnEnv -> Int -> Prim1 -> Text -> MSection -> Gen ()
genPrim1 fe d op x _sect = case op of
  DECI -> binop "sub" x "1" >>= int
  DECN -> binop "sub" x "1" >>= nat
  INCI -> binop "add" x "1" >>= int
  INCN -> binop "add" x "1" >>= nat
  NEGI -> binop "sub" "0" x >>= int
  COMN -> binop "xor" x "-1" >>= nat
  COMI -> binop "xor" x "-1" >>= int
  TRNC -> do
    neg <- cmp "slt" x "0"
    r <- fresh "r"
    emit (r <> " = select i1 " <> neg <> ", i64 0, i64 " <> x)
    nat r
  LZRO -> count "ctlz" ", i1 false"
  TZRO -> count "cttz" ", i1 false"
  POPC -> count "ctpop" ""
  SGNI -> do
    neg <- cmp "slt" x "0"
    pos <- cmp "sgt" x "0"
    a <- fresh "r"
    emit (a <> " = select i1 " <> pos <> ", i64 1, i64 0")
    r <- fresh "r"
    emit (r <> " = select i1 " <> neg <> ", i64 -1, i64 " <> a)
    int r
  -- Floats. A Float is its IEEE bits in the unboxed slot. These match the
  -- interpreter's Haskell operation for operation (Machine/Primops.hs):
  -- compiled Haskell calls libm for exp, log, the trigonometric and
  -- hyperbolic functions, uses an instruction for sqrt and abs, and converts
  -- to Int with a saturating conversion (fcvtzs on arm64; NaN gives 0),
  -- which is what the saturating intrinsic lowers to. `round` is rint
  -- (half to even) then that conversion. `ceiling` and `floor` are
  -- GHC.Float's ceilingDoubleInt and floorDoubleInt: truncate, then add or
  -- subtract one (wrapping) if the remainder x - n has the right sign, which
  -- is not what a saturating ceil would give out of range, so it is done
  -- the same way here.
  ITOF -> do
    r <- fresh "f"
    emit (r <> " = sitofp i64 " <> x <> " to double")
    flt r
  NTOF -> do
    r <- fresh "f"
    emit (r <> " = uitofp i64 " <> x <> " to double")
    flt r
  ABSF -> toF x >>= call1 "llvm.fabs.f64" >>= flt
  SQRT -> toF x >>= call1 "llvm.sqrt.f64" >>= flt
  EXPF -> toF x >>= call1 "exp" >>= flt
  LOGF -> toF x >>= call1 "log" >>= flt
  COSF -> toF x >>= call1 "cos" >>= flt
  SINF -> toF x >>= call1 "sin" >>= flt
  TANF -> toF x >>= call1 "tan" >>= flt
  COSH -> toF x >>= call1 "cosh" >>= flt
  SINH -> toF x >>= call1 "sinh" >>= flt
  TANH -> toF x >>= call1 "tanh" >>= flt
  ACOS -> toF x >>= call1 "acos" >>= flt
  ASIN -> toF x >>= call1 "asin" >>= flt
  ATAN -> toF x >>= call1 "atan" >>= flt
  ASNH -> toF x >>= call1 "asinh" >>= flt
  ACSH -> toF x >>= call1 "acosh" >>= flt
  ATNH -> toF x >>= call1 "atanh" >>= flt
  CEIL -> ceilFloor "ogt" "add"
  FLOR -> ceilFloor "olt" "sub"
  RNDF -> toF x >>= call1 "llvm.rint.f64" >>= toInt >>= int
  TRNF -> toF x >>= toInt >>= int
  _ -> error "genPrim1: unsupported"
  where
    int = result fe d (feTagInt fe)
    nat = result fe d (feTagNat fe)
    flt f = ofF f >>= result fe d (feTagFloat fe)
    count name flag = do
      r <- fresh "r"
      emit (r <> " = call i64 @llvm." <> name <> ".i64(i64 " <> x <> flag <> ")")
      nat r
    call1 name f = do
      r <- fresh "f"
      emit (r <> " = call double @" <> name <> "(double " <> f <> ")")
      pure r
    toInt f = do
      r <- fresh "r"
      emit (r <> " = call i64 @llvm.fptosi.sat.i64.f64(double " <> f <> ")")
      pure r
    -- n = truncate x; r = x - n; if r `pred` 0 then n `op` 1 else n
    ceilFloor pred op = do
      f <- toF x
      n <- toInt f
      back <- fresh "f"
      emit (back <> " = sitofp i64 " <> n <> " to double")
      rem <- fresh "f"
      emit (rem <> " = fsub double " <> f <> ", " <> back)
      c <- fresh "c"
      emit (c <> " = fcmp " <> pred <> " double " <> rem <> ", 0.0")
      n1 <- binop op n "1"
      r <- fresh "r"
      emit (r <> " = select i1 " <> c <> ", i64 " <> n1 <> ", i64 " <> n)
      int r

-- | An unboxed slot's bits as a double, and back.
toF, ofF :: Text -> Gen Text
toF x = do
  f <- fresh "f"
  emit (f <> " = bitcast i64 " <> x <> " to double")
  pure f
ofF f = do
  r <- fresh "bits"
  emit (r <> " = bitcast double " <> f <> " to i64")
  pure r

genPrim2 :: FnEnv -> Int -> Prim2 -> Text -> Text -> MSection -> Gen ()
genPrim2 fe d op x y sect = case op of
  ADDN -> binop "add" x y >>= nat
  SUBN -> binop "sub" x y >>= int -- Nat.sub gives an Int
  MULN -> binop "mul" x y >>= nat
  ANDN -> binop "and" x y >>= nat
  IORN -> binop "or" x y >>= nat
  XORN -> binop "xor" x y >>= nat
  DRPN -> do
    -- if n >= m then 0 else m - n
    c <- cmp "uge" y x
    s <- binop "sub" x y
    r <- fresh "r"
    emit (r <> " = select i1 " <> c <> ", i64 0, i64 " <> s)
    nat r
  DIVN -> divZero >> binop "udiv" x y >>= nat
  MODN -> divZero >> binop "urem" x y >>= nat
  ADDI -> binop "add" x y >>= int
  SUBI -> binop "sub" x y >>= int
  MULI -> binop "mul" x y >>= int
  ANDI -> binop "and" x y >>= int
  IORI -> binop "or" x y >>= int
  XORI -> binop "xor" x y >>= int
  DIVI -> do
    divZero
    divOverflow
    -- Haskell's div rounds toward negative infinity
    q <- binop "sdiv" x y
    r <- binop "srem" x y
    nz <- cmp "ne" r "0"
    signs <- binop "xor" r y
    neg <- cmp "slt" signs "0"
    adjust <- fresh "adj"
    emit (adjust <> " = and i1 " <> nz <> ", " <> neg)
    q1 <- binop "sub" q "1"
    res <- fresh "r"
    emit (res <> " = select i1 " <> adjust <> ", i64 " <> q1 <> ", i64 " <> q)
    int res
  MODI -> do
    divZero
    divOverflow
    r <- binop "srem" x y
    nz <- cmp "ne" r "0"
    signs <- binop "xor" r y
    neg <- cmp "slt" signs "0"
    adjust <- fresh "adj"
    emit (adjust <> " = and i1 " <> nz <> ", " <> neg)
    r1 <- binop "add" r y
    res <- fresh "r"
    emit (res <> " = select i1 " <> adjust <> ", i64 " <> r1 <> ", i64 " <> r)
    int res
  EQLN -> cmp "eq" x y >>= bool
  NEQN -> cmp "ne" x y >>= bool
  LEQN -> cmp "ule" x y >>= bool
  LESN -> cmp "ult" x y >>= bool
  EQLI -> cmp "eq" x y >>= bool
  NEQI -> cmp "ne" x y >>= bool
  LEQI -> cmp "sle" x y >>= bool
  LESI -> cmp "slt" x y >>= bool
  SHLN -> shift "shl" "0" >>= nat
  SHRN -> shift "lshr" "0" >>= nat
  SHLI -> shift "shl" "0" >>= int
  SHRI -> do
    -- an arithmetic shift by 64 or more gives the sign
    sign <- fresh "sign"
    emit (sign <> " = ashr i64 " <> x <> ", 63")
    shift "ashr" sign >>= int
  -- Haskell's ^ by squaring; the same result modulo 2^64 whatever the order
  -- of the multiplications, so a C loop is exact.
  POWI -> callPow >>= int
  POWN -> callPow >>= nat
  -- a value with another type tag (the tag's number is the second argument,
  -- a literal in the builtins that use this)
  CAST -> do
    isChar <- cmp "eq" y "0"
    isFloat <- cmp "eq" y "1"
    isInt <- cmp "eq" y "2"
    t0 <- fresh "tag"
    emit (t0 <> " = select i1 " <> isInt <> ", ptr " <> feTagInt fe <> ", ptr " <> feTagNat fe)
    t1 <- fresh "tag"
    emit (t1 <> " = select i1 " <> isFloat <> ", ptr " <> feTagFloat fe <> ", ptr " <> t0)
    t2 <- fresh "tag"
    emit (t2 <> " = select i1 " <> isChar <> ", ptr " <> feTagChar fe <> ", ptr " <> t1)
    result fe d t2 x
  -- Floats (see genPrim1 for the conventions)
  ADDF -> fbin "fadd" >>= flt
  SUBF -> fbin "fsub" >>= flt
  MULF -> fbin "fmul" >>= flt
  DIVF -> fbin "fdiv" >>= flt
  -- Haskell's == and /= on Double are the ordered and unordered IEEE tests
  EQLF -> fcmp "oeq" >>= bool
  NEQF -> fcmp "une" >>= bool
  LEQF -> fcmp "ole" >>= bool
  LESF -> fcmp "olt" >>= bool
  -- Ord Double's defaults: max x y = if x <= y then y else x, min likewise
  MAXF -> do
    Pair fx fy <- floats
    c <- fcmpOn "ole" fx fy
    r <- fresh "f"
    emit (r <> " = select i1 " <> c <> ", double " <> fy <> ", double " <> fx)
    flt r
  MINF -> do
    Pair fx fy <- floats
    c <- fcmpOn "ole" fx fy
    r <- fresh "f"
    emit (r <> " = select i1 " <> c <> ", double " <> fx <> ", double " <> fy)
    flt r
  POWF -> do
    Pair fx fy <- floats
    r <- fresh "f"
    emit (r <> " = call double @pow(double " <> fx <> ", double " <> fy <> ")")
    flt r
  -- logBase x y = log y / log x
  LOGB -> do
    Pair fx fy <- floats
    ly <- fresh "f"
    emit (ly <> " = call double @log(double " <> fy <> ")")
    lx <- fresh "f"
    emit (lx <> " = call double @log(double " <> fx <> ")")
    r <- fresh "f"
    emit (r <> " = fdiv double " <> ly <> ", " <> lx)
    flt r
  -- GHC's atan2 is defined by cases in Haskell, not libm's; the C helper is a port
  ATN2 -> do
    Pair fx fy <- floats
    r <- fresh "f"
    emit (r <> " = call double @unison_jit_atan2(double " <> fx <> ", double " <> fy <> ")")
    flt r
  _ -> error "genPrim2: unsupported"
  where
    int = result fe d (feTagInt fe)
    nat = result fe d (feTagNat fe)
    bool = resultBool fe d
    flt f = ofF f >>= result fe d (feTagFloat fe)
    floats = Pair <$> toF x <*> toF y
    fbin instr = do
      Pair fx fy <- floats
      r <- fresh "f"
      emit (r <> " = " <> instr <> " double " <> fx <> ", " <> fy)
      pure r
    fcmpOn pred fx fy = do
      c <- fresh "c"
      emit (c <> " = fcmp " <> pred <> " double " <> fx <> ", " <> fy)
      pure c
    fcmp pred = floats >>= \(Pair fx fy) -> fcmpOn pred fx fy
    callPow = do
      r <- fresh "r"
      emit (r <> " = call i64 @unison_jit_pow(i64 " <> x <> ", i64 " <> y <> ")")
      pure r
    divZero = cmp "eq" y "0" >>= exitIf fe d sect
    divOverflow = cmp "eq" y "-1" >>= exitIf fe d sect
    -- Haskell rejects negative shifts and gives `big` for shifts of 64 or more
    shift instr big = do
      neg <- cmp "slt" y "0"
      exitIf fe d sect neg
      wide <- cmp "sge" y "64"
      s <- binop instr x y
      r <- fresh "r"
      emit (r <> " = select i1 " <> wide <> ", i64 " <> big <> ", i64 " <> s)
      pure r
