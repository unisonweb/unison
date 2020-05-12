{-# language GADTs #-}
{-# language BangPatterns #-}
{-# language PatternGuards #-}
{-# language EmptyDataDecls #-}
{-# language PatternSynonyms #-}

module Unison.Runtime.MCode
  ( Args'(..)
  , Args(..)
  , Instr(..)
  , Section(..)
  , Comb(..)
  , Ref(..)
  , Prim1(..)
  , Prim2(..)
  , Branch(..)
  , bcount
  , ucount
  , emitCombs
  , emitComb
  ) where

import GHC.Stack (HasCallStack)

import Data.Bifunctor (bimap)
import Data.List (partition)

import Data.Primitive.PrimArray

import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Data.Text (Text)
import qualified Data.Text as Text

import Unison.Var (Var)
import Unison.ABT.Normalized (pattern TAbss)
import Unison.Runtime.ANF
  ( ANormal
  , ANormalT
  , ANormalTF(..)
  , Branched(..)
  , Func(..)
  , Mem(..)
  , SuperNormal(..)
  , SuperGroup(..)
  , pattern TVar
  , pattern TLit
  , pattern TApv
  , pattern TCom
  , pattern TCon
  , pattern TKon
  , pattern TReq
  , pattern THnd
  , pattern TShift
  , pattern TLets
  , pattern TName
  , pattern TTm
  , pattern TMatch
  )
import qualified Unison.Runtime.ANF as ANF
import Unison.Runtime.Foreign
import Unison.Util.Bytes as Bytes

import Network.Socket as SYS
  ( accept
  )
import Network.Simple.TCP as SYS
  ( HostPreference(..)
  , bindSock
  , connectSock
  , listenSock
  , closeSock
  , send
  , recv
  )
import System.IO as SYS
  ( BufferMode(..)
  , Handle
  , openFile
  , hClose
  , hGetBuffering
  , hIsEOF
  , hIsOpen
  , hIsSeekable
  , hSeek
  , hTell
  )
import Data.Text.IO as SYS
  ( hGetLine
  , hPutStr
  )
import Control.Concurrent as SYS
  ( threadDelay
  , killThread
  )
import Data.Time.Clock.POSIX as SYS
  ( getPOSIXTime
  , utcTimeToPOSIXSeconds
  )
import System.Directory as SYS
  ( getCurrentDirectory
  , setCurrentDirectory
  , getTemporaryDirectory
  , getDirectoryContents
  , doesPathExist
  -- , doesDirectoryExist
  , renameDirectory
  , removeFile
  , renameFile
  , createDirectoryIfMissing
  , removeDirectoryRecursive
  , getModificationTime
  , getFileSize
  )

-- This outlines some of the ideas/features in this core
-- language, and how they may be used to implement features of
-- the surface language.

-----------------------
-- Delimited control --
-----------------------

-- There is native support for delimited control operations in
-- the core language. This means we can:
--   1. delimit a block of code with an integer tagged prompt,
--      which corresponds to pushing a frame onto the
--      continuation with said tag
--   2. capture a portion of the continuation up to a particular
--      tag frame and turn it into a value, which _removes_ the
--      tag frame from the continuation in the process
--   3. push such a captured value back onto the continuation

-- TBD: Since the captured continuations in _delimited_ control
-- are (in this case impure) functions, it may make sense to make
-- the representation of functions support these captured
-- continuations directly.

-- The obvious use case of this feature is effects and handlers.
-- Delimiting a block with a prompt is part of installing a
-- handler for said block at least naively. The other part is
-- establishing the code that should be executed for each
-- operation to be handled.

-- It's important (I believe) in #2 that the prompt be removed
-- from the continuation by a control effect. The captured
-- continuation not being automatically delimited corresponds to
-- a shallow handler's obligation to re-establish the handling of
-- a re-invoked computation if it wishes to do so. The delimiter
-- being removed from the capturing code's continuation
-- corresponds to a handler being allowed to yield effects from
-- the same siganture that it is handling.

-- In special cases, it should be possible to omit use of control
-- effects in handlers. At the least, if a handler case resumes
-- the computation in tail position, it should be unnecessary to
-- capture the continuation at all. If all cases act this way, we
-- don't need a delimiter, because we will never capture.

-- TBD: it may make more sense to have prompt pushing be part of
-- some other construct, due to A-normal forms of the code.

-----------------------------
-- Unboxed sum-of-products --
-----------------------------

-- It is not usually stated this way, but one of the core
-- features of the STG machine is that functions/closures can
-- return unboxed sum-of-products types. This is actually the way
-- _all_ data types work in STG. The discriminee of a case
-- statement must eventually return by pushing several values
-- onto the stack (the product part) and specifying which branch
-- to return to (the sum part).

-- The way heap allocated data is produced is that an
-- intermediate frame may be in the continuation that grabs this
-- information from the local storage and puts it into the heap.
-- If this frame were omitted, only the unboxed component would
-- be left. Also, in STG, the heap allocated data is just a means
-- of reconstructing its unboxed analogue. Evaluating a heap
-- allocated data type value just results in pushing its stored
-- fields back on the stack, and immediately returning the tag.

-- The portion of this with the heap allocation frame omitted
-- seems to be a natural match for the case analysis portion of
-- handlers. A naive implementation of an effect algebra is as
-- the data type of the polynomial functor generated by the
-- signature, and handling corresponds to case analysis. However,
-- in a real implementation, we don't want a heap allocated
-- representation of this algebra, because its purpose is control
-- flow. Each operation will be handled once as it occurs, and we
-- won't save work by remembering some reified representation of
-- which operations were used.

-- Since handlers in unison are written as functions, it seems to
-- make sense to define a calling convention for unboxed
-- sum-of-products as arguments. Variable numbers of stack
-- positions could be pushed for such arguments, with tags
-- specifying which case is being provided.

-- TBD: sum arguments to a function correspond to a product of
-- functions, so it's possible that the calling convention for
-- these functions should be similar to returning to a case,
-- where we push arguments and then select which of several
-- pieces of code to jump to. This view also seems relevant to
-- the optimized implementation of certain forms of handler,
-- where we want effects to just directly select some code to
-- execute based on state that has been threaded to that point.

-- One thing to note: it probably does not make sense to
-- completely divide returns into unboxed returns and allocation
-- frames. The reason this works in STG is laziness. Naming a
-- computation with `let` does not do any evaluation, but it does
-- allocate space for its (boxed) result. The only thing that
-- _does_ demand evaluation is case analysis. So, if a value with
-- sum type is being evaluated, we know it must be about to be
-- unpacked, and it makes little sense to pack it on the stack,
-- though we can build a closure version of it in the writeback
-- location established by `let`.

-- By contrast, in unison a `let` of a sum type evaluates it
-- immediately, even if no one is analyzing it. So we might waste
-- work rearranging the stack with the unpacked contents when we
-- only needed the closure version to begin with. Instead, we
-- gain the ability to make the unpacking operation use no stack,
-- because we know what we are unpacking must be a value. Turning
-- boxed function calls into unboxed versions thus seems like a
-- situational optimization, rather than a universal calling
-- convention.

-------------------------------
-- Delimited Dynamic Binding --
-------------------------------

-- There is a final component to the implementation of ability
-- handlers in this runtime system, and that is dynamically
-- scoped variables associated to each prompt. Each prompt
-- corresponds to an ability signature, and `reset` to a handler
-- for said signature, but we need storage space for the code
-- installed by said handler. It is possible to implement
-- dynamically scoped variables entirely with delimited
-- continuations, but it is more efficient to keep track of the
-- storage directly when manipulating the continuations.

-- The dynamic scoping---and how it interacts with
-- continuations---corresponds to the nested structure of
-- handlers. Installing a handler establishes a variable scope,
-- shadowing outer scopes for the same prompt. Shifting, however,
-- can exit these scopes dynamically. So, for instance, if we
-- have a structure like:

--    reset 0 $ ...
--      reset 1 $ ...
--        reset 0 $ ...
--          shift 1 <E>

-- We have nested scopes 0>1>0, with the second 0 shadowing the
-- first. However, when we shift to 1, the inner 0 scope is
-- captured into the continuation, and uses of the 0 ability in
-- <E> will be handled by the outer handler until it is shadowed
-- again (and the captured continuation will re-establish the
-- shadowing).

-- Mutation of the variables is possible, but mutation only
-- affects the current scope. Essentially, the dynamic scoping is
-- of mutable references, and when scope changes, we switch
-- between different references, and the mutation of each
-- reference does not affect the others. The purpose of the
-- mutation is to enable more efficient implementation of
-- certain recursive, 'deep' handlers, since those can operate
-- more like stateful code than control operators.

data Args'
  = Arg1 !Int
  | Arg2 !Int !Int
  -- frame index of each argument to the function
  | ArgN {-# unpack #-} !(PrimArray Int)
  | ArgR !Int !Int

data Args
  = ZArgs
  | UArg1 !Int
  | UArg2 !Int !Int
  | BArg1 !Int
  | BArg2 !Int !Int
  | DArg2 !Int !Int
  | UArgR !Int !Int
  | BArgR !Int !Int
  | DArgR !Int !Int !Int !Int
  | BArgN !(PrimArray Int)
  | UArgN !(PrimArray Int)
  | DArgN !(PrimArray Int) !(PrimArray Int)
  deriving (Show)

ucount, bcount :: Args -> Int

ucount (UArg1 _) = 1
ucount (UArg2 _ _) = 2
ucount (DArg2 _ _) = 1
ucount (UArgR _ l) = l
ucount (DArgR _ l _ _) = l
ucount _ = 0
{-# inline ucount #-}

bcount (BArg1 _) = 1
bcount (BArg2 _ _) = 2
bcount (DArg2 _ _) = 1
bcount (BArgR _ l) = l
bcount (DArgR _ _ _ l) = l
bcount (BArgN a) = sizeofPrimArray a
bcount _ = 0
{-# inline bcount #-}

data Prim1
  = DECI | INCI | NEGI | SGNI
  deriving (Show)

data Prim2
  = ADDI | SUBI | MULI | DIVI | MODI
  | SHLI | SHRI | SHRN | POWI
  | EQLI | LESI | LESN | LEQI | LEQN
  deriving (Show)

-- Instructions for manipulating the data stack in the main portion of
-- a block
data Instr
  -- 1-argument primitive operations
  = Prim1 !Prim1 -- primitive instruction
          !Int   -- index of prim argument

  -- 2-argument primitive operations
  | Prim2 !Prim2 -- primitive instruction
          !Int   -- index of first prim argument
          !Int   -- index of second prim argument

  -- Call out to a Haskell function. This is considerably slower
  -- for very simple operations, hence the primops.
  | ForeignCall !ForeignFunc
                !Args

  -- Set the value of a dynamic reference
  | SetDyn !Int -- the prompt tag of the reference
           !Int -- the stack index of the closure to store

  -- Capture the continuation up to a given marker.
  | Capture !Int -- the prompt tag

  -- This is essentially the opposite of `Call`. Pack a given
  -- statically known function into a closure with arguments.
  -- No stack is necessary, because no nested evaluation happens,
  -- so the instruction directly takes a follow-up.
  | Name !Int !Args

  -- Dump some debugging information about the machine state to
  -- the screen.
  | Info !String -- prefix for output

  -- Pack a data type value into a closure and place it
  -- on the stack.
  | Pack !Int  -- tag
         !Args -- arguments to pack

  -- Unpack the contents of a data type onto the stack
  | Unpack !Int -- stack index of data to unpack


  -- Push a particular value onto the unboxed stack
  | Lit !Int -- value to push onto the stack

  -- Print a value on the unboxed stack
  | Print !Int -- index of the primitive value to print

  -- Put a delimiter on the continuation
  | Reset !IntSet -- prompt ids

  | Fork !Section
  deriving (Show)

data Section
  -- Apply a function to arguments. This is the 'slow path', and
  -- handles applying functions from arbitrary sources. This
  -- requires checks to determine what exactly should happen.
  = App
      !Bool -- skip argument check for known calling convention
      !Ref  -- function to call
      !Args -- arguments

  -- This is the 'fast path', for when we statically know we're
  -- making an exactly saturated call to a statically known
  -- function. This allows skipping various checks that can cost
  -- time in very tight loops. This also allows skipping the
  -- stack check if we know that the current stack allowance is
  -- sufficient for where we're jumping to.
  | Call
      !Bool -- skip stack check
      !Int  -- global function reference
      !Args -- arguments

  -- Jump to a captured continuation value.
  | Jump
      !Int  -- index of captured continuation
      !Args -- arguments to send to continuation

  -- Branch on the value in the unboxed data stack
  | Match !Int    -- index of unboxed item to match on
          !Branch -- branches

  -- Yield control to the current continuation, with arguments
  | Yield !Args -- values to yield

  -- Prefix an instruction onto a section
  | Ins !Instr !Section

  -- Sequence two sections. The second is pushed as a return
  -- point for the results of the first. Stack modifications in
  -- the first are lost on return to the second.
  | Let !Section !Section

  | Die String

  | Exit
  deriving (Show)

data Comb
  = Lam !Int -- Number of unboxed arguments
        !Int -- Number of boxed arguments
        !Int -- Maximum needed unboxed frame size
        !Int -- Maximum needed boxed frame size
        !Section -- Entry
  deriving (Show)

data Ref
  = Stk !Int -- stack reference to a closure
  | Env !Int -- global environment reference to a combinator
  | Dyn !Int -- dynamic scope reference to a closure
  deriving (Show)

data Branch
  -- if tag == n then t else f
  = Test1 !Int
          !Section
          !Section
  | Test2 !Int !Section    -- if tag == m then ...
          !Int !Section    -- else if tag == n then ...
          !Section         -- else ...
  | TestT !Section
          !(IntMap Section)
  deriving (Show)

type Ctx v = [(Maybe v,Mem)]
type RCtx v = M.Map v Int

ctxResolve :: Var v => Ctx v -> v -> Maybe (Int,Mem)
ctxResolve ctx v = walk 0 0 ctx
  where
  walk _  _  [] = Nothing
  walk ui bi ((mx,m):xs)
    | Just x <- mx
    , v == x = case m of BX -> Just (bi,m) ; UN -> Just (ui,m)
    | otherwise = walk ui' bi' xs
    where
    (ui',bi') = case m of BX -> (ui,bi+1) ; UN -> (ui+1,bi)

rctxResolve :: Var v => RCtx v -> v -> Maybe Int
rctxResolve ctx u = M.lookup u ctx

emitCombs :: Var v => Int -> SuperGroup v -> (Comb, IntMap Comb, Int)
emitCombs frsh (Rec grp ent)
  = (emitComb rec ent, IM.fromList aux, frsh')
  where
  frsh' = frsh + length grp
  (rvs, cmbs) = unzip grp
  rec = M.fromList $ zip rvs [frsh..]
  aux = zip [frsh..] $ emitComb rec <$> cmbs

emitComb :: Var v => RCtx v -> SuperNormal v -> Comb
emitComb rec (Lambda ccs (TAbss vs bd))
  = Lam 0 (length vs) 10 10 $ emitSection rec (zip (Just <$> vs) ccs) bd

emitSection :: Var v => RCtx v -> Ctx v -> ANormal v -> Section
emitSection rec ctx (TLets us ms bu bo)
  = emitLet rec ctx bu $ emitSection rec (ectx ++ ctx) bo
  where
  ectx = zip (Just <$> us) ms
emitSection rec ctx (TName u (Left f) as bo)
  = Ins (Name f $ emitArgs ctx as)
  $ emitSection rec ((Just u,BX) : ctx) bo
emitSection rec ctx (TName u (Right v) as bo)
  | Just f <- rctxResolve rec v
  = Ins (Name f $ emitArgs ctx as)
  $ emitSection rec ((Just u,BX) : ctx) bo
  | otherwise = emitSectionVErr v
emitSection rec ctx (TVar v)
  | Just (i,BX) <- ctxResolve ctx v = Yield $ BArg1 i
  | Just (i,UN) <- ctxResolve ctx v = Yield $ UArg1 i
  | Just j <- rctxResolve rec v = App False (Env j) ZArgs
  | otherwise = emitSectionVErr v
emitSection rec ctx (TApv v args)
  | Just (i,BX) <- ctxResolve ctx v
  = App False (Stk i) $ emitArgs ctx args
  | Just j <- rctxResolve rec v
  = App False (Env j) $ emitArgs ctx args
  | otherwise = emitSectionVErr v
emitSection _   ctx (TCom n args)
  | False -- known saturated call
  = Call False n $ emitArgs ctx args
  | False -- known unsaturated call
  = Ins (Name n $ emitArgs ctx args) $ Yield (BArg1 0)
  | otherwise -- slow path
  = App False (Env n) $ emitArgs ctx args
emitSection _   ctx (TCon _ t args)
  = Ins (Pack t (emitArgs ctx args))
  . Yield $ BArg1 0
emitSection _   ctx (TReq a e args)
  -- Currently implementing packed calling convention for abilities
  = Ins (Pack e (emitArgs ctx args))
  . App True (Dyn a) $ BArg1 0
emitSection _   ctx (TKon k args)
  | Just (i, BX) <- ctxResolve ctx k = Jump i $ emitArgs ctx args
  | Nothing <- ctxResolve ctx k = emitSectionVErr k
  | otherwise = error $ "emitSection: continuations are boxed"
emitSection _   _   (TLit l)
  = Ins (emitLit l)
  . Ins (Pack 0 $ UArg1 0)
  . Yield $ BArg1 0
emitSection rec ctx (TMatch v bs)
  | Just (i,BX) <- ctxResolve ctx v
  , MatchData _ cs df <- bs
  = Ins (Unpack i)
  $ emitDataMatching rec ctx cs df
  | Just (i,BX) <- ctxResolve ctx v
  , MatchRequest hs <- bs
  = Ins (Unpack i)
  $ emitRequestMatching rec ctx hs
  | Just (i,UN) <- ctxResolve ctx v
  , MatchIntegral cs df <- bs
  = emitIntegralMatching rec ctx i cs df
  | Just (i,UN) <- ctxResolve ctx v
  , MatchSum cs <- bs
  = emitSumMatching rec ctx i cs
  | Just (_,cc) <- ctxResolve ctx v
  = error
  $ "emitSection: mismatched calling convention for match: "
  ++ matchCallingError cc bs
  | otherwise
  = error "emitSection: could not resolve match variable"
emitSection rec ctx (THnd rs h df b)
  | Just (i,BX) <- ctxResolve ctx h
  = Ins (Reset $ IS.fromList rs)
  $ flip (foldr (\r -> Ins (SetDyn r i))) rs
  $ maybe id (\(TAbss us d) l ->
      Let l $ emitSection rec (fmap ((,BX).Just) us ++ ctx) d) df
  $ emitSection rec ctx b
  | otherwise = emitSectionVErr h
emitSection rec ctx (TShift i v e)
  = Ins (Capture i)
  $ emitSection rec ((Just v, BX):ctx) e
emitSection _ _ _ = error "emitSection: unhandled code"

matchCallingError :: Mem -> Branched v -> String
matchCallingError cc b = "(" ++ show cc ++ "," ++ brs ++ ")"
  where
  brs | MatchData _ _ _ <- b = "MatchData"
      | MatchEmpty <- b = "MatchEmpty"
      | MatchIntegral _ _ <- b = "MatchIntegral"
      | MatchRequest _ <- b = "MatchRequest"
      | MatchSum _ <- b = "MatchSum"

emitSectionVErr :: (Var v, HasCallStack) => v -> a
emitSectionVErr v
  = error
  $ "emitSection: could not resolve function variable: " ++ show v

emitLet :: Var v => RCtx v -> Ctx v -> ANormalT v -> Section -> Section
-- Currently packed literals
emitLet _   _   (ALit l)
  = Ins (emitLit l)
emitLet _    ctx (AApp (FComb n) args)
  -- We should be able to tell if we are making a saturated call
  -- or not here. We aren't carrying the information here yet, though.
  | False -- not saturated
  = Ins . Name n $ emitArgs ctx args
emitLet _   ctx (AApp (FCon _ n) args) -- TODO: use reference number
  = Ins . Pack n $ emitArgs ctx args
emitLet _   ctx (AApp (FPrim p) args)
  = Ins . either emitPOp emitIOp p $ emitArgs ctx args
emitLet rec ctx bnd = Let (emitSection rec ctx (TTm bnd))

-- -- Float
-- | ADDF | SUBF | MULF | DIVF -- +,-,*,/
-- | LESF | LEQF | EQLF        -- <,<=,==
emitPOp :: ANF.POp -> Args -> Instr
emitPOp ANF.ADDI = emitP2 ADDI
emitPOp ANF.ADDN = emitP2 ADDI
emitPOp ANF.SUBI = emitP2 SUBI
emitPOp ANF.SUBN = emitP2 SUBI
emitPOp ANF.MULI = emitP2 MULI
emitPOp ANF.MULN = emitP2 MULI
emitPOp ANF.DIVI = emitP2 DIVI
emitPOp ANF.DIVN = emitP2 DIVI
emitPOp ANF.TRNI = error "I don't think this is used"
emitPOp ANF.MODI = emitP2 MODI -- TODO: think about how these behave
emitPOp ANF.MODN = emitP2 MODI -- TODO: think about how these behave
emitPOp ANF.POWI = emitP2 POWI
emitPOp ANF.POWN = emitP2 POWI
emitPOp ANF.SHLI = emitP2 SHLI
emitPOp ANF.SHLN = emitP2 SHLI -- Note: left shift behaves uniformly
emitPOp ANF.SHRI = emitP2 SHRI
emitPOp ANF.SHRN = emitP2 SHRN
emitPOp ANF.LESI = emitP2 LESI
emitPOp ANF.LESN = emitP2 LESN
emitPOp ANF.LEQI = emitP2 LEQI
emitPOp ANF.LEQN = emitP2 LEQN
emitPOp ANF.EQLI = emitP2 EQLI
emitPOp ANF.EQLN = emitP2 EQLI

emitPOp ANF.SGNI = emitP1 SGNI
emitPOp ANF.NEGI = emitP1 NEGI
emitPOp ANF.INCI = emitP1 INCI
emitPOp ANF.INCN = emitP1 INCI
emitPOp ANF.DECI = emitP1 DECI
emitPOp ANF.DECN = emitP1 DECI

emitPOp p@ANF.ADDF = error $ "unhandled prim op: " ++ show p
emitPOp p@ANF.SUBF = error $ "unhandled prim op: " ++ show p
emitPOp p@ANF.MULF = error $ "unhandled prim op: " ++ show p
emitPOp p@ANF.DIVF = error $ "unhandled prim op: " ++ show p
emitPOp p@ANF.LESF = error $ "unhandled prim op: " ++ show p
emitPOp p@ANF.LEQF = error $ "unhandled prim op: " ++ show p
emitPOp p@ANF.EQLF = error $ "unhandled prim op: " ++ show p

emitPOp ANF.FORK = \case
  BArg1 i -> Fork $ App True (Stk i) ZArgs
  _ -> error "fork takes exactly one boxed argument"

emitIOp :: ANF.IOp -> Args -> Instr
emitIOp iop = ForeignCall (iopToForeign iop)

bufferModeResult :: BufferMode -> ForeignRslt
bufferModeResult NoBuffering = [Left 0]
bufferModeResult LineBuffering = [Left 1]
bufferModeResult (BlockBuffering Nothing) = [Left 3]
bufferModeResult (BlockBuffering (Just n)) = [Left 4, Right $ Wrap n]

booleanResult :: Bool -> ForeignRslt
booleanResult b = [Left $ fromEnum b]

intResult :: Int -> ForeignRslt
intResult i = [Left i]

stringResult :: String -> ForeignRslt
stringResult = wrappedResult . Text.pack

wrappedResult :: a -> ForeignRslt
wrappedResult x = [Right $ Wrap x]

handleResult :: Handle -> ForeignRslt
handleResult h = [Right $ Wrap h]

timeResult :: RealFrac r => r -> ForeignRslt
timeResult t = intResult $ round t

maybeResult'
  :: (a -> (Int, ForeignRslt)) -> Maybe a -> ForeignRslt
maybeResult' _ Nothing = [Left 0]
maybeResult' f (Just x)
  | (i, r) <- f x = Left (i+1) : r


iopToForeign :: ANF.IOp -> ForeignFunc
iopToForeign ANF.OPENFI
  = foreign2 $ \fp mo -> handleResult <$> openFile fp mo
iopToForeign ANF.CLOSFI
  = foreign1 $ \h -> [] <$ hClose h
iopToForeign ANF.ISFEOF
  = foreign1 $ \h -> booleanResult <$> hIsEOF h
iopToForeign ANF.ISFOPN
  = foreign1 $ \h -> booleanResult <$> hIsOpen h
iopToForeign ANF.ISSEEK
  = foreign1 $ \h -> booleanResult <$> hIsSeekable h
iopToForeign ANF.SEEKFI
  = foreign3 $ \h sm n -> [] <$ hSeek h sm (fromIntegral n)
iopToForeign ANF.POSITN
  = foreign1 $ \h -> wrappedResult <$> hTell h
iopToForeign ANF.GBUFFR
  = foreign1 $ \h -> bufferModeResult <$> hGetBuffering h
iopToForeign ANF.SBUFFR = error "todo"
iopToForeign ANF.GTLINE
  = foreign1 $ \h -> wrappedResult <$> hGetLine h
iopToForeign ANF.GTTEXT
  = error "todo" -- foreign1 $ \h -> pure . Right . Wrap <$> hGetText h
iopToForeign ANF.PUTEXT
  = foreign2 $ \h t -> [] <$ hPutStr h t
iopToForeign ANF.SYTIME
  = foreign0 $ timeResult <$> getPOSIXTime
iopToForeign ANF.GTMPDR
  = foreign0 $ stringResult <$> getTemporaryDirectory
iopToForeign ANF.GCURDR
  = foreign0 $ stringResult <$> getCurrentDirectory
iopToForeign ANF.SCURDR
  = foreign1 $ \fp -> [] <$ setCurrentDirectory (Text.unpack fp)
iopToForeign ANF.DCNTNS
  = foreign1 $ \fp ->
      error "todo" <$ getDirectoryContents (Text.unpack fp)
iopToForeign ANF.FEXIST
  = foreign1 $ \fp -> booleanResult <$> doesPathExist (Text.unpack fp)
iopToForeign ANF.ISFDIR = error "todo"
iopToForeign ANF.CRTDIR
  = foreign1 $ \fp ->
      [] <$ createDirectoryIfMissing True (Text.unpack fp)
iopToForeign ANF.REMDIR
  = foreign1 $ \fp -> [] <$ removeDirectoryRecursive (Text.unpack fp)
iopToForeign ANF.RENDIR
  = foreign2 $ \fmp top ->
      [] <$ renameDirectory (Text.unpack fmp) (Text.unpack top)
iopToForeign ANF.REMOFI
  = foreign1 $ \fp -> [] <$ removeFile (Text.unpack fp)
iopToForeign ANF.RENAFI
  = foreign2 $ \fmp top ->
      [] <$ renameFile (Text.unpack fmp) (Text.unpack top)
iopToForeign ANF.GFTIME
  = foreign1 $ \fp ->
      timeResult . utcTimeToPOSIXSeconds
        <$> getModificationTime (Text.unpack fp)
iopToForeign ANF.GFSIZE
  = foreign1 $ \fp -> wrappedResult <$> getFileSize (Text.unpack fp)
iopToForeign ANF.SRVSCK
  = foreign1m2 $ \mhst port ->
      wrappedResult
        <$> SYS.bindSock (hostPreference mhst) (Text.unpack port)
iopToForeign ANF.LISTEN
  = foreign1 $ \sk ->
      [] <$ SYS.listenSock sk 2048
iopToForeign ANF.CLISCK
  = foreign2 $ \ho po ->
      wrappedResult <$> SYS.connectSock (Text.unpack ho) (Text.unpack po)
iopToForeign ANF.CLOSCK
  = foreign1 $ \sk -> [] <$ SYS.closeSock sk
iopToForeign ANF.SKACPT
  = foreign1 $ \sk ->
      wrappedResult <$> SYS.accept sk
iopToForeign ANF.SKSEND
  = foreign2 $ \sk bs ->
      [] <$ SYS.send sk (Bytes.toByteString bs)
iopToForeign ANF.SKRECV
  = foreign2 $ \hs n ->
      maybeResult' ((0,) . wrappedResult) . fmap Bytes.fromByteString
        <$> SYS.recv hs n
iopToForeign ANF.THKILL
  = foreign1 $ \tid -> [] <$ killThread tid
iopToForeign ANF.THDELY
  = foreign1 $ \n -> [] <$ threadDelay n

hostPreference :: Maybe Text -> SYS.HostPreference
hostPreference Nothing = SYS.HostAny
hostPreference (Just host) = SYS.Host $ Text.unpack host

emitP1 :: Prim1 -> Args -> Instr
emitP1 p (UArg1 i) = Prim1 p i
emitP1 _ _ = error "emitP1: prim ops must be saturated"

emitP2 :: Prim2 -> Args -> Instr
emitP2 p (UArg2 i j) = Prim2 p i j
emitP2 _ _ = error "emitP2: prim ops must be saturated"

emitEmptyMatching :: Section
emitEmptyMatching = Die "empty match"

emitDataMatching
  :: Var v
  => RCtx v
  -> Ctx v
  -> IntMap ([Mem], ANormal v)
  -> Maybe (ANormal v)
  -> Section
emitDataMatching rec ctx cs df
  = Match 0 . TestT edf $ fmap (emitCase rec ctx) cs
  where
  edf | Just co <- df = emitSection rec ctx co
      | otherwise = Die "missing data case"

emitSumMatching
  :: Var v
  => RCtx v
  -> Ctx v
  -> Int
  -> IntMap ([Mem], ANormal v)
  -> Section
emitSumMatching rec ctx i cs
  = Match i . TestT edf $ fmap (emitCase rec ctx) cs
  where
  edf = Die "uncovered unboxed sum case"

emitRequestMatching
  :: Var v
  => RCtx v
  -> Ctx v
  -> IntMap (IntMap ([Mem], ANormal v))
  -> Section
emitRequestMatching rec ctx hs
  = Match 0 . TestT edf $ fmap f hs
  where
  f cs = Match 1 . TestT edf $ fmap (emitCase rec ctx) cs
  edf = Die "unhandled ability"

emitIntegralMatching
  :: Var v
  => RCtx v
  -> Ctx v
  -> Int
  -> IntMap (ANormal v)
  -> Maybe (ANormal v)
  -> Section
emitIntegralMatching rec ctx i cs df
  = Match i . TestT edf $ fmap (emitCase rec ctx . ([],)) cs
  where
  edf | Just co <- df = emitSection rec ctx co
      | otherwise = Die "missing integral case"

emitCase :: Var v => RCtx v -> Ctx v -> ([Mem], ANormal v) -> Section
emitCase rec ctx (ccs, TAbss vs bo)
  = emitSection rec ((Nothing,UN) : zip (Just <$> vs) ccs ++ ctx) bo

emitLit :: ANF.Lit -> Instr
emitLit l = Lit i
  where
  i = case l of
        ANF.I i -> fromIntegral i
        ANF.N n -> fromIntegral n
        _ -> error "unhandled literal"

emitArgs :: Var v => Ctx v -> [v] -> Args
emitArgs ctx args
  | Just l <- traverse (ctxResolve ctx) args = demuxArgs l
  | otherwise
  = error $ "could not resolve argument variables: " ++ show args

demuxArgs :: [(Int,Mem)] -> Args
demuxArgs as0
  = case bimap (fmap fst) (fmap fst) $ partition ((==UN).snd) as0 of
      ([],[]) -> ZArgs
      ([],[i]) -> BArg1 i
      ([],[i,j]) -> BArg2 i j
      ([i],[]) -> UArg1 i
      ([i,j],[]) -> UArg2 i j
      ([i],[j]) -> DArg2 i j
      ([],bs) -> BArgN $ primArrayFromList bs
      (us,[]) -> UArgN $ primArrayFromList us
      -- TODO: handle ranges
      (us,bs) -> DArgN (primArrayFromList us) (primArrayFromList bs)
