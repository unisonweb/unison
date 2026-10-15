{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

-- | Closure layouts, found by probing sample closures at startup
-- (see internals.md, "Allocation and the barrier"). Generated code
-- uses these as constants. If a layout isn't what the code generator
-- expects, the JIT is turned off.
module Unison.Runtime.JIT.Layout
  ( Layout (..),
    Layouts (..),
    probeLayouts,
    probeLists,
    probeTexts,
    probeBytes,
    probeNames,
    probeArrays,
    probeMurmur,
    probeCompare,
    probeEnv,
    probeRecords,
  )
where

import Control.Exception (evaluate)
import Control.Monad (foldM, forM)
import Data.Maybe (catMaybes, isJust)
import Data.Word (Word64, Word8)
import GHC.Float (castDoubleToWord64)
import Text.Read (readMaybe)
import Data.Bits (shiftR, xor, (.&.), (.|.))
import Data.Primitive.Array (MutableArray, arrayFromList, newArray, readArray, writeArray)
import Data.IORef (newIORef)
import GHC.IORef (IORef (..))
import Unison.Util.EnumContainers qualified as EC
import Data.Sequence qualified as Seq
import Data.Map.Strict qualified as M
import Data.Primitive.ByteArray (byteArrayFromList, newByteArray)
import Data.Atomics qualified as Atomic
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (IntPtr (..), ptrToIntPtr)
import GHC.Exts (Any, RealWorld)
import Unison.Runtime.ANF (PackedTag (..))
import Unison.Builtin.Decls qualified as Ty (eitherRef, failureRef, optionalRef, pairRef, seqViewRef, unitRef)
import Data.Digest.Murmur64 (asWord64)
import Unison.Runtime.ANF qualified as ANF
import Unison.Runtime.ANF.MurmurHash.Untyped (hash64ValueUntyped)
import Unison.Runtime.Referenced (Referenced (Plain))
import Unison.Reference (Reference)
import Unison.Runtime.JIT.Strict (Pair (..))
import Unison.Runtime.JIT.Native (envInit, arrayInit, bytesCheck, bytesInit, bytesTest, closureInit, cmpInit, cmpTest, listCheck, listInit, listTest, murmurInit, murmurTest, nameTest, probeClosure, recordInit, textCheck, textInit, textTest)
import Unison.Runtime.MCode (CombIx (..), FieldRef (..), GCombInfo (..), GSection (..), RecordShape (..), noNativeCell)
import Data.Set qualified as Set
import Data.Vector qualified as V
import Unison.Runtime.Stack
import Unison.Runtime.TypeTags qualified as TT
import Unison.Type qualified as Ty
import Data.Text qualified as Text
import Unison.Util.Bytes qualified as By
import Unison.Util.Deque qualified as Sq
import Unison.Util.Rope qualified as Rope
import Unison.Util.Text qualified as UText
import Unsafe.Coerce (unsafeCoerce)

data Layout = Layout
  { -- | info pointer, to write into new closures of this kind
    lInfo :: !Int,
    -- | the low bits of a pointer to this constructor
    lPtrTag :: !Int,
    -- | pointer fields come first in the payload; this is how many
    lPtrs :: !Int,
    lNptrs :: !Int,
    -- | byte offset from the untagged closure address to payload word i
    lFieldOffset :: Int -> Int
  }

data Layouts = Layouts
  { -- | @Enum ref tag@: the tag word is the first non-pointer field
    lEnum :: !Layout,
    -- | @Data1 ref tag (Val u b)@
    lData1 :: !Layout,
    -- | @Data2 ref tag (Val u b) (Val u b)@
    lData2 :: !Layout,
    -- | @DataG ref tag (useg, bseg)@: pointers ref, useg, bseg; one non-pointer, the tag
    lDataG :: !Layout,
    -- | @PAp (CIx ref grp w) (LamI arity fsize entry cell) (useg, bseg)@, with
    -- CombIx and GCombInfo unpacked: pointers ref, entry, useg, bseg;
    -- non-pointers grp, w, arity, fsize, cell
    lPAp :: !Layout,
    -- | info pointers of the lifted boxes around a Seg's arrays
    lByteArrayBoxInfo, lArrayBoxInfo :: !Int,
    -- | @Foreign x@: pointer tag 7, one pointer, the Foreign
    lForeignInfo :: !Int,
    -- | @WrapIORef ref@: pointer tag 7, one pointer, the MutVar#
    lIORefInfo :: !Int,
    -- | @WrapMutableArray arr@: pointer tag 7, one pointer, the MutableArray#
    lMutableArrayInfo :: !Int,
    -- | the other wrappers the array and ref builtins see, as (info
    -- pointer, pointer tag): @WrapArray@, @WrapMutableByteArray@,
    -- @WrapByteArray@ (one pointer each, the unlifted array), @WrapTicket@
    -- (one pointer, the ticket's value) and @WrapPtr@ (one non-pointer, the
    -- address)
    lArrayWrap, lMutableByteArrayWrap, lByteArrayWrap, lTicketWrap, lPtrWrap :: !(Pair Int Int),
    -- | @Val u b@: pointer tag 1; pointer b, then non-pointer u
    lVal :: !Layout,
    -- | @RecordG shape vals@ with the shape and the vector unpacked: pointer
    -- tag 7; pointers the field names' Array#, the positions map and the
    -- values' Array#; non-pointers the shape reference, the names' offset and
    -- length, the values' offset and length ("records" in jit_internal.h)
    lRecord :: !Layout,
    -- | header size in bytes; payload starts after it
    lHeaderBytes :: !Int
  }

any' :: a -> Any
any' = unsafeCoerce

probe :: Any -> [Any] -> [Int] -> IO (Either String Layout)
probe sample0 fields planted = do
  -- Force everything here: the array must hold the evaluated objects, not
  -- thunks that produce them. (Without optimization the arguments are thunks.)
  let !sample = sample0
  arr <- newArray (1 + length fields) sample :: IO (MutableArray RealWorld Any)
  mapM_ (\(i, f0) -> let !f = f0 in writeArray arr i f) (zip [1 ..] fields)
  r <- probeClosure arr
  case r of
    (info : tag : _ty : ptrs : nptrs : _con : rest) -> do
      let total = ptrs + nptrs
          matches = take ptrs (drop total rest)
          words = take nptrs (drop ptrs rest)
      -- every pointer field must be one of the objects we put there, in
      -- order, and the non-pointer words must be the values planted, in order
      if matches == [1 .. ptrs] && length fields == ptrs && words == planted
        then pure (Right (Layout info tag ptrs nptrs (\i -> 8 + 8 * i)))
        else pure (Left ("probe: " ++ show r))
    _ -> pure (Left ("probe: " ++ show r))

-- | The info pointer of a box with one pointer field and pointer tag 1.
probeBox :: Any -> IO (Either String Int)
probeBox = probeInfo 1

-- | The info pointer and pointer tag of a constructor with one pointer
-- field (or, with @False@, one non-pointer field).
probeWrap :: Bool -> Any -> IO (Either String (Pair Int Int))
probeWrap ptrField sample0 = do
  let !sample = sample0
  arr <- newArray 1 sample :: IO (MutableArray RealWorld Any)
  r <- probeClosure arr
  pure $ case r of
    (info : t : _ty : p : n : _) | (p, n) == (if ptrField then (1, 0) else (0, 1)), t >= 1, t <= 7 -> Right (Pair info t)
    _ -> Left ("wrapper probe: " ++ show r)

-- | The info pointer of a constructor with the given pointer tag and one
-- pointer field.
probeInfo :: Int -> Any -> IO (Either String Int)
probeInfo tag sample0 = do
  let !sample = sample0
  arr <- newArray 1 sample :: IO (MutableArray RealWorld Any)
  r <- probeClosure arr
  pure $ case r of
    (info : t : _ty : 1 : 0 : _) | t == tag -> Right info
    _ -> Left ("info probe: " ++ show r)

-- | Probes the layouts the code generator relies on. Returns an
-- explanation if anything is unexpected.
probeLayouts :: IO (Either String Layouts)
probeLayouts = do
  let !ref = Ty.booleanRef
      !nat = natTypeTag
      !nil = Enum ref (PackedTag 0)
      !useg = byteArrayFromList [11 :: Int, 12]
      !bseg = arrayFromList [nat, nil]
  e <- probe (any' $! Enum ref (PackedTag 7)) [any' ref] [7]
  d1 <- probe (any' $! Data1 ref (PackedTag 7) (Val 9 nat)) [any' ref, any' nat] [7, 9]
  d2 <- probe (any' $! Data2 ref (PackedTag 7) (Val 9 nat) (Val 10 nil)) [any' ref, any' nat, any' nil] [7, 9, 10]
  dg <- probe (any' $! DataG ref (PackedTag 7) (useg, bseg)) [any' ref, any' useg, any' bseg] [7]
  let !entry = Exit
      !cell = noNativeCell
      IntPtr cellAddr = ptrToIntPtr cell
  pap <- probe (any' $! PAp (CIx ref 21 22) (LamI 23 24 entry cell) (useg, bseg)) [any' ref, any' entry, any' useg, any' bseg] [21, 22, 23, 24, cellAddr]
  -- the boxes: one pointer field each (the unlifted array), tag 1
  ub <- probeBox (any' useg)
  bb <- probeBox (any' bseg)
  -- a Ref: Foreign (WrapIORef ref), the IORef unpacked to its MutVar#
  ref' <- newIORef (Val 9 nat)
  let !w = WrapIORef ref'
      !fo = Foreign w
  fi <- probeInfo 7 (any' fo)
  wi <- probeInfo 7 (any' w)
  marr <- newArray 1 (Val 9 nat) :: IO (MutableArray RealWorld Val)
  let !wa = WrapMutableArray marr
  ai <- probeInfo 7 (any' wa)
  -- the other array wrappers, a ticket and a pointer
  aw <- probeWrap True (any' $! WrapArray (arrayFromList [Val 9 nat]))
  mba <- newByteArray 8
  mbw <- probeWrap True (any' $! WrapMutableByteArray mba)
  bw <- probeWrap True (any' $! WrapByteArray (byteArrayFromList [1 :: Word8, 2]))
  -- a ticket is the value itself (a newtype): its wrapper's field must be the Val
  let !tv = Val 9 nat
  tref <- newIORef tv
  ticket <- Atomic.readForCAS tref
  tw <- probe (any' $! WrapTicket ticket) [any' tv] []
  pw <- probeWrap False (any' $! WrapPtr nullPtr)
  v <- probe (any' $! Val 9 nat) [any' nat] [9]
  -- a record: its words are checked by unison_jit_record_init (probeRecords);
  -- here only the counts and the pointer tag
  rc <- probeKind 7 3 5 (any' $! RecordG (recordShapeOf 7 [("a", 3), ("bc", 5)]) (recordVals (segFromList [NatVal 9, NatVal 10])))
  pure $ case (e, d1, d2, dg, pap, ub, bb, fi, wi, ai, v, aw, mbw, bw, tw, pw, rc) of
    (Right le, Right ld1, Right ld2, Right ldg, Right lp, Right ubi, Right bbi, Right fii, Right wii, Right aii, Right lv, Right awi, Right mbwi, Right bwi, Right ltw, Right pwi, Right lr)
      | lPtrTag le == 2 && lPtrs le == 1 && lNptrs le == 1,
        lPtrTag ld1 == 3 && lPtrs ld1 == 2 && lNptrs ld1 == 2,
        lPtrTag ld2 == 4 && lPtrs ld2 == 3 && lNptrs ld2 == 3,
        lPtrTag ldg == 5 && lPtrs ldg == 3 && lNptrs ldg == 1,
        lPtrTag lp == 1 && lPtrs lp == 4 && lNptrs lp == 5,
        lPtrTag lv == 1 && lPtrs lv == 1 && lNptrs lv == 1,
        lPtrs ltw == 1 && lNptrs ltw == 0 ->
          Right (Layouts le ld1 ld2 ldg lp ubi bbi fii wii aii awi mbwi bwi (Pair (lInfo ltw) (lPtrTag ltw)) pwi lv lr 8)
      | otherwise -> Left ("closure layouts are not what the code generator expects: " ++ unwords (map summary [le, ld1, ld2, ldg, lp, lv, ltw]))
    _ -> Left ("closure layouts could not be probed: " ++ show ([either id (const "ok") x | x <- [e, d1, d2, dg, pap, v, tw, rc]] ++ [either id (const "ok") x | x <- [ub, bb, fi, wi, ai]] ++ [either id (const "ok") x | x <- [aw, mbw, bw, pw]]))

-- | The layout of a constructor with the given pointer tag and field counts.
probeKind :: Int -> Int -> Int -> Any -> IO (Either String Layout)
probeKind tag ptrs nptrs sample0 = do
  let !sample = sample0
  arr <- newArray 1 sample :: IO (MutableArray RealWorld Any)
  r <- probeClosure arr
  pure $ case r of
    (info : t : _ty : p : n : _) | t == tag, p == ptrs, n == nptrs -> Right (Layout info t p n (\i -> 8 + 8 * i))
    _ -> Left ("kind probe: " ++ show r)

-- | A record shape for the probes: the reference, and the field names with
-- their numbers, which the shape keeps ascending by name.
recordShapeOf :: Word64 -> [(Text.Text, Word64)] -> RecordShape
recordShapeOf ref fields =
  let sorted = M.toAscList (M.fromList fields)
   in RecordShape
        { shapeRef = ANF.RecordRef ref,
          shapeFields = V.fromList (map fst sorted),
          shapePositions = EC.mapFromList (zip (map (FieldRef . snd) sorted) [0 ..])
        }

-- | Records: hands the record helpers sample records (see
-- unison_jit_record_init), which checks the layout and the field lookup
-- against them, then runs records through the hash and the comparison
-- helpers against the Haskell answers. After 'probeEnv': the positions map
-- is an IntMap, whose constructors the environment probe learned.
probeRecords :: Layouts -> IO (Either String ())
probeRecords _ls = do
  let PackedTag someTag = TT.someTag
      shape2 = recordShapeOf 7 [("a", 3), ("bc", 5)]
      shape0 = recordShapeOf 8 []
      shape3 = recordShapeOf 9 [("x", 11), ("y", 12), ("zz", 13)]
      -- the same field names, as a shape compiled elsewhere would have them
      shape3' = recordShapeOf 9 [("x", 11), ("y", 12), ("zz", 13)]
      -- (the pattern synonyms store tagged type-tag pointers; a let-bound
      -- natTypeTag in this unoptimized module would be an untagged CAF)
      !v0 = NatVal 9
      !v1 = NatVal 10
      -- the values through the interpreter's builder, which evaluates them
      -- (a list literal's elements are thunks in this unoptimized module)
      !r2 = RecordG shape2 (recordVals (segFromList [v0, v1]))
      !r0 = RecordG shape0 V.empty
      !r3 = RecordG shape3 (recordVals (segFromList [NatVal 1, NatVal 2, NatVal 3]))
      -- the names are evaluated texts in a shape the emitter builds; here
      -- they are literals, thunks in an unoptimized build
      forceNames sh = mapM_ evaluate (V.toList (shapeFields sh))
  mapM_ forceNames [shape2, shape0, shape3, shape3']
  arr <- newArray 5 (any' r2) :: IO (MutableArray RealWorld Any)
  let put i v = evaluate v >>= writeArray arr i
  put 0 (any' r2)
  put 1 (any' v0)
  put 2 (any' v1)
  put 3 (any' r0)
  put 4 (any' r3)
  ok <- recordInit arr [3, 5, 7, 11, 12, 13]
  if ok /= 1
    then pure (Left ("the record layout is not what the record helpers expect (check " ++ show ok ++ ")"))
    else do
      -- the hash: reflection then the Haskell hash, as probeMurmur does
      let expected :: ANF.Value Reference -> Word64
          expected v = asWord64 (hash64ValueUntyped (Plain v))
          pos = ANF.BLit . ANF.Pos
          recV names vs = ANF.Record (ANF.RecordSchema (Set.fromList names)) vs
          some v = BoxedVal (Data1 Ty.optionalRef (PackedTag someTag) v)
          text t = BoxedVal (Foreign (WrapText (UText.pack t)))
          rec sh vs = BoxedVal (RecordG sh (recordVals (segFromList vs)))
          shapeN = recordShapeOf 10 [("inner", 21), ("name", 22)]
          !rNested = RecordG shapeN (recordVals (segFromList [BoxedVal r3, text "héllo"]))
          hashes :: [(String, Closure, ANF.Value Reference)]
          hashes =
            [ ("two fields", r2, recV ["a", "bc"] [pos 9, pos 10]),
              ("no fields", r0, recV [] []),
              ("three fields", r3, recV ["x", "y", "zz"] [pos 1, pos 2, pos 3]),
              ("nested", rNested, recV ["inner", "name"] [recV ["x", "y", "zz"] [pos 1, pos 2, pos 3], ANF.BLit (ANF.Text (UText.pack "héllo"))])
            ]
      forceNames shapeN
      harr <- newArray 1 (any' r2) :: IO (MutableArray RealWorld Any)
      hs <- forM hashes $ \(name, c, v) -> do
        evaluate (any' c) >>= writeArray harr 0
        got <- murmurTest harr 0 False
        pure (if got == Just (expected v) then Nothing else Just ("murmurHashUntyped of a record (" ++ name ++ "): native " ++ show got ++ ", Haskell " ++ show (expected v)))
      -- the comparisons, as probeCompare does
      let anyOf v = BoxedVal (Data1 Ty.anyRef TT.anyTag v)
          both = (True, True)
          pairs :: [(String, Val, Val, (Bool, Bool))]
          pairs =
            [ ("equal records", rec shape3 [NatVal 1, NatVal 2, NatVal 3], rec shape3' [NatVal 1, NatVal 2, NatVal 3], both),
              ("records differing late", rec shape3 [NatVal 1, NatVal 2, NatVal 3], rec shape3' [NatVal 1, NatVal 2, NatVal 4], both),
              ("records differing early", rec shape3 [NatVal 2, NatVal 0, NatVal 0], rec shape3' [NatVal 1, NatVal 9, NatVal 9], both),
              ("records of texts", rec shape2 [text "a", text "b"], rec shape2 [text "a", text "c"], both),
              ("nested records", rec shape2 [BoxedVal r3, NatVal 1], rec shape2 [BoxedVal r3, NatVal 2], both),
              ("empty records", rec shape0 [], rec shape0 [], both),
              ("record against data", rec shape0 [], some (NatVal 1), both),
              ("record against text", rec shape0 [], text "a", both),
              ("Any records of the same shape", anyOf (rec shape2 [NatVal 1, NatVal 2]), anyOf (rec shape2 [NatVal 1, NatVal 3]), both),
              -- different shapes order by their field names: the interpreter's
              ("Any records of different shapes", anyOf (rec shape2 [NatVal 1, NatVal 2]), anyOf (rec shape3 [NatVal 1, NatVal 2, NatVal 3]), (False, True))
            ]
      carr <- newArray 2 (any' r2) :: IO (MutableArray RealWorld Any)
      let toInt o = fromEnum o - 1
      cs <- forM pairs $ \(name, l, r, (handledC, handledE)) -> do
        evaluate (any' (getBoxedVal l)) >>= writeArray carr 0
        evaluate (any' (getBoxedVal r)) >>= writeArray carr 1
        gotC <- cmpTest carr (getUnboxedVal l) (getUnboxedVal r) False
        gotE <- cmpTest carr (getUnboxedVal l) (getUnboxedVal r) True
        let wantC = toInt (compare l r)
            wantE = fromEnum (l == r)
            okC = if handledC then gotC == Just wantC else gotC == Nothing
            okE = if handledE then gotE == Just wantE else gotE == Nothing
        pure $
          if okC && okE
            then Nothing
            else Just ("Universal comparison of records (" ++ name ++ "): native " ++ show (gotC, gotE) ++ ", Haskell " ++ show (wantC, wantE) ++ (if handledC && handledE then "" else " (expected to be left to the interpreter)"))
      pure $ case catMaybes (hs ++ cs) of
        [] -> Right ()
        e : _ -> Left e

-- | Teaches the C array and ref helpers the wrappers' info pointers and
-- tags, and the runtime's empty value (what @Scope.array@ fills with).
probeArrays :: Layouts -> IO (Either String ())
probeArrays ls = do
  let !e = emptyVal
  arr <- newArray 1 (any' e) :: IO (MutableArray RealWorld Any)
  let pair (Pair i t) = [i, t]
  ok <- arrayInit arr (concatMap pair [Pair (lMutableArrayInfo ls) 7, lArrayWrap ls, lMutableByteArrayWrap ls, lByteArrayWrap ls, Pair (lIORefInfo ls) 7, lTicketWrap ls, lPtrWrap ls])
  pure (if ok == 1 then Right () else Left "the array helpers could not be initialized")

summary :: Layout -> String
summary l = show (lPtrTag l, lPtrs l, lNptrs l)

-- | Teaches the hash helper the closures it needs to recognize and checks
-- it against the Haskell hash (reflection then 'hash64ValueUntyped') on
-- sample values of every kind it handles.
probeMurmur :: Layouts -> IO (Either String ())
probeMurmur ls = do
  let !nat = natTypeTag
      !int = intTypeTag
      !flt = floatTypeTag
      !chr = charTypeTag
      PackedTag someTag = TT.someTag
      PackedTag noneTag = TT.noneTag
      PackedTag pairTag = TT.pairTag
  arr <- newArray 1 (any' nat) :: IO (MutableArray RealWorld Any)
  let put :: Any -> IO ()
      put v = evaluate v >>= writeArray arr 0
  put (any' nat)
  ok <- murmurInit arr [lInfo (lEnum ls), lInfo (lDataG ls)]
  if ok /= 1
    then pure (Left "the murmur hash helper could not be initialized")
    else do
      let expected :: ANF.Value Reference -> Word64
          expected v = asWord64 (hash64ValueUntyped (Plain v))
          pos = ANF.BLit . ANF.Pos
          dat r t vs = ANF.Data r (TT.maskTags (PackedTag t)) vs
          some v = dat Ty.optionalRef someTag [v]
          none = dat Ty.optionalRef noneTag []
          pair a b = dat Ty.pairRef pairTag [a, dat Ty.pairRef pairTag [b, dat Ty.unitRef 0 []]]
          text t = ANF.BLit (ANF.Text (UText.pack t))
          bytes bs = ANF.BLit (ANF.Bytes (By.fromWord8s bs))
          list vs = ANF.BLit (ANF.List (Seq.fromList vs))
          !unit = Enum Ty.unitRef (PackedTag 0)
          !noneC = Enum Ty.optionalRef (PackedTag noneTag)
          someC v = Data1 Ty.optionalRef (PackedTag someTag) v
          pairC a b = Data2 Ty.pairRef (PackedTag pairTag) a (BoxedVal (Data2 Ty.pairRef (PackedTag pairTag) b (BoxedVal unit)))
          !useg = byteArrayFromList [3 :: Int, 2, 1]
          !bseg = arrayFromList [nat, nat, nat]
          -- a three-field constructor: the segments hold the fields in reverse
          !dg = DataG Ty.failureRef (PackedTag 5) (useg, bseg)
          dgV = dat Ty.failureRef 5 [pos 1, pos 2, pos 3]
          textC t = Foreign (WrapText (UText.pack t))
          bytesC bs = Foreign (WrapBytes (By.fromWord8s bs))
          listC vs = Foreign (WrapSeq (Sq.fromList vs))
          boxed :: [(String, Closure, ANF.Value Reference)]
          boxed =
            [ ("None", noneC, none),
              ("Some 7", someC (NatVal 7), some (pos 7)),
              ("Some (-3)", someC (IntVal (-3)), some (ANF.BLit (ANF.Neg 3))),
              ("Some +3", someC (IntVal 3), some (pos 3)),
              ("Some 1.5", someC (DoubleVal 1.5), some (ANF.BLit (ANF.Float 1.5))),
              ("Some 'x'", someC (CharVal 'x'), some (ANF.BLit (ANF.Char 'x'))),
              ("pair", pairC (NatVal 1) (NatVal 2), pair (pos 1) (pos 2)),
              ("DataG", dg, dgV),
              ("text", textC "héllo wörld ✓", text "héllo wörld ✓"),
              ("empty text", textC "", text ""),
              ("bytes", bytesC [1, 2, 255], bytes [1, 2, 255]),
              ("list", listC [NatVal 1, NatVal 2, BoxedVal (someC (NatVal 3))], list [pos 1, pos 2, some (pos 3)]),
              ("empty list", listC [], list []),
              ("long list", listC (map NatVal [1 .. 500]), list (map pos [1 .. 500])),
              ("nested", someC (BoxedVal (listC [BoxedVal (textC "a"), BoxedVal noneC])), some (list [text "a", none]))
            ]
          unboxed :: [(String, Int, Closure, ANF.Value Reference)]
          unboxed =
            [ ("Nat 5", 5, nat, pos 5),
              ("Nat max", -1, nat, pos maxBound),
              ("Int -1", -1, int, ANF.BLit (ANF.Neg 1)),
              ("Int 9", 9, int, pos 9),
              ("Float 2.5", fromIntegral (castDoubleToWord64 2.5), flt, ANF.BLit (ANF.Float 2.5)),
              ("Char 'é'", fromEnum 'é', chr, ANF.BLit (ANF.Char 'é'))
            ]
          check name got want
            | got == Just want = pure Nothing
            | otherwise = pure (Just ("murmurHashUntyped of " ++ name ++ ": native " ++ show got ++ ", Haskell " ++ show want))
      rs <- forM boxed $ \(name, c, v) -> do
        put (any' c)
        got <- murmurTest arr 0 False
        check name got (expected v)
      rs2 <- forM unboxed $ \(name, u, t, v) -> do
        put (any' t)
        got <- murmurTest arr u True
        check name got (expected v)
      pure $ case catMaybes (rs ++ rs2) of
        [] -> Right ()
        e : _ -> Left e

-- | Universal equality and ordering: hands the comparison helpers what
-- they need beyond the other tables (see unison_jit_cmp_init), then runs
-- pairs of every kind of value through the C copies and the interpreter's
-- @Eq Val@ and @Ord Val@; any difference, or a pair the helper should
-- handle and doesn't, turns the JIT off.
probeCompare :: Layouts -> IO (Either String ())
probeCompare ls = do
  let PackedTag intTag = TT.intTag
      PackedTag natTag = TT.natTag
  ok <- cmpInit [lInfo (lPAp ls), fromIntegral intTag, fromIntegral natTag, fromIntegral (TT.typeTag TT.anyTag)]
  if ok /= 1
    then pure (Left "the comparison helpers could not be initialized")
    else do
      let PackedTag someTag = TT.someTag
          PackedTag noneTag = TT.noneTag
          PackedTag pairTag = TT.pairTag
          !unit = BoxedVal (Enum Ty.unitRef (PackedTag 0))
          !none = BoxedVal (Enum Ty.optionalRef (PackedTag noneTag))
          some v = BoxedVal (Data1 Ty.optionalRef (PackedTag someTag) v)
          pair a b = BoxedVal (Data2 Ty.pairRef (PackedTag pairTag) a (BoxedVal (Data2 Ty.pairRef (PackedTag pairTag) b unit)))
          -- a three-field constructor: the segments hold the fields in reverse
          !nat = natTypeTag
          -- the fields are (word, evaluated tag closure) pairs: a thunk in the array is "not handled"
          dg :: [Int] -> Val
          dg us = let !u = byteArrayFromList (reverse us); !b = arrayFromList (replicate (length us) nat) in BoxedVal (DataG Ty.failureRef (PackedTag 5) (u, b))
          anyOf v = BoxedVal (Data1 Ty.anyRef TT.anyTag v)
          text t = BoxedVal (Foreign (WrapText (UText.pack t)))
          bytes bs = BoxedVal (Foreign (WrapBytes (By.fromWord8s bs)))
          list vs = BoxedVal (Foreign (WrapSeq (Sq.fromList vs)))
          -- an array's elements are lazy: evaluate them first, as the runtime's arrays hold values
          array :: [Val] -> IO Val
          array vs = traverse evaluate vs >>= \vs' -> pure (BoxedVal (Foreign (WrapArray (arrayFromList vs'))))
          barray bs = BoxedVal (Foreign (WrapByteArray (byteArrayFromList (bs :: [Word8]))))
          !ref = Ty.booleanRef
          !cix = CIx ref 21 22
          !comb = LamI 5 7 Exit noNativeCell
          pap us n = let !u = byteArrayFromList (us :: [Int]); !b = arrayFromList (replicate n nat) in BoxedVal (PAp cix comb (u, b))
          !cix2 = CIx Ty.unitRef 21 22
          pap2 = BoxedVal (PAp cix2 comb (byteArrayFromList ([] :: [Int]), arrayFromList []))
          dbl = DoubleVal
      arr12 <- array [NatVal 1, NatVal 2]
      arr12' <- array [NatVal 1, NatVal 2]
      arr13 <- array [NatVal 1, NatVal 3]
      let -- (name, left, right, whether the helper must handle the pair for ordering and for equality)
          pairs :: [(String, Val, Val, (Bool, Bool))]
          both = (True, True)
          neither = (False, False)
          pairs =
            [ ("Nat Nat", NatVal 3, NatVal 5, both),
              ("Nat max against 1", NatVal maxBound, NatVal 1, both),
              ("Int Int", IntVal (-1), IntVal 1, both),
              ("Nat against negative Int", NatVal 1, IntVal (-1), both),
              ("negative Int against Nat", IntVal (-1), NatVal 1, both),
              ("Nat against equal Int", NatVal 7, IntVal 7, both),
              ("Char against Nat", CharVal 'a', NatVal 97, both),
              ("Char Char", CharVal 'a', CharVal 'b', both),
              ("Float Float", dbl 1.5, dbl 2.5, both),
              ("negative Floats", dbl (-1.0), dbl (-2.0), both),
              ("negative against positive Float", dbl (-1.0), dbl 2.0, both),
              ("zero against negative zero", dbl 0.0, dbl (-0.0), both),
              ("NaN NaN", dbl (0 / 0), dbl (0 / 0), both),
              ("Float against Nat", dbl 1.0, NatVal 1, both),
              ("unboxed against boxed", NatVal 1, none, both),
              ("None None", none, none, both),
              ("None Some", none, some (NatVal 1), both),
              ("Some Some", some (NatVal 1), some (NatVal 2), both),
              ("Some Int against Some Nat", some (IntVal 1), some (NatVal 1), both),
              ("pairs differing late", pair (NatVal 1) (NatVal 2), pair (NatVal 1) (NatVal 3), both),
              ("equal pairs", pair (NatVal 1) (NatVal 2), pair (NatVal 1) (NatVal 2), both),
              ("DataG DataG", dg [1, 2, 3], dg [1, 2, 4], both),
              ("equal DataG", dg [1, 2, 3], dg [1, 2, 3], both),
              ("unit against None", unit, none, both),
              ("false against true", BoxedVal (Enum Ty.booleanRef (PackedTag 0)), BoxedVal (Enum Ty.booleanRef (PackedTag 1)), both),
              ("true against true", BoxedVal (Enum Ty.booleanRef (PackedTag 1)), BoxedVal (Enum Ty.booleanRef (PackedTag 1)), both),
              ("Some against pair", some (NatVal 1), pair (NatVal 1) (NatVal 1), both),
              ("Any Any", anyOf (NatVal 1), anyOf (NatVal 2), both),
              ("Any Nat against Any Char", anyOf (NatVal 97), anyOf (CharVal 'a'), both),
              ("Any Some against Any None", anyOf (some (NatVal 1)), anyOf none, (False, True)),
              ("texts", text "abc", text "abd", both),
              ("equal texts", text "abc", text "abc", both),
              ("bytes", bytes [1, 2, 3], bytes [1, 2], both),
              ("prefix list", list [NatVal 1, NatVal 2], list [NatVal 1, NatVal 2, NatVal 3], both),
              ("equal lists", list [NatVal 1, NatVal 2, NatVal 3], list [NatVal 1, NatVal 2, NatVal 3], both),
              ("lists differing early", list [NatVal 1, NatVal 3], list [NatVal 1, NatVal 2, NatVal 5], both),
              ("empty lists", list [], list [], both),
              ("empty against list", list [], list [NatVal 0], both),
              ("nested lists", list [some (NatVal 1), text "a"], list [some (NatVal 1), text "b"], both),
              ("long lists", list (map NatVal [1 .. 300]), list (map NatVal ([1 .. 299] ++ [0])), both),
              ("arrays", arr12, arr13, both),
              ("equal arrays", arr12, arr12', both),
              ("byte arrays", barray [1, 2], barray [1, 2, 0], both),
              ("equal byte arrays", barray [1, 2], barray [1, 2], both),
              ("text against list", text "a", list [], neither),
              ("data against text", none, text "a", both),
              ("functions differing in an argument", pap [1] 1, pap [2] 1, both),
              ("equal functions", pap [1] 1, pap [1] 1, both),
              ("function against data", pap [] 0, some (NatVal 1), both),
              ("functions of different references", pap [] 0, pap2, neither),
              -- two different maps: identical samples would be one closure at -O2, and the same object is equal to itself
              ("maps", BoxedVal (Foreign (WrapMap (M.singleton (NatVal 1) (NatVal 1)))), BoxedVal (Foreign (WrapMap (M.singleton (NatVal 2) (NatVal 2)))), neither)
            ]
      arr <- newArray 2 (any' natTypeTag) :: IO (MutableArray RealWorld Any)
      let put :: Int -> Any -> IO ()
          put i v = evaluate v >>= writeArray arr i
          toInt o = fromEnum o - 1
      rs <- forM pairs $ \(name, l, r, (handledC, handledE)) -> do
        put 0 (any' (getBoxedVal l))
        put 1 (any' (getBoxedVal r))
        gotC <- cmpTest arr (getUnboxedVal l) (getUnboxedVal r) False
        gotE <- cmpTest arr (getUnboxedVal l) (getUnboxedVal r) True
        let wantC = toInt (compare l r)
            wantE = fromEnum (l == r)
            okC = if handledC then gotC == Just wantC else gotC == Nothing
            okE = if handledE then gotE == Just wantE else gotE == Nothing
        pure $
          if okC && okE
            then Nothing
            else Just ("universal comparison of " ++ name ++ ": native " ++ show (gotC, gotE) ++ ", Haskell " ++ show (wantC, wantE) ++ ", expected handled " ++ show (handledC, handledE))
      pure $ case catMaybes rs of
        [] -> Right ()
        e : _ -> Left e

-- | The handler environment: teaches the request helpers the layouts of
-- @HEnv@, the @IntMap@ under @EnumMap@, the @STRef@ box inside an
-- @AffineRef@ and the @Affine@ closure, from samples, and checks the
-- native lookup against what the interpreter's @resolve@ would find
-- (see unison_jit_env_init in jit_rt.c).
probeEnv :: IO (Either String ())
probeEnv = do
  let !ref = Ty.booleanRef
      !v3 = Enum ref (PackedTag 3)
      !v10 = Enum ref (PackedTag 10)
      !v77 = Enum ref (PackedTag 77)
  r3 <- newIORef v3
  r10 <- newIORef v10
  r77 <- newIORef v77
  let IORef box3 = r3
      !aenv = EC.mapFromList [(3 :: Word64, ARef r3), (10, ARef r10), (77, ARef r77)]
      !denv2 = EC.mapFromList [(10 :: Word64, BoxedVal v10)]
      !empty = EC.mapFromList [] :: EC.EnumMap Word64 AffineRef
      !set3 = EC.setSingleton (3 :: Word64)
  _ <- evaluate emptyDEnv
  h1 <- evaluate (HEnv aenv emptyDEnv)
  h2 <- evaluate (HEnv aenv denv2)
  aff <- evaluate (Affine set3 aenv (ARef r3))
  arr <- newArray 9 (any' h1) :: IO (MutableArray RealWorld Any)
  let put i v = evaluate v >>= writeArray arr i
  put 0 (any' h1)
  put 1 (any' aenv)
  put 2 (any' h2)
  put 3 (any' box3)
  put 4 (any' v3)
  put 5 (any' v10)
  put 6 (any' empty)
  put 7 (any' aff)
  put 8 (any' set3)
  ok <- envInit arr
  if ok == 1
    then pure (Right ())
    else do
      dbg <- forM [any' empty, any' aenv, any' box3, any' h1] $ \x -> do
        a <- newArray 1 x :: IO (MutableArray RealWorld Any)
        evaluate x >>= writeArray a 0
        probeClosure a
      pure (Left ("the handler environment is not what the request helpers expect (check " ++ show ok ++ "; " ++ show dbg ++ ")"))

-- | Lists: teaches the C helpers (jit_rt.c) the constructors of
-- Unison.Util.Deque from samples, checks the structure of a range of lists
-- against what the helpers assume, and then runs every helper against the
-- Haskell operation it stands in for; each list a helper builds is checked
-- for its structure too. Returns an explanation if anything differs.
--
-- @steps@ (stress mode @lists=N@) adds a longer test: that many random
-- operations, each done by a helper on the results of earlier ones and
-- compared with the Haskell operation.
probeLists :: Layouts -> Int -> IO (Either String ())
probeLists ls steps = do
  let nat i = NatVal (fromIntegral (i :: Int))
      wrap :: Sq.Deque Val -> Any
      wrap d = any' $! Foreign (WrapSeq d)
      !x = nat 1000
      !sample = x Sq.<| (Sq.empty Sq.|> nat 1 Sq.|> nat 2 Sq.|> nat 3)
      PackedTag elemTag = TT.seqViewElemTag
      PackedTag someTag = TT.someTag
      !emptyView = Enum Ty.seqViewRef TT.seqViewEmptyTag
      !none = Enum Ty.optionalRef TT.noneTag
      range a b = Sq.fromList (map nat [a .. b])
      -- lists of many shapes: built from either end, appended, cut, and drained
      built n = [range 1 n, foldr (Sq.<|) Sq.empty (map nat [1 .. n])]
      samples =
        concatMap built ([0 .. 12] ++ [63, 64, 65, 100, 200, 513, 1000, 3000])
          ++ [range 1 12000]
          ++ [range 1 a Sq.>< range 1 b | (a, b) <- [(9, 10), (33, 300), (150, 47), (1200, 2500)]]
          ++ [Sq.drop k (range 1 n) | (n, k) <- [(100, 7), (1000, 60), (5000, 1), (5000, 97)]]
          ++ [Sq.take k (range 1 n) | (n, k) <- [(100, 50), (1000, 93), (5000, 3)]]
          ++ take 10 (iterate (\d -> case d of _ Sq.:<| r -> r; r -> r) (range 1 300))
          ++ take 10 (iterate (\d -> case d of r Sq.:|> _ -> r; r -> r) (range 1 300))
  -- The array must hold the evaluated objects, never thunks that produce
  -- them. (`evaluate`, not `seq`: the optimizer makes top-level constants of
  -- some samples, and a `seq` on one can leave the reference to the
  -- unevaluated constant in the array.)
  let put :: MutableArray RealWorld Any -> Int -> Any -> IO ()
      put a i v = evaluate v >>= writeArray a i
      !first = wrap sample
  arr <- newArray 4 first :: IO (MutableArray RealWorld Any)
  put arr 1 (any' x)
  put arr 2 (wrap Sq.empty)
  put arr 3 (wrap (range 1 1000))
  ok <- listInit arr [lForeignInfo ls, lInfo (lVal ls), lInfo (lData1 ls), lInfo (lData2 ls)]
  if ok /= 1
    then pure (Left ("the list constructors are not laid out as the JIT's helpers expect (check " ++ show ok ++ ")"))
    else do
      let same a b = BoxedVal a == BoxedVal b
          listOf c = case c of
            Foreign (WrapSeq d) -> Just d
            _ -> Nothing
          -- a helper on the list d, with its other arguments; its result
          run :: Sq.Deque Val -> Int -> Any -> Int -> Int -> IO (Maybe Closure)
          run d op other a b = do
            put arr 0 (wrap d)
            put arr 1 other
            h <- listTest arr op a b
            if h then Just . unsafeCoerce <$> readArray arr 3 else pure Nothing
          -- a few positions of a list of n elements: all, if it is short
          spread n
            | n <= 40 = [0 .. n - 1]
            | otherwise = [0 .. 11] ++ [(n * k) `div` 23 | k <- [1 .. 22]] ++ [n - 12 .. n - 1]
          agree full d e
            | Sq.length d /= Sq.length e = False
            -- (the C walk has checked the structure; the Haskell check of it is for the longer test)
            | full = (steps == 0 || either (const False) (const True) (Sq.valid d)) && d == e
            | otherwise = all (\i -> Sq.lookup i d == Sq.lookup i e) (spread (Sq.length e))
          -- What a helper built against the list it should be: Nothing if
          -- they differ, else the helper's list. Checking every element is
          -- for lists that aren't long (full).
          checked :: Bool -> Sq.Deque Val -> Maybe Closure -> IO (Maybe (Sq.Deque Val))
          checked full expected = \case
            Just c | Just d <- listOf c -> do
              put arr 0 (any' c)
              k <- listCheck arr
              pure (if k >= 0 && agree full d expected then Just d else Nothing)
            _ -> pure Nothing
          push full front v d =
            run d (if front then 2 else 3) (any' natTypeTag) v 0
              >>= checked full (if front then nat v Sq.<| d else d Sq.|> nat v)
          view full left d = do
            r <- run d (if left then 0 else 1) (any' emptyView) (fromIntegral elemTag) 0
            case (r, left, d) of
              (Just c, _, Sq.Empty) -> pure (if same c emptyView then Just d else Nothing)
              (Just (Data2 rf t a (BoxedVal c)), True, e Sq.:<| rest)
                | rf == Ty.seqViewRef, t == TT.seqViewElemTag, a == e -> checked full rest (Just c)
              (Just (Data2 rf t (BoxedVal c) b), False, rest Sq.:|> e)
                | rf == Ty.seqViewRef, t == TT.seqViewElemTag, b == e -> checked full rest (Just c)
              _ -> pure Nothing
          -- a negative count is a Nat too large to be a size
          cut full tk k d =
            run d (if tk then 5 else 6) (any' none) k 0
              >>= checked full (if tk then (if k < 0 then d else Sq.take k d) else (if k < 0 then Sq.empty else Sq.drop k d))
          split full left k d = do
            r <- run d (if left then 7 else 8) (any' emptyView) k (fromIntegral elemTag)
            let n = Sq.length d
                (ea, eb) = Sq.splitAt (if left then k else n - k) d
            case r of
              Just c | n < k -> pure (if same c emptyView then Just (d, d) else Nothing)
              Just (Data2 rf t (BoxedVal a) (BoxedVal b)) | rf == Ty.seqViewRef, t == TT.seqViewElemTag -> do
                ra <- checked full ea (Just a)
                rb <- checked full eb (Just b)
                pure ((,) <$> ra <*> rb)
              _ -> pure Nothing
          cat full a b = run a 9 (wrap b) 0 0 >>= checked full (a Sq.>< b)
          lit k v = run Sq.empty 10 (any' natTypeTag) k v >>= checked True (Sq.fromList (map nat [v .. v + k - 1]))
          at d i = do
            r <- run d 4 (any' none) i (fromIntegral someTag)
            pure (maybe False (\c -> same c (maybe none (Data1 Ty.optionalRef TT.someTag) (Sq.lookup i d))) r)
          yes = maybe False (const True)
          -- one sample: its structure, then each helper against Haskell
          check :: Sq.Deque Val -> IO (Either String Int)
          check d = do
            let n = Sq.length d
                full = n <= 64
                cuts = [-1, 1, 10, n `div` 2, n - 10, n + 1]
            put arr 0 (wrap d)
            kinds <- listCheck arr
            if kinds < 0
              then pure (Left ("a list of " ++ show n ++ " elements isn't laid out as expected"))
              else do
                ixOk <- and <$> mapM (at d) ([-1, n] ++ spread n)
                viewOk <- and <$> mapM (\left -> yes <$> view full left d) [True, False]
                pushOk <- and <$> mapM (\front -> yes <$> push full front 77 d) [True, False]
                cutOk <- and <$> sequence [yes <$> cut full tk k d | tk <- [True, False], k <- cuts]
                splitOk <- and <$> sequence [yes <$> split full left k d | left <- [True, False], k <- [3, n `div` 2, n + 1]]
                pure $ case () of
                  _
                    | not ixOk -> Left ("List.at on a list of " ++ show n ++ " elements differs from the interpreter's")
                    | not viewOk -> Left ("a list pattern on a list of " ++ show n ++ " elements differs from the interpreter's")
                    | not pushOk -> Left ("adding to a list of " ++ show n ++ " elements differs from the interpreter")
                    | not cutOk -> Left ("List.take or List.drop on a list of " ++ show n ++ " elements differs from the interpreter's")
                    | not splitOk -> Left ("splitting a list of " ++ show n ++ " elements differs from the interpreter")
                    | otherwise -> Right kinds
          firstLeft :: [IO (Either String a)] -> IO (Either String [a])
          firstLeft = foldM (\acc io -> case acc of Left e -> pure (Left e); Right rs -> fmap (: rs) <$> io) (Right [])
          -- lists that helpers built, used again: a full digit at each
          -- level, then drained from the other end
          grow front = go (200 :: Int) Sq.empty
            where
              go 0 d = shrink (200 :: Int) d
              go k d = push (k > 160) front k d >>= maybe (pure (Left "a list built by the helpers differs from the interpreter's")) (go (k - 1))
              shrink 0 _ = pure (Right ())
              shrink k d = view (k < 40) (not front) d >>= maybe (pure (Left "a list drained by the helpers differs from the interpreter's")) (shrink (k - 1))
          pairs = [d | (i, d) <- zip [0 :: Int ..] samples, i `mod` 6 == 0]
          appends = [(\r -> if yes r then Right () else Left ("List.++ on lists of " ++ show (Sq.length a) ++ " and " ++ show (Sq.length b) ++ " elements differs from the interpreter's")) <$> cat (Sq.length a + Sq.length b <= 64) a b | a <- pairs, b <- pairs]
          lits = [(\r -> if yes r then Right () else Left ("a list literal of " ++ show k ++ " elements differs from the interpreter's")) <$> lit k 5 | k <- [0 .. 24]]
          -- (The checks above are sized to cost little at every startup: they
          -- confirm the layouts and that each helper works here. The thorough
          -- test of the helpers is the longer one, run when they change.)
          -- the longer test: random operations on eight lists
          mix :: Int -> Int
          mix z0 =
            let z1 = (z0 `xor` (z0 `shiftR` 30)) * (-4658895280553007687)
                z2 = (z1 `xor` (z1 `shiftR` 27)) * (-7723592293110705685)
             in (z2 `xor` (z2 `shiftR` 31)) .&. maxBound
          stress :: Int -> [Sq.Deque Val] -> IO (Either String ())
          stress step pool
            | step >= steps = pure (Right ())
            | otherwise = do
                let r k = mix (step * 16 + k)
                    i = r 0 `mod` 8
                    d = pool !! i
                    e = pool !! (r 1 `mod` 8)
                    n = Sq.length d
                    full = n <= 3000 || r 2 `mod` 16 == 0
                    pos = case r 3 `mod` 4 of
                      0 -> r 4 `mod` 12
                      1 -> n - r 4 `mod` 12
                      _ -> r 4 `mod` (n + 2) - 1
                    op = r 5 `mod` 16
                    front = even (r 7)
                    times :: Int -> (Sq.Deque Val -> IO (Maybe (Sq.Deque Val))) -> Sq.Deque Val -> IO (Maybe (Sq.Deque Val))
                    times 0 _ l = pure (Just l)
                    times k f l = f l >>= maybe (pure Nothing) (times (k - 1) f)
                res <-
                  if
                    | op < 2 -> push full True step d
                    | op < 4 -> push full False step d
                    | op == 4 -> view full True d
                    | op == 5 -> view full False d
                    | op == 6 -> cut full True pos d
                    | op == 7 -> cut full False pos d
                    | op == 8 -> fmap fst <$> split full True pos d
                    | op == 9 -> fmap snd <$> split full False pos d
                    | op <= 11 -> if n + Sq.length e > 400000 then cut full True (r 6 `mod` 50) d else cat full d e
                    | op == 12 -> (\good -> if good then Just d else Nothing) <$> at d pos
                    | op == 13 -> times (r 6 `mod` 300) (view (n <= 300) front) d
                    | otherwise -> times (r 6 `mod` 300) (push (n <= 300) front step) d
                case res of
                  Nothing -> pure (Left ("the list helpers' test failed at step " ++ show step ++ " (operation " ++ show op ++ " on a list of " ++ show n ++ " elements)"))
                  Just d' -> stress (step + 1) (take i pool ++ [d'] ++ drop (i + 1) pool)
      r <- firstLeft (map check samples)
      r2 <- firstLeft ([grow True, grow False] ++ appends ++ lits)
      r3 <- if steps > 0 then stress 0 (replicate 8 Sq.empty) else pure (Right ())
      pure $ case (r, r2, r3) of
        (Left e, _, _) -> Left e
        (_, Left e, _) -> Left e
        (_, _, Left e) -> Left e
        (Right kinds, _, _)
          | foldr (.|.) 0 kinds /= 15 -> Left ("the list samples don't cover every constructor (" ++ show (foldr (.|.) 0 kinds) ++ ")")
          | otherwise -> Right ()

-- | Text: the same for the text helpers. Each helper is run against the
-- Haskell operation on texts of many shapes (one chunk, deep ropes, several
-- bytes per character), and every text a helper builds is checked for its
-- structure too.
--
-- @steps@ (stress mode @texts=N@) adds the longer test, as for lists: that
-- many random operations, each done by a helper on the results of earlier
-- ones and compared with the Haskell operation.
probeTexts :: Layouts -> Int -> IO (Either String ())
probeTexts ls steps = do
  let wrap :: UText.Text -> Any
      wrap t = any' $! Foreign (WrapText t)
      put :: MutableArray RealWorld Any -> Int -> Any -> IO ()
      put a i v = evaluate v >>= writeArray a i
      !abc = UText.pack "abc"
      !first = wrap abc
      same a b = BoxedVal a == BoxedVal b
      PackedTag someTag = TT.someTag
      PackedTag pairTag = TT.pairTag
      PackedTag rightTag = TT.rightTag
      !none = Enum Ty.optionalRef TT.noneTag
      unitVal = BoxedVal (Enum Ty.unitRef TT.unitTag)
      -- the pair (x, y) as the interpreter builds it
      tup2 x y = Data2 Ty.pairRef TT.pairTag x (BoxedVal (Data2 Ty.pairRef TT.pairTag y unitVal))
      some = Data1 Ty.optionalRef TT.someTag
      textC = Foreign . WrapText
      chars t = UText.toString t
      -- readIntegral and readi of Machine/Primops.hs
      readClamped :: forall n. (Bounded n, Integral n) => String -> Maybe n
      readClamped str = case readMaybe str :: Maybe Integer of
        Just i | i >= fromIntegral (minBound :: n), i <= fromIntegral (maxBound :: n) -> Just (fromInteger i)
        _ -> Nothing
      readI ('+' : str) = readClamped str :: Maybe Int
      readI str = readClamped str
      -- two chunks with more characters than this between them stay two
      th = Rope.threshold
      pieces = ["", "a", "abc", "h\233llo w\246rld", "\8364\128512x\128512", "0123456789abcdef", replicate (th - 1) 'x', replicate th 'y', replicate (th + 1) 'z', concat (replicate 40 "\955x"), replicate 600 'q']
      base = map UText.pack pieces
      -- ropes with structure: built up piece by piece from either side, by
      -- repetition, and cut; `chunky` is in chunks too big to be joined, so
      -- it has the most levels for its size
      grown = [foldl (\acc i -> acc <> UText.pack (show i)) mempty [1 .. n :: Int] | n <- [5, 40, 300]]
      grownL = [foldl (\acc i -> UText.pack (show i) <> acc) mempty [1 .. n :: Int] | n <- [40, 300]]
      big = UText.replicate 2000 (UText.pack "a\233")
      chunky = foldl (\acc i -> acc <> UText.pack (take (th `div` 2 + 1) (drop (i `mod` 7) (cycle "0123456789\955abcdefghij")))) mempty [1 .. 260 :: Int]
      samples = base ++ grown ++ grownL ++ [big, UText.drop 7 big, UText.take 1500 big, UText.drop 100 (grown !! 2), UText.take 400 (grownL !! 1), chunky, UText.drop 1000 chunky]
  arr <- newArray 12 first :: IO (MutableArray RealWorld Any)
  put arr 1 (wrap (UText.pack (replicate (th + 1) 'a') <> UText.pack (replicate (th + 1) 'b')))
  put arr 2 (wrap UText.empty)
  -- the constructors the helpers build results from (see rope_test)
  put arr 4 (any' none)
  put arr 5 (any' (Enum Ty.pairRef (PackedTag 0)))
  put arr 6 (any' (Enum Ty.unitRef TT.unitTag))
  put arr 7 (any' (Enum Ty.eitherRef (PackedTag 0)))
  put arr 8 (any' charTypeTag)
  put arr 9 (any' natTypeTag)
  put arr 10 (any' intTypeTag)
  put arr 11 (any' floatTypeTag)
  ok <- textInit arr [lForeignInfo ls, th, 0]
  if ok /= 1
    then pure (Left ("the text constructors are not laid out as the JIT's helpers expect (check " ++ show ok ++ ")"))
    else do
      let slot3 :: IO Closure
          slot3 = unsafeCoerce <$> readArray arr 3
          result :: IO (Maybe UText.Text)
          result = do
            c <- slot3
            good <- do
              put arr 0 (any' c)
              textCheck arr
            pure $ case c of
              Foreign (WrapText t) | good -> Just t
              _ -> Nothing
          -- a text inside a result: laid out right and the expected one
          textIn :: Closure -> UText.Text -> IO Bool
          textIn c expected = case c of
            Foreign (WrapText t) | t == expected -> put arr 0 (any' c) >> textCheck arr
            _ -> pure False
          one :: UText.Text -> IO [String]
          one t = do
            let n = UText.size t
            put arr 0 (wrap t)
            shape <- textCheck arr
            size <- textTest arr 3 0 0 0
            cuts <- forM ([-1 .. min n (if n > 600 then 12 else 70)] ++ [(n * j) `div` 13 | j <- [1 .. 12], n > 600] ++ [n - 3 .. n + 1]) $ \k -> do
              put arr 0 (wrap t)
              h1 <- textTest arr 1 k 0 0
              r1 <- result
              put arr 0 (wrap t)
              h2 <- textTest arr 2 k 0 0
              r2 <- result
              -- a negative count is a Nat too large to be a size
              let tk = if k < 0 then t else UText.take k t
                  dr = if k < 0 then UText.empty else UText.drop k t
              pure (h1 == 1 && r1 == Just tk && h2 == 1 && r2 == Just dr)
            -- uncons and unsnoc: Some (c, rest) / Some (rest, c), the rest laid out right
            uns <- forM [True, False] $ \front -> do
              put arr 0 (wrap t)
              h <- textTest arr (if front then 5 else 6) (fromIntegral someTag) (fromIntegral pairTag) 0
              c <- slot3
              let expected
                    | front = maybe none (\(ch, rest) -> some (BoxedVal (tup2 (CharVal ch) (BoxedVal (textC rest))))) (UText.uncons t)
                    | otherwise = maybe none (\(rest, ch) -> some (BoxedVal (tup2 (BoxedVal (textC rest)) (CharVal ch)))) (UText.unsnoc t)
              restOk <- case c of
                Data1 _ _ (BoxedVal (Data2 _ _ a (BoxedVal (Data2 _ _ b _))))
                  | BoxedVal r <- if front then b else a -> put arr 0 (any' r) >> textCheck arr
                _ -> pure (n == 0)
              pure (h == 1 && same c expected && restOk)
            -- the rest only on the smaller texts, to keep every startup cheap (the
            -- random test covers the big ones): toCharList then fromCharList of
            -- that, reverse, the case mappings (ASCII only), repeat
            let small = n <= 1500
            (hu, cu, charList) <-
              if small
                then do
                  put arr 0 (wrap t)
                  h <- textTest arr 14 0 0 0
                  c <- slot3
                  pure (h, c, Foreign (WrapSeq (Sq.fromList (map CharVal (chars t)))))
                else pure (1, none, none)
            (hp, rp) <-
              if small
                then do
                  put arr 0 (any' charList)
                  h <- textTest arr 13 0 0 0
                  r <- result
                  pure (h, r)
                else pure (1, Just t)
            (hr, rr) <-
              if small
                then do
                  put arr 0 (wrap t)
                  h <- textTest arr 18 0 0 0
                  r <- result
                  pure (h, r)
                else pure (1, Just (UText.reverse t))
            cases <- forM [(19, UText.toUppercase), (20, UText.toLowercase)] $ \(op, f) ->
              if not small
                then pure True
                else do
                  put arr 0 (wrap t)
                  h <- textTest arr op 0 0 0
                  r <- result
                  pure (if h == 1 then r == Just (f t) else any (>= '\128') (chars t))
            reps <- forM (if small then [0, 1, 2, 3, 7, 50] else [0, 1, 2]) $ \j -> do
              put arr 0 (wrap t)
              h <- textTest arr 17 j 0 0
              r <- result
              pure (h == 1 && r == Just (UText.replicate j t))
            -- toUtf8, then fromUtf8 of that
            put arr 0 (wrap t)
            h8 <- textTest arr 21 0 0 0
            c8 <- slot3
            utf <- case c8 of
              Foreign (WrapBytes bs) | h8 == 1, bs == UText.toUtf8 t -> do
                put arr 0 (any' c8)
                okB <- bytesCheck arr
                put arr 0 (any' c8)
                h9 <- textTest arr 22 (fromIntegral rightTag) 0 0
                c9 <- slot3
                okT <- case c9 of
                  Data1 rf tg (BoxedVal inner) | rf == Ty.eitherRef, tg == TT.rightTag -> textIn inner t
                  _ -> pure False
                pure (okB && h9 == 1 && okT)
              _ -> pure False
            pure . catMaybes $
              [ if shape then Nothing else Just "a text isn't laid out as expected",
                if size == n then Nothing else Just "Text.size differs from the interpreter's",
                if and cuts then Nothing else Just ("Text.take or Text.drop differs from the interpreter's on a text of " ++ show n ++ " characters"),
                if and uns then Nothing else Just ("Text.uncons or Text.unsnoc differs from the interpreter's on a text of " ++ show n),
                if hu == 1 && same cu charList then Nothing else Just ("Text.toCharList differs from the interpreter's on a text of " ++ show n),
                if hp == 1 && rp == Just t then Nothing else Just ("Text.fromCharList differs from the interpreter's on a text of " ++ show n),
                if hr == 1 && rr == Just (UText.reverse t) then Nothing else Just ("Text.reverse differs from the interpreter's on a text of " ++ show n),
                if and cases then Nothing else Just ("Text.toUppercase or toLowercase differs from the interpreter's on a text of " ++ show n),
                if and reps then Nothing else Just ("Text.repeat differs from the interpreter's on a text of " ++ show n),
                if utf then Nothing else Just ("Text.toUtf8 or Text.fromUtf8 differs from the interpreter's on a text of " ++ show n)
              ]
          two :: UText.Text -> UText.Text -> IO [String]
          two a b = do
            put arr 0 (wrap a)
            put arr 1 (wrap b)
            eq <- textTest arr 4 0 0 0
            h <- textTest arr 0 0 0 0
            r <- result
            put arr 0 (wrap a)
            put arr 1 (wrap b)
            hi <- textTest arr 15 (fromIntegral someTag) 0 0
            ci <- slot3
            put arr 0 (wrap a)
            put arr 1 (wrap b)
            cm <- textTest arr 16 0 0 0
            let expI = maybe none (some . NatVal) (UText.indexOf a b)
                expC = case compare a b of LT -> -1; EQ -> 0; GT -> 1
            pure . catMaybes $
              [ if eq == (if a == b then 1 else 0) then Nothing else Just "Text equality differs from the interpreter's",
                if h == 1 && r == Just (a <> b) then Nothing else Just ("Text.++ differs from the interpreter's on texts of " ++ show (UText.size a) ++ " and " ++ show (UText.size b) ++ " characters"),
                if hi == 1 && same ci expI then Nothing else Just "Text.indexOf differs from the interpreter's",
                if cm == expC then Nothing else Just "Text comparison differs from the interpreter's"
              ]
          -- the longer test: random operations on eight texts
          mix :: Int -> Int
          mix z0 =
            let z1 = (z0 `xor` (z0 `shiftR` 30)) * (-4658895280553007687)
                z2 = (z1 `xor` (z1 `shiftR` 27)) * (-7723592293110705685)
             in (z2 `xor` (z2 `shiftR` 31)) .&. maxBound
          letters = cycle "abcd\233fghij\8364klmnop\128512qrstuvwxyz0123456789\955"
          stress :: Int -> [UText.Text] -> IO (Either String ())
          stress step pool
            | step >= steps = pure (Right ())
            | otherwise = do
                let r k = mix (step * 16 + k)
                    i = r 0 `mod` 8
                    t = pool !! i
                    e = pool !! (r 1 `mod` 8)
                    n = UText.size t
                    full = n <= 3000 || r 2 `mod` 16 == 0
                    pos = case r 3 `mod` 4 of
                      0 -> r 4 `mod` 12
                      1 -> n - r 4 `mod` 12
                      _ -> r 4 `mod` (n + 2) - 1
                    op = r 5 `mod` 18
                    piece k = UText.pack (take (r k `mod` (if even (r (k + 1)) then 7 else 90)) (drop (r (k + 2) `mod` 40) letters))
                    -- what a helper built, if it is laid out right and is the expected text
                    -- (the characters are compared when `whole`; the size always)
                    verdict :: Bool -> Int -> UText.Text -> IO (Maybe UText.Text)
                    verdict whole h expected = do
                      got <- result
                      pure $ case got of
                        Just g | h == 1, UText.size g == UText.size expected, not whole || g == expected -> Just g
                        _ -> Nothing
                    cat whole a b = do
                      put arr 0 (wrap a)
                      put arr 1 (wrap b)
                      h <- textTest arr 0 0 0 0
                      verdict whole h (a <> b)
                    cut whole tk k x = do
                      put arr 0 (wrap x)
                      h <- textTest arr (if tk then 1 else 2) k 0 0
                      verdict whole h (if tk then (if k < 0 then x else UText.take k x) else (if k < 0 then UText.empty else UText.drop k x))
                    same a b = do
                      put arr 0 (wrap a)
                      put arr 1 (wrap b)
                      eq <- textTest arr 4 0 0 0
                      pure (eq == (if a == b then 1 else 0))
                    times :: Int -> (UText.Text -> IO (Maybe UText.Text)) -> UText.Text -> IO (Maybe UText.Text)
                    times 0 _ x = pure (Just x)
                    times k f x = f x >>= maybe (pure Nothing) (times (k - 1) f)
                res <-
                  if
                    | op < 2 -> cat full t (piece 6)
                    | op < 4 -> cat full (piece 6) t
                    | op == 4 -> cut full True pos t
                    | op == 5 -> cut full False pos t
                    | op == 6 -> cut full False pos t >>= maybe (pure Nothing) (cut full True (r 6 `mod` 200))
                    | op <= 8 -> if n + UText.size e > 60000 then cut full True (r 6 `mod` 50) t else cat full t e
                    | op == 9 -> do
                        -- the same text cut into chunks differently, and another text
                        a <- same t (UText.take pos t <> UText.drop pos t)
                        b <- same t e
                        c <- if n > 0 then same t (UText.take (n - 1) t <> piece 9) else pure True
                        pure (if a && b && c then Just t else Nothing)
                    | op == 10 -> times (r 6 `mod` 40) (\x -> cat (n <= 300) x (piece 9)) t
                    | op == 11 -> times (r 6 `mod` 40) (\x -> cat (n <= 300) (piece 9) x) t
                    | op == 12 -> times (r 6 `mod` 40) (cut (n <= 300) False 1) t
                    | op == 13 -> times (r 6 `mod` 40) (\x -> cut (n <= 300) True (UText.size x - 1) x) t
                    | op == 14 -> do
                        put arr 0 (wrap t)
                        h <- textTest arr 18 0 0 0
                        verdict full h (UText.reverse t)
                    | op == 15 -> do
                        -- toCharList, then fromCharList
                        put arr 0 (wrap t)
                        h <- textTest arr 14 0 0 0
                        c <- slot3
                        case c of
                          Foreign (WrapSeq _) | h == 1 -> do
                            put arr 0 (any' c)
                            h2 <- textTest arr 13 0 0 0
                            verdict full h2 t
                          _ -> pure Nothing
                    | op == 16 -> do
                        -- toUtf8, then fromUtf8
                        put arr 0 (wrap t)
                        h <- textTest arr 21 0 0 0
                        c <- slot3
                        case c of
                          Foreign (WrapBytes _) | h == 1 -> do
                            put arr 0 (any' c)
                            h2 <- textTest arr 22 (fromIntegral rightTag) 0 0
                            c2 <- slot3
                            case c2 of
                              Data1 _ _ (BoxedVal inner) | h2 == 1 -> put arr 3 (any' inner) >> verdict full 1 t
                              _ -> pure Nothing
                          _ -> pure Nothing
                    | otherwise -> do
                        -- uncons or unsnoc: the rest goes on
                        let front = even (r 6)
                        put arr 0 (wrap t)
                        h <- textTest arr (if front then 5 else 6) (fromIntegral someTag) (fromIntegral pairTag) 0
                        c <- slot3
                        case c of
                          Data1 _ _ (BoxedVal (Data2 _ _ a (BoxedVal (Data2 _ _ b _))))
                            | h == 1, BoxedVal rest <- if front then b else a ->
                                put arr 3 (any' rest) >> verdict full 1 (if front then UText.drop 1 t else UText.take (n - 1) t)
                          _ -> pure (if n == 0 && h == 1 then Just t else Nothing)
                case res of
                  Nothing -> pure (Left ("the text helpers' test failed at step " ++ show step ++ " (operation " ++ show op ++ " on a text of " ++ show n ++ " characters)"))
                  Just t' -> stress (step + 1) (take i pool ++ [t'] ++ drop (i + 1) pool)
      e1 <- concat <$> mapM one samples
      -- equal texts cut into chunks differently must compare equal
      let recut t = UText.take 3 t <> UText.drop 3 t
          -- (sized to cost little at every startup, like the list checks;
          -- the thorough test is the longer one)
          some3 = [t | (j, t) <- zip [0 :: Int ..] samples, j `mod` 3 == 0]
      e2 <- concat <$> sequence ([two a b | a <- some3, b <- some3] ++ [two t (recut t) | t <- samples])
      -- numbers to text and back, and Char.toText
      let ints = [0, 1, -1, 42, -42, 1000000007, minBound, maxBound, 2 ^ (62 :: Int), negate (2 ^ (62 :: Int))] :: [Int]
          nats = [0, 1, 255, 256, 2 ^ (32 :: Int), maxBound, maxBound - 1, 2 ^ (63 :: Int)] :: [Word64]
          floats =
            [0, -0.0, 1, -1, 0.1, 0.5, 1.5, 100, 1234567, 9999999, 10000000, 12345678.9, 0.01, 123456.789, 1 / 3, pi, 1e22, 1e21, 1e-10, 2 ** (-30), 5e-324, 1.7976931348623157e308, 2.2250738585072014e-308, 0.3, 2.5e-5, 123e300, 1 / 0, -1 / 0, 0 / 0, 4.35, 0.1 + 0.2, 1e7, 9007199254740993, 65536 * 65536 * 65536, 0.09999999999999999, 1e-7, 123456789.123, 2 ** 70, 2 ** (-70), 1.0e-45] ::
              [Double]
          parses = ["12", "-12", "+12", "0", "-0", "9223372036854775807", "9223372036854775808", "-9223372036854775808", "-9223372036854775809", "18446744073709551615", "18446744073709551616", "1.5", "-1.5", "1e3", "1.", ".5", "abc", "", "12a", " 12", "0x1F", "1_000", "007", "1E5", "1e", "+", "-", "Infinity", "NaN", "1.0e-2", "+1.5", "00", "1e400", "-1e-400", "123456789012345678901234567890", "3.14159", "1.5e+3", "1e-3", "0.000001", "1.0e7", "-", "--1", "1-"]
          -- the plain forms the helper must decide itself (the rest may go to the interpreter)
          mustParseInt = ["12", "-12", "+12", "0", "9223372036854775808", "007"]
          mustParseFloat = ["12", "-12", "0", "1.5", "-1.5", "1e3", "1.0e-2", "3.14159", "007"]
          numText h r expected = h == 1 && r == Just expected
      i2t <- forM ints $ \v -> do
        h <- textTest arr 7 v 0 0
        r <- result
        pure (numText h r (UText.pack (show v)))
      n2t <- forM nats $ \v -> do
        h <- textTest arr 8 (fromIntegral v) 0 0
        r <- result
        pure (numText h r (UText.pack (show v)))
      f2t <- forM floats $ \v -> do
        h <- textTest arr 9 (fromIntegral (castDoubleToWord64 v)) 0 0
        r <- result
        pure (numText h r (UText.pack (show v)))
      c2t <- forM ("a\233\8364\128512\0z" :: String) $ \ch -> do
        h <- textTest arr 23 (fromEnum ch) 0 0
        r <- result
        pure (numText h r (UText.pack [ch]))
      t2n <- forM parses $ \sx -> do
        let t = UText.pack sx
            run op = put arr 0 (wrap t) >> textTest arr op (fromIntegral someTag) 0 0 >>= \h -> (,) h <$> slot3
        (hi, ci) <- run 10
        (hn, cn) <- run 11
        (hf, cf) <- run 12
        let expI = maybe none (some . IntVal) (readI sx)
            expN = maybe none (some . NatVal) (readClamped sx :: Maybe Word64)
            expF = maybe none (some . DoubleVal) (readMaybe sx :: Maybe Double)
            handled = (sx `notElem` mustParseInt || (hi == 1 && hn == 1)) && (sx `notElem` mustParseFloat || hf == 1)
        pure (handled && (hi /= 1 || same ci expI) && (hn /= 1 || same cn expN) && (hf /= 1 || same cf expF))
      let e3 =
            [ "Int.toText differs from the interpreter's" | not (and i2t) ]
              ++ [ "Nat.toText differs from the interpreter's" | not (and n2t) ]
              ++ [ "Float.toText differs from the interpreter's on " ++ show [v | (v, ok) <- zip floats f2t, not ok] | not (and f2t) ]
              ++ [ "Char.toText differs from the interpreter's" | not (and c2t) ]
              ++ [ "Text.toInt, toNat or toFloat differs from the interpreter's on " ++ show [sx | (sx, ok) <- zip parses t2n, not ok] | not (and t2n) ]
      case e1 ++ e2 ++ e3 of
        e : _ -> pure (Left e)
        [] -> if steps > 0 then stress 0 (replicate 8 UText.empty) else pure (Right ())

-- | The bytes helpers: the same arrangement as 'probeTexts' for @Bytes@,
-- which is the same rope with chunks of bytes, plus @Bytes.at@ and
-- @Bytes.flatten@. Stress mode @bytes=N@.
probeBytes :: Layouts -> Int -> IO (Either String ())
probeBytes ls steps = do
  let wrap :: By.Bytes -> Any
      wrap b = any' $! Foreign (WrapBytes b)
      put :: MutableArray RealWorld Any -> Int -> Any -> IO ()
      put a i v = evaluate v >>= writeArray a i
      bytes :: [Int] -> By.Bytes
      bytes = By.fromWord8s . map fromIntegral
      same a b = BoxedVal a == BoxedVal b
      PackedTag someTag = TT.someTag
      PackedTag pairTag = TT.pairTag
      PackedTag rightTag = TT.rightTag
      !none = Enum Ty.optionalRef TT.noneTag
      unitVal = BoxedVal (Enum Ty.unitRef TT.unitTag)
      tup2 x y = Data2 Ty.pairRef TT.pairTag x (BoxedVal (Data2 Ty.pairRef TT.pairTag y unitVal))
      some = Data1 Ty.optionalRef TT.someTag
      bytesC = Foreign . WrapBytes
      -- the number functions: width, big-endian, and the Haskell operations
      numKinds =
        [ (2, True, By.decodeNat16be, By.encodeNat16be, \i bs -> fromIntegral <$> By.index16be i bs),
          (2, False, By.decodeNat16le, By.encodeNat16le, \i bs -> fromIntegral <$> By.index16le i bs),
          (4, True, By.decodeNat32be, By.encodeNat32be, \i bs -> fromIntegral <$> By.index32be i bs),
          (4, False, By.decodeNat32le, By.encodeNat32le, \i bs -> fromIntegral <$> By.index32le i bs),
          (8, True, By.decodeNat64be, By.encodeNat64be, By.index64be),
          (8, False, By.decodeNat64le, By.encodeNat64le, By.index64le)
        ] ::
          [(Int, Bool, By.Bytes -> Maybe (Word64, By.Bytes), Word64 -> By.Bytes, Int -> By.Bytes -> Maybe Word64)]
      baseKinds =
        [ (16, By.toBase16, By.fromBase16),
          (32, By.toBase32, By.fromBase32),
          (64, By.toBase64, By.fromBase64),
          (65, By.toBase64UrlUnpadded, By.fromBase64UrlUnpadded)
        ] ::
          [(Int, By.Bytes -> By.Bytes, By.Bytes -> Either Text.Text By.Bytes)]
      -- "abc", cut from a longer chunk so that its offset and its size differ
      first = wrap (By.drop 1 (bytes [120, 97, 98, 99]))
      th = Rope.threshold
      pieces = [[], [1], [97, 98, 99], [0, 255, 128, 7, 0], [0 .. 255], replicate (th - 1) 120, replicate th 121, replicate (th + 1) 122, take 600 (cycle [0 .. 255])]
      base = map bytes pieces
      digits i = map fromEnum (show (i :: Int))
      grown = [foldl (\acc i -> acc <> bytes (digits i)) mempty [1 .. n] | n <- [5, 40, 300]]
      grownL = [foldl (\acc i -> bytes (digits i) <> acc) mempty [1 .. n] | n <- [40, 300]]
      big = mconcat (replicate 2000 (bytes [0xC3, 0xA9]))
      chunky = foldl (\acc i -> acc <> bytes (take (th `div` 2 + 1) (drop (i `mod` 7) (cycle [0 .. 255])))) mempty [1 .. 260 :: Int]
      samples = base ++ grown ++ grownL ++ [big, By.drop 7 big, By.take 1500 big, By.drop 100 (grown !! 2), By.take 400 (grownL !! 1), chunky, By.drop 1000 chunky]
  -- (every sample goes through `put`: see probeLists on `evaluate`)
  arr <- newArray 12 (wrap By.empty) :: IO (MutableArray RealWorld Any)
  put arr 0 first
  put arr 1 (wrap (bytes (replicate (th + 1) 97) <> bytes (replicate (th + 1) 98)))
  put arr 2 (wrap By.empty)
  put arr 4 (any' none)
  put arr 5 (any' (Enum Ty.pairRef (PackedTag 0)))
  put arr 6 (any' (Enum Ty.unitRef TT.unitTag))
  put arr 7 (any' (Enum Ty.eitherRef (PackedTag 0)))
  put arr 8 (any' charTypeTag)
  put arr 9 (any' natTypeTag)
  put arr 10 (any' intTypeTag)
  put arr 11 (any' floatTypeTag)
  ok <- bytesInit arr [lForeignInfo ls, th, 1]
  if ok /= 1
    then pure (Left ("the bytes constructors are not laid out as the JIT's helpers expect (check " ++ show ok ++ ")"))
    else do
      let slot3 :: IO Closure
          slot3 = unsafeCoerce <$> readArray arr 3
          result :: IO (Maybe By.Bytes)
          result = do
            c <- slot3
            good <- do
              put arr 0 (any' c)
              bytesCheck arr
            pure $ case c of
              Foreign (WrapBytes b) | good -> Just b
              _ -> Nothing
          bytesIn :: Closure -> By.Bytes -> IO Bool
          bytesIn c expected = case c of
            Foreign (WrapBytes b) | b == expected -> put arr 0 (any' c) >> bytesCheck arr
            _ -> pure False
          -- Bytes.at through the helper, against the interpreter's answer
          at :: By.Bytes -> Int -> IO Bool
          at b i = do
            put arr 0 (wrap b)
            put arr 1 (any' none)
            put arr 2 (any' natTypeTag)
            h <- bytesTest arr 5 i (fromIntegral someTag) 0
            c <- unsafeCoerce <$> readArray arr 3 :: IO Closure
            pure (h == 1 && same c (maybe none (Data1 Ty.optionalRef TT.someTag . NatVal . fromIntegral) (By.at i b)))
          -- Bytes.flatten: the same bytes, in one chunk
          flat :: By.Bytes -> IO (Maybe By.Bytes)
          flat b = do
            put arr 0 (wrap b)
            h <- bytesTest arr 6 0 0 0
            r <- result
            pure $ case r of
              Just f | h == 1, f == b, length (By.chunks f) <= 1 -> Just f
              _ -> Nothing
          one :: By.Bytes -> IO [String]
          one b = do
            let n = By.size b
            put arr 0 (wrap b)
            shape <- bytesCheck arr
            size <- bytesTest arr 3 0 0 0
            cuts <- forM ([-1 .. min n (if n > 600 then 12 else 70)] ++ [(n * j) `div` 13 | j <- [1 .. 12], n > 600] ++ [n - 3 .. n + 1]) $ \k -> do
              put arr 0 (wrap b)
              h1 <- bytesTest arr 1 k 0 0
              r1 <- result
              put arr 0 (wrap b)
              h2 <- bytesTest arr 2 k 0 0
              r2 <- result
              -- a negative count is a Nat too large to be a size
              let tk = if k < 0 then b else By.take k b
                  dr = if k < 0 then By.empty else By.drop k b
              pure (h1 == 1 && r1 == Just tk && h2 == 1 && r2 == Just dr)
            ats <- mapM (at b) ([-1, 0, 1, n - 1, n] ++ [(n * j) `div` 7 | j <- [1 .. 6]])
            fl <- flat b
            -- toList, then fromList of that
            put arr 0 (wrap b)
            hu <- bytesTest arr 8 0 0 0
            cu <- slot3
            let natList = Foreign (WrapSeq (Sq.fromList (map (NatVal . fromIntegral) (By.toWord8s b))))
            put arr 0 (any' natList)
            hp <- bytesTest arr 7 0 0 0
            rp <- result
            -- the number functions
            nums <- forM numKinds $ \(w, be, dec, enc, rd) -> do
              let be' = if be then 1 else 0
              put arr 0 (wrap b)
              hd <- bytesTest arr 11 (w * 2 + be') (fromIntegral someTag) (fromIntegral pairTag)
              cd <- slot3
              let expD = maybe none (\(v, rest) -> some (BoxedVal (tup2 (NatVal v) (BoxedVal (bytesC rest))))) (dec b)
              restOk <- case cd of
                Data1 _ _ (BoxedVal (Data2 _ _ _ (BoxedVal (Data2 _ _ (BoxedVal rest) _)))) -> put arr 0 (any' rest) >> bytesCheck arr
                _ -> pure (n < w)
              encs <- forM [0, 1, 255, 256, 65535, 65536, 2 ^ (32 :: Int) - 1, 2 ^ (32 :: Int), maxBound] $ \v -> do
                h <- bytesTest arr 12 (fromIntegral v) (w * 2 + be') 0
                r <- result
                pure (h == 1 && r == Just (enc v))
              rds <- forM [-1, 0, 1, n - w, n - w + 1, n, n `div` 2] $ \i -> do
                put arr 0 (wrap b)
                v <- bytesTest arr 13 i (w * 2 + be') 0
                pure (v == maybe (-1) fromIntegral (if i < 0 then Nothing else rd i b))
              pure (hd == 1 && same cd expD && restOk && and encs && and rds)
            -- the encodings: to, then from (the smaller bytes only; the random test covers the rest)
            bases <- forM (if n <= 1500 then baseKinds else []) $ \(base, enc, dec) -> do
              put arr 0 (wrap b)
              h <- bytesTest arr 14 base 0 0
              r <- result
              let e = enc b
              put arr 0 (wrap e)
              h2 <- bytesTest arr 15 base (fromIntegral rightTag) 0
              c2 <- slot3
              back <- case (c2, dec e) of
                (Data1 rf tg (BoxedVal inner), Right d) | rf == Ty.eitherRef, tg == TT.rightTag -> bytesIn inner d
                _ -> pure False
              pure (h == 1 && r == Just e && h2 == 1 && back)
            pure . catMaybes $
              [ if shape then Nothing else Just "a bytes isn't laid out as expected",
                if size == n then Nothing else Just "Bytes.size differs from the interpreter's",
                if and cuts then Nothing else Just ("Bytes.take or Bytes.drop differs from the interpreter's on a bytes of " ++ show n),
                if and ats then Nothing else Just ("Bytes.at differs from the interpreter's on a bytes of " ++ show n),
                if isJust fl then Nothing else Just ("Bytes.flatten differs from the interpreter's on a bytes of " ++ show n),
                if hu == 1 && same cu natList then Nothing else Just ("Bytes.toList differs from the interpreter's on a bytes of " ++ show n),
                if hp == 1 && rp == Just b then Nothing else Just ("Bytes.fromList differs from the interpreter's on a bytes of " ++ show n),
                if and nums then Nothing else Just ("a Bytes number function differs from the interpreter's on a bytes of " ++ show n),
                if and bases then Nothing else Just ("a Bytes encoding differs from the interpreter's on a bytes of " ++ show n)
              ]
          two :: By.Bytes -> By.Bytes -> IO [String]
          two a b = do
            put arr 0 (wrap a)
            put arr 1 (wrap b)
            eq <- bytesTest arr 4 0 0 0
            h <- bytesTest arr 0 0 0 0
            r <- result
            put arr 0 (wrap a)
            put arr 1 (wrap b)
            hi <- bytesTest arr 9 (fromIntegral someTag) 0 0
            ci <- slot3
            put arr 0 (wrap a)
            put arr 1 (wrap b)
            cm <- bytesTest arr 10 0 0 0
            let expI = maybe none (some . NatVal) (By.indexOf a b)
                expC = case compare a b of LT -> -1; EQ -> 0; GT -> 1
            pure . catMaybes $
              [ if eq == (if a == b then 1 else 0) then Nothing else Just "Bytes equality differs from the interpreter's",
                if h == 1 && r == Just (a <> b) then Nothing else Just ("Bytes.++ differs from the interpreter's on bytes of " ++ show (By.size a) ++ " and " ++ show (By.size b)),
                if By.size a == 0 || (hi == 1 && same ci expI) then Nothing else Just "Bytes.indexOf differs from the interpreter's",
                if cm == expC then Nothing else Just "Bytes comparison differs from the interpreter's"
              ]
          -- the longer test: random operations on eight bytes
          mix :: Int -> Int
          mix z0 =
            let z1 = (z0 `xor` (z0 `shiftR` 30)) * (-4658895280553007687)
                z2 = (z1 `xor` (z1 `shiftR` 27)) * (-7723592293110705685)
             in (z2 `xor` (z2 `shiftR` 31)) .&. maxBound
          values = cycle ([0 .. 255] ++ [255, 0, 128, 1])
          stress :: Int -> [By.Bytes] -> IO (Either String ())
          stress step pool
            | step >= steps = pure (Right ())
            | otherwise = do
                let r k = mix (step * 16 + k)
                    i = r 0 `mod` 8
                    t = pool !! i
                    e = pool !! (r 1 `mod` 8)
                    n = By.size t
                    full = n <= 3000 || r 2 `mod` 16 == 0
                    pos = case r 3 `mod` 4 of
                      0 -> r 4 `mod` 12
                      1 -> n - r 4 `mod` 12
                      _ -> r 4 `mod` (n + 2) - 1
                    op = r 5 `mod` 20
                    piece k = bytes (take (r k `mod` (if even (r (k + 1)) then 7 else 90)) (drop (r (k + 2) `mod` 40) values))
                    -- what a helper built, if it is laid out right and is the expected bytes
                    -- (the bytes are compared when `whole`; the size always)
                    verdict :: Bool -> Int -> By.Bytes -> IO (Maybe By.Bytes)
                    verdict whole h expected = do
                      got <- result
                      pure $ case got of
                        Just g | h == 1, By.size g == By.size expected, not whole || g == expected -> Just g
                        _ -> Nothing
                    cat whole a b = do
                      put arr 0 (wrap a)
                      put arr 1 (wrap b)
                      h <- bytesTest arr 0 0 0 0
                      verdict whole h (a <> b)
                    cut whole tk k x = do
                      put arr 0 (wrap x)
                      h <- bytesTest arr (if tk then 1 else 2) k 0 0
                      verdict whole h (if tk then (if k < 0 then x else By.take k x) else (if k < 0 then By.empty else By.drop k x))
                    sameAs a b = do
                      put arr 0 (wrap a)
                      put arr 1 (wrap b)
                      eq <- bytesTest arr 4 0 0 0
                      pure (eq == (if a == b then 1 else 0))
                    times :: Int -> (By.Bytes -> IO (Maybe By.Bytes)) -> By.Bytes -> IO (Maybe By.Bytes)
                    times 0 _ x = pure (Just x)
                    times k f x = f x >>= maybe (pure Nothing) (times (k - 1) f)
                res <-
                  if
                    | op < 2 -> cat full t (piece 6)
                    | op < 4 -> cat full (piece 6) t
                    | op == 4 -> cut full True pos t
                    | op == 5 -> cut full False pos t
                    | op == 6 -> cut full False pos t >>= maybe (pure Nothing) (cut full True (r 6 `mod` 200))
                    | op <= 8 -> if n + By.size e > 60000 then cut full True (r 6 `mod` 50) t else cat full t e
                    | op == 9 -> do
                        -- the same bytes cut into chunks differently, and another bytes
                        a <- sameAs t (By.take pos t <> By.drop pos t)
                        b <- sameAs t e
                        c <- if n > 0 then sameAs t (By.take (n - 1) t <> piece 9) else pure True
                        pure (if a && b && c then Just t else Nothing)
                    | op == 10 -> times (r 6 `mod` 40) (\x -> cat (n <= 300) x (piece 9)) t
                    | op == 11 -> times (r 6 `mod` 40) (\x -> cat (n <= 300) (piece 9) x) t
                    | op == 12 -> times (r 6 `mod` 40) (cut (n <= 300) False 1) t
                    | op == 13 -> times (r 6 `mod` 40) (\x -> cut (n <= 300) True (By.size x - 1) x) t
                    | op == 14 -> do
                        oks <- mapM (at t) [pos, 0, n - 1, r 6 `mod` (n + 1)]
                        pure (if and oks then Just t else Nothing)
                    | op == 15 -> flat t
                    | op == 16 -> do
                        -- toList, then fromList
                        put arr 0 (wrap t)
                        h <- bytesTest arr 8 0 0 0
                        c <- slot3
                        case c of
                          Foreign (WrapSeq _) | h == 1 -> do
                            put arr 0 (any' c)
                            h2 <- bytesTest arr 7 0 0 0
                            verdict full h2 t
                          _ -> pure Nothing
                    | op == 17 || op == 18 -> do
                        -- an encoding and back
                        let base = if op == 17 then 16 else [32, 64, 65] !! (r 6 `mod` 3)
                        put arr 0 (wrap t)
                        h <- bytesTest arr 14 base 0 0
                        c <- slot3
                        case c of
                          Foreign (WrapBytes _) | h == 1 -> do
                            put arr 0 (any' c)
                            h2 <- bytesTest arr 15 base (fromIntegral rightTag) 0
                            c2 <- slot3
                            case c2 of
                              Data1 _ _ (BoxedVal inner) | h2 == 1 -> put arr 3 (any' inner) >> verdict full 1 t
                              _ -> pure Nothing
                          _ -> pure Nothing
                    | otherwise -> do
                        -- decode a number off the front: the rest goes on
                        let (w, be, dec, _, _) = numKinds !! (r 6 `mod` 6)
                        put arr 0 (wrap t)
                        h <- bytesTest arr 11 (w * 2 + (if be then 1 else 0)) (fromIntegral someTag) (fromIntegral pairTag)
                        c <- slot3
                        case (c, dec t) of
                          (Data1 _ _ (BoxedVal (Data2 _ _ _ (BoxedVal (Data2 _ _ (BoxedVal rest) _)))), Just (_, expected)) | h == 1 -> put arr 3 (any' rest) >> verdict full 1 expected
                          (_, Nothing) | h == 1 -> pure (Just t)
                          _ -> pure Nothing
                case res of
                  Nothing -> pure (Left ("the bytes helpers' test failed at step " ++ show step ++ " (operation " ++ show op ++ " on a bytes of " ++ show n ++ ")"))
                  Just t' -> stress (step + 1) (take i pool ++ [t'] ++ drop (i + 1) pool)
      e1 <- concat <$> mapM one samples
      -- equal bytes cut into chunks differently must compare equal
      let recut b = By.take 3 b <> By.drop 3 b
          some3 = [b | (j, b) <- zip [0 :: Int ..] samples, j `mod` 3 == 0]
      e2 <- concat <$> sequence ([two a b | a <- some3, b <- some3] ++ [two b (recut b) | b <- samples])
      -- decoding inputs that aren't canonical: whatever the helper decides must be the interpreter's answer
      odd <- forM [(base, dec, inp) | (base, _, dec) <- baseKinds, inp <- ["", "00ff", "00FF", "0", "zz", "AA==", "AA", "AAA=", "QUJD", "QUJDRA==", "QUJDRA", "MFRGG===", "MFRGG", "-_-_", "QUI=", "QUJ=", "ME======", "MF======", "====", "ab", "AB"]] $ \(base, dec, inp) -> do
        let e = bytes (map fromEnum inp)
        put arr 0 (wrap e)
        h <- bytesTest arr 15 base (fromIntegral rightTag) 0
        c <- slot3
        case (h, dec e) of
          (1, Right d) -> case c of
            Data1 rf tg (BoxedVal inner) | rf == Ty.eitherRef, tg == TT.rightTag -> bytesIn inner d
            _ -> pure False
          (1, Left _) -> pure False
          _ -> pure True
      let e3 = ["Bytes.fromBase decodes something the interpreter doesn't" | not (and odd)]
      case e1 ++ e2 ++ e3 of
        e : _ -> pure (Left e)
        [] -> if steps > 0 then stress 0 (replicate 8 By.empty) else pure (Right ())

-- | Partial applications: the helper for the @Name@ instruction against
-- what the interpreter builds, for closures with and without arguments
-- already captured.
probeNames :: Layouts -> IO (Either String ())
probeNames ls = do
  closureInit [lInfo (lPAp ls), lByteArrayBoxInfo ls, lArrayBoxInfo ls]
  let put :: MutableArray RealWorld Any -> Int -> Any -> IO ()
      put a i v = evaluate v >>= writeArray a i
      !ref = Ty.booleanRef
      !cix = CIx ref 21 22
      !comb = LamI 5 7 Exit noNativeCell
      !nat = natTypeTag
      pap us bs = let !u = byteArrayFromList (us :: [Int]); !b = arrayFromList bs in PAp cix comb (u, b)
      !first = any' $! pap [] []
  arr <- newArray 4 first :: IO (MutableArray RealWorld Any)
  put arr 1 (any' nat)
  oks <- forM [(old, n) | old <- [0, 1, 3], n <- [1 .. 4 :: Int]] $ \(old, n) -> do
    let olds = [50 .. 49 + old]
    put arr 0 (any' $! pap olds (map (const nat) olds))
    h <- nameTest arr n
    r <- unsafeCoerce <$> readArray arr 3 :: IO Closure
    let expected = pap (reverse [100 .. 99 + n] ++ olds) (replicate (n + old) nat)
    pure (h && BoxedVal r == BoxedVal expected)
  pure (if and oks then Right () else Left "partial applications built by the JIT's helper differ from the interpreter's")
