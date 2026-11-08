-- | x86-64: an instruction-length decoder that also says where an
-- instruction's address-carrying fields are, the recursive-descent
-- decoding of a Haskell definition (with GHC's jump tables), the linear
-- sweep of fixed code, and the abort stub.
--
-- GHC's native code generator refers to addresses through rel32 branches
-- and calls, RIP-relative operands, and in a non-PIE executable through
-- sign-extended 32-bit immediates and displacements (the small code
-- model). Switches with dense cases compile to a jump table of
-- displacements from the table (64-bit, in the text section, on Darwin;
-- 32-bit, in read-only data, on Linux with PIC) or of absolute addresses
-- (in read-only data, without PIC).
module Minify.X86
  ( X86Insn (..),
    XKind (..),
    decodeInsn,
    decodeFrom,
    scanTables,
    tableEntries,
    sweepFixed,
    stubCode,
    stubSize,
    branchRange,
    needsIslands,
  )
where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import Data.Int
import Data.Word
import Minify.Bytes
import Minify.Hooks
import Minify.Ref

data XKind
  = KJmp
  | KJcc
  | KCall
  | KRet
  | -- | jmp through a register or memory
    KJmpInd
  | KCallInd
  | -- | ud2, hlt, int3: no fall-through
    KTrap
  | KOther
  | KInvalid
  deriving (Eq, Show)

data X86Insn = X86Insn
  { xLen :: !Int,
    xKind :: !XKind,
    -- | a branch displacement: (offset in the instruction, 1 or 4 bytes), from the instruction's end
    xRel :: Maybe (Int, Int),
    -- | offset of a RIP-relative disp32, from the instruction's end
    xRip :: Maybe Int,
    -- | offset of a disp32 that is an absolute address (a SIB with no base)
    xAbs :: Maybe Int,
    -- | an immediate operand: (offset, size)
    xImm :: Maybe (Int, Int),
    -- | the opcode byte, and its map (0: one byte, 1: 0F, 2: 0F38, 3: 0F3A)
    xOp :: !Word8,
    xMap :: !Int,
    -- | (mod, reg, rm) with the REX bits applied
    xModRm :: Maybe (Int, Int, Int),
    -- | (scale, index, base) with the REX bits applied
    xSib :: Maybe (Int, Int, Int),
    xRexW :: !Bool
  }
  deriving (Show)

invalid :: X86Insn
invalid = X86Insn 1 KInvalid Nothing Nothing Nothing Nothing 0 0 Nothing Nothing False

data Imm = ImmNone | Imm8 | Imm16 | ImmZ | ImmV | ImmEnter | Rel8 | Rel32 | Moffs | ImmGrp3 | ImmC7

-- | Decodes the instruction at the start of the bytes (at least 15 should
-- be given when available).
decodeInsn :: B.ByteString -> X86Insn
decodeInsn bs = prefixes 0 False False
  where
    n = B.length bs
    at i = if i < n then u8 bs i else 0
    prefixes i op16 ad32
      | i >= min n 14 = invalid
      | b `elem` [0x66] = prefixes (i + 1) True ad32
      | b == 0x67 = prefixes (i + 1) op16 True
      | b `elem` [0xf0, 0xf2, 0xf3, 0x2e, 0x36, 0x3e, 0x26, 0x64, 0x65] = prefixes (i + 1) op16 ad32
      | b >= 0x40 && b <= 0x4f = opcode (i + 1) op16 ad32 (testBit b 3) (testBit b 2) (testBit b 1) (testBit b 0)
      | otherwise = opcode i op16 ad32 False False False False
      where
        b = at i
    opcode i op16 ad32 rexW rexR rexX rexB
      | i >= n = invalid
      | b == 0xc5 = vex (i + 2) 1 (testBit (at (i + 1)) 7) rexX rexB False
      | b == 0xc4 =
          let b1 = at (i + 1)
              b2 = at (i + 2)
           in vex (i + 3) (fromIntegral (b1 .&. 0x1f)) (not (testBit b1 7)) (not (testBit b1 6)) (not (testBit b1 5)) (testBit b2 7)
      | b == 0x62 =
          let b1 = at (i + 1)
              b2 = at (i + 2)
           in vex (i + 4) (fromIntegral (b1 .&. 0x07)) (not (testBit b1 7)) (not (testBit b1 6)) (not (testBit b1 5)) (testBit b2 7)
      | b == 0x0f =
          let b2 = at (i + 1)
           in if
                | b2 == 0x38 -> finish (i + 3) 2 (at (i + 2)) True ImmNone KOther
                | b2 == 0x3a -> finish (i + 3) 3 (at (i + 2)) True Imm8 KOther
                | otherwise -> uncurry3 (finish (i + 2) 1 b2) (spec1 b2)
      | otherwise = uncurry3 (finish (i + 1) 0 b) (spec0 b)
      where
        b = at i
        uncurry3 f (x, y, z) = f x y z
        -- VEX and EVEX: an opcode in map 1, 2 or 3 and always a ModRM, but for vzeroupper
        vex j m r x bb w =
          let op = at j
              hasModRm = not (m == 1 && op == 0x77)
              imm
                | m == 3 = Imm8
                | m == 1 && op `elem` [0x70, 0x71, 0x72, 0x73, 0xc2, 0xc4, 0xc5, 0xc6] = Imm8
                | otherwise = ImmNone
           in modrmAndImm (j + 1) m op hasModRm imm KOther op16 ad32 w r x bb
        finish j m op hasModRm imm kind = modrmAndImm j m op hasModRm imm kind op16 ad32 rexW rexR rexX rexB
    modrmAndImm j m op hasModRm imm kind0 op16 ad32 rexW rexR rexX rexB
      | kind0 == KInvalid = invalid
      | otherwise =
          let (j1, modrm, sib, rip, absd) =
                if not hasModRm
                  then (j, Nothing, Nothing, Nothing, Nothing)
                  else
                    let mb = at j
                        md = fromIntegral (mb `shiftR` 6) :: Int
                        rg = fromIntegral ((mb `shiftR` 3) .&. 7) + (if rexR then 8 else 0)
                        rm0 = fromIntegral (mb .&. 7) :: Int
                        rmx = rm0 + (if rexB then 8 else 0)
                     in if md == 3
                          then (j + 1, Just (md, rg, rmx), Nothing, Nothing, Nothing)
                          else
                            if rm0 == 4
                              then
                                let sb = at (j + 1)
                                    sc = fromIntegral (sb `shiftR` 6) :: Int
                                    ix = fromIntegral ((sb `shiftR` 3) .&. 7) + (if rexX then 8 else 0)
                                    bs0 = fromIntegral (sb .&. 7) :: Int
                                    bsx = bs0 + (if rexB then 8 else 0)
                                    sibv = Just (sc, ix, bsx)
                                 in if
                                      | md == 0 && bs0 == 5 -> (j + 6, Just (md, rg, rmx), sibv, Nothing, Just (j + 2))
                                      | md == 1 -> (j + 3, Just (md, rg, rmx), sibv, Nothing, Nothing)
                                      | md == 2 -> (j + 6, Just (md, rg, rmx), sibv, Nothing, Nothing)
                                      | otherwise -> (j + 2, Just (md, rg, rmx), sibv, Nothing, Nothing)
                              else
                                if
                                  | md == 0 && rm0 == 5 -> (j + 5, Just (md, rg, rmx), Nothing, Just (j + 1), Nothing)
                                  | md == 1 -> (j + 2, Just (md, rg, rmx), Nothing, Nothing, Nothing)
                                  | md == 2 -> (j + 5, Just (md, rg, rmx), Nothing, Nothing, Nothing)
                                  | otherwise -> (j + 1, Just (md, rg, rmx), Nothing, Nothing, Nothing)
              regField = maybe 0 (\(_, r, _) -> r .&. 7) modrm
              zsize = if op16 then 2 else 4
              (immv, relv, j2) = case imm of
                ImmNone -> (Nothing, Nothing, j1)
                Imm8 -> (Just (j1, 1), Nothing, j1 + 1)
                Imm16 -> (Just (j1, 2), Nothing, j1 + 2)
                ImmZ -> (Just (j1, zsize), Nothing, j1 + zsize)
                ImmV -> let s = if rexW then 8 else zsize in (Just (j1, s), Nothing, j1 + s)
                ImmEnter -> (Just (j1, 2), Nothing, j1 + 3)
                Rel8 -> (Nothing, Just (j1, 1), j1 + 1)
                Rel32 -> (Nothing, Just (j1, 4), j1 + 4)
                Moffs -> let s = if ad32 then 4 else 8 in (Just (j1, s), Nothing, j1 + s)
                ImmGrp3
                  | regField <= 1 -> if op == 0xf6 then (Just (j1, 1), Nothing, j1 + 1) else (Just (j1, zsize), Nothing, j1 + zsize)
                  | otherwise -> (Nothing, Nothing, j1)
                ImmC7
                  | regField == 7 -> (Nothing, Just (j1, zsize), j1 + zsize) -- xbegin
                  | otherwise -> (Just (j1, zsize), Nothing, j1 + zsize)
              kind = case kind0 of
                KJmpInd | m == 0 && op == 0xff -> case regField of
                  2 -> KCallInd
                  3 -> KCallInd
                  4 -> KJmpInd
                  5 -> KJmpInd
                  _ -> KOther
                k -> k
           in if j2 > n || j2 > 15
                then invalid
                else
                  X86Insn
                    { xLen = j2,
                      xKind = kind,
                      xRel = relv,
                      xRip = rip,
                      xAbs = absd,
                      xImm = immv,
                      xOp = op,
                      xMap = m,
                      xModRm = modrm,
                      xSib = sib,
                      xRexW = rexW
                    }

-- | The one-byte opcode map in 64-bit mode: has a ModRM byte, immediate, kind.
spec0 :: Word8 -> (Bool, Imm, XKind)
spec0 b
  | b `elem` [0x06, 0x07, 0x0e, 0x16, 0x17, 0x1e, 0x1f, 0x27, 0x2f, 0x37, 0x3f, 0x60, 0x61, 0x82, 0x9a, 0xc4, 0xc5, 0xce, 0xd4, 0xd5, 0xd6, 0xea] = (False, ImmNone, KInvalid)
  | b < 0x40 = case b .&. 7 of
      4 -> (False, Imm8, KOther)
      5 -> (False, ImmZ, KOther)
      _ -> (True, ImmNone, KOther)
  | b >= 0x50 && b <= 0x5f = (False, ImmNone, KOther)
  | b == 0x63 = (True, ImmNone, KOther)
  | b == 0x68 = (False, ImmZ, KOther)
  | b == 0x69 = (True, ImmZ, KOther)
  | b == 0x6a = (False, Imm8, KOther)
  | b == 0x6b = (True, Imm8, KOther)
  | b >= 0x6c && b <= 0x6f = (False, ImmNone, KOther)
  | b >= 0x70 && b <= 0x7f = (False, Rel8, KJcc)
  | b == 0x80 = (True, Imm8, KOther)
  | b == 0x81 = (True, ImmZ, KOther)
  | b == 0x83 = (True, Imm8, KOther)
  | b >= 0x84 && b <= 0x8f = (True, ImmNone, KOther)
  | b >= 0x90 && b <= 0x9f = (False, ImmNone, KOther)
  | b >= 0xa0 && b <= 0xa3 = (False, Moffs, KOther)
  | b >= 0xa4 && b <= 0xa7 = (False, ImmNone, KOther)
  | b == 0xa8 = (False, Imm8, KOther)
  | b == 0xa9 = (False, ImmZ, KOther)
  | b >= 0xaa && b <= 0xaf = (False, ImmNone, KOther)
  | b >= 0xb0 && b <= 0xb7 = (False, Imm8, KOther)
  | b >= 0xb8 && b <= 0xbf = (False, ImmV, KOther)
  | b == 0xc0 || b == 0xc1 = (True, Imm8, KOther)
  | b == 0xc2 = (False, Imm16, KRet)
  | b == 0xc3 = (False, ImmNone, KRet)
  | b == 0xc6 = (True, Imm8, KOther)
  | b == 0xc7 = (True, ImmC7, KOther)
  | b == 0xc8 = (False, ImmEnter, KOther)
  | b == 0xc9 = (False, ImmNone, KOther)
  | b == 0xca = (False, Imm16, KRet)
  | b == 0xcb = (False, ImmNone, KRet)
  | b == 0xcc = (False, ImmNone, KTrap)
  | b == 0xcd = (False, Imm8, KOther)
  | b == 0xcf = (False, ImmNone, KRet)
  | b >= 0xd0 && b <= 0xd3 = (True, ImmNone, KOther)
  | b == 0xd7 = (False, ImmNone, KOther)
  | b >= 0xd8 && b <= 0xdf = (True, ImmNone, KOther)
  | b >= 0xe0 && b <= 0xe3 = (False, Rel8, KJcc)
  | b == 0xe4 || b == 0xe5 || b == 0xe6 || b == 0xe7 = (False, Imm8, KOther)
  | b == 0xe8 = (False, Rel32, KCall)
  | b == 0xe9 = (False, Rel32, KJmp)
  | b == 0xeb = (False, Rel8, KJmp)
  | b >= 0xec && b <= 0xef = (False, ImmNone, KOther)
  | b == 0xf1 = (False, ImmNone, KTrap)
  | b == 0xf4 = (False, ImmNone, KTrap)
  | b == 0xf5 = (False, ImmNone, KOther)
  | b == 0xf6 || b == 0xf7 = (True, ImmGrp3, KOther)
  | b >= 0xf8 && b <= 0xfd = (False, ImmNone, KOther)
  | b == 0xfe = (True, ImmNone, KOther)
  | b == 0xff = (True, ImmNone, KJmpInd)
  | otherwise = (False, ImmNone, KInvalid)

-- | The 0F opcode map.
spec1 :: Word8 -> (Bool, Imm, XKind)
spec1 b
  | b `elem` [0x05, 0x06, 0x07, 0x08, 0x09, 0x0e, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x37, 0x77, 0xa0, 0xa1, 0xa2, 0xa8, 0xa9, 0xaa] = (False, ImmNone, KOther)
  | b == 0x0b = (False, ImmNone, KTrap)
  | b >= 0x80 && b <= 0x8f = (False, Rel32, KJcc)
  | b >= 0xc8 && b <= 0xcf = (False, ImmNone, KOther)
  | b `elem` [0x0f, 0x70, 0x71, 0x72, 0x73, 0xa4, 0xac, 0xba, 0xc2, 0xc4, 0xc5, 0xc6] = (True, Imm8, KOther)
  | b `elem` [0x04, 0x0a, 0x0c, 0x24, 0x25, 0x26, 0x27, 0x36, 0x39, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0x7a, 0x7b, 0xa6, 0xa7] = (False, ImmNone, KInvalid)
  | otherwise = (True, ImmNone, KOther)

sext32 :: Word32 -> Int64
sext32 w = fromIntegral (fromIntegral w :: Int32)

sext8 :: Word8 -> Int64
sext8 w = fromIntegral (fromIntegral w :: Int8)

hex :: Word64 -> String
hex a = "0x" ++ go a
  where
    go 0 = ""
    go v = go (v `shiftR` 4) ++ [("0123456789abcdef" !!) (fromIntegral (v .&. 15))]

-- | Recursive-descent decoding from a label within a Haskell definition.
-- GHC emits a switch's table lookup as one contiguous sequence, so a lea
-- of a table is recognised by looking at the instructions after it:
-- @lea T(%rip), r; add (r, i, 8), r; jmp *r@ (64-bit displacements, on
-- Darwin) or @lea T(%rip), r; movslq (r, i, 4), c; add c, r; jmp *r@
-- (32-bit displacements, on Linux with PIC). Without PIC the table holds
-- absolute addresses and is read directly: @jmp *T(, i, 8)@.
decodeFrom :: Hooks -> Word64 -> IO ()
decodeFrom hk start = go start
  where
    inDef a = a >= hkDefStart hk && a < hkDefEnd hk
    name = hkDefName hk
    go pc
      | pc >= hkDefEnd hk = pure ()
      | otherwise = do
          let ins = decodeInsn (bytesAt hk pc 15)
          if xKind ins == KInvalid
            then do
              first <- hkFirstPass hk
              unless first (hkAnomaly hk ("undecodable instruction at " ++ hex pc ++ " in " ++ name))
            else do
              fresh <- hkVisit hk pc (xLen ins)
              when fresh (step pc ins)
    end pc ins = pc + fromIntegral (xLen ins)
    field32 pc off = sext32 (u32 (bytesAt hk (pc + fromIntegral off) 4) 0)
    step pc ins = do
      let next = end pc ins
          dest = maybe 0 (\(_, r, _) -> r) (xModRm ins)
          isLea = xMap ins == 0 && xOp ins == 0x8d
      -- RIP-relative operand
      case xRip ins of
        Nothing -> pure ()
        Just off -> do
          let t = fromIntegral (fromIntegral next + field32 pc off)
          hkRef hk (pc + fromIntegral off) RRel32 t next
          case if isLea then tableAfter next dest else Nothing of
            Just size -> hkJumpTable hk t size True
            Nothing
              | hkInText hk t -> hkLabel hk t True
              | otherwise -> hkData hk t
      -- absolute operands, in a non-PIE executable
      let absFields = [(o, 4) | not (hkPie hk), Just o <- [xAbs ins]] ++ [(o, s) | not (hkPie hk), Just (o, s) <- [xImm ins], s == 4]
      forM_ absFields $ \(off, _) -> do
        let v = fromIntegral (field32 pc off) :: Word64
        when (hkAbsField hk (pc + fromIntegral off) && hkIsHaskellAt hk v) $ do
          hkRef hk (pc + fromIntegral off) RAbs32 v 0
          if hkInText hk v then hkLabel hk v True else hkData hk v
      case xKind ins of
        KJcc -> do
          let t = branchTarget pc ins
          when (relSize ins == 4) $ hkRef hk (pc + fromIntegral (relOff ins)) RRel32 t next
          if inDef t
            then go t >> go next
            else do
              hkAnomaly hk ("conditional branch leaves the definition at " ++ hex pc ++ " in " ++ name)
              go next
        KJmp -> do
          let t = branchTarget pc ins
          when (relSize ins == 4) $ hkRef hk (pc + fromIntegral (relOff ins)) RRel32 t next
          if inDef t then go t else hkLabel hk t False
        KCall -> do
          let t = branchTarget pc ins
          hkRef hk (pc + fromIntegral (relOff ins)) RRel32 t next
          when (inDef t || hkIsHaskellAt hk t) (hkPin hk "call from Haskell" t)
          go next
        KJmpInd -> case (xModRm ins, xSib ins, xAbs ins) of
          -- jmp *T(, i, 8): a table of absolute addresses
          (Just (0, _, _), Just (3, _, 5), Just off) | not (hkPie hk) -> hkJumpTable hk (fromIntegral (field32 pc off)) 8 False
          _ -> pure ()
        KCallInd -> go next
        KRet -> pure ()
        KTrap -> pure ()
        _ -> go next
    relOff ins = maybe 0 fst (xRel ins)
    relSize ins = maybe 0 snd (xRel ins)
    branchTarget pc ins = case xRel ins of
      Just (off, 1) -> fromIntegral (fromIntegral (end pc ins) + sext8 (byteAt hk (pc + fromIntegral off)))
      Just (off, _) -> fromIntegral (fromIntegral (end pc ins) + field32 pc off)
      Nothing -> 0
    tableAfter = tableFollowing hk

-- | The entry size of the table a lea into register r begins to read at
-- pc, if any: the register allocator may spill and reload other registers
-- between the lea and the add, so instructions that leave r alone are
-- skipped (a few at most).
tableFollowing :: Hooks -> Word64 -> Int -> Maybe Int
tableFollowing hk pc0 r = scan pc0 (8 :: Int) Nothing
  where
    scan _ 0 _ = Nothing
    scan pc n loadedReg =
      let i = decodeInsn (bytesAt hk pc 15)
          next = pc + fromIntegral (xLen i)
       in if
            | xKind i == KInvalid || xKind i `elem` [KJmp, KJcc, KCall, KRet, KTrap, KCallInd] -> Nothing
            | isJmpReg i -> case loadedReg of
                Just (True, _) -> Just 8
                Just (False, _) -> Just 4
                Nothing -> Nothing
            | isAdd8 i -> scan next (n - 1) (Just (True, r))
            | isMovslq i -> scan next (n - 1) (Just (False, loaded i))
            | Just (False, c) <- loadedReg, isAddReg i c -> scan next (n - 1) (Just (False, c))
            | mentions i -> Nothing
            | otherwise -> scan next (n - 1) loadedReg
    isAdd8 i = xMap i == 0 && xOp i == 0x03 && case (xModRm i, xSib i) of
      (Just (md, rr, _), Just (3, _, base)) -> md /= 3 && rr == r && base == r
      _ -> False
    isMovslq i = xMap i == 0 && xOp i == 0x63 && xRexW i && case (xModRm i, xSib i) of
      (Just (md, _, _), Just (2, _, base)) -> md /= 3 && base == r
      _ -> False
    loaded i = maybe 0 (\(_, rr, _) -> rr) (xModRm i)
    isAddReg i cReg = xMap i == 0 && case (xOp i, xModRm i) of
      (0x01, Just (3, src, dst)) -> src == cReg && dst == r
      (0x03, Just (3, dst, src)) -> src == cReg && dst == r
      _ -> False
    isJmpReg i = xKind i == KJmpInd && case xModRm i of
      Just (3, _, rr) -> rr == r
      _ -> False
    -- whether an instruction names r in any operand
    mentions i = case xModRm i of
      Just (md, rr, rm) -> rr == r || (md == 3 && rm == r) || (md /= 3 && case xSib i of
        Just (_, ix, base) -> ix == r || base == r
        Nothing -> rm == r && md /= 0 || (rm == r && md == 0 && rm /= 5))
      Nothing -> case xOp i of
        -- push/pop/xchg/mov-immediate forms name a register in the opcode
        op | xMap i == 0 && (op >= 0x50 && op <= 0x5f || op >= 0x90 && op <= 0x97 || op >= 0xb8 && op <= 0xbf) -> fromIntegral (op .&. 7) == r .&. 7
        _ -> False

-- | Every jump table a definition's bytes name, found by scanning them for
-- the lookup idiom rather than by reaching the code: a table's entries can
-- only be read once the tables that follow it are known, and the code that
-- names a table may itself be reachable only through another table.
-- Returns (table address, entry size, relative).
scanTables :: Hooks -> [(Word64, Int, Bool)]
scanTables hk = go (hkDefStart hk)
  where
    go pc
      | pc + 7 > hkDefEnd hk = []
      | otherwise =
          let b0 = byteAt hk pc
              b1 = byteAt hk (pc + 1)
              b2 = byteAt hk (pc + 2)
              -- lea r, disp32(%rip): optional REX, 8d, modrm with mod 00 rm 101
              leaAt p = let i = decodeInsn (bytesAt hk p 15) in if xMap i == 0 && xOp i == 0x8d && xRip i /= Nothing then Just i else Nothing
              fromLea p i =
                let next = p + fromIntegral (xLen i)
                    r = maybe 0 (\(_, rr, _) -> rr) (xModRm i)
                    t = fromIntegral (fromIntegral next + sext32 (u32 (bytesAt hk (p + fromIntegral (maybe 0 id (xRip i))) 4) 0))
                 in case tableFollowing hk next r of
                      Just size -> [(t, size, True)]
                      Nothing -> []
              here
                | b0 == 0x8d && b1 .&. 0xc7 == 0x05 = maybe [] (fromLea pc) (leaAt pc)
                | b0 >= 0x48 && b0 <= 0x4f && b1 == 0x8d && b2 .&. 0xc7 == 0x05 = maybe [] (fromLea pc) (leaAt pc)
                -- jmp *disp32(, i, 8): ff 24 sib with base 101 and scale 8
                | b0 == 0xff && b1 == 0x24 && b2 .&. 0xc7 == 0xc5 && not (hkPie hk) =
                    [(fromIntegral (sext32 (u32 (bytesAt hk (pc + 3) 4) 0)), 8, False)]
                | otherwise = []
           in here ++ go (pc + 1)

-- | Reads a jump table's entries, once decoding has settled: each is a
-- reference from the table to a block, and a label to decode from.
-- Entries are read while they are plausible: a displacement that is zero
-- (a missing case) or negative and lands in the definition, or an absolute
-- address in it; never past the next table, and never one that lands
-- inside an instruction already decoded, which only a misread entry does.
tableEntries :: Hooks -> Word64 -> Int -> Bool -> IO ()
tableEntries hk t size relative = do
  bound <- hkNextTable hk t
  let loop k
        | k >= 65536 = pure k
        | otherwise = do
            let a = t + fromIntegral (k * size)
                raw = if size == 8 then word64At hk a else fromIntegral (sext32 (word32At hk a))
                target = if relative then t + raw else raw
                okRel = relative && (raw == 0 || (fromIntegral raw < (0 :: Int64) && target >= hkDefStart hk && target < hkDefEnd hk))
                okAbs = not relative && raw /= 0 && target >= hkDefStart hk && target < hkDefEnd hk
            interior <- if raw /= 0 then hkInterior hk target else pure False
            if
              | not (hkInImage hk a) || maybe False (a >=) bound || interior -> pure k
              | okRel || okAbs -> do
                  when (raw /= 0) $ do
                    hkRef hk a (if relative then (if size == 8 then RRel64 else RRel32) else RPointer) target t
                    hkLabel hk target False
                  loop (k + 1)
              | otherwise -> pure k
  count <- loop 0
  when (count == 0) (hkAnomaly hk ("empty jump table at " ++ hex t ++ " in " ++ hkDefName hk))
  when (hkInText hk t) (hkMarkData hk t (count * size))

-- | A linear sweep of fixed code: every address it names inside a Haskell
-- definition is pinned. A byte that decodes as nothing is skipped.
sweepFixed :: Hooks -> Word64 -> Word64 -> IO ()
sweepFixed hk start stop = go start
  where
    go pc
      | pc >= stop = pure ()
      | otherwise = do
          let ins = decodeInsn (bytesAt hk pc (fromIntegral (min 15 (stop - pc))))
          if xKind ins == KInvalid
            then go (pc + 1)
            else do
              let next = pc + fromIntegral (xLen ins)
                  f32 off = sext32 (u32 (bytesAt hk (pc + fromIntegral off) 4) 0)
                  rel = case xRel ins of
                    Just (off, 1) -> [fromIntegral (fromIntegral next + sext8 (byteAt hk (pc + fromIntegral off)))]
                    Just (off, _) -> [fromIntegral (fromIntegral next + f32 off)]
                    Nothing -> []
                  rip = [fromIntegral (fromIntegral next + f32 off) | Just off <- [xRip ins]]
                  abs' = [fromIntegral (f32 off) | not (hkPie hk), Just off <- [xAbs ins]] ++ [fromIntegral (f32 off) | not (hkPie hk), Just (off, 4) <- [xImm ins]]
              forM_ rel $ \t -> when (hkIsHaskellAt hk t) (hkPin hk "branch" t)
              forM_ rip $ \t -> do
                when (hkIsHaskellAt hk t) (hkPin hk "rip-relative" t)
                -- GHC code in a fixed region: the table before a label it names must be parsed
                when (hkHasTable hk t) (hkLabel hk t True)
              forM_ abs' $ \t -> when (hkIsHaskellAt hk t) (hkPin hk "absolute" t)
              -- (a pin from a coincidental value only costs size, so no relocation check here)
              go next

-- | An abort stub at @code@: lea rdi, [msg]; call barf; ud2. 16 bytes.
stubCode :: Word64 -> Word64 -> Maybe Word64 -> [Word8]
stubCode code msg barf =
  let le32 :: Int64 -> [Word8]
      le32 v = [fromIntegral (v `shiftR` (8 * k)) | k <- [0 .. 3]]
      lea = [0x48, 0x8d, 0x3d] ++ le32 (fromIntegral msg - fromIntegral (code + 7))
      call = case barf of
        Just b -> [0xe8] ++ le32 (fromIntegral b - fromIntegral (code + 12))
        Nothing -> [0x0f, 0x0b, 0x0f, 0x0b, 0x90]
   in lea ++ call ++ [0x0f, 0x0b, 0xcc, 0xcc]

stubSize :: Word64
stubSize = 16

-- | rel32 reaches 2 GB either way: no islands are ever needed.
branchRange :: Word64
branchRange = 1 `shiftL` 31

needsIslands :: Bool
needsIslands = False
