-- | AArch64: the instruction forms that carry a PC-relative or
-- page-relative address, decoded and re-encoded; the recursive-descent
-- decoding of a Haskell definition and the linear sweep of fixed code; the
-- stubs and islands the writer plants.
module Minify.Arm64
  ( Insn (..),
    decode,
    decodeFrom,
    sweepFixed,
    patchInsn,
    maskInsn,
    stubCode,
    islandCode,
    islandSize,
    branchRange,
    needsIslands,
    isAdd12For,
    isLdr12For,
    add12Imm,
    ldr12Imm,
    encodeAdrp,
    encodeAdd12,
    encodeLdr12,
    encodeImm26,
    encodeImm19,
    encodeImm14,
    isBlockEnd,
    adrpConsumers,
    adrpInsn,
    addInsn,
    bInsn,
    blInsn,
    brkInsn,
  )
where

import Control.Monad
import Data.Bits
import Data.Int
import Data.Word
import Minify.Hooks
import Minify.Ref

-- | What an instruction word is, as far as this tool cares.
data Insn
  = -- | destination register, page target
    Adrp !Int !Word64
  | -- | target
    Adr !Word64
  | B !Word64
  | Bl !Word64
  | -- | b.cond
    BCond !Word64
  | -- | cbz / cbnz
    Cbz !Word64
  | -- | tbz / tbnz
    Tbz !Word64
  | -- | ldr (literal), ldrsw (literal), prfm (literal)
    LdrLit !Word64
  | -- | br, ret: an unconditional transfer through a register
    RegJump
  | -- | permanently undefined, or a zero word: data, not code
    Udf
  | Other
  deriving (Show, Eq)

signExtend :: Int -> Word32 -> Int64
signExtend bits v =
  let s = fromIntegral v :: Int64
   in if testBit v (bits - 1) then s - (1 `shiftL` bits) else s

decode :: Word64 -> Word32 -> Insn
decode pc w
  | w .&. 0x9f000000 == 0x90000000 =
      let immlo = fromIntegral ((w `shiftR` 29) .&. 3) :: Word32
          immhi = (w `shiftR` 5) .&. 0x7ffff
          imm21 = signExtend 21 ((immhi `shiftL` 2) .|. immlo)
       in Adrp (fromIntegral (w .&. 0x1f)) (fromIntegral (fromIntegral (pc .&. complement 0xfff) + imm21 * 4096))
  | w .&. 0x9f000000 == 0x10000000 =
      let immlo = fromIntegral ((w `shiftR` 29) .&. 3) :: Word32
          immhi = (w `shiftR` 5) .&. 0x7ffff
       in Adr (rel pc (signExtend 21 ((immhi `shiftL` 2) .|. immlo)))
  | w .&. 0xfc000000 == 0x14000000 = B (rel pc (4 * signExtend 26 (w .&. 0x3ffffff)))
  | w .&. 0xfc000000 == 0x94000000 = Bl (rel pc (4 * signExtend 26 (w .&. 0x3ffffff)))
  | w .&. 0xff000010 == 0x54000000 = BCond (rel pc (4 * signExtend 19 ((w `shiftR` 5) .&. 0x7ffff)))
  | w .&. 0x7e000000 == 0x34000000 = Cbz (rel pc (4 * signExtend 19 ((w `shiftR` 5) .&. 0x7ffff)))
  | w .&. 0x7e000000 == 0x36000000 = Tbz (rel pc (4 * signExtend 14 ((w `shiftR` 5) .&. 0x3fff)))
  | w .&. 0x3b000000 == 0x18000000 = LdrLit (rel pc (4 * signExtend 19 ((w `shiftR` 5) .&. 0x7ffff)))
  | w .&. 0xfffffc1f == 0xd61f0000 = RegJump -- br
  | w .&. 0xfffffc1f == 0xd65f0000 = RegJump -- ret
  | w .&. 0xffff0000 == 0 = Udf
  | otherwise = Other
  where
    rel p d = fromIntegral (fromIntegral p + d)

-- | @add Xd, Xn, #imm12@ (64-bit, no shift) with the given Xn.
isAdd12For :: Int -> Word32 -> Bool
isAdd12For rn w = w .&. 0xffc00000 == 0x91000000 && fromIntegral ((w `shiftR` 5) .&. 0x1f) == rn

-- | @ldr Xt, [Xn, #imm12*8]@ with the given Xn.
isLdr12For :: Int -> Word32 -> Bool
isLdr12For rn w = w .&. 0xffc00000 == 0xf9400000 && fromIntegral ((w `shiftR` 5) .&. 0x1f) == rn

add12Imm :: Word32 -> Word64
add12Imm w = fromIntegral ((w `shiftR` 10) .&. 0xfff)

ldr12Imm :: Word32 -> Word64
ldr12Imm w = fromIntegral ((w `shiftR` 10) .&. 0xfff) * 8

-- | Re-encodes an adrp at a new pc for a new target page.
encodeAdrp :: Word32 -> Word64 -> Word64 -> Either String Word32
encodeAdrp w pc target =
  let d = (fromIntegral (target `shiftR` 12) - fromIntegral (pc `shiftR` 12)) :: Int64
   in if d < -(1 `shiftL` 20) || d >= (1 `shiftL` 20)
        then Left "adrp target out of range"
        else
          let imm = fromIntegral d .&. 0x1fffff :: Word32
              immlo = imm .&. 3
              immhi = imm `shiftR` 2
           in Right ((w .&. 0x9f00001f) .|. (immlo `shiftL` 29) .|. (immhi `shiftL` 5))

encodeAdd12 :: Word32 -> Word64 -> Word32
encodeAdd12 w target = (w .&. complement 0x3ffc00) .|. (fromIntegral (target .&. 0xfff) `shiftL` 10)

encodeLdr12 :: Word32 -> Word64 -> Either String Word32
encodeLdr12 w target
  | target .&. 7 /= 0 = Left "GOT load target not 8-aligned"
  | otherwise = Right ((w .&. complement 0x3ffc00) .|. (fromIntegral ((target .&. 0xfff) `shiftR` 3) `shiftL` 10))

encodeImm26 :: Word32 -> Word64 -> Word64 -> Either String Word32
encodeImm26 w pc target =
  let d = (fromIntegral target - fromIntegral pc) `div` 4 :: Int64
   in if d < -(1 `shiftL` 25) || d >= (1 `shiftL` 25)
        then Left "branch target out of range"
        else Right ((w .&. 0xfc000000) .|. (fromIntegral d .&. 0x3ffffff))

encodeImm19 :: Word32 -> Word64 -> Word64 -> Either String Word32
encodeImm19 w pc target =
  let d = (fromIntegral target - fromIntegral pc) `div` 4 :: Int64
   in if d < -(1 `shiftL` 18) || d >= (1 `shiftL` 18)
        then Left "conditional branch target out of range"
        else Right ((w .&. complement 0xffffe0) .|. ((fromIntegral d .&. 0x7ffff) `shiftL` 5))

encodeImm14 :: Word32 -> Word64 -> Word64 -> Either String Word32
encodeImm14 w pc target =
  let d = (fromIntegral target - fromIntegral pc) `div` 4 :: Int64
   in if d < -(1 `shiftL` 13) || d >= (1 `shiftL` 13)
        then Left "test-bit branch target out of range"
        else Right ((w .&. complement 0x7ffe0) .|. ((fromIntegral d .&. 0x3fff) `shiftL` 5))

-- | Whether linear decoding stops after this instruction.
isBlockEnd :: Insn -> Bool
isBlockEnd = \case
  B _ -> True
  RegJump -> True
  _ -> False

-- | Fresh instructions for the abort stubs: @adrp Xd, page@, @add Xd, Xd, #lo@,
-- @b target@, @bl target@, @brk #0@.
adrpInsn :: Int -> Word64 -> Word64 -> Either String Word32
adrpInsn rd pc target = encodeAdrp (0x90000000 .|. fromIntegral rd) pc target

addInsn :: Int -> Int -> Word64 -> Word32
addInsn rd rn target = encodeAdd12 (0x91000000 .|. (fromIntegral rn `shiftL` 5) .|. fromIntegral rd) target

bInsn :: Word64 -> Word64 -> Either String Word32
bInsn = encodeImm26 0x14000000

blInsn :: Word64 -> Word64 -> Either String Word32
blInsn = encodeImm26 0x94000000

brkInsn :: Word32
brkInsn = 0xd4200000

-- | The page offsets that the instructions following an @adrp@ into
-- register @rd@ add to it (C compilers may schedule the @add@ or @ldr@
-- away from the @adrp@, and may use one @adrp@ for several accesses).
-- Nothing when the register's use cannot be followed: it is overwritten
-- before any use, a branch intervenes, or the window ends with it live.
adrpConsumers :: Int -> [Word32] -> Maybe [Word64]
adrpConsumers rd = go (8 :: Int) []
  where
    -- the register still live when the window ends or control flow forks:
    -- a consumer on another path would be missed, so no claim is made
    go _ _ [] = Nothing
    go 0 _ _ = Nothing
    go n found (w : rest)
      | isLoadStore w && rn w == rd =
          let size = fromIntegral (w `shiftR` 30) :: Int
              vector = testBit w 26
              scale = if vector && testBit w 23 then 4 else size
              off = (fromIntegral ((w `shiftR` 10) .&. 0xfff) :: Word64) `shiftL` scale
           in if writesRd w then Just (reverse (off : found)) else go (n - 1) (off : found) rest
      | w .&. 0xff800000 == 0x91000000 && rn w == rd =
          let off = fromIntegral ((w `shiftR` 10) .&. 0xfff) `shiftL` (if testBit w 22 then 12 else 0)
           in if fromIntegral (w .&. 0x1f) == rd then Just (reverse (off : found)) else go (n - 1) (off : found) rest
      | isControl (decode 0 w) = Nothing
      | fromIntegral (w .&. 0x1f) == rd && not (isStore w) = finish found
      | otherwise = go (n - 1) found rest
    -- the register was overwritten: its uses are all seen
    finish [] = Nothing
    finish found = Just (reverse found)
    rn w = fromIntegral ((w `shiftR` 5) .&. 0x1f) :: Int
    isLoadStore w = w .&. 0x3b000000 == 0x39000000
    isStore w = isLoadStore w && not (testBit w 22)
    writesRd w = not (isStore w) && fromIntegral (w .&. 0x1f) == rd
    isControl = \case
      B _ -> True
      Bl _ -> True
      BCond _ -> True
      Cbz _ -> True
      Tbz _ -> True
      RegJump -> True
      Udf -> True
      _ -> False

-- * Decoding a definition

hex :: Word64 -> String
hex a = "0x" ++ go a
  where
    go 0 = ""
    go n = go (n `shiftR` 4) ++ [("0123456789abcdef" !!) (fromIntegral (n .&. 15))]

-- | Recursive-descent decoding from a label within a Haskell definition.
-- GHC's native code generator emits a closed set of address-carrying
-- forms: adrp+add (adjacent) for a label, adrp+ldr for a GOT slot, b for
-- jumps and tail calls, bl for foreign calls, and b.cond, cbz, tbz within
-- a procedure. Anything else that names an address is an anomaly.
decodeFrom :: Hooks -> Word64 -> IO ()
decodeFrom hk = go
  where
    inDef a = a >= hkDefStart hk && a < hkDefEnd hk
    name = hkDefName hk
    go pc
      | pc + 4 > hkDefEnd hk = pure ()
      | otherwise = do
          fresh <- hkVisit hk pc 4
          when fresh $ do
            let w = word32At hk pc
            case decode pc w of
              Adrp rd page -> do
                let w2 = word32At hk (pc + 4)
                if
                  | isAdd12For rd w2 -> do
                      let t = page + add12Imm w2
                      hkRef hk pc RAdrpAdd t 0
                      if hkInText hk t then hkLabel hk t True else hkData hk t
                      _ <- hkVisit hk (pc + 4) 4
                      go (pc + 8)
                  | isLdr12For rd w2 -> do
                      hkRef hk pc RAdrpLdr (page + ldr12Imm w2) 0
                      _ <- hkVisit hk (pc + 4) 4
                      go (pc + 8)
                  | otherwise -> hkAnomaly hk ("adrp without add or ldr at " ++ hex pc ++ " in " ++ name)
              B t -> do
                hkRef hk pc RB t 0
                if inDef t then go t else hkLabel hk t False
              Bl t -> do
                hkRef hk pc RBl t 0
                -- a call into a Haskell definition's region means C code lives there; it must not move
                when (inDef t || hkIsHaskellAt hk t) (hkPin hk "bl from Haskell" t)
                go (pc + 4)
              BCond t -> branch pc t RBcond
              Cbz t -> branch pc t RCbz
              Tbz t -> branch pc t RTbz
              Adr _ -> hkAnomaly hk ("adr at " ++ hex pc ++ " in " ++ name)
              LdrLit _ -> hkAnomaly hk ("literal load at " ++ hex pc ++ " in " ++ name)
              Udf -> do
                -- a zero word before an info table is alignment padding after a fall-through
                nextIsTable <- if pc + 4 < hkDefEnd hk then hkIsTable hk (pc + 4) else pure False
                first <- hkFirstPass hk
                unless (first || (w == 0 && nextIsTable)) $
                  hkAnomaly hk ("data decoded as code at " ++ hex pc ++ " in " ++ name)
              RegJump -> pure ()
              Other -> go (pc + 4)
    branch pc t kind = do
      hkRef hk pc kind t 0
      if inDef t
        then go t >> go (pc + 4)
        else hkAnomaly hk ("conditional branch leaves the definition at " ++ hex pc ++ " in " ++ name)

-- | A linear sweep of fixed code for what it refers to. An adrp is paired
-- with its consumers when the register's uses can all be followed, and
-- each target pinned; otherwise the page is pinned. A target in code is
-- also a label: GHC code lies in fixed regions too (a module's unnamed
-- local closures follow whatever symbol precedes them, the C @main@ of the
-- program's own module for one), and the info table before such a label
-- holds an SRT offset that must follow its closure.
sweepFixed :: Hooks -> Word64 -> Word64 -> IO ()
sweepFixed hk start end = go start
  where
    go pc
      | pc + 4 > end = pure ()
      | otherwise = do
          case decode pc (word32At hk pc) of
            Adrp rd page ->
              case adrpConsumers rd [word32At hk (pc + 4 * k) | k <- [1 .. 8], pc + 4 * k + 4 <= end] of
                Just offs -> forM_ offs $ \off -> do
                  hkPin hk "adrp" (page + off)
                  -- a linear sweep misreads the tables between blocks, so only a target with a table counts
                  when (hkHasTable hk (page + off)) (hkLabel hk (page + off) True)
                Nothing -> hkPinPage hk page
            Adr t -> hkPin hk "adr" t
            B t -> hkPin hk "branch" t
            Bl t -> hkPin hk "branch" t
            LdrLit t -> hkPin hk "literal" t
            _ -> pure ()
          go (pc + 4)

-- * Rewriting

-- | Re-encodes one instruction reference: the words at the field, its new
-- address, its new target, and the island for a branch that cannot reach.
-- Returns the new words, or an error.
patchInsn :: RefKind -> (Word64 -> Maybe Word64) -> [Word32] -> Word64 -> Word64 -> Either String [Word32]
patchInsn kind island ws at' t' = case (kind, ws) of
  (RAdrpAdd, [w1, w2]) -> (\a -> [a, encodeAdd12 w2 t']) <$> encodeAdrp w1 at' t'
  (RAdrpLdr, [w1, w2]) -> do
    a <- encodeAdrp w1 at' t'
    l <- encodeLdr12 w2 t'
    pure [a, l]
  (RB, [w]) -> imm26 w
  (RBl, [w]) -> imm26 w
  (RBcond, [w]) -> (: []) <$> encodeImm19 w at' t'
  (RCbz, [w]) -> (: []) <$> encodeImm19 w at' t'
  (RTbz, [w]) -> (: []) <$> encodeImm14 w at' t'
  _ -> Left ("not an AArch64 reference: " ++ show kind)
  where
    imm26 w = case encodeImm26 w at' t' of
      Right w' -> Right [w']
      Left e -> case island t' of
        Just a -> (: []) <$> encodeImm26 w at' a
        Nothing -> Left (e ++ " and no island")

-- | The instruction words with their address bits cleared, for comparing
-- code by shape.
maskInsn :: RefKind -> [Word32] -> [Word32]
maskInsn kind ws = case (kind, ws) of
  (RAdrpAdd, [w1, w2]) -> [w1 .&. 0x9f00001f, w2 .&. complement 0x003ffc00]
  (RAdrpLdr, [w1, w2]) -> [w1 .&. 0x9f00001f, w2 .&. complement 0x003ffc00]
  (RB, [w]) -> [w .&. 0xfc000000]
  (RBl, [w]) -> [w .&. 0xfc000000]
  (RBcond, [w]) -> [w .&. complement 0x00ffffe0]
  (RCbz, [w]) -> [w .&. complement 0x00ffffe0]
  (RTbz, [w]) -> [w .&. complement 0x0007ffe0]
  _ -> ws

-- | An abort stub at @code@: loads the message into x0 and calls barf (or
-- traps when there is no barf), then traps. 16 bytes.
stubCode :: Word64 -> Word64 -> Maybe Word64 -> Either String [Word32]
stubCode code msg barf = do
  w1 <- adrpInsn 0 code msg
  w3 <- maybe (Right brkInsn) (blInsn (code + 8)) barf
  pure [w1, addInsn 0 0 msg, w3, brkInsn]

-- | A branch island at @a@ for @target@: adrp x16; add x16; br x16. x16 is
-- the register the ABI reserves for exactly this.
islandCode :: Word64 -> Word64 -> Either String [Word32]
islandCode a t = do
  w1 <- adrpInsn 16 a t
  pure [w1, addInsn 16 16 t, 0xd61f0200]

islandSize :: Word64
islandSize = 12

-- | A b or bl reaches this far either way.
branchRange :: Word64
branchRange = 128 * 1024 * 1024

needsIslands :: Bool
needsIslands = True
