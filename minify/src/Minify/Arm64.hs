-- | The AArch64 instruction forms that carry a PC-relative or page-relative
-- address, decoded and re-encoded. This is the one architecture-specific
-- module.
module Minify.Arm64
  ( Insn (..),
    decode,
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
    adrpInsn,
    addInsn,
    bInsn,
    blInsn,
    brkInsn,
  )
where

import Data.Bits
import Data.Int
import Data.Word

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
