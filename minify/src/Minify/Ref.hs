-- | The kinds of reference the tool finds and rewrites. The first group is
-- AArch64 instruction forms, the second x86-64 forms and jump-table
-- entries, the third fields shared by every target: info table offsets
-- and pointer slots.
module Minify.Ref
  ( RefKind (..),
    fieldLen,
    anchored,
  )
where

data RefKind
  = -- | adrp + add pair (the pair's address; 8 bytes)
    RAdrpAdd
  | -- | adrp + ldr pair for a GOT slot
    RAdrpLdr
  | RB
  | RBl
  | RBcond
  | RCbz
  | RTbz
  | -- | a 4-byte signed displacement measured from an anchor stored with the
    -- reference: x86 branches and calls, RIP-relative operands, and 32-bit
    -- jump table entries (anchor: the table)
    RRel32
  | -- | an 8-byte signed displacement from an anchor: 64-bit jump table entries
    RRel64
  | -- | a 4-byte absolute address, sign-extended (x86 in a non-PIE executable)
    RAbs32
  | -- | the SRT offset in the last 4 bytes of an info table
    RSrt
  | -- | the con_desc offset of a constructor table
    RConDesc
  | RLargeBitmap
  | RSlowApply
  | RFunBitmap
  | -- | an 8-byte absolute pointer in data
    RPointer
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Bytes the field occupies.
fieldLen :: RefKind -> Int
fieldLen = \case
  RAdrpAdd -> 8
  RAdrpLdr -> 8
  RB -> 4
  RBl -> 4
  RBcond -> 4
  RCbz -> 4
  RTbz -> 4
  RRel32 -> 4
  RRel64 -> 8
  RAbs32 -> 4
  RSrt -> 4
  _ -> 8

-- | Whether the reference's anchor (the address the displacement is
-- measured from) is stored with it rather than implied by the kind.
anchored :: RefKind -> Bool
anchored k = k == RRel32 || k == RRel64
