-- | What the tool needs to know about an executable regardless of its
-- file format: its segments and sections, its symbols, where the loader
-- fixes pointers up, and which parts play which role in compaction. The
-- Mach-O and ELF readers each produce a 'Loaded'.
module Minify.Format
  ( Format (..),
    Arch (..),
    Role (..),
    SegRole (..),
    Section (..),
    SectRaw (..),
    Segment (..),
    Symbol (..),
    SymClass (..),
    Loaded (..),
    Raw (..),
    isCompactedSection,
    isCode,
    hsKindSuffixes,
    hasHsKindSuffix,
    ghcLocalLabel,
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isAlphaNum, isAlpha)
import Data.Word

data Format = MachOFormat | ElfFormat deriving (Eq, Show)

data Arch = Arm64 | X86_64 deriving (Eq, Show)

-- | A section's part in compaction: code, string literals, read-only
-- constants and static closures are compacted; anything else keeps its
-- bytes and addresses.
data Role = Code | Strings | Const | Data | Other deriving (Eq, Show)

data SegRole
  = -- | the segment holding the code (and, in Mach-O, the file header)
    SegText
  | SegData
  | -- | Mach-O's link-edit segment, rebuilt by the writer
    SegLinkedit
  | -- | Mach-O's zero page: no file bytes
    SegPageZero
  | SegOther
  deriving (Eq, Show)

data Section = Section
  { secName :: !B.ByteString,
    -- | the segment's name (Mach-O), or a synthetic one for an ELF program header
    secSegName :: !B.ByteString,
    secAddr :: !Word64,
    secSize :: !Word64,
    secOffset :: !Word64,
    -- | no bytes in the file (zero fill)
    secNoBits :: !Bool,
    secRole :: !Role,
    -- | index in the format's own section list (Mach-O: n_sect - 1; ELF: the header index)
    secOrig :: !Int,
    secRaw :: !SectRaw
  }
  deriving (Show)

-- | The format-specific fields a writer has to reproduce.
data SectRaw
  = MachOSect {msAlign :: !Word32, msRelOff :: !Word32, msNReloc :: !Word32, msFlags :: !Word32, msReserved1 :: !Word32, msReserved2 :: !Word32, msReserved3 :: !Word32}
  | ElfSect {esType :: !Word32, esFlags :: !Word64, esLink :: !Word32, esInfo :: !Word32, esAlign :: !Word64, esEntSize :: !Word64}
  deriving (Show)

-- | A loadable region: a Mach-O segment or an ELF program header of type
-- PT_LOAD. Protections use the Mach-O encoding (1 read, 2 write, 4 execute).
data Segment = Segment
  { segName :: !B.ByteString,
    segRole :: !SegRole,
    segVmAddr :: !Word64,
    segVmSize :: !Word64,
    segFileOff :: !Word64,
    segFileSize :: !Word64,
    segMaxProt :: !Word32,
    segInitProt :: !Word32,
    segFlags :: !Word32,
    segSections :: [Section],
    segOrig :: !Int
  }
  deriving (Show)

-- | How the carving treats a symbol.
data SymClass
  = -- | an external definition: begins a region, classified by its name
    SymExternal
  | -- | a local definition the C compiler made: begins a fixed region
    SymLocalC
  | -- | a local label GHC made (a return point, a local closure, a constructor
    -- name string): a Haskell definition without a module of its own
    SymLocalHs
  | -- | not a definition (a mapping symbol, a dead-strip marker)
    SymIgnore
  deriving (Eq, Show)

data Symbol = Symbol
  { syName :: !B.ByteString,
    syValue :: !Word64,
    -- | defined in a section of this file
    syDefined :: !Bool,
    syClass :: !SymClass,
    -- | index in the format's symbol table
    syOrig :: !Int
  }
  deriving (Show)

data Raw = RawMachO | RawElf

-- | What a reader produces. The raw format record stays with the reader's
-- module; 'ldRaw' only says which one it is.
data Loaded = Loaded
  { ldFormat :: !Format,
    ldArch :: !Arch,
    -- | the file has an exact list of the pointer slots the loader fixes up
    -- (Mach-O rebases, ELF relative relocations); when it does not, the
    -- slots are found by parsing the static closures
    ldSlots :: Maybe [Word64],
    -- | slots the loader binds to a named symbol: (slot, symbol)
    ldBinds :: [(Word64, B.ByteString)],
    -- | when the link kept its relocations: the addresses of every 32-bit
    -- absolute field in code, so that nothing else is taken for one
    ldAbsFields :: Maybe [Word64],
    ldPageSize :: !Word64,
    ldPie :: !Bool,
    ldSegments :: [Segment],
    -- | in the format's own order
    ldSections :: [Section],
    ldSymbols :: [Symbol],
    ldRaw :: Raw
  }

isCompactedSection :: Section -> Bool
isCompactedSection s = secRole s /= Other

isCode :: Section -> Bool
isCode s = secRole s == Code

-- | The suffixes GHC gives the symbols of a binding's parts.
hsKindSuffixes :: [B.ByteString]
hsKindSuffixes = ["_con_info", "_static_info", "_info", "_closure", "_entry", "_bytes", "_srt", "_slow", "_fast", "_str", "_ret", "_closure_tbl"]

hasHsKindSuffix :: B.ByteString -> Bool
hasHsKindSuffix n = any (`B.isSuffixOf` n) hsKindSuffixes

-- | Whether a Mach-O local symbol's name is one GHC gives a local label on
-- Darwin: @L@, a unique of letters and digits, and a kind suffix
-- (@_Lc4py_info@, @_Lg1Kz_closure@). C's own local names have words
-- separated by underscores (@_mark_closure@) and no @L@. ELF readers use
-- the symbol's type instead: GHC's local labels are untyped there.
ghcLocalLabel :: B.ByteString -> Bool
ghcLocalLabel n0 =
  let n = if "_" `B.isPrefixOf` n0 then B.drop 1 n0 else n0
   in case [s | s <- hsKindSuffixes, s `B.isSuffixOf` n] of
        s : _ ->
          let u = BC.unpack (B.take (B.length n - B.length s) n)
           in case u of
                'L' : rest -> not (null rest) && all isAlphaNum rest
                _ -> False
        [] -> False

_unusedFormat :: Char -> Bool
_unusedFormat = isAlpha
