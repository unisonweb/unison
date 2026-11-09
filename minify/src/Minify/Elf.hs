-- Optimized in every build, --fast too: minifying is slow without it
{-# OPTIONS_GHC -O2 #-}

-- | Reading a 64-bit little-endian ELF executable: the header, the program
-- and section headers, the symbol tables and the dynamic relocations.
module Minify.Elf
  ( Elf (..),
    Phdr (..),
    Shdr (..),
    ElfSym (..),
    Rela (..),
    parseElf,
    loadElf,
    sectionName,
    symbolsOf,
    dynamicEntries,

    -- * Constants
    ptLoad,
    ptPhdr,
    ptDynamic,
    ptGnuRelro,
    shtNull,
    shtProgbits,
    shtSymtab,
    shtStrtab,
    shtRela,
    shtNobits,
    shtDynsym,
    shtRelr,
    shfAlloc,
    emX86_64,
    emAarch64,
    etExec,
    etDyn,
    relativeType,
    isBindType,
  )
where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Int
import Data.List (sortOn)
import Data.Word
import Minify.Bytes
import qualified Minify.Format as F

data Phdr = Phdr
  { phType :: !Word32,
    phFlags :: !Word32,
    phOffset :: !Word64,
    phVaddr :: !Word64,
    phPaddr :: !Word64,
    phFilesz :: !Word64,
    phMemsz :: !Word64,
    phAlign :: !Word64
  }
  deriving (Show, Eq)

data Shdr = Shdr
  { shName :: !Word32,
    shType :: !Word32,
    shFlags :: !Word64,
    shAddr :: !Word64,
    shOffset :: !Word64,
    shSize :: !Word64,
    shLink :: !Word32,
    shInfo :: !Word32,
    shAddralign :: !Word64,
    shEntsize :: !Word64
  }
  deriving (Show, Eq)

data ElfSym = ElfSym
  { stName :: !Word32,
    stInfo :: !Word8,
    stOther :: !Word8,
    stShndx :: !Word16,
    stValue :: !Word64,
    stSize :: !Word64
  }
  deriving (Show, Eq)

-- | A relocation with addend: where, the type, the symbol index, the addend.
data Rela = Rela {rOffset :: !Word64, rType :: !Word32, rSym :: !Word32, rAddend :: !Int64}
  deriving (Show, Eq)

data Elf = Elf
  { elfBytes :: !B.ByteString,
    elfType :: !Word16,
    elfMachine :: !Word16,
    elfEntry :: !Word64,
    elfPhoff :: !Word64,
    elfShoff :: !Word64,
    elfFlags :: !Word32,
    elfShstrndx :: !Int,
    elfPhdrs :: [Phdr],
    elfShdrs :: [Shdr],
    -- | the section header string table
    elfShstr :: !B.ByteString,
    -- | the index of .symtab, if any
    elfSymtabIx :: Maybe Int,
    elfDynsymIx :: Maybe Int,
    -- | every entry of the loaded RELA sections (.rela.dyn, .rela.plt)
    elfRelas :: [Rela],
    -- | every RELA section with its entries, loaded or not
    elfRelaSections :: [(Shdr, [Rela])],
    -- | slots listed in a RELR section, if any
    elfRelr :: [Word64]
  }

ptLoad, ptPhdr, ptDynamic, ptGnuRelro :: Word32
ptLoad = 1
ptPhdr = 6
ptDynamic = 2
ptGnuRelro = 0x6474e552

shtNull, shtProgbits, shtSymtab, shtStrtab, shtRela, shtNobits, shtDynsym, shtRelr :: Word32
shtNull = 0
shtProgbits = 1
shtSymtab = 2
shtStrtab = 3
shtRela = 4
shtNobits = 8
shtDynsym = 11
shtRelr = 19

shfAlloc :: Word64
shfAlloc = 2

emX86_64, emAarch64 :: Word16
emX86_64 = 62
emAarch64 = 183

etExec, etDyn :: Word16
etExec = 2
etDyn = 3

-- | The relocation type that slides a pointer by the load bias.
relativeType :: Word16 -> Word32
relativeType m = if m == emAarch64 then 1027 else 8

-- | Relocations that bind a slot to a named symbol: GLOB_DAT, JUMP_SLOT
-- and the absolute 64-bit one with a symbol.
isBindType :: Word16 -> Word32 -> Bool
isBindType m t
  | m == emAarch64 = t `elem` [1025, 1026, 257]
  | otherwise = t `elem` [6, 7, 1]

parseElf :: B.ByteString -> Either String Elf
parseElf b
  | B.length b < 64 = Left "file too short for an ELF header"
  | B.take 4 b /= "\x7f\&ELF" = Left "not an ELF file"
  | u8 b 4 /= 2 = Left "not a 64-bit ELF file"
  | u8 b 5 /= 1 = Left "not a little-endian ELF file"
  | otherwise =
      let phoff = u64 b 32
          shoff = u64 b 40
          phentsize = fromIntegral (u16 b 54) :: Int
          phnum = fromIntegral (u16 b 56) :: Int
          shentsize = fromIntegral (u16 b 58) :: Int
          shnum = fromIntegral (u16 b 60) :: Int
          shstrndx = fromIntegral (u16 b 62) :: Int
          phdrs = [parsePhdr (fromIntegral phoff + i * phentsize) | i <- [0 .. phnum - 1]]
          shdrs = [parseShdr (fromIntegral shoff + i * shentsize) | i <- [0 .. shnum - 1]]
          shstr = case drop shstrndx shdrs of
            s : _ -> B.take (fromIntegral (shSize s)) (B.drop (fromIntegral (shOffset s)) b)
            [] -> B.empty
          withIndex = zip [0 :: Int ..] shdrs
          symtabIx = case [i | (i, s) <- withIndex, shType s == shtSymtab] of
            i : _ -> Just i
            [] -> Nothing
          dynsymIx = case [i | (i, s) <- withIndex, shType s == shtDynsym] of
            i : _ -> Just i
            [] -> Nothing
          relaSections = [(s, parseRelas s) | s <- shdrs, shType s == shtRela]
          relas = concat [rs | (s, rs) <- relaSections, shFlags s .&. shfAlloc /= 0]
          relr = concat [parseRelr s | s <- shdrs, shType s == shtRelr]
          parsePhdr o = Phdr (u32 b o) (u32 b (o + 4)) (u64 b (o + 8)) (u64 b (o + 16)) (u64 b (o + 24)) (u64 b (o + 32)) (u64 b (o + 40)) (u64 b (o + 48))
          parseShdr o = Shdr (u32 b o) (u32 b (o + 4)) (u64 b (o + 8)) (u64 b (o + 16)) (u64 b (o + 24)) (u64 b (o + 32)) (u32 b (o + 40)) (u32 b (o + 44)) (u64 b (o + 48)) (u64 b (o + 56))
          parseRelas s =
            [ Rela (u64 b o) (fromIntegral (info .&. 0xffffffff)) (fromIntegral (info `shiftR` 32)) (i64 b (o + 16))
            | i <- [0 .. fromIntegral (shSize s `div` 24) - 1],
              let o = fromIntegral (shOffset s) + 24 * i,
              let info = u64 b (o + 8)
            ]
          -- RELR: an even entry is an address; an odd one a bitmap of the
          -- 63 words following the last address
          parseRelr s = go 0 (fromIntegral (shOffset s)) [fromIntegral (shOffset s) + 8 * i | i <- [0 .. fromIntegral (shSize s `div` 8) - 1]]
            where
              go _ _ [] = []
              go base _ (o : rest) =
                let e = u64 b o
                 in if e .&. 1 == 0
                      then e : go (e + 8) o rest
                      else [base + 8 * fromIntegral k | k <- [0 .. 62 :: Int], testBit e (k + 1)] ++ go (base + 63 * 8) o rest
       in Right
            Elf
              { elfBytes = b,
                elfType = u16 b 16,
                elfMachine = u16 b 18,
                elfEntry = u64 b 24,
                elfPhoff = phoff,
                elfShoff = shoff,
                elfFlags = u32 b 48,
                elfShstrndx = shstrndx,
                elfPhdrs = phdrs,
                elfShdrs = shdrs,
                elfShstr = shstr,
                elfSymtabIx = symtabIx,
                elfDynsymIx = dynsymIx,
                elfRelas = relas,
                elfRelaSections = relaSections,
                elfRelr = relr
              }

sectionName :: Elf -> Shdr -> B.ByteString
sectionName e s = cstringAt (elfShstr e) (fromIntegral (shName s))

-- | The symbols of a symbol table section, with their names.
symbolsOf :: Elf -> Int -> [(ElfSym, B.ByteString)]
symbolsOf e ix =
  let b = elfBytes e
      s = elfShdrs e !! ix
      strs = elfShdrs e !! fromIntegral (shLink s)
      strBase = fromIntegral (shOffset strs)
      n = fromIntegral (shSize s `div` 24)
   in [ (sym, cstringAt b (strBase + fromIntegral (stName sym)))
      | i <- [0 .. n - 1],
        let o = fromIntegral (shOffset s) + 24 * i,
        let sym = ElfSym (u32 b o) (u8 b (o + 4)) (u8 b (o + 5)) (u16 b (o + 6)) (u64 b (o + 8)) (u64 b (o + 16))
      ]

-- | The (tag, value) pairs of the dynamic section.
dynamicEntries :: Elf -> [(Word64, Word64)]
dynamicEntries e =
  case [p | p <- elfPhdrs e, phType p == ptDynamic] of
    p : _ ->
      let b = elfBytes e
          o0 = fromIntegral (phOffset p)
       in takeWhile ((/= 0) . fst) [(u64 b (o0 + 16 * i), u64 b (o0 + 16 * i + 8)) | i <- [0 .. fromIntegral (phFilesz p `div` 16) - 1]]
    [] -> []

-- | The format-neutral view of an ELF executable (see "Minify.Format").
loadElf :: Elf -> Either String F.Loaded
loadElf e = do
  arch <-
    if
      | elfMachine e == emAarch64 -> Right F.Arm64
      | elfMachine e == emX86_64 -> Right F.X86_64
      | otherwise -> Left ("unsupported machine " ++ show (elfMachine e))
  symIx <- maybe (Left "no symbol table: the file is stripped") Right (elfSymtabIx e)
  let loads = [(i, p) | (i, p) <- zip [0 :: Int ..] (elfPhdrs e), phType p == ptLoad]
      shdrs = zip [0 :: Int ..] (elfShdrs e)
      -- a section this tool cut into pieces is .text.1, .text.2 and so on
      role n
        | isNamed ".text" n = F.Code
        | isNamed ".rodata" n = F.Strings
        | isNamed ".data" n = F.Data
        | otherwise = F.Other
      isNamed base n = n == base || ((base <> ".") `B.isPrefixOf` n && BC.all (`elem` ['0' .. '9']) (B.drop (B.length base + 1) n))
      inLoad p s = shAddr s >= phVaddr p && shAddr s < phVaddr p + max 1 (phMemsz p)
      loadName i = BC.pack ("LOAD" ++ show i)
      sectionsIn (li, p) =
        [ F.Section
            { F.secName = nm,
              F.secSegName = loadName li,
              F.secAddr = shAddr s,
              F.secSize = shSize s,
              F.secOffset = shOffset s,
              F.secNoBits = shType s == shtNobits,
              F.secRole = role nm,
              F.secOrig = si,
              F.secRaw = F.ElfSect (shType s) (shFlags s) (shLink s) (shInfo s) (shAddralign s) (shEntsize s)
            }
        | (si, s) <- shdrs,
          shType s /= shtNull,
          shFlags s .&. shfAlloc /= 0,
          inLoad p s,
          let nm = sectionName e s
        ]
      prot f = (if f .&. 4 /= 0 then 1 else 0) .|. (if f .&. 2 /= 0 then 2 else 0) .|. (if f .&. 1 /= 0 then 4 else 0)
      segs =
        [ F.Segment
            { F.segName = loadName li,
              F.segRole = if any F.isCode secs then F.SegText else F.SegData,
              F.segVmAddr = phVaddr p,
              F.segVmSize = phMemsz p,
              F.segFileOff = phOffset p,
              F.segFileSize = phFilesz p,
              F.segMaxProt = prot (phFlags p),
              F.segInitProt = prot (phFlags p),
              F.segFlags = 0,
              F.segSections = secs,
              F.segOrig = li
            }
        | (li, p) <- loads,
          let secs = sectionsIn (li, p)
        ]
      sects = sortOn F.secOrig (concatMap F.segSections segs)
      classify sym name
        | stShndx sym == 0 || stShndx sym >= 0xff00 = F.SymIgnore
        | ty == 3 || ty == 4 = F.SymIgnore -- section, file
        | bind /= 0 = F.SymExternal
        | ty == 1 || ty == 2 || ty == 10 = F.SymLocalC -- object, func, ifunc
        | "$" `B.isPrefixOf` name || ".L" `B.isPrefixOf` name = F.SymIgnore
        | F.hasHsKindSuffix name = F.SymLocalHs
        | otherwise = F.SymLocalC
        where
          ty = stInfo sym .&. 0xf
          bind = stInfo sym `shiftR` 4
      syms =
        [ F.Symbol {F.syName = name, F.syValue = stValue sym, F.syDefined = stShndx sym /= 0 && stShndx sym < 0xff00, F.syClass = classify sym name, F.syOrig = i}
        | (i, (sym, name)) <- zip [0 ..] (symbolsOf e symIx)
        ]
      relTy = relativeType (elfMachine e)
      -- a link with --emit-relocs keeps every relocation in non-loaded
      -- sections (.rela.data and so on): their absolute 64-bit entries are
      -- an exact list of the pointer slots in data
      absTy = if elfMachine e == emAarch64 then 257 else 1
      emitted =
        [ rOffset r
        | (sh, rs) <- elfRelaSections e,
          shFlags sh .&. shfAlloc == 0,
          r <- rs,
          rType r == absTy
        ]
      relative = [rOffset r | r <- elfRelas e, rType r == relTy] ++ elfRelr e ++ emitted
      hasEmitted = any (\(sh, _) -> shFlags sh .&. shfAlloc == 0) (elfRelaSections e)
      -- R_X86_64_32 and R_X86_64_32S: the only ways a 32-bit field holds an address
      absFields =
        [ rOffset r
        | (sh, rs) <- elfRelaSections e,
          shFlags sh .&. shfAlloc == 0,
          r <- rs,
          elfMachine e == emX86_64,
          rType r == 10 || rType r == 11
        ]
      dynNames = maybe [] (map snd . symbolsOf e) (elfDynsymIx e)
      binds =
        [ (rOffset r, nm)
        | r <- elfRelas e,
          isBindType (elfMachine e) (rType r),
          rSym r /= 0,
          nm <- take 1 (drop (fromIntegral (rSym r)) dynNames)
        ]
      pageSize = maximum (4096 : [phAlign p | (_, p) <- loads])
  pure
    F.Loaded
      { F.ldFormat = F.ElfFormat,
        F.ldArch = arch,
        F.ldSlots = if null relative then Nothing else Just relative,
        F.ldBinds = binds,
        F.ldAbsFields = if hasEmitted then Just absFields else Nothing,
        F.ldPageSize = pageSize,
        F.ldPie = elfType e == etDyn,
        F.ldSegments = segs,
        F.ldSections = sects,
        F.ldSymbols = syms,
        F.ldRaw = F.RawElf
      }
