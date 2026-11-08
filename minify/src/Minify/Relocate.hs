-- | What the writers share: the choices made on the command line, the
-- statistics they report, and the relocation of an address through the
-- plan (including to the stub of a cut definition).
module Minify.Relocate
  ( Choices (..),
    Stats (..),
    relocate,
    alignUp,
    toStrict,
    hex,
    definedSymbol,
  )
where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Word
import Minify.Analysis
import Minify.Format
import Minify.Layout

-- | The flag keeps Haskell definitions in the export trie; by default only
-- the other symbols stay exported, since nothing looks a Haskell closure
-- up by name with dlsym. 'rwStrip' drops the symbol table (all but the
-- undefined symbols the stubs bind through) and the function starts.
data Choices = Choices {rwKeepHaskellExports :: Bool, rwStrip :: Bool, rwZeroFolded :: Bool}

data Stats = Stats
  { stSymbolsKept :: Int,
    stSymbolsDropped :: Int,
    stRebases :: Int,
    stBinds :: Int,
    stExports :: Int,
    stLinkeditBytes :: Int,
    stFileBytes :: Int
  }
  deriving (Show)

alignUp :: Word64 -> Word64 -> Word64
alignUp a n = (a + n - 1) .&. complement (n - 1)

toStrict :: BB.Builder -> B.ByteString
toStrict = BL.toStrict . BB.toLazyByteString

hex :: Word64 -> String
hex a = "0x" ++ go a
  where
    go 0 = ""
    go v = go (v `shiftR` 4) ++ [("0123456789abcdef" !!) (fromIntegral (v .&. 15))]

-- | The new address of an old one, or the stub that replaces a cut
-- definition it points into; an error for a removed one.
relocate :: Analysis -> Plan -> Word64 -> Either String Word64
relocate an pl a = case newAddrOf an pl a of
  Just a' -> Right a'
  Nothing -> case defAt im a of
    Just d | Just st <- M.lookup (dIndex d) (plStubs pl) -> Right $ case sbClosure st of
      -- a pointer to a cut closure keeps its tag bits
      Just c -> c + (a .&. 7)
      Nothing -> sbCode st
    _ -> Left ("reference into a removed definition at " ++ hex a ++ maybe "" ((" in " ++) . hsDisplay) (defAt im a))
  where
    im = anImage an

-- | The address of a defined symbol, by name.
definedSymbol :: Image -> B.ByteString -> Maybe Word64
definedSymbol im name = case [syValue s | s <- imSymbols im, syDefined s, syName s == name] of
  v : _ -> Just v
  [] -> Nothing

_unused :: V.Vector ()
_unused = V.empty
