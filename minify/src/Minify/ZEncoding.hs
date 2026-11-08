-- Optimized in every build, --fast too: minifying is slow without it
{-# OPTIONS_GHC -O2 #-}
-- | GHC's z-encoding of symbol names, and how a linked symbol's name splits
-- into package, module, binding and kind.
module Minify.ZEncoding
  ( zdecode,
    HsName (..),
    parseHsName,
    displayName,
    packageBase,
  )
where

import Data.Char (chr, isDigit, isHexDigit, isUpper)
import qualified Data.ByteString.Char8 as BC
import Numeric (readHex)

-- | Decodes a z-encoded string. Unknown escapes are left as they are.
zdecode :: String -> String
zdecode [] = []
zdecode ('Z' : c : rest)
  | isDigit c =
      let (ds, rest') = span isDigit (c : rest)
       in case rest' of
            'T' : r -> tuple (read ds) ++ zdecode r
            'H' : r -> "(#" ++ replicate (max 0 (read ds - 1)) ',' ++ "#)" ++ zdecode r
            _ -> 'Z' : zdecode (c : rest)
  | otherwise = case c of
      'Z' -> 'Z' : zdecode rest
      'L' -> '(' : zdecode rest
      'R' -> ')' : zdecode rest
      'M' -> '[' : zdecode rest
      'N' -> ']' : zdecode rest
      'C' -> ':' : zdecode rest
      _ -> 'Z' : zdecode (c : rest)
  where
    tuple :: Int -> String
    tuple 0 = "()"
    tuple n = "(" ++ replicate (n - 1) ',' ++ ")"
zdecode ('z' : c : rest)
  | isHexDigit c && isUpper c || isDigit c =
      let (hs, rest') = span isHexDigit (c : rest)
       in case rest' of
            'U' : r | [(v, "")] <- readHex hs -> chr v : zdecode r
            _ -> 'z' : zdecode (c : rest)
  | otherwise = case lookup c table of
      Just d -> d : zdecode rest
      Nothing -> 'z' : zdecode (c : rest)
  where
    table =
      [ ('z', 'z'), ('a', '&'), ('b', '|'), ('c', '^'), ('d', '$'), ('e', '='), ('g', '>'), ('h', '#'),
        ('i', '.'), ('l', '<'), ('m', '-'), ('n', '!'), ('p', '+'), ('q', '\''), ('r', '\\'), ('s', '/'),
        ('t', '*'), ('u', '_'), ('v', '%')
      ]
zdecode (c : rest) = c : zdecode rest

-- | A GHC-generated symbol, split: package (z-encoded, may be empty for the
-- main program), module (z-encoded), binding (z-encoded), kind suffix.
data HsName = HsName
  { hsPackage :: !BC.ByteString,
    hsModule :: !BC.ByteString,
    hsBinding :: !BC.ByteString,
    hsKind :: !BC.ByteString
  }
  deriving (Show, Eq)

-- | The kind suffixes GHC appends, longest first.
kinds :: [BC.ByteString]
kinds = ["_con_info", "_static_info", "_info", "_closure", "_entry", "_bytes", "_srt", "_slow", "_fast", "_str", "_ret"]

-- | Splits a linked symbol name (with its leading underscore on Darwin,
-- without on ELF). A module component starts with an uppercase letter; a
-- binding and a module never contain an underscore, since z-encoding
-- escapes it, so the components are unambiguous.
parseHsName :: BC.ByteString -> Maybe HsName
parseHsName sym0 = do
  let sym = maybe sym0 id (BC.stripPrefix "_" sym0)
  (stem, kind) <- firstJust [(,k) <$> BC.stripSuffix k sym | k <- kinds]
  case BC.split '_' stem of
    [m, b] | m `elem` ["ZCMain", "Main"] -> Just (HsName BC.empty m b kind)
    [p, m, b] | moduleLike m, not (BC.null p), p `notElem` runtimePrefixes -> Just (HsName p m b kind)
    _ -> Nothing
  where
    -- the runtime's Cmm procedures (stg_ap_p_info) and C entry points (hs_init) are not Haskell
    runtimePrefixes = ["stg", "rts", "hs"]
    firstJust xs = case [x | Just x <- xs] of
      x : _ -> Just x
      [] -> Nothing
    moduleLike m = not (BC.null m) && isUpper (BC.head m)

-- | The package name without its version and hash: @unison-runtime@ from
-- @unisonzmruntimezm0zi0zi0zmIhqCSoMNjyC4h5j74gBxPE@.
packageBase :: BC.ByteString -> String
packageBase p = strip (zdecode (BC.unpack p))
  where
    strip s = case break (== '-') s of
      (a, '-' : c : _) | isDigit c -> a
      (a, '-' : r) -> a ++ "-" ++ strip r
      (a, _) -> a

-- | @package:Module.binding@, or @Module.binding@ for the main program.
displayName :: HsName -> String
displayName HsName {..} =
  (if BC.null hsPackage then "" else packageBase hsPackage ++ ":")
    ++ zdecode (BC.unpack hsModule)
    ++ "."
    ++ zdecode (BC.unpack hsBinding)
