
module Unison.Util.Pattern.Bytes where

import qualified Unison.Util.Bytes as Bytes
import qualified Unison.Util.Pattern
import qualified Unison.Util.Pattern as Pat
import Data.Word8

data BytesPattern
    = Join [BytesPattern]
    | Or BytesPattern BytesPattern
    | Literal Bytes.Bytes
    | Capture BytesPattern
    | CaptureAs Bytes.Bytes BytesPattern
    | Many BytesPattern
    | Replicate Int Int BytesPattern
    | Eof
    | Byte BytePattern
    deriving (Eq, Ord, Show)

data BytePattern
    = Any
    | ByteRange Word8 Word8
    | Not BytePattern
    | Union BytePattern BytePattern
    | Intersect BytePattern BytePattern
    | ByteSet [Word8]
    | CharClass Pat.CharClass
    deriving (Eq, Ord, Show)

instance Pat.Compile Bytes.Bytes BytePattern where
  compilePattern p !err success = go
    where
      ok = bytePatternPred p
      go acc bs = case Bytes.uncons bs of
        Nothing -> err acc bs
        Just (b, bs) -> if ok b then success acc bs else err acc bs
  compileSize = Bytes.size
  compileTake = Bytes.take
  compileDrop = Bytes.drop

byteInPred, byteNotInPred :: [Word8] -> Word8 -> Bool
byteInPred [] = const False
byteInPred (c : chs) = let ok = byteInPred chs in \ci -> ci == c || ok ci
byteNotInPred [] = const True
byteNotInPred (c : chs) = let ok = byteNotInPred chs in (\ci -> ci /= c && ok ci)

charClassPred :: Pat.CharClass -> Word8 -> Bool
charClassPred Pat.AlphaNum = isAlphaNum
charClassPred Pat.Upper = isUpper
charClassPred Pat.Lower = isLower
charClassPred Pat.Whitespace = isSpace
charClassPred Pat.Control = isControl
charClassPred Pat.Printable = isPrint
charClassPred Pat.MarkChar = isMark
charClassPred Pat.Number = isNumber
charClassPred Pat.Punctuation = isPunctuation
charClassPred Pat.Symbol = isSymbol
charClassPred Pat.Separator = isSeparator
charClassPred Pat.Letter = isLetter

bytePatternPred :: BytePattern -> Word8 -> Bool
bytePatternPred Any = const True
bytePatternPred (Not cp) = let notOk = bytePatternPred cp in not . notOk
bytePatternPred (Union cp1 cp2) = let ok1 = bytePatternPred cp1; ok2 = bytePatternPred cp2 in \ci -> ok1 ci || ok2 ci
bytePatternPred (Intersect cp1 cp2) = let ok1 = bytePatternPred cp1; ok2 = bytePatternPred cp2 in \ci -> ok1 ci && ok2 ci
bytePatternPred (ByteRange c1 c2) = \ci -> ci >= c1 && ci <= c2
bytePatternPred (ByteSet cs) = byteInPred cs
bytePatternPred (CharClass cc) = charClassPred cc

-- compile :: BytesPattern -> Compiled r
-- compile !Eof !err !success = go
--   where
--     go acc bs
--       | Bytes.size bs == 0 = success acc bs
--       | otherwise = err acc bs
-- compile (Literal txt) !err !success = go
--   where
--     go acc bs
--       | Bytes.size bs == 0 = success acc bs
--       | otherwise = err acc bs
-- compile (Join ps) !err !success = go ps
--   where
--     go [] = success
--     go (p:ps) =
--         let pc = compile p err psc
--             psc = compile (Join ps) err success
--         in pc
-- compile (Or p1 p2) err success = cp1
--   where
--     cp2 = compile p2 err success
--     cp1 = try "Or" (compile p1) cp2 success
-- compile (Capture p) !err !success = go
--   where
--     err' _ _ acc0 t0 = err acc0 t0
--     success' _ rem acc0 t0 = success (pushCapture (Bytes.take (Bytes.size t0 - Bytes.size rem) t0) acc0) rem
--     compiled = compile p err' success'
--     go acc t = compiled acc t acc t
-- compile (CaptureAs t p) !err !success = go
--   where
--     err' _ _ acc0 t0 = err acc0 t0
--     success' _ rem acc0 _ = success (pushCapture t acc0) rem
--     compiled = compile p err' success'
--     go acc t = compiled acc t acc t
-- compile (Byte b) !err !success = go
--   where
--     ok = bytePatternPred b
--     go acc bs = case Bytes.uncons bs of
--       Nothing -> err acc bs
--       Just (b, bs) -> if ok b then success acc bs else err acc bs
-- compile (Replicate m n p) !err !success = case p of
--   Byte Any -> \acc t ->
--     if Bytes.size t < m
--       then err acc t
--       else success acc (Bytes.drop m t)
--   Byte bp -> dropper (bytePatternPred bp)
--   _ -> try "Replicate" (go1 m) err (go2 (n - m))
--   where
--     go1 0 = \_err success stk rem -> success stk rem
--     go1 n = \err success -> compile p err (go1 (n - 1) err success)
--     go2 0 = success
--     go2 n = try "Replicate" (compile p) success (go2 (n - 1))
--     dropper ok acc t
--       | (i, rest) <- Bytes.dropWhileMax ok n t, i >= m = success acc rest
--       | otherwise = err acc t
-- compile (Many p) !_ !success = case p of
--   Byte Any -> (\acc _ -> success acc Bytes.empty)
--   Byte bp -> walker (bytePatternPred bp)
--   p -> go
--     where
--       go = compile p success success'
--       success' acc rem
--         | Bytes.size rem == 0 = success acc rem
--         | otherwise = go acc rem
--   where
--     walker ok = go
--       where
--         go acc bs = case Bytes.unconsChunk bs of
--           Nothing -> success acc bs
--           Just (bs, t) -> case V.dropWhile ok bs of
--             rem
--               | V.null rem -> go acc t
--               | otherwise ->
--                   -- moving the remainder to the root of the tree is much more efficient
--                   -- since the next uncons will be O(1) rather than O(log n)
--                   -- this can't unbalance the tree too badly since these promoted chunks
--                   -- are being consumed and will get removed by a subsequent uncons
--                   success acc (Bytes.appendUnbalanced (Bytes.chunkToBytes rem) t)
