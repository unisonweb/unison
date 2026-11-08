-- | Checking the output by analyzing it the same way as the input: every
-- kept definition must have the same references, each landing where the
-- plan says its old target went. See design.md, "Checking the result".
module Minify.Verify (verify) where

import Data.Bits
import qualified Data.ByteString as B
import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Minify.Analysis
import Minify.Layout

-- | Problems found, as messages; empty when the output checks out.
verify :: Analysis -> Plan -> B.ByteString -> IO [String]
verify an pl out = case loadImage out of
  Left e -> pure ["the output does not load: " ++ e]
  Right im2 -> do
    an2 <- analyze (Options [] True) im2
    let im = anImage an
        defs = imDefs im
        byName = M.fromList [(dName d, d) | d <- V.toList (imDefs im2)]
        islandTargets = M.fromList [(a, t) | (t, a) <- M.toList (plIslands pl)]
        (poolStart, poolSize) = plIslandPool pl
        inPool a = a >= poolStart && a < poolStart + poolSize
        -- where an old target ends up: moved, unchanged, or at its stub
        resolve t = case newAddrOf an pl t of
          Just t' -> Just t'
          Nothing -> case defAt im t of
            Just d | Just st <- M.lookup (dIndex d) (plStubs pl) -> Just (maybe (sbCode st) (\c -> c + (t .&. 7)) (sbClosure st))
            _ -> Nothing
        expectedFor i =
          let d = defs V.! i
              delta = plNewStart pl VU.! i - dStart d
           in sort [(at + delta, k, t') | (at, k, t) <- VU.toList (anRefs an V.! i), Just t' <- [resolve t]]
        actualFor d2 =
          sort
            [ (at, k, M.findWithDefault t t islandTargets)
              | (at, k, t) <- VU.toList (anRefs an2 V.! dIndex d2),
                not (inPool at)
            ]
        -- Every reference the output's decoding finds must be one the plan
        -- made. The output may find fewer: a label that only a removed
        -- static closure pointed at is gone, and the code behind it with
        -- it, though the kept definition around it stays.
        problems =
          [ msg
            | i <- [0 .. V.length defs - 1],
              anKept an VU.! i,
              let d = defs V.! i,
              dKind d == Haskell,
              msg <- case M.lookup (dName d) byName of
                Nothing -> ["missing in the output: " ++ hsDisplay d]
                Just d2 ->
                  let e = S.fromList (expectedFor i)
                      a = actualFor d2
                      wrong = [r | r <- a, not (S.member r e)]
                   in [ hsDisplay d ++ ": " ++ show (length wrong) ++ " of " ++ show (length a) ++ " references are not as planned; first: " ++ showRef (head wrong)
                        | not (null wrong)
                      ]
          ]
        showRef (at, k, t) = show (toEnum (fromIntegral k) :: RefKind) ++ " at " ++ hex at ++ " -> " ++ hex t
        extra = [hsDisplay d2 | d2 <- V.toList (imDefs im2), dKind d2 == Haskell, not (M.member (dName d2) inputNames)]
        inputNames = M.fromList [(dName d, ()) | (i, d) <- zip [0 ..] (V.toList defs), anKept an VU.! i]
    pure (map ("anomaly in the output: " ++) (anAnomalies an2) ++ problems ++ map ("unexpected in the output: " ++) extra)

hex :: Word64 -> String
hex a = "0x" ++ go a
  where
    go 0 = ""
    go n = go (n `shiftR` 4) ++ [("0123456789abcdef" !!) (fromIntegral (n .&. 15))]
