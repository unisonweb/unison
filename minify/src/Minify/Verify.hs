-- | Checking the output by analyzing it the same way as the input: every
-- kept definition must have the same references, each landing where the
-- plan says its old target went. See design.md, "Checking the result".
module Minify.Verify (verify) where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Minify.Analysis
import Data.Maybe (isJust)
import Minify.Format (Loaded (..), Role (..), Section (..))
import Minify.Layout

-- | Problems found, as messages; empty when the output checks out.
verify :: Analysis -> Plan -> B.ByteString -> IO [String]
verify an pl out = case loadImage out of
  Left e -> pure ["the output does not load: " ++ e]
  Right im2 -> do
    let im0 = anImage an
        planned = [(plNewStart pl VU.! i + fromIntegral (dHead d), dHead d > 0) | i <- [0 .. V.length (imDefs im0) - 1], let d = imDefs im0 V.! i, anKept an VU.! i, anRep an VU.! i == i, dKind d == Haskell, isCode (imSections im0 V.! dSect d)]
        plannedData = [plNewStart pl VU.! i | i <- [0 .. V.length (imDefs im0) - 1], let d = imDefs im0 V.! i, anKept an VU.! i, anRep an VU.! i == i, dKind d == Haskell, secRole (imSections im0 V.! dSect d) == Data]
    an2 <- analyze (Options [] True True True True True [RPointer, RAdrpAdd, RSrt, RRel32, RAbs32] Nothing True True False [] planned False [] plannedData) im2
    let im = anImage an
        defs = imDefs im
        islandTargets = M.fromList [(a, t) | (t, a) <- M.toList (plIslands pl)]
        (poolStart, poolSize) = plIslandPool pl
        inPool a = a >= poolStart && a < poolStart + poolSize
        conDesc = fromIntegral (fromEnum RConDesc) :: Word8
        -- where an old target ends up: moved, unchanged, or at its stub
        resolve k t
          | k == conDesc && not (anKeepConDesc an) = Just (plEmptyString pl)
          | otherwise = case newAddrOf an pl t of
              Just t' -> Just t'
              Nothing -> case defAt im t of
                Just d | Just st <- M.lookup (dIndex d) (plStubs pl) -> Just (maybe (sbCode st) (\c -> c + (t .&. 7)) (sbClosure st))
                _ -> Nothing
        expectedFor i =
          let d = defs V.! i
              delta = plNewStart pl VU.! i - dStart d
           in S.fromList [(at + delta, k, t') | (at, k, t, _) <- VU.toList (anRefs an V.! i), Just t' <- [resolve k t]]
        actualFor d2 =
          [ (at, k, M.findWithDefault t t islandTargets)
            | (at, k, t, _) <- VU.toList (anRefs an2 V.! dIndex d2),
              not (inPool at)
          ]
        -- Every reference the output's decoding finds must be one the plan
        -- made. The output may find fewer: a label that only a removed
        -- static closure pointed at is gone, and the code behind it with
        -- it, though the kept definition around it stays. The output may
        -- also split less finely, when the reference that marked a split
        -- point was in a removed definition, so each output definition is
        -- checked against the union of the input definitions placed in it.
        keptNew = sortOn fst [(plNewStart pl VU.! i, i) | i <- [0 .. V.length defs - 1], anKept an VU.! i, anRep an VU.! i == i, dKind (defs V.! i) == Haskell]
        keptNewV = V.fromList keptNew
        -- input definitions whose new start lies in [lo, hi)
        inputsIn lo hi =
          let n = V.length keptNewV
              first = let go a b | a >= b = a | otherwise = let mid = (a + b) `div` 2 in if fst (keptNewV V.! mid) < lo then go (mid + 1) b else go a mid in go 0 n
           in map snd (takeWhile ((< hi) . fst) (V.toList (V.drop first keptNewV)))
        problems =
          [ msg
            | d2 <- V.toList (imDefs (anImage an2)),
              dKind d2 == Haskell,
              let inputs = inputsIn (dStart d2) (dEnd d2),
              not (null inputs),
              let e = S.unions (map expectedFor inputs),
              let a = actualFor d2,
              let wrong = [r | r <- a, not (S.member r e)],
              not (null wrong),
              let msg = hsDisplay d2 ++ " (output): " ++ show (length wrong) ++ " of " ++ show (length a) ++ " references are not as planned; first: " ++ showRef (head wrong)
          ]
        missing =
          [ "missing in the output: " ++ hsDisplay d ++ " planned at " ++ hex new ++ (if anPinned an VU.! i then " (pinned)" else "")
            | (new, i) <- keptNew,
              let d = defs V.! i,
              case defAt (anImage an2) new of
                Nothing -> True
                Just _ -> False
          ]
        showRef (at, k, t) = show (toEnum (fromIntegral k) :: RefKind) ++ " at " ++ hex at ++ " -> " ++ hex t
        newStarts = S.fromList [plNewStart pl VU.! i | i <- [0 .. V.length defs - 1], anKept an VU.! i]
        extra = [hsDisplay d2 | d2 <- V.toList (imDefs (anImage an2)), dKind d2 == Haskell, not (dLocal d2), not (S.member (dStart d2) newStarts)]
        -- every pointer slot in a kept definition must still be a slot
        outSlots = S.fromList (map fst (VU.toList (imSlots (anImage an2))))
        expectedSlots = [(slot, d) | (slot, _) <- VU.toList (imSlots im), Just d <- [defAt im slot], anKept an VU.! dIndex d, anRep an VU.! dIndex d == dIndex d]
        -- when the loader listed the input's slots but not the output's (a
        -- link with --emit-relocs, whose relocation sections are not kept),
        -- the output's slots are only what closure parsing finds, and the
        -- comparison means nothing
        comparable = maybe True (const (isJust (ldSlots (imLoaded (anImage an2))))) (ldSlots (imLoaded im))
        lostRebases =
          [ "pointer slot lost at " ++ hex slot ++ " (now " ++ hex slot' ++ ") in " ++ hsDisplay d
            | comparable,
              (slot, d) <- expectedSlots,
              Just slot' <- [newAddrOf an pl slot],
              not (S.member slot' outSlots)
          ]
        -- every bind slot in a kept definition must be bound at its new place, and nothing else bound
        expectedBinds = S.fromList [(a', sym) | (a, sym) <- imBinds im, Just a' <- [newAddrOf an pl a]]
        actualBinds = S.fromList (imBinds (anImage an2))
        bindProblems =
          ["bind of " ++ BC.unpack sym ++ " expected at " ++ hex a | (a, sym) <- S.toList (S.difference expectedBinds actualBinds)]
            ++ ["bind of " ++ BC.unpack sym ++ " at " ++ hex a ++ " was not planned" | (a, sym) <- S.toList (S.difference actualBinds expectedBinds)]
    pure (map ("anomaly in the output: " ++) (anAnomalies an2) ++ take 20 lostRebases ++ take 20 bindProblems ++ missing ++ problems ++ map ("unexpected in the output: " ++) extra)

hex :: Word64 -> String
hex a = "0x" ++ go a
  where
    go 0 = ""
    go n = go (n `shiftR` 4) ++ [("0123456789abcdef" !!) (fromIntegral (n .&. 15))]
