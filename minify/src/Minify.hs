-- Optimized in every build, --fast too: minifying is slow without it
{-# OPTIONS_GHC -O2 #-}
-- | Minifying an executable as the command line does by default
-- (README.md), with the symbol table dropped and the result verified, for
-- programs that minify as a step of their own work (ucm's `bundle`).
module Minify
  ( minifyExecutable,
  )
where

import qualified Data.ByteString as B
import Data.Maybe (isNothing)
import Minify.Analysis
import Minify.Format
import Minify.Layout
import Minify.Rewrite
import Minify.Verify

-- | The minified executable, or why there is none. The result of a Mach-O
-- input has to be signed again (codesign) before macOS runs it.
minifyExecutable :: B.ByteString -> IO (Either String B.ByteString)
minifyExecutable bytes = case loadImage bytes of
  Left err -> pure (Left err)
  Right im -> do
    an <- analyze options im
    if
      | not (null (anAnomalies an)) ->
          pure (Left ("the analysis found code it did not understand: " ++ unlines (take 5 (anAnomalies an))))
      | isNothing (ldSlots (imLoaded im)) ->
          pure (Left "the executable has no relocation list (on Linux, link with -optl-fuse-ld=bfd -optl-Wl,--emit-relocs)")
      | otherwise -> case plan an 200 True [Code, Data, Strings, Const] (Just (0, maxBound)) False Nothing of
          Left err -> pure (Left err)
          Right pl -> do
            (out, _) <- rewrite an pl (Choices False True False)
            problems <- verify an pl out
            pure $
              if null problems
                then Right out
                else Left ("the minified executable failed verification: " ++ unlines (take 5 problems))
  where
    options =
      Options
        { optRemovePrefixes = [],
          optKeepDead = False,
          optKeepConDesc = False,
          optSplitText = True,
          optSplitData = True,
          optSplitStrings = True,
          optDataSplitKinds = [RPointer, RAdrpAdd, RSrt, RRel32, RAbs32],
          optSplitRange = Nothing,
          optPatchFixedPointers = True,
          optPatchFixedSrt = True,
          optFold = True,
          optFoldRange = [],
          optExtraLabels = [],
          optGotRoots = False,
          optKeepRanges = [],
          optExtraData = [],
          optPinRanges = []
        }
