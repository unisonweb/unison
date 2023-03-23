{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Parser.Ann where

import qualified Unison.Lexer.Pos as L

data Ann
  = -- Used for things like Builtins which don't have a source position.
    Intrinsic -- { sig :: String, start :: L.Pos, end :: L.Pos }
  | External
  | Ann {start :: L.Pos, end :: L.Pos}
  deriving (Eq, Ord, Show)

startingLine :: Ann -> Maybe L.Line
startingLine (Ann (L.line -> line) _) = Just line
startingLine _ = Nothing

instance Monoid Ann where
  mempty = External

instance Semigroup Ann where
  Ann s1 e1 <> Ann s2 e2 = Ann (min s1 s2) (max e1 e2)
  -- If we have a concrete location from a file, use it
  External <> a = a
  a <> External = a
  Intrinsic <> a = a
  a <> Intrinsic = a

-- | Checks whether an annotation contains a given position
-- i.e. pos ∈ [start, end)
--
-- >>> Intrinsic `contains` L.Pos 1 1
-- False
--
-- >>> External `contains` L.Pos 1 1
-- False
--
-- >>> Ann (L.Pos 0 0) (L.Pos 0 10) `contains` L.Pos 0 5
-- True
--
-- >>> Ann (L.Pos 0 0) (L.Pos 0 10) `contains` L.Pos 0 10
-- False
contains :: Ann -> L.Pos -> Bool
contains Intrinsic _ = False
contains External _ = False
contains (Ann start end) p = start <= p && p < end

-- | Checks whether an annotation contains another annotation.
--
-- i.e. pos ∈ [start, end)
--
-- >>> Intrinsic `encompasses` Ann (L.Pos 1 1) (L.Pos 2 1)
-- Nothing
--
-- >>> External `encompasses` Ann (L.Pos 1 1) (L.Pos 2 1)
-- Nothing
--
-- >>> Ann (L.Pos 0 0) (L.Pos 0 10) `encompasses` Ann (L.Pos 0 1) (L.Pos 0 5)
-- Just True
--
-- >>> Ann (L.Pos 1 0) (L.Pos 1 10) `encompasses` Ann (L.Pos 0 0) (L.Pos 2 0)
-- Just False
encompasses :: Ann -> Ann -> Maybe Bool
encompasses Intrinsic _ = Nothing
encompasses External _ = Nothing
encompasses _ Intrinsic = Nothing
encompasses _ External = Nothing
encompasses (Ann start1 end1) (Ann start2 end2) =
  Just $ start1 <= start2 && end1 >= end2
