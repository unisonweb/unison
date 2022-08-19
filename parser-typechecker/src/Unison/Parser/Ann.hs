{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Parser.Ann where

import qualified Unison.Lexer.Pos as L

data Ann
  = Intrinsic -- { sig :: String, start :: L.Pos, end :: L.Pos }
  | External
  | Ann {start :: L.Pos, end :: L.Pos}
  deriving (Eq, Ord, Show)

startingLine :: Ann -> Maybe L.Line
startingLine (Ann (L.line -> line) _) = Just line
startingLine _ = Nothing

instance Monoid Ann where
  mempty = External

instance Semigroup Ann where
  Ann s1 _ <> Ann _ e2 = Ann s1 e2
  -- If we have a concrete location from a file, use it
  External <> a = a
  a <> External = a
  Intrinsic <> a = a
  a <> Intrinsic = a
