{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Parser.Ann where

import qualified Unison.Lexer.Pos as L

data Ann
  = Intrinsic -- { sig :: String, start :: L.Pos, end :: L.Pos }
  | External
  | -- Indicates that the construct was generated from something at the given location.
    -- E.g. record constructors are generated from their field definition.
    GeneratedFrom Ann
  | Ann {start :: L.Pos, end :: L.Pos}
  deriving (Eq, Ord, Show)

startingLine :: Ann -> Maybe L.Line
startingLine (Ann (L.line -> line) _) = Just line
startingLine (GeneratedFrom a) = startingLine a
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
  GeneratedFrom a <> b = a <> b
  a <> GeneratedFrom b = a <> b
