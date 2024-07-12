{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Lexer.Pos (Pos (..), Line, Column) where

type Line = Int

type Column = Int

data Pos = Pos { line :: {-# UNPACK #-} !Line, column :: {-# UNPACK #-} !Column} deriving (Show, Eq, Ord)

instance Semigroup Pos where
  Pos line col <> Pos line2 col2 =
    if line2 == 0
      then Pos line (col + col2)
      else Pos (line + line2) col2

instance Monoid Pos where
  mempty = Pos 0 0
