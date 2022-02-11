{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Lexer.Pos (Pos (..), Line, Column, line, column) where

type Line = Int
type Column = Int

data Pos = Pos {-# UNPACK #-} !Line {-# UNPACK #-} !Column deriving (Eq, Ord)

line :: Pos -> Line
line (Pos line _) = line

column :: Pos -> Column
column (Pos _ column) = column

instance Show Pos where show (Pos line col) = "line " <> show line <> ", column " <> show col

instance Semigroup Pos where (<>) = mappend

instance Monoid Pos where
  mempty = Pos 0 0
  Pos line col `mappend` Pos line2 col2 =
    if line2 == 0
      then Pos line (col + col2)
      else Pos (line + line2) col2
