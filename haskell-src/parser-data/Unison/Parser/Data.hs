{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Unison.Parser.Data where

type Line = Int
type Column = Int

data Pos = Pos {-# Unpack #-} !Line {-# Unpack #-} !Column deriving (Eq,Ord,Show)

instance Semigroup Pos where (<>) = mappend

instance Monoid Pos where
  mempty = Pos 0 0
  Pos line col `mappend` Pos line2 col2 =
    if line2 == 0 then Pos line (col + col2)
    else Pos (line + line2) col2

line :: Pos -> Line
line (Pos line _) = line

column :: Pos -> Column
column (Pos _ column) = column
