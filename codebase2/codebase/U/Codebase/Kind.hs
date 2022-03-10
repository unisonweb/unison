module U.Codebase.Kind where

data Kind = Star | Arrow Kind Kind deriving (Eq, Ord, Read, Show)
