{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module U.Codebase.Kind where

data Kind = Star | Arrow Kind Kind deriving (Eq,Ord,Read,Show)
