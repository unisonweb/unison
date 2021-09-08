{-# LANGUAGE DeriveGeneric #-}

module U.Codebase.Kind where

import GHC.Generics (Generic)

data Kind = Star | Arrow Kind Kind deriving (Eq,Ord,Read,Show,Generic)
