{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE DeriveGeneric #-}

module Unison.Kind where

import Unison.Prelude

data Kind = Star | Arrow Kind Kind deriving (Eq,Ord,Read,Show,Generic)
