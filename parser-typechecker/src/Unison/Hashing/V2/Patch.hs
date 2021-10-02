{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Hashing.V2.Patch (Patch(..)) where

import Unison.Hashing.V2.Reference (Reference)
import Data.Map (Map)
import Unison.Hashing.V2.Referent (Referent)
import Data.Set (Set)
import Unison.Hashing.V2.TermEdit (TermEdit)
import Unison.Hashing.V2.TypeEdit (TypeEdit)
import Unison.Hashable (Hashable)
import qualified Unison.Hashable as H

data Patch = Patch
  { termEdits :: Map Referent (Set TermEdit),
    typeEdits :: Map Reference (Set TypeEdit)
  }

instance Hashable Patch where
  tokens p =
    [ H.accumulateToken (termEdits p),
      H.accumulateToken (typeEdits p)
    ]

