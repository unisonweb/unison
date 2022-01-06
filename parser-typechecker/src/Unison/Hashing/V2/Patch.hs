{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Hashing.V2.Patch (Patch (..), hashPatch) where

import Data.Map (Map)
import Data.Set (Set)
import Unison.Hash (Hash)
import Unison.Hashing.V2.BuildHashable (Hashable)
import qualified Unison.Hashing.V2.BuildHashable as H
import Unison.Hashing.V2.Reference (Reference)
import Unison.Hashing.V2.Referent (Referent)
import Unison.Hashing.V2.TermEdit (TermEdit)
import Unison.Hashing.V2.TypeEdit (TypeEdit)

hashPatch :: Patch -> Hash
hashPatch = H.accumulate'

data Patch = Patch
  { termEdits :: Map Referent (Set TermEdit),
    typeEdits :: Map Reference (Set TypeEdit)
  }

instance Hashable Patch where
  tokens p =
    [ H.accumulateToken (termEdits p),
      H.accumulateToken (typeEdits p)
    ]
