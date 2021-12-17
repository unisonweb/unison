{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Hashing.V2.Branch (NameSegment (..), Raw (..), MdValues (..), hashBranch) where

import Unison.Hash (Hash)
import Unison.Hashing.V2.BuildHashable (Tokenizable)
import qualified Unison.Hashing.V2.BuildHashable as H
import Unison.Hashing.V2.Reference (Reference)
import Unison.Hashing.V2.Referent (Referent)
import Unison.Prelude

type MetadataValue = Reference

newtype MdValues = MdValues (Set MetadataValue)
  deriving (Eq, Ord, Show)
  deriving (Tokenizable) via Set MetadataValue

newtype NameSegment = NameSegment Text deriving (Eq, Ord, Show)

hashBranch :: Raw -> Hash
hashBranch = H.hashTokenizable

data Raw = Raw
  { terms :: Map NameSegment (Map Referent MdValues),
    types :: Map NameSegment (Map Reference MdValues),
    patches :: Map NameSegment Hash,
    children :: Map NameSegment Hash -- the Causal Hash
  }

instance Tokenizable Raw where
  tokens b =
    [ H.accumulateToken (terms b),
      H.accumulateToken (types b),
      H.accumulateToken (children b),
      H.accumulateToken (patches b)
    ]

instance H.Tokenizable NameSegment where
  tokens (NameSegment t) = [H.Text t]
