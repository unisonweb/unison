{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Codebase.V1.Branch.Raw where

import Data.Map (Map)
import qualified U.Util.Hash as Hash
import Unison.Codebase.V1.Star3
import Unison.Codebase.V1.Reference
import Unison.Codebase.V1.Referent
import Unison.Codebase.V1.Branch.NameSegment (NameSegment)

type MetadataType = Reference
type MetadataValue = Reference

-- `a` is generally the type of references or hashes
-- `n` is generally the the type of name associated with the references
-- `Type` is the type of metadata. Duplicate info to speed up certain queries.
-- `(Type, Value)` is the metadata value itself along with its type.
type Star r n = Star3 r n MetadataType (MetadataType, MetadataValue)

newtype EditHash = EditHash Hash.Hash
newtype BranchHash = BranchHash Hash.Hash deriving Show

-- The raw Branch
data Raw = Raw
  { terms :: Star Referent NameSegment
  , types :: Star Reference NameSegment
  , children :: Map NameSegment BranchHash
  , edits :: Map NameSegment EditHash
  }