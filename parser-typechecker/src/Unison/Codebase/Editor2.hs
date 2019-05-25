{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
--
module Unison.Codebase.Editor2(Event(..)) where

-- import Debug.Trace

import           Data.Set (Set)
import           Data.Text                      ( Text
                                                )
import qualified Unison.Codebase.Branch2        as Branch

data Event
  = UnisonFileChanged SourceName Source
  | IncomingRootBranch (Set Branch.Hash)

type Source = Text -- "id x = x\nconst a b = a"
type SourceName = Text -- "foo.u" or "buffer 7"

-- but really "Path" not "Name"
data NameTarget = BranchName | TermName | TypeName | EditsName
  deriving (Eq, Ord, Show)

data DefnTarget = TermName' | TypeName'
  deriving (Eq, Ord, Show)
