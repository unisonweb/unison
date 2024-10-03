{-# LANGUAGE PatternSynonyms #-}

-- | The data model for Unison transcripts.
module Unison.Codebase.Transcript
  ( Result (..),
    ScratchFileName,
    Hidden (..),
    UcmLine (..),
    UcmContext (..),
    APIRequest (..),
    pattern CMarkCodeBlock,
    Stanza,
    ProcessedBlock (..),
  )
where

import CMark qualified
import Unison.Core.Project (ProjectBranchName, ProjectName)
import Unison.Prelude
import Unison.Project (ProjectAndBranch)

data Result = Success | Incorrect | Error | Failure

type ScratchFileName = Text

data Hidden = Shown | HideOutput | HideAll
  deriving (Eq, Show)

data UcmLine
  = UcmCommand UcmContext Text
  | -- | Text does not include the '--' prefix.
    UcmComment Text

-- | Where a command is run: a project branch (myproject/mybranch>).
data UcmContext
  = UcmContextProject (ProjectAndBranch ProjectName ProjectBranchName)

data APIRequest
  = GetRequest Text
  | APIComment Text

pattern CMarkCodeBlock :: (Maybe CMark.PosInfo) -> Text -> Text -> CMark.Node
pattern CMarkCodeBlock pos info body = CMark.Node pos (CMark.CODE_BLOCK info body) []

type Stanza = Either CMark.Node ProcessedBlock

data ProcessedBlock
  = Ucm Hidden Result [UcmLine]
  | Unison Hidden Result (Maybe ScratchFileName) Text
  | API [APIRequest]
