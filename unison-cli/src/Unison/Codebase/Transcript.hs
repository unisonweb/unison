{-# LANGUAGE PatternSynonyms #-}

-- | The data model for Unison transcripts.
module Unison.Codebase.Transcript
  ( ExpectingError,
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

type ExpectingError = Bool

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
  = Ucm Hidden ExpectingError [UcmLine]
  | Unison Hidden ExpectingError (Maybe ScratchFileName) Text
  | API [APIRequest]
