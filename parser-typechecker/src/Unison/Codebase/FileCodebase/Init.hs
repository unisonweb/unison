{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.FileCodebase.Init (Init(..), CreateCodebaseError(..), Pretty) where

import Unison.Codebase.FileCodebase.Codebase (Codebase)
import Unison.CodebasePath (CodebasePath)
import qualified Unison.Util.Pretty as P

type Pretty = P.Pretty P.ColorText

data CreateCodebaseError
  = CreateCodebaseAlreadyExists
  | CreateCodebaseOther Pretty

type DebugName = String

data Init m v a = Init
  { -- | open an existing codebase
    openCodebase :: DebugName -> CodebasePath -> m (Either Pretty (m (), Codebase m v a)),
    -- | create a new codebase
    createCodebase' :: DebugName -> CodebasePath -> m (Either CreateCodebaseError (m (), Codebase m v a)),
    -- | given a codebase root, and given that the codebase root may have other junk in it,
    -- give the path to the "actual" files; e.g. what a forked transcript should clone.
    codebasePath :: CodebasePath -> CodebasePath
  }

