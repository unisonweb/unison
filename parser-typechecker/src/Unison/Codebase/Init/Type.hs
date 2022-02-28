{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Init.Type (Init(..)) where

import Unison.Codebase.Init.CreateCodebaseError (CreateCodebaseError, Pretty)
import Unison.Codebase (Codebase, CodebasePath)

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

