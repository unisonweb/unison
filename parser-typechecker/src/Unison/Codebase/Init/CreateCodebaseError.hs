{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Init.CreateCodebaseError (CreateCodebaseError(..), Pretty) where

import qualified Unison.Util.Pretty as P

type Pretty = P.Pretty P.ColorText

data CreateCodebaseError
  = CreateCodebaseAlreadyExists
  | CreateCodebaseOther Pretty
