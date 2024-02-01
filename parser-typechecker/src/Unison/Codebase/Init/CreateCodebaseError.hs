module Unison.Codebase.Init.CreateCodebaseError
  ( CreateCodebaseError (..),
    Pretty,
  )
where

import Unison.Util.Pretty qualified as P

type Pretty = P.Pretty P.ColorText

data CreateCodebaseError
  = CreateCodebaseAlreadyExists
  deriving stock (Show)
