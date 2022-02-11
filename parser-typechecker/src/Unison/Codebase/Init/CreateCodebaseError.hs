module Unison.Codebase.Init.CreateCodebaseError
  ( CreateCodebaseError (..),
    Pretty,
  )
where

import qualified Unison.Util.Pretty as P

type Pretty = P.Pretty P.ColorText

data CreateCodebaseError
  = CreateCodebaseAlreadyExists
  | CreateCodebaseOther Pretty
