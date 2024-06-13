module Unison.Codebase.Editor.TodoOutput
  ( TodoOutput (..),
    noConflicts,
  )
where

import Unison.Names (Names)

data TodoOutput v a = TodoOutput
  { nameConflicts :: Names
  }

noConflicts :: TodoOutput v a -> Bool
noConflicts todo =
  nameConflicts todo == mempty
