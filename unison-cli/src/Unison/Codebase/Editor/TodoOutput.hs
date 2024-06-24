module Unison.Codebase.Editor.TodoOutput
  ( TodoOutput (..),
    noConflicts,
  )
where

import Unison.Names (Names)
import Unison.Prelude
import Unison.Reference (TermReference, TypeReference)
import Unison.Util.Defns (DefnsF)

data TodoOutput v a = TodoOutput
  { directDependenciesWithoutNames :: DefnsF Set TermReference TypeReference,
    nameConflicts :: Names
  }

noConflicts :: TodoOutput v a -> Bool
noConflicts todo =
  nameConflicts todo == mempty
