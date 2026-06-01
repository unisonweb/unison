module Unison.TypeEntry
  ( TypeEntry (..),
  )
where

import Unison.DataDeclaration (Decl)
import Unison.TypeAlias (TypeAlias)

-- | A namespace entry pointed at by a type-position reference. Either a
-- data/effect declaration or a type alias.
data TypeEntry v a
  = TypeEntryDecl (Decl v a)
  | TypeEntryAlias (TypeAlias v a)
  deriving (Show)
