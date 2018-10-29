module Unison.Names where

import           Data.Map                       ( Map )
import           Data.Text                      ( Text )
import           Unison.Reference               ( Reference )
import           Unison.Term                    ( AnnotatedTerm )
import           Unison.Type                    ( AnnotatedType )

type Name = Text

data Names v a = Names
  { termNames :: Map Name (AnnotatedTerm v a, AnnotatedType v a)
  , patternNames :: Map Name (Reference, Int)
  , typeNames :: Map Name Reference
  }

