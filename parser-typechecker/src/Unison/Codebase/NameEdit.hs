module Unison.Codebase.NameEdit where

import Data.Set (Set)
import Unison.Reference (Reference)

data NameEdit =
  NameEdit { added :: Set Reference, removed :: Set Reference }
