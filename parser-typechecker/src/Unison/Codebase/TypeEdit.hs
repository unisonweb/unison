module Unison.Codebase.TypeEdit where

import Unison.Reference (Reference)

data TypeEdit = Replace Reference | Deprecated
