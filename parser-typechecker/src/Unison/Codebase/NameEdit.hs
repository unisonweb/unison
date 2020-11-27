module Unison.Codebase.NameEdit where

import Unison.Hashable (Hashable, tokens)
import Unison.Prelude
import Unison.Reference (Reference)

data NameEdit = NameEdit {added :: Set Reference, removed :: Set Reference}

instance Semigroup NameEdit where
  NameEdit add1 del1 <> NameEdit add2 del2 = NameEdit (add1 <> add2) (del1 <> del2)

instance Hashable NameEdit where
  tokens (NameEdit added removed) = tokens (toList added, toList removed)
