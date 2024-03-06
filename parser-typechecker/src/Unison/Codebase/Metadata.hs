module Unison.Codebase.Metadata
  ( Star,
    Value,
    insert,
    delete,
  )
where

import Unison.Reference (TermReference)
import Unison.Util.Star2 (Star2)
import Unison.Util.Star2 qualified as Star2

type Value = TermReference

-- `a` is generally the type of references or hashes
-- `n` is generally the the type of name associated with the references
-- `Value` is the metadata value itself.
type Star a n = Star2 a n Value

insert :: (Ord a, Ord n) => (a, Value) -> Star a n -> Star a n
insert (a, v) = Star2.insertD2 (a, v)

delete :: (Ord a, Ord n) => (a, Value) -> Star a n -> Star a n
delete (a, v) s = Star2.deleteD2 (a, v) s
