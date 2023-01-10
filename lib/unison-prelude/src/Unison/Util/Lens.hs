module Unison.Util.Lens
  ( Field1',
    Field2',
    Field3',
    Field4',
  )
where

import qualified Control.Lens as Lens

type Field1' s a = Lens.Field1 s s a a

type Field2' s a = Lens.Field2 s s a a

type Field3' s a = Lens.Field3 s s a a

type Field4' s a = Lens.Field4 s s a a
