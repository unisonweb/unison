module Unison.View where

import Array (Array)
import Elmz.Distance (Distance)
import Text (Style)
import Unison.Var (I)

data View e
  = Var I
  | Source Distance
  | Reactive (View e)
  | Fn Int (View e)
  | Horizontal (Array e) -- I think this should just be Horizontal nullary, get rid of `e` param
  | Vertical (Array e)
  | Text Style
