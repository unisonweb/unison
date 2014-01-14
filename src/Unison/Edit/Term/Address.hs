module Unison.Edit.Term.Address where

import Unison.Edit.Term.Movement

data Address l = Absolute l [Movement]
               | Relative [Movement]
