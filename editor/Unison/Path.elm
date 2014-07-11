module Unison.Path where

import Unison.Parser as P

data E
  = Fn -- ^ Points at function in a function application
  | Arg -- ^ Points at the argument of a function application
  | Body -- ^ Points at the body of a lambda

type Path = [E]

-- toJson : Jsonify Path
-- toJson j =
