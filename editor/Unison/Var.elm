module Unison.Var where

import Unison.Parser as P

type I = Int

parse : P.Parser I
parse = P.int

