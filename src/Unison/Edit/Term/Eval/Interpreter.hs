module Unison.Edit.Term.Eval.Interpreter where

import Data.Map (Map)
import qualified Data.Map as M
import Unison.Syntax.Term (Term)
import qualified Unison.Syntax.Term as E
import Unison.Syntax.Hash (Hash)
import qualified Unison.Syntax.Hash as H

data Primop f =
  Primop { arity :: Int, eval :: [Term] -> f Term }

-- eval :: Map Hash (Primop f) -> Eval f
