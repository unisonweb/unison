module Unison.Edit.Term.Eval.Interpreter where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Unison.Edit.Term.Eval
import Unison.Syntax.Term (Term)
import qualified Unison.Syntax.Term as E
import Unison.Syntax.Hash (Hash)
import qualified Unison.Syntax.Hash as H

data Primop f =
  Primop { arity :: Int, call :: [Term] -> f Term }

eval :: Monad f => Map Hash (Primop f) -> Eval f
eval env = Eval step whnf
  where
    step resolveRef e = undefined
    whnf resolveRef e = undefined
    -- case e of
    -- E.App f x | S.null (E.dependencies f) -> E.betaReduce
    -- E.link resolveRef f >>=
