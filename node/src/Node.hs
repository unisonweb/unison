module Main where

import qualified Data.Map as M
import qualified Unison.Note as N
import qualified Unison.Node.Server as S
import qualified Unison.Node.Common as C
import qualified Unison.Node.Store.File as F
import qualified Unison.Edit.Term.Eval.Interpreter as I

main :: IO ()
main =
  let store = F.store "store"
      eval = I.eval M.empty
      env b = N.failure ("unknown builtin: " ++ show b)
      node = C.node eval store
  in S.server 8080 node
