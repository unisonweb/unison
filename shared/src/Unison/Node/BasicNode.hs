{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Unison.Node.BasicNode where

import Unison.Node (Node)
import Unison.Node.Store (Store)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Data.Map as M
import qualified Unison.Eval as Eval
import qualified Unison.Eval.Interpreter as I
import qualified Unison.Node as Node
import qualified Unison.Node.Builtin as B
import qualified Unison.Node.Store as Store
import qualified Unison.Note as N
import qualified Unison.Reference as R
import qualified Unison.Type as Type
import qualified Unison.View as View

infixr 7 -->
(-->) :: Ord v => Type v -> Type v -> Type v
(-->) = Type.arrow

type DFO = View.DFO
type V = Symbol DFO

make :: (Term V -> R.Reference)
     -> Store IO V
     -> (B.WHNFEval -> [B.Builtin])
     -> IO (Node IO V R.Reference (Type V) (Term V))
make hash store getBuiltins =
  let
    builtins = getBuiltins whnf
    eval = I.eval (M.fromList [ (k,v) | (B.Builtin k (Just v) _ _) <- builtins ])
    readTerm h = Store.readTerm store h
    whnf = Eval.whnf eval readTerm
    node = Node.node eval hash store
  in N.run $ do
    mapM_ (\(B.Builtin r _ t md) -> Node.updateMetadata node r md *> Store.annotateTerm store r t)
          builtins
    pure node
