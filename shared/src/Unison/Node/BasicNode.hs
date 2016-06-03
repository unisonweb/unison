{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Unison.Node.BasicNode where

import Data.Text (Text)
import Unison.Metadata (Metadata(..))
import Unison.Node (Node)
import Unison.Node.Store (Store)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Data.Map as M
import qualified Data.Vector as Vector
import qualified Unison.Eval as Eval
import qualified Unison.Eval.Interpreter as I
import qualified Unison.Metadata as Metadata
import qualified Unison.Node as Node
import qualified Unison.Node.Builtin as B
import qualified Unison.Node.Store as Store
import qualified Unison.Note as N
import qualified Unison.Reference as R
import qualified Unison.Symbol as Symbol
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.Var as Var
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

    stub :: Metadata V R.Reference -> Type V -> N.Noted IO ()
    stub s t = () <$ Node.createTerm node (Term.blank `Term.ann` t) s

  in N.run $ do
    _ <- Node.createTerm node (Term.lam' ["a"] (Term.var' "a")) (prefix "identity")
    stub (prefix "at") $ Type.forall' ["a"] (Type.builtin "Node" --> B.v' "a" --> B.remote (B.v' "a"))
    stub (prefix "here") $ B.remote (Type.builtin "Node")
    stub (prefix "send") $ Type.forall' ["a"] (Type.builtin "Node" --> B.channel (B.v' "a") --> B.v' "a" --> B.remote' B.unitT)
    stub (prefix "channel") $ Type.forall' ["a"] (B.remote' (B.v' "a"))
    stub (prefix "map") $ Type.forall' ["a","b"] ((B.v' "a" --> B.v' "b") --> B.remote (B.v' "a") --> B.remote (B.v' "b"))
    stub (prefix "map") $ Type.forall' ["a","b"] ((B.v' "a" --> B.v' "b") --> B.remote' (B.v' "a") --> B.remote' (B.v' "b"))
    stub (prefix "bind") $ Type.forall' ["a","b"] ((B.v' "a" --> B.remote (B.v' "b")) --> B.remote (B.v' "a") --> B.remote (B.v' "b"))
    stub (prefix "bind") $ Type.forall' ["a","b"] ((B.v' "a" --> B.remote' (B.v' "b")) --> B.remote' (B.v' "a") --> B.remote' (B.v' "b"))
    mapM_ (\(B.Builtin r _ t md) -> Node.updateMetadata node r md *> Store.annotateTerm store r t)
          builtins
    pure node

prefix :: Text -> Metadata V h
prefix s = prefixes [s]

prefixes :: [Text] -> Metadata V h
prefixes s = Metadata Metadata.Term
                      (Metadata.Names (map Var.named s))
                      Nothing
