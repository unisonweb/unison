{-# LANGUAGE OverloadedStrings #-}
module Unison.Test.Common where

import Control.Monad.IO.Class
import Unison.Symbol (Symbol)
import Unison.Node (Node)
import Unison.Reference (Reference)
import Unison.Term (Term)
import Unison.Type (defaultSymbol,Type)
import qualified Data.Map as Map
import qualified Unison.Metadata as Metadata
import qualified Unison.Node as Node
import qualified Unison.Node.MemNode as MemNode
import qualified Unison.Note as Note
import qualified Unison.Term as Term
import qualified Unison.View as View

type V = Symbol View.DFO
-- A Node for testing
type TNode = (Node IO V Reference (Type V) (Term V), Reference -> V)

node :: IO TNode
node = do
  node <- MemNode.make
  symbols <- liftIO . Note.run $
    Map.fromList . Node.references <$> Node.search node Term.blank [] 1000 (Metadata.Query "") Nothing
  let firstName (Metadata.Names (n:_)) = n
  let lookupSymbol ref = maybe (defaultSymbol ref) (firstName . Metadata.names) (Map.lookup ref symbols)
  pure (node, lookupSymbol)
