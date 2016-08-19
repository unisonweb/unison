{-# LANGUAGE OverloadedStrings #-}
module Unison.Test.Common where

import Control.Monad.IO.Class
import Data.Foldable
import System.IO (FilePath)
import Unison.Symbol (Symbol)
import Unison.Node (Node)
import Unison.Reference (Reference)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Views (defaultSymbol)
import qualified Data.Map as Map
import qualified Data.Text.IO as Text.IO
import qualified Data.Text as Text
import qualified Unison.Metadata as Metadata
import qualified Unison.Node as Node
import qualified Unison.Node.MemNode as MemNode
import qualified Unison.Note as Note
import qualified Unison.Term as Term
import qualified Unison.View as View

type V = Symbol View.DFO
-- A Node for testing
type TNode = (Node IO V Reference (Type V) (Term V), Reference -> V, [(V, Term V)])

loadDeclarations :: FilePath -> Node IO V Reference (Type V) (Term V) -> IO ()
loadDeclarations path node = do
  txt <- Text.IO.readFile path
  let str = Text.unpack txt
  Note.run $ Node.declare' Term.ref str node

node :: IO TNode
node = do
  node <- MemNode.make
  loadDeclarations "unison-src/base.u" node
  symbols <- liftIO . Note.run $
    Map.fromList . Node.references <$> Node.search node Term.blank [] 1000 (Metadata.Query "") Nothing
  base <- Note.run $ do
    -- grab all definitions in the node
    results <- Node.search node Term.blank [] 1000000 (Metadata.Query "") Nothing
    sources <- Node.terms node (map fst $ Node.references results)
    Note.lift $ putStrLn (show sources)
    let x = [ (v, Term.ref h) | (h, md) <- Node.references results
                              , v <- toList $ Metadata.firstName (Metadata.names md) ]
    Note.lift $ putStrLn (show x)
    pure x
  let firstName (Metadata.Names (n:_)) = n
  let lookupSymbol ref = maybe (defaultSymbol ref) (firstName . Metadata.names) (Map.lookup ref symbols)
  pure (node, lookupSymbol, base)
