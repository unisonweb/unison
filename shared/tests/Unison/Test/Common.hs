{-# LANGUAGE OverloadedStrings #-}
module Unison.Test.Common where

import Control.Applicative
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
import qualified System.FilePath as FP
import qualified Unison.Metadata as Metadata
import qualified Unison.Node as Node
import qualified Unison.Node.MemNode as MemNode
import qualified Unison.Note as Note
import qualified Unison.Term as Term
import qualified Unison.View as View
import qualified Unison.Util.Logger as L

type V = Symbol View.DFO
-- A Node for testing
type TNode = (Node IO V Reference (Type V) (Term V), Reference -> V, [(V, Term V)])

loadDeclarations :: FilePath -> Node IO V Reference (Type V) (Term V) -> IO ()
loadDeclarations path node = do
  -- note - when run from repl current directory is root, but when run via stack test, current
  -- directory is the shared subdir - so we check both locations
  txt <- Text.IO.readFile path <|> Text.IO.readFile (".." `FP.combine` path)
  let str = Text.unpack txt
  _ <- Note.run $ Node.declare' Term.ref str node
  putStrLn $ "loaded file: " ++ path

node :: IO TNode
node = do
  logger <- L.atomic (L.atInfo L.toStandardOut)
  node <- MemNode.make logger
  loadDeclarations "unison-src/base.u" node
  symbols <- liftIO . Note.run $
    Map.fromList . Node.references <$> Node.search node Term.blank [] 1000 (Metadata.Query "") Nothing
  base <- Note.run $ Node.allTermsByVarName Term.ref node
  let firstName (Metadata.Names (n:_)) = n
  let lookupSymbol ref = maybe (defaultSymbol ref) (firstName . Metadata.names) (Map.lookup ref symbols)
  pure (node, lookupSymbol, base)
