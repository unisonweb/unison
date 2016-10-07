{-# LANGUAGE OverloadedStrings #-}
module Unison.Test.Common where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Foldable
import Data.Text.Encoding (decodeUtf8)
import System.IO (FilePath)
import Unison.Codebase (Codebase)
import Unison.Note (Noted)
import Unison.Reference (Reference)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Views (defaultSymbol)
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.FilePath as FP
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.MemCodebase as MemCodebase
import qualified Unison.Metadata as Metadata
import qualified Unison.Note as Note
import qualified Unison.Term as Term
import qualified Unison.Util.Logger as L
import qualified Unison.View as View

type V = Symbol View.DFO
-- A codebase for testing
type TCodebase =
  ( Codebase IO V Reference (Type V) (Term V) -- the codebase
  , Reference -> V -- resolve references to symbols
  , [(V, Term V)] -- all symbol bindings
  , Term V -> Noted IO (Term V)) -- evaluator

loadDeclarations :: FilePath -> Codebase IO V Reference (Type V) (Term V) -> IO ()
loadDeclarations path codebase = do
  -- note - when run from repl current directory is root, but when run via stack test, current
  -- directory is the shared subdir - so we check both locations
  txt <- decodeUtf8 <$> (B.readFile path <|> B.readFile (".." `FP.combine` path))
  let str = Text.unpack txt
  _ <- Note.run $ Codebase.declare' Term.ref str codebase
  putStrLn $ "loaded file: " ++ path

codebase :: IO TCodebase
codebase = do
  logger <- L.atomic (L.atInfo L.toStandardOut)
  (codebase, eval) <- MemCodebase.make logger
  loadDeclarations "unison-src/base.u" codebase
  symbols <- liftIO . Note.run $
    Map.fromList . Codebase.references <$> Codebase.search codebase Term.blank [] 1000 (Metadata.Query "") Nothing
  base <- Note.run $ Codebase.allTermsByVarName Term.ref codebase
  let firstName (Metadata.Names (n:_)) = n
  let lookupSymbol ref = maybe (defaultSymbol ref) (firstName . Metadata.names) (Map.lookup ref symbols)
  pure (codebase, lookupSymbol, base, eval)
