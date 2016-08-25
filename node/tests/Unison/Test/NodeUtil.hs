{-# Language OverloadedStrings #-}

module Unison.Test.NodeUtil where

import Control.Applicative
import Unison.Hash (Hash)
import Unison.Node (Node)
import Unison.Reference (Reference)
import Unison.Runtime.Address
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.FilePath as FP
import qualified Unison.ABT as ABT
import qualified Unison.BlockStore.MemBlockStore as MBS
import qualified Unison.Cryptography as C
import qualified Unison.Hash as Hash
import qualified Unison.Node as Node
import qualified Unison.Node.BasicNode as BasicNode
import qualified Unison.Node.Builtin as Builtin
import qualified Unison.Node.FileStore as FS
import qualified Unison.Node.UnisonBlockStore as UBS
import qualified Unison.Note as Note
import qualified Unison.Parsers as Parsers
import qualified Unison.Reference as R
import qualified Unison.Reference as Reference
import qualified Unison.Runtime.ExtraBuiltins as EB
import qualified Unison.Term as Term
import qualified Unison.View as View
import qualified Unison.Util.Logger as L

type DFO = View.DFO
type V = Symbol DFO
type TermV = Term V
type TestNode = Node IO V R.Reference (Type V) (Term V)

hash :: Var v => Term.Term v -> Reference
hash (Term.Ref' r) = r
hash t = Reference.Derived (ABT.hash t)

makeRandomAddress :: C.Cryptography k syk sk skp s h c -> IO Address
makeRandomAddress crypt = Address <$> C.randomBytes crypt 64

loadDeclarations :: L.Logger -> FilePath -> Node IO V Reference (Type V) (Term V) -> IO ()
loadDeclarations logger path node = do
  -- note - when run from repl current directory is root, but when run via stack test, current
  -- directory is the shared subdir - so we check both locations
  txt <- Text.IO.readFile path <|> Text.IO.readFile (".." `FP.combine` path)
  let str = Text.unpack txt
  _ <- Note.run $ Node.declare' Term.ref str node
  L.info logger $ "loaded file: " ++ path

makeTestNode :: IO (TestNode, String -> Term V)
makeTestNode = do
  logger <- L.atomic (L.atInfo L.toStandardOut)
  let crypto = C.noop "dummypublickey"
  putStrLn "creating block store..."
  blockStore <- MBS.make' (makeRandomAddress crypto) makeAddress
  putStrLn "created block store, creating Node store..."
  store' <- UBS.make blockStore
  -- store' <- FS.make "blockstore.file"
  putStrLn "created Node store..., building extra builtins"
  extraBuiltins <- EB.make logger blockStore crypto
  putStrLn "extra builtins created"
  let makeBuiltins whnf = concat [Builtin.makeBuiltins logger whnf, extraBuiltins whnf]
  node <- BasicNode.make hash store' makeBuiltins
  L.info logger "Node created"
  loadDeclarations logger "unison-src/base.u" node
  loadDeclarations logger "unison-src/extra.u" node
  builtins <- Note.run $ Node.allTermsByVarName Term.ref node
  let parse = Parsers.bindBuiltins builtins [] . Parsers.unsafeParseTerm
  pure (node, parse)
