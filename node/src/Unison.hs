{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import System.Environment (getArgs)
import System.IO
import Unison.Codebase (Codebase)
import Unison.Codebase.Store (Store)
import Unison.Hash.Extra ()
import Unison.Reference (Reference)
import Unison.Runtime.Address
import Unison.Symbol.Extra ()
import Unison.Term.Extra ()
import Unison.Var (Var)
import qualified System.IO.Temp as Temp
import qualified Unison.ABT as ABT
import qualified Unison.BlockStore.FileBlockStore as FBS
import qualified Unison.Builtin as Builtin
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.FileStore as FileStore
import qualified Unison.Cryptography as C
import qualified Unison.Reference as Reference
import qualified Unison.Runtime.ExtraBuiltins as EB
import qualified Unison.Symbol as Symbol
import qualified Unison.Term as Term
import qualified Unison.Util.Logger as L
import qualified Unison.View as View

{-
uc new
uc add [<name>]
uc edit name
uc view name
uc rename src target
uc statistics [name]
uc help
uc eval [name]
-}

process :: Codebase m v h t e -> [String] -> IO ()
process _ [] = putStrLn $ intercalate "\n"
  [ "usage: uc <subcommand> [<args>*]"
  , ""
  , "subcommands: "
  , "  uc new"
  , "  uc add [<name>]"
  , "  uc edit <name>"
  , "  uc view <name>"
  , "  uc rename <name-src> <name-target>"
  , "  uc statistics [<name>]"
  , "  uc help [{new, add, edit, view, rename, statistics}]" ]
process codebase ["help"] = process codebase []
process codebase ("help" : sub) = case sub of
  ["new"] -> putStrLn "Creates a new set of scratch files (for new definitions)"
  ["add"] -> putStrLn "Typechecks definitions in scratch files"
  ["edit"] -> putStrLn "Opens a definition for editing"
  ["view"] -> putStrLn "Views the current source of a definition"
  ["rename"] -> putStrLn "Renames a definition"
  ["statistics"] -> do
    putStrLn "Gets statistics about a definition (with args)"
    putStrLn "or about all open edits (no args)"
  ["help"] -> putStrLn "prints this message"
  _ -> do putStrLn $ intercalate " " sub ++ " is not a subcommand"
          process codebase []
process _ ["new"] = do
  (path, handle) <- Temp.openTempFile "." ".u"
  let mdpath = stripu path ++ ".markdown"
  writeFile mdpath ""
  putStrLn $ "Created " ++ stripu path ++ ".{u, markdown} for code + docs"
  hPutStrLn handle "-- add your definition(s) here"
  hClose handle
process codebase ["add"] = error "todo"
process codebase _ = process codebase []

stripu :: [a] -> [a]
stripu = reverse . drop 2 . reverse

hash :: Var v => Term.Term v -> Reference
hash (Term.Ref' r) = r
hash t = Reference.Derived (ABT.hash t)

store :: IO (Store IO (Symbol.Symbol View.DFO))
store = FileStore.make "store"

makeRandomAddress :: C.Cryptography k syk sk skp s h c -> IO Address
makeRandomAddress crypt = Address <$> C.randomBytes crypt 64

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
  store' <- store
  logger <- L.atomic (L.atInfo L.toStandardError)
  let crypto = C.noop "dummypublickey"
  blockStore <- FBS.make' (makeRandomAddress crypto) makeAddress "blockstore"
  builtins0 <- pure $ Builtin.make logger
  builtins1 <- EB.make logger blockStore crypto
  codebase <- pure $ Codebase.make hash store'
  Codebase.addBuiltins (builtins0 ++ builtins1) store' codebase
  getArgs >>= process codebase
