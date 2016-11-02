{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.List
import System.Environment (getArgs)
import System.IO
import Unison.Codebase (Codebase)
import Unison.Codebase.Store (Store)
import Unison.Hash.Extra ()
import Unison.Reference (Reference)
import Unison.Runtime.Address
import Unison.Symbol.Extra ()
import Unison.Term (Term)
import Unison.Term.Extra ()
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Crypto.Random as Random
import qualified Data.ByteString.Base58 as Base58
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified System.Directory as Directory
import qualified System.Process as Process
import qualified Unison.ABT as ABT
import qualified Unison.BlockStore.FileBlockStore as FBS
import qualified Unison.Builtin as Builtin
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.FileStore as FileStore
import qualified Unison.Cryptography as C
import qualified Unison.Note as Note
import qualified Unison.Parser as Parser
import qualified Unison.Reference as Reference
import qualified Unison.Runtime.ExtraBuiltins as EB
import qualified Unison.Symbol as Symbol
import qualified Unison.Term as Term
import qualified Unison.TermParser as TermParser
import qualified Unison.TypeParser as TypeParser
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

randomBase58 :: Int -> IO String
randomBase58 numBytes = do
  bytes <- Random.getRandomBytes numBytes
  let base58 = Base58.encodeBase58 Base58.bitcoinAlphabet bytes
  pure (Text.unpack $ Text.decodeUtf8 base58)

view :: Codebase IO v Reference (Type v) (Term v) -> Term v -> String
view code e = "todo"

process :: (Show v, Var v) => IO (Codebase IO v Reference (Type v) (Term v)) -> [String] -> IO ()
process _ [] = putStrLn $ intercalate "\n"
  [ "usage: uc <subcommand> [<args>]"
  , ""
  , "subcommands: "
  , "  uc new"
  , "  uc add [<name>]"
  , "  uc edit <name>"
  , "  uc view <name>"
  , "  uc rename <name-src> [<name-target>]"
  , "  uc statistics [<name>]"
  , "  uc help [{new, add, edit, view, rename, statistics}]" ]
process codebase ["help"] = process codebase []
process codebase ("help" : sub) = case sub of
  ["new"] -> putStrLn "Creates a new set of scratch files (for new definitions)"
  ["add"] -> putStrLn "Add definitions in scratch files to codebase"
  ["edit"] -> putStrLn "Opens a definition for editing"
  ["view"] -> putStrLn "Views the current source of a definition"
  ["rename"] -> do
    putStrLn "Renames a definition (with args)"
    putStrLn "or appends first few characters of hash to the name (no args)"
  ["statistics"] -> do
    putStrLn "Gets statistics about a definition (with args)"
    putStrLn "or about all open edits (no args)"
  ["help"] -> putStrLn "prints this message"
  _ -> do putStrLn $ intercalate " " sub ++ " is not a subcommand"
          process codebase []
process _ ["new"] = do
  name <- randomBase58 10
  writeFile (name ++ ".u") ("-- add your definition(s) here, then do\n--  uc add " ++ name ++ ".u")
  let mdpath = name ++ ".markdown"
  writeFile mdpath ""
  putStrLn $ "Created " ++ name ++ ".{u, markdown} for code and docs"
  editorCommand <- (Text.unpack . Text.strip . Text.pack <$> readFile ".editor") <|> pure ""
  case editorCommand of
    "" -> do putStrLn "  TIP: Create a file named .editor with the command to launch your"
             putStrLn "       editor and `uc new` will invoke it on the created files"
    _ -> do
      let cmdu = editorCommand ++ " " ++ name ++ ".u"
          mdu = editorCommand ++ " " ++ name ++ ".markdown"
      putStrLn cmdu; Process.callCommand cmdu
      putStrLn mdu; Process.callCommand mdu
process codebase ("add" : []) = do
  files <- Directory.getDirectoryContents "."
  let ufiles = filter (".u" `Text.isSuffixOf`) (map Text.pack files)
  case ufiles of
    [] -> putStrLn "No .u files in current directory"
    [name] -> process codebase ("add" : [Text.unpack name])
    _ -> do
      putStrLn "Multiple .u files in current directory"
      putStr "  "
      putStrLn . Text.unpack . Text.intercalate "\n  " $ ufiles
      putStrLn "Supply one of these files as the argument to `uc add`"
process codebase ("add" : [name]) = go0 name where
  baseName = stripu name -- todo - more robust file extension stripping
  go0 name = do
    codebase <- codebase
    str <- readFile name
    hasParent <- Directory.doesFileExist (baseName `mappend` ".parent")
    bs <- case Parser.run TermParser.moduleBindings str TypeParser.s0 of
      Parser.Fail err _ -> putStrLn ("FAILED parsing " ++ name) >> mapM_ putStrLn err >> fail "parse failure"
      Parser.Succeed bs _ _ -> bs <$ putStrLn ("OK parsed " ++ name ++ ", processing declarations ... ")
    go codebase name hasParent bs
  go codebase name hasParent bs = do
    let hooks' = Codebase.Hooks startingToProcess nameShadowing duplicateDefinition renamedOldDefinition ambiguousReferences finishedDeclaring
        startingToProcess (v, _) = putStrLn (show v)
        nameShadowing [] (_, _) = do
          putStrLn "  OK name does not collide with existing definitions"
          pure Codebase.FailIfShadowed
        nameShadowing _ (_, _) | hasParent = pure Codebase.RenameOldIfShadowed
        nameShadowing tms (v, _) = do
          putStrLn $ "  WARN name collides with existing definition(s):"
          putStrLn $ "\n" ++ (unlines $ map (("    " ++) . show) tms)
          putStrLn $ unlines
            [ "  You can:", ""
            , "    1) `rename` - append first few characters of hash to old name"
            , "    2) `allow` the ambiguity - uses of this name will need to disambiguate via hash"
            , "    3) `cancel` or <Enter> - exit without making changes" ]
          putStr "  > "; hFlush stdout
          line <- Text.strip . Text.pack <$> getLine
          case line of
            _ | line == "rename" || line == "1" -> pure Codebase.RenameOldIfShadowed
              | line == "allow" || line == "2" -> pure Codebase.AllowShadowed
              | otherwise -> pure Codebase.FailIfShadowed
        duplicateDefinition t (v, _) = do
          putStrLn "  WARN definition already exists in codebase"
          putStrLn "       <Enter> - use newer provided form"
          putStrLn "       `q` - use existing form"
          putStr "  > "; hFlush stdout
          line <- Text.strip . Text.pack <$> getLine
          case line of
            "" -> pure True
            _ -> pure False
        renamedOldDefinition v v' = putStrLn $ "  OK renamed old " ++ show v ++ " to " ++ show v'
        ambiguousReferences vs v = do
          putStrLn "  FAILED ambiguous references in body of binding\n"
          forM_ vs $ \(v, tms) -> putStrLn $ "  could refer to any of " ++ intercalate "  " (map show tms)
          putStrLn "\n\n  Use syntax foo#8adj3 to pick a version of 'foo' by hash prefix #8adj3"
        finishedDeclaring (v, _) h = do
          putStrLn $ "  OK finished declaring " ++ show v ++ ", definition has hash:"
          putStrLn $ "     " ++ show h
    results <- Note.run $ Codebase.declareCheckAmbiguous hooks' bs codebase
    case results of
      [] -> do
        Directory.removeFile (baseName `mappend` ".markdown") <|> pure ()
        Directory.removeFile (baseName `mappend` ".u") <|> pure ()
        hasParent <- (True <$ Directory.removeFile (baseName `mappend` ".parent")) <|> pure False
        let suffix = if hasParent then ".{u, markdown, parent}" else ".{u, markdown}"
        putStrLn $ "OK removed files " ++ baseName ++ suffix
        -- todo - if hasParent, ask if want to update usages
      _ -> pure ()
process codebase _ = process codebase []

stripu :: String -> String
stripu s | Text.isSuffixOf ".u" (Text.pack s) = reverse . drop 2 . reverse $ s
stripu s = s

hash :: Var v => Term.Term v -> Reference
hash (Term.Ref' r) = r
hash t = Reference.Derived (ABT.hash t)

store :: IO (Store IO (Symbol.Symbol View.DFO))
store = FileStore.make "store"

makeRandomAddress :: C.Cryptography k syk sk skp s h c -> IO Address
makeRandomAddress crypt = Address <$> C.randomBytes crypt 64

main :: IO ()
main = getArgs >>= process codebase
  where
  codebase = do
    mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
    store' <- store
    logger <- L.atomic (L.atInfo L.toStandardError)
    let crypto = C.noop "dummypublickey"
    blockStore <- FBS.make' (makeRandomAddress crypto) makeAddress "blockstore"
    builtins0 <- pure $ Builtin.make logger
    builtins1 <- EB.make logger blockStore crypto
    codebase <- pure $ Codebase.make hash store'
    Codebase.addBuiltins (builtins0 ++ builtins1) store' codebase
    pure codebase
