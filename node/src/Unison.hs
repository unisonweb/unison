{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import System.Environment (getArgs)
import System.IO
import Unison.Codebase (Codebase)
import Unison.Codebase.Store (Store)
import Unison.Hash.Extra ()
import Unison.Note (Noted)
import Unison.Reference (Reference)
import Unison.Runtime.Address
import Unison.Symbol (Symbol)
import Unison.Symbol.Extra ()
import Unison.Term (Term)
import Unison.Term.Extra ()
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Crypto.Random as Random
import qualified Data.ByteString.Base58 as Base58
import qualified Data.Map as Map
import qualified Data.Set as Set
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
import qualified Unison.Doc as Doc
import qualified Unison.Hash as Hash
import qualified Unison.Metadata as Metadata
import qualified Unison.Note as Note
import qualified Unison.Parser as Parser
import qualified Unison.Parsers as Parsers
import qualified Unison.Reference as Reference
import qualified Unison.Runtime.ExtraBuiltins as EB
import qualified Unison.Symbol as Symbol
import qualified Unison.Term as Term
import qualified Unison.TermParser as TermParser
import qualified Unison.TypeParser as TypeParser
import qualified Unison.Util.Logger as L
import qualified Unison.Var as Var
import qualified Unison.View as View
import qualified Unison.Views as Views

type V = Symbol View.DFO

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

readLineTrimmed :: IO String
readLineTrimmed = Text.unpack . Text.strip . Text.pack <$> getLine

viewResult :: Codebase IO V Reference (Type V) (Term V)
           -> Codebase.SearchResults V Reference (Term V)
           -> Term V
           -> Noted IO String
viewResult code _ (Term.Ref' r) = Codebase.viewAsBinding code r
viewResult code rs e = pure (Doc.formatText80 (Views.termMd (Map.fromList $ Codebase.references rs) e))

maxSearchResults = 100

search :: Codebase IO V Reference (Type V) (Term V) -> String -> IO (Codebase.SearchResults V Reference (Term V))
search code query = case Parsers.unsafeParseTerm query of
  Term.Ann' Term.Blank' t ->
    Note.run $ Codebase.search code Term.blank [] maxSearchResults (Metadata.Query "") (Just t)
  Term.Ann' (Term.Var' v) t ->
    Note.run $ Codebase.search code Term.blank [] maxSearchResults (Metadata.Query $ Var.name v) (Just t)
  Term.Var' v ->
    Note.run $ Codebase.search code Term.blank [] maxSearchResults (Metadata.Query $ Var.name v) Nothing
  _ -> fail "FAILED search syntax invalid, must be `<name>` or `<name> : <type>`"

formatSearchResults :: Codebase IO V Reference (Type V) (Term V)
                    -> Codebase.SearchResults V Reference (Term V) -> IO ()
formatSearchResults code rs = mapM_ fmt (fst . Codebase.matches $ rs) where
  fmt e = putStrLn =<< Note.run (viewResult code rs e)

pickSearchResult :: Codebase IO V Reference (Type V) (Term V)
                 -> Codebase.SearchResults V Reference (Term V) -> IO (Maybe (Term V))
pickSearchResult code rs = case fst (Codebase.matches rs) of
  [] -> pure Nothing
  [e] -> pure (Just e)
  es -> do
    putStrLn "Multiple search results, pick one to edit\n"
    let fmt (e, n) = do
          putStrLn $ show n ++ ")"
          putStrLn =<< Note.run (viewResult code rs e)
    mapM_ fmt (es `zip` [(1::Int) ..])
    putStrLn ""
    putStr "> "; hFlush stdout
    choice <- readLineTrimmed
    case choice of
      "" -> pure Nothing
      choice -> pure $ listToMaybe $ drop (read choice - 1) es

tryEdits :: [FilePath] -> IO ()
tryEdits paths = do
  editorCommand <- (Text.unpack . Text.strip . Text.pack <$> readFile ".editor") <|> pure ""
  case editorCommand of
    "" -> do putStrLn "  TIP: Create a file named .editor with the command to launch your"
             putStrLn "       editor and `uc new` and `uc edit` will invoke it on newly created files"
    _ -> forM_ paths $ \path -> let cmd = editorCommand ++ " " ++ path
                                in do putStrLn cmd; Process.callCommand cmd

refsOnly results =
  results { Codebase.matches = tweak (Codebase.matches results) }
  where
  tweak (es, rem) = ([ Term.ref r | Term.Ref' r <- es ], rem)

process :: IO (Codebase IO V Reference (Type V) (Term V)) -> [String] -> IO ()
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
  tryEdits [name ++ ".u"]
process codebase ("view" : rest) = do
  codebase <- codebase
  results <- search codebase (intercalate " " rest)
  formatSearchResults codebase (refsOnly results)
process codebase ("rename" : src : target) = do
  codebase <- codebase
  results <- search codebase src
  r <- pickSearchResult codebase (refsOnly results)
  case r of
    Just (Term.Ref' r) -> do
      [md] <- Map.elems <$> Note.run (Codebase.metadatas codebase [r])
      let suffix = "#" ++ show r
          md' = if null target then Metadata.mangle (Text.pack suffix) md
                else md { Metadata.names = Metadata.Names (map rename (Metadata.allNames (Metadata.names md))) }
          rename n | Var.name n == Text.pack src = Var.named (Text.pack $ intercalate " " target)
                   | otherwise = n
      Note.run $ Codebase.updateMetadata codebase r md'
      putStrLn $ if null target then "OK appended " ++ suffix ++ " onto name(s)"
                                else "OK"
    _ -> pure ()
process codebase ("edit" : rest) = do
  codebase <- codebase
  results <- search codebase (intercalate " " rest)
  r <- pickSearchResult codebase (refsOnly results)
  case r of
    Just (Term.Ref' r@(Reference.Derived h)) -> do
      s <- Note.run $ Codebase.viewAsBinding codebase r
      name <- randomBase58 10
      writeFile (name ++ ".u") s
      writeFile (name ++ ".parent") (Text.unpack $ Hash.base64 h)
      let mdpath = name ++ ".markdown"
      writeFile mdpath ""
      tryEdits [name ++ ".u"]
    _ -> pure ()
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
process codebase ("statistics" : []) = do
  files <- Directory.getDirectoryContents "."
  let parentFiles = map Text.unpack $ filter (".parent" `Text.isSuffixOf`) (map Text.pack files)
  refs <- mapM readFile parentFiles
  refs <- pure (map (Reference.Derived . Hash.fromBase64 . Text.pack) refs)
  codebase <- codebase
  scores <- Note.run $ Codebase.statistics codebase refs
  mds <- Note.run $ Codebase.metadatas codebase refs
  mapM_ (fmt mds) (Map.toList scores)
  where
  fmt mds (ref, score) = case Map.lookup ref mds of
    Nothing -> putStrLn $ show ref ++ "  -  "  ++ show score
    Just md -> case Metadata.firstName (Metadata.names md) of
      Just v -> putStrLn $ show v ++ "  -  " ++ show score
      Nothing -> putStrLn $ show ref ++ "  -  "  ++ show score
process codebase ("statistics" : stuff) = putStrLn "Not implemented yet"
process codebase ("add" : [name]) = go0 name where
  baseName = stripu name
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
          line <- readLineTrimmed
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
          putStrLn "  FAILED ambiguous or unresolved references in body of binding\n"
          forM_ vs $ \(v, tms) -> case tms of
            [] -> putStrLn $ "  " ++ show v ++ " could not be resolved"
            tms -> putStrLn $ "  " ++ show v ++ " could refer to any of " ++ intercalate "  " (map show tms)
          putStrLn "\n\n  Use syntax foo#8adj3 to pick a version of 'foo' by hash prefix #8adj3"
        finishedDeclaring (v, _) h = do
          putStrLn $ "  OK finished declaring " ++ show v ++ ", definition has hash:"
          putStrLn $ "     " ++ show h
    results <- Note.run $ Codebase.declareCheckAmbiguous hooks' bs codebase
    case results of
      Right declared -> do
        Directory.removeFile (baseName `mappend` ".markdown") <|> pure ()
        Directory.removeFile (baseName `mappend` ".u") <|> pure ()
        let parentFile = baseName `mappend` ".parent"
        parent <- readFile parentFile <|> pure ""
        hasParent <- (True <$ Directory.removeFile parentFile) <|> pure False
        let suffix = if hasParent then ".{u, markdown, parent}" else ".{u, markdown}"
        putStrLn $ "OK removed files " ++ baseName ++ suffix
        case hasParent of
          False -> pure ()
          True -> do
            let pr = Reference.Derived (Hash.fromBase64 (Text.pack parent))
            Just v <- Note.run $ Codebase.firstName codebase pr
            dependents <- Note.run $ Codebase.dependents codebase Nothing pr
            prevType <- Note.run $ Codebase.typeAt codebase (Term.ref pr) []
            let declared' = if length declared == 1 then declared
                            else filter (\(v',_) -> v == v') declared
                edits deps = mapM_ go [ h | Reference.Derived h <- deps ]
                go h = process (pure codebase) ["edit", Text.unpack $ Hash.base64 h ]
            when (Set.size dependents > 0) $ case declared' of
              [] -> putStrLn "OK scratch file contained no declarations"
              (v, r) : _ -> do
                updatedType <- Note.run $ Codebase.typeAt codebase (Term.ref r) []
                case updatedType == prevType of
                  False -> do
                    putStrLn "\nThis edit was not type-preserving, you can:\n"
                    putStrLn "1) Do nothing"
                    putStrLn "2) Open direct dependents for editing\n"
                    putStrLn "> "; hFlush stdout
                    line <- readLineTrimmed
                    case line of
                      "1" -> pure ()
                      "2" -> edits (Set.toList dependents)
                      _ -> pure ()
                  True -> do
                    putStrLn "\nThis edit was type-preserving, you can:\n"
                    putStrLn "1) Do nothing"
                    putStrLn "2) Open direct dependents for editing"
                    putStrLn "3) Propagate to all transitive dependents"
                    putStrLn "> "; hFlush stdin
                    line <- readLineTrimmed
                    case line of
                      "1" -> pure ()
                      "2" -> edits (Set.toList dependents)
                      _ | line == "" || line == "3" -> do
                        replaced <- Note.run $ Codebase.replace codebase pr r
                        putStrLn $ "OK updated " ++ show (Map.size replaced) ++ " definitions"
                      _ -> pure ()
      Left _ -> pure ()

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
