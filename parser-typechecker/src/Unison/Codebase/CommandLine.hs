{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase.CommandLine (main) where

import Data.Bifunctor (second)
import System.Random (randomRIO)
import           Control.Concurrent           (forkIO)
import           Control.Exception            (catch, finally)
import           Control.Monad                (forM_, forever, liftM2,
                                               void, when)
import           Control.Monad.STM            (STM, atomically)
import qualified Data.Char                    as Char
import           Data.Foldable                (toList, traverse_)
import           Data.IORef                   (IORef, newIORef, writeIORef, readIORef)
import           Data.List                    (find, isSuffixOf, isPrefixOf,
                                               sort)
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.String                  (fromString)
import           Data.Strings                 (strPadLeft)
import           Data.Text                    (Text, pack, unpack)
import qualified Data.Text.IO
import qualified System.Console.ANSI          as Console
import           System.FilePath              (FilePath)
import qualified Text.Read                    as Read
import qualified Unison.Reference             as Reference
import           System.IO.Error              (isEOFError)
import qualified Unison.Builtin               as B
import           Unison.Codebase              (Codebase)
import qualified Unison.Codebase              as Codebase
import           Unison.Codebase.Branch       (Branch)
import qualified Unison.Codebase.Branch       as Branch
import           Unison.Names                 (Name)
import           Unison.Codebase.Runtime      (Runtime)
import qualified Unison.Codebase.Runtime      as RT
import qualified Unison.Codebase.Watch        as Watch
import           Unison.FileParsers           (parseAndSynthesizeFile)
import qualified Unison.Parser                as Parser
import qualified Unison.PrintError            as PrintError
import           Unison.PrintError            (prettyParseError,
                                               prettyTypecheckedFile,
                                               renderNoteAsANSI)
import           Unison.Result                (pattern Result)
import qualified Unison.Result                as Result
import qualified Unison.UnisonFile            as UF
import qualified Unison.Util.ColorText        as Color
import qualified Unison.Util.Menu             as Menu
import           Unison.Util.Monoid
import qualified Unison.Util.PrettyPrint      as PP
import           Unison.Util.TQueue           (TQueue)
import qualified Unison.Util.TQueue           as TQueue
import           Unison.Var                   (Var)
import qualified Unison.Var as Var
import qualified Data.Map as Map
import Unison.Parser (Ann)
import qualified Data.Text as Text
import Unison.Names (Names)
import qualified Unison.Term as Term

data Event
  = UnisonFileChanged FilePath Text
  | UnisonBranchChanged (Set Name)
  | EOF

allow :: FilePath -> Bool
allow = liftM2 (||) (".u" `isSuffixOf`) (".uu" `isSuffixOf`)

data CreateCancel = Create | Cancel deriving Show

main
  :: forall v
   . Var v
  => FilePath
  -> Name
  -> Maybe FilePath
  -> IO (Runtime v)
  -> Codebase IO v Ann
  -> IO ()
main dir currentBranchName initialFile startRuntime codebase = do
  queue           <- TQueue.newIO
  lineQueue       <- TQueue.newIO
  runtime         <- startRuntime
  lastTypechecked <- newIORef
    (Nothing, UF.TypecheckedUnisonFile Map.empty Map.empty [], mempty)
  let takeActualLine = atomically (takeLine lineQueue)

  -- load initial unison file if specified
  case initialFile of
    Just file | allow file -> do
      text <- Data.Text.IO.readFile file
      atomically . TQueue.enqueue queue $ UnisonFileChanged file text
    _ -> pure ()

  -- enqueue stdin into lineQueue
  void
    .   forkIO
    .   (`catch` eofHandler queue)
    .   forever
    $   getChar
    >>= atomically
    .   TQueue.enqueue lineQueue

  -- watch for .u file changes
  void . forkIO $ do
    watcher <- Watch.watchDirectory dir allow
    forever $ do
      (filePath, text) <- watcher
      atomically . TQueue.enqueue queue $ UnisonFileChanged filePath text

  -- watch for external branch changes
  (cancelExternalBranchUpdates, externalBranchUpdates) <- Codebase.branchUpdates
    codebase
  void . forkIO . forever $ do
    updatedBranches <- externalBranchUpdates
    atomically . TQueue.enqueue queue . UnisonBranchChanged $ updatedBranches

  -- load current branch from disk
  branch <- Codebase.getBranch codebase currentBranchName
  (`finally` (RT.terminate runtime *> cancelExternalBranchUpdates))
    $ case branch of
        Nothing -> do
          selectBranch codebase currentBranchName takeActualLine >>= \case
            Just (name, branch) ->
              go0 branch name queue lineQueue lastTypechecked runtime
            Nothing -> putStrLn "Exiting."
        Just b ->
          go0 b currentBranchName queue lineQueue lastTypechecked runtime
 where
  eofHandler queue e =
    if isEOFError e then (atomically . TQueue.enqueue queue) EOF else ioError e
  go0
    :: Branch
    -> Name
    -> TQueue Event
    -> TQueue Char
    -> IORef
         ( Maybe FilePath
         , UF.TypecheckedUnisonFile v Parser.Ann
         , PrintError.Env
         )
    -> Runtime v
    -> IO ()
  go0 branch branchName queue lineQueue lastTypechecked runtime = go
    branch
    branchName
   where
    clearLastTypechecked =
      writeIORef lastTypechecked (Nothing, UF.typecheckedUnisonFile0, mempty)
    -- print prompt and whatever input was on it / at it
    printPrompt :: Name -> IO ()
    printPrompt branchName = do
      incompleteLine <- atomically . peekIncompleteLine $ lineQueue
      putStr $ "\r" ++ unpack branchName ++ "> " ++ incompleteLine

    handleUnisonFile :: Runtime v -> Names v Ann -> FilePath -> Text -> IO ()
    handleUnisonFile runtime names filePath src = do
      let Result notes r = parseAndSynthesizeFile names filePath src
      case r of
        Nothing -> do -- parsing failed
          Console.setTitle "Unison \128721"
          forM_ notes $ \case
            Result.Parsing err -> do
              print . Color.renderText $ prettyParseError (unpack src) err
              clearLastTypechecked
            err ->
              error
                $  "I was expecting a parsing error here but got:\n"
                ++ show err

        Just (errorEnv, r) -> case r of
          Nothing -> do -- typechecking failed
            Console.setTitle "Unison \128721"
            let showNote notes = intercalateMap
                  "\n\n"
                  (show . renderNoteAsANSI errorEnv (unpack src))
                  (filter notInfo notes)
                notInfo (Result.TypeInfo _) = False
                notInfo _                   = True
            putStrLn . showNote . toList $ notes
            clearLastTypechecked
          Just unisonFile -> do
            Console.setTitle "Unison ✅"
            let emoticons = "🌸🌺🌹🌻🌼🌷🌵🌴🍄🌲"
            n <- randomRIO (0, length emoticons - 1)
            putStrLn
              $  "✅ "
              ++ [emoticons !! n]
              ++ "  Found and typechecked the following definitions in " ++ filePath ++ ":\n"
            let uf = UF.discardTerm unisonFile
            writeIORef lastTypechecked (Just filePath, uf, errorEnv)
            putStrLn . show . Color.renderText $ prettyTypecheckedFile
              uf
              errorEnv
            putStrLn ""
            putStrLn
              "👀  Now evaluating any watch expressions (lines starting with `>`) ...\n"
            (watchExpressions, _term) <-
                    RT.evaluate runtime (UF.discardTypes' unisonFile) codebase
            uncurry (Watch.watchPrinter names) `traverse_` watchExpressions

    go :: Branch -> Name -> IO ()
    go branch name = do
      printPrompt name

      -- wait for new lines from user or asynchronous events from filesystem
      TQueue.raceIO (TQueue.peek queue) (awaitCompleteLine lineQueue) >>= \case
        Right _ -> processLine branch name
        Left  _ -> atomically (TQueue.dequeue queue) >>= \case
          EOF                             -> putStrLn "^D"
          UnisonFileChanged filePath text -> do
            Console.setTitle "Unison"
            Console.clearScreen
            Console.setCursorPosition 0 0
            names <- Codebase.branchToNames codebase branch
            handleUnisonFile runtime (names <> B.names) filePath text
            go branch name
          UnisonBranchChanged branches -> if Set.member name branches
            then do
              b' <- Codebase.getBranch codebase name
              case b' of
                Just b' -> do
                  when (branch /= b') $ do
                    putStrLn "I've merged some external changes to the branch."
                    putStrLn
                      $ "TODO: tell the user what changed as a result of the merge"
                  go b' name
                Nothing -> do
                  putStrLn
                    $ "The current branch was deleted by some external process, "
                    ++ "so I'm going to re-save what I have in memory."
                  branch' <- mergeBranchAndShowDiff codebase name branch
                  go branch' name
            else go branch name

    -- Looks at `lastTypechecked` for matching definitions and lets the user
    -- add them to the codebase. Present the user with a menu if args doesn't
    -- match what's in lastTypechecked.
    addDefinitions :: Branch -> Name -> [String] -> IO ()
    addDefinitions branch name args = case args of
      _ -> readIORef lastTypechecked >>= \(filePath, typecheckedFile, env) ->
        case filePath of
          Nothing -> do
            putStrLn
              $  "Nothing to do. Editing a .u file in "
              <> dir
              <> " will tell me about new definitions."
            go branch name
          Just _ -> do
            let branchUpdate = Branch.typecheckedFile typecheckedFile
                collisions   = Branch.nameCollisions branchUpdate branch
                -- todo: collisions should really be collisions `Branch.subtract` branch,
                -- since if the names have a matching hash that's fine
            if collisions /= mempty
              then do
                putStrLn
                  $ "The following names collided with existing definitions:\n"
                putStrLn $ intercalateMap
                  " "
                  Text.unpack
                  (toList $ Branch.allNames collisions)
                putStrLn
                  "\nUse the `> edit` command to have these definitions replace the existing ones."
                go branch name
              else do
              -- todo: this should probably just be a function in Codebase, something like
              --       addFile :: Codebase -> TypecheckedUnisonFile -> m ()
                let hashedTerms = UF.hashTerms typecheckedFile
                putStrLn $ "Adding the following definitions:"
                putStrLn ""
                putStrLn . show $ Color.renderText
                  (prettyTypecheckedFile typecheckedFile env)
                putStrLn ""
                let
                  allTypeDecls =
                    (second Left <$> UF.effectDeclarations' typecheckedFile)
                      `Map.union` (   second Right
                                  <$> UF.dataDeclarations' typecheckedFile
                                  )

                forM_ (Map.toList allTypeDecls)
                  $ \(v, (r@(Reference.DerivedId id), dd)) -> do
                      decl <- Codebase.getTypeDeclaration codebase id
                      case decl of
                        Nothing -> do
                          Codebase.putTypeDeclaration codebase id dd
                        Just _ ->
                          -- todo - can treat this as adding an alias (same hash, but different name in this branch)
                          putStrLn
                            $  Var.nameStr v
                            ++ " already exists with hash "
                            ++ show r
                            ++ ", skipping."
                forM_ (Map.toList hashedTerms)
                  $ \(v, (r@(Reference.DerivedId id), tm, typ)) -> do
                      o <- Codebase.getTerm codebase id
                      case o of
                        Just _ ->
                          -- todo - can treat this as adding an alias (same hash, but different name in this branch)
                          putStrLn
                            $  Var.nameStr v
                            ++ " already exists with hash "
                            ++ show r
                            ++ ", skipping."
                        Nothing ->
                          -- Discard all line/column info when adding to the codebase
                                   Codebase.putTerm
                          codebase
                          id
                          (Term.amap (const Parser.External) tm)
                          typ

                branch <- mergeBranchAndShowDiff
                  codebase
                  name
                  (Branch.append branchUpdate branch)

                let emoticons = "🌉🏙🌃🌁🌅🎆🌄🌠🌇"
                n <- randomRIO (0, length emoticons - 1)
                putStrLn
                  $ (emoticons !! n)
                  : "  All done. You can view or edit any definition via `> view <name>`."
                putStrLn ""
                clearLastTypechecked
                go branch name

    viewDefinitions :: Branch -> Name -> [String] -> IO ()
    viewDefinitions branch name args = do
      prettys <- traverse (\q -> Codebase.prettyBindingsQ codebase q branch)
                          args
      putStrLn . PP.render 80 $ PP.linesSpaced prettys
      go branch name

    processLine :: Branch -> Name -> IO ()
    processLine branch name = do
      let takeActualLine = atomically $ takeLine lineQueue
      line <- takeActualLine
      case words line of
        "add"  : args -> addDefinitions branch name args
        "view" : args -> viewDefinitions branch name args
        ls : args
          | ls
            == "list"
            || -- todo: more comprehensive way of allowing command abbreviations
               ls
            == "ls"
            || ls
            == "l"
          -> let
               query    = intercalateMap " " id args
               allNames = Branch.allNames (Branch.head branch)
               filtered = filter (query `isPrefixOf`)
                                 (Text.unpack <$> Set.toList allNames)
-- todo: show types of each
             in
               do
                 putStrLn $ intercalateMap "\n" id filtered
                 go branch name

        ["branch"] -> do
          branches <- sort <$> Codebase.branches codebase
          forM_ branches $ \name' -> if name' == name
            then putStrLn $ " * " ++ unpack name'
            else putStrLn $ "   " ++ unpack name'
          -- idea: could instead prompt user and read directly from lineQueue to handle
          go branch name

        ["branch", name'] -> do
          branch' <- selectBranch codebase (pack name') takeActualLine
          case branch' of
            Just (name, branch) -> go branch name
            Nothing             -> putStrLn "Ok, nevermind." *> go branch name

        ["fork", newName0] -> do
          let newName = pack newName0
          branchExists <- Codebase.branchExists codebase newName
          if branchExists
            then do
              putStrLn $ "Sorry, a branch by that name already exists."
              go branch name
            else do
              branch' <- mergeBranchAndShowDiff codebase newName branch
              go branch' newName

        ["merge", from] -> do
          branch' <- Codebase.getBranch codebase $ pack from
          case branch' of
            Nothing -> do
              putStrLn
                $ "Sorry, I can't find a branch by that name to merge from."
              go branch name
            Just branch' -> do
              branch'' <- mergeBranchAndShowDiff codebase name branch'
              putStrLn $ "Flawless victory!"
              go branch'' name

        -- rename a term/type/... in the current branch
        ["rename", from, to]
          -> let
               terms = Branch.termsNamed (pack from) branch
               types = Branch.typesNamed (pack from) branch
               renameTerm branch = do
                 let branch' = Branch.renameTerm (pack from) (pack to) branch
                 mergeBranchAndShowDiff codebase name branch'
               renameType branch = do
                 let branch' = Branch.renameType (pack from) (pack to) branch
                 mergeBranchAndShowDiff codebase name branch'
               go' b = go b name
             in
               case (toList terms, toList types) of
                 ([], []) -> putStrLn "I couldn't find anything by that name."
                 ([_term], []    ) -> renameTerm branch >>= go'
                 ([]     , [_typ]) -> renameType branch >>= go'
                 ([_term], [_typ]) -> do
                   putStrLn
                     "Do you want to rename the [term], [type], [both], or [neither]?"
                   putStr ">> "
                   (atomically . fmap words . takeLine) lineQueue >>= \case
                     ["term"] -> renameTerm branch >>= go'
                     ["type"] -> renameType branch >>= go'
                     ["both"] -> renameTerm branch >>= renameType >>= go'
                     _        -> go' branch
                 (_terms, _types) -> do
                   -- idea: print out _terms and _types, so user can view them
                   putStrLn
                     $  "There's more than one thing called "
                     ++ from
                     ++ "."
                   putStrLn
                     $  "Use `> <command to resolve conflicts> unname "
                     ++ from
                     ++ "` to resolve conflicts, then try again."
                   go' branch
        [] -> go branch name
        x ->
          putStrLn ("I don't know how to " ++ unwords x ++ ".")
            *> go branch name
-- should never block
peekIncompleteLine :: TQueue Char -> STM String
peekIncompleteLine q = TQueue.tryPeekWhile (/= '\n') q

-- block until a full line is available
takeLine :: TQueue Char -> STM String
takeLine q = do
  line <- TQueue.takeWhile (/= '\n') q
  ch <- TQueue.dequeue q
  if (ch /= '\n') then error "unpossibility in takeLine" else pure line

-- blocks until a line ending in '\n' is available
awaitCompleteLine :: TQueue Char -> STM ()
awaitCompleteLine ch = void $ TQueue.peekWhile (/= '\n') ch

-- let the user pick from a list of labeled `a`s
-- todo: rewrite this to let them toggle stuff
_multipleChoice :: [(String, a)] -> TQueue Char -> IO [a]
_multipleChoice as lineQueue = do
  let render ((s, _), index) = putStrLn $ strPadLeft ' ' 5 ("[" ++ show index ++ "] ") ++ s
  traverse_ render (as `zip` [(1::Int)..])
  putStrLn "Please enter your selection as a space separated list of numbers."
  putStr ">> "
  numbers <- (atomically . fmap words . takeLine) lineQueue
  case traverse Read.readMaybe numbers of
    Nothing ->
      putStrLn "Sorry, I couldn't understand at least one of those numbers."
      >> _multipleChoice as lineQueue
    Just numbers -> case find (\i -> i < 1 || i > length as) numbers of
      Just i ->
        (putStrLn $ "You entered the number " ++ show i ++ " which wasn't one of the choices.")
          >> _multipleChoice as lineQueue
      Nothing -> pure $ snd . (as !!) . (+ (-1)) <$> numbers

-- Merges `branch` into any the branch `name`, creating it if necessary.
mergeBranchAndShowDiff :: Monad m => Codebase m v a -> Name -> Branch -> m Branch
mergeBranchAndShowDiff codebase targetName sourceBranch = do
  branch' <- Codebase.mergeBranch codebase targetName sourceBranch
  -- when (branch' /= branch) $
  --   putStrLn $ "Some extra stuff appeared right when you forked, "
  --           ++ "and I went ahead and smashed it all together for you!"
  pure branch'

foo :: Text -> (String, Text)
foo name = (unpack name, name)

selectBranch :: Codebase IO v a -> Name -> IO String -> IO (Maybe (Name, Branch))
selectBranch codebase name takeLine = do
  let branchMenu caption branches =
        Menu.menu1
          takeLine -- console
          caption -- caption
          (fromString . unpack) -- render
          (fromString . fmap Char.toLower . show) -- renderMeta
          (foo <$> branches) -- groups
          [("create", Create), ("cancel", Cancel)] -- metas
          Nothing -- initial

  branch <- Codebase.getBranch codebase name
  case branch of
    -- if branch named `name` exists, load it,
    Just branch -> pure . Just $ (name, branch)
    -- otherwise,
      -- list branches that do exist, plus option to create, plus option to cancel
    Nothing -> do
      let caption = fromString $
            "The branch " ++ show name ++ " doesn't exist. " ++
             "Do you want to create it, or pick a different one?"
      branches <- Codebase.branches codebase
      choice <- branchMenu caption branches
      case choice of
        Just (Left Cancel) -> pure Nothing
        Just (Left Create) -> do
          branch <- mergeBranchAndShowDiff codebase name mempty
          pure $ Just (name, branch)
        Just (Right name) -> selectBranch codebase name takeLine
        Nothing -> pure Nothing
