{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Unison.Codebase.CommandLine2 where

import           Data.String                    ( fromString, IsString )
import qualified Unison.Util.ColorText         as CT
import           Control.Exception              ( finally )
import           Control.Monad.Trans            ( lift )
import           Data.Foldable                  ( traverse_ )
import           Data.IORef
import           Data.List                      ( isSuffixOf
                                                , sort
                                                , intercalate
                                                )
import           Data.Maybe                     ( listToMaybe
                                                , fromMaybe
                                                )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import qualified Data.Text                     as Text
import           Control.Concurrent             ( forkIO
                                                , killThread
                                                )
import qualified Control.Concurrent.Async      as Async
import           Control.Concurrent.STM         ( atomically )
import           Control.Monad                  ( forever
                                                , when
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Unison.Codebase                ( Codebase )
import qualified Unison.Codebase               as Codebase
import qualified Unison.Codebase.Branch        as Branch
import           Unison.Codebase.Branch         ( Branch )
import           Unison.Codebase.Editor         ( Output(..)
                                                , BranchName
                                                , Event(..)
                                                , Input(..)
                                                )
import qualified Unison.Codebase.Editor        as Editor
import qualified Unison.Codebase.Editor.Actions
                                               as Actions
import           Unison.Codebase.Runtime        ( Runtime )
import qualified Unison.Codebase.Runtime       as Runtime
import qualified Unison.Codebase.Watch         as Watch
import           Unison.Parser                  ( Ann )
import qualified Unison.Util.Pretty            as P
import qualified Unison.Util.Relation          as R
import           Unison.Util.TQueue             ( TQueue )
import qualified Unison.Util.TQueue            as Q
import           Unison.Util.Monoid             ( intercalateMap )
import           Unison.Var                     ( Var )
import qualified System.Console.Haskeline      as Line
import           System.Directory               ( canonicalizePath )
import qualified System.Console.Terminal.Size  as Terminal

notifyUser :: Var v => FilePath -> Output v -> IO ()
notifyUser dir o = do
  -- note - even if user's terminal is huge, we restrict available width since
  -- it's hard to read code or text that's super wide.
  width <- fromMaybe 80 . fmap (\s -> 100 `min` Terminal.width s) <$> Terminal.size
  let putPrettyLn = putStrLn . P.toANSI width
  case o of
    Success _    -> putStrLn "Done."
    NoUnisonFile -> do
      dir' <- canonicalizePath dir
      putPrettyLn $ P.lines [
        nothingTodo $ P.wrap "There's nothing for me to add right now.", "",
        P.column2 [(P.bold "Hint:", msg dir')], ""]
      where
        msg dir = P.wrap $
          "I'm currently watching for definitions in .u files under the" <>
          P.group (P.blue (fromString dir)) <>
          "directory. Double-check that you've updated something there before using the" <> P.bold "`add`" <> "command."
    UnknownBranch branchName -> putPrettyLn . warn $
      "I don't know of a branch named" <> P.red (P.text branchName) <> "."
    UnknownName branchName _nameTarget name -> putPrettyLn . warn . P.wrap $
        "I don't know of anything named" <>
        P.red (P.text name) <>
        "in the branch" <> P.blue (P.text branchName)
    DisplayConflicts branch -> do
      let terms    = R.dom $ Branch.termNamespace branch
          patterns = R.dom $ Branch.patternNamespace branch
          types    = R.dom $ Branch.typeNamespace branch
      when (not $ null terms) $ do
        putStrLn "🙅 The following terms have conflicts: "
        traverse_ (\x -> putStrLn ("  " ++ Text.unpack x)) terms
      when (not $ null patterns) $ do
        putStrLn "🙅 The following patterns have conflicts: "
        traverse_ (\x -> putStrLn ("  " ++ Text.unpack x)) patterns
      when (not $ null types) $ do
        putStrLn "🙅 The following types have conflicts: "
        traverse_ (\x -> putStrLn ("  " ++ Text.unpack x)) types
      -- TODO: Present conflicting TermEdits and TypeEdits
      -- if we ever allow users to edit hashes directly.
    ListOfBranches current branches ->
      putPrettyLn $
        let go n = if n == current then P.bold ("* " <> P.text n)
                   else "  " <> P.text n
          in intercalateMap "\n" go (sort branches)
    BranchAlreadyExists b ->
      putPrettyLn $
        warn ("There's already a branch called " <> P.text b <> ".\n\n") <>
        (tip $ "You can switch to that branch via"
            <> backtick ("branch " <> P.text b)
            <> "or delete it via" <> backtickEOS ("branch.delete " <> P.text b))

    _ -> putStrLn $ show o

allow :: FilePath -> Bool
allow = (||) <$> (".u" `isSuffixOf`) <*> (".uu" `isSuffixOf`)

-- TODO: Return all of these thread IDs so we can throw async exceptions at
-- them when we need to quit.

watchFileSystem :: TQueue Event -> FilePath -> IO (IO ())
watchFileSystem q dir = do
  (cancel, watcher) <- Watch.watchDirectory dir allow
  t <- forkIO . forever $ do
    (filePath, text) <- watcher
    atomically . Q.enqueue q $ UnisonFileChanged (Text.pack filePath) text
  pure (cancel >> killThread t)

watchBranchUpdates :: TQueue Event -> Codebase IO v a -> IO (IO ())
watchBranchUpdates q codebase = do
  (cancelExternalBranchUpdates, externalBranchUpdates) <-
    Codebase.branchUpdates codebase
  thread <- forkIO . forever $ do
    updatedBranches <- externalBranchUpdates
    atomically . Q.enqueue q . UnisonBranchChanged $ updatedBranches
  pure (cancelExternalBranchUpdates >> killThread thread)

warnNote :: String -> String
warnNote s = "⚠️  " <> s

backtick :: IsString s => P.Pretty s -> P.Pretty s
backtick s = P.group ("`" <> s <> "`")

backtickEOS :: IsString s => P.Pretty s -> P.Pretty s
backtickEOS s = P.group ("`" <> s <> "`.")

tip :: P.Pretty CT.ColorText -> P.Pretty CT.ColorText
tip s = P.column2 [(P.bold "Tip:", P.wrap s)]

warn :: IsString s => P.Pretty s -> P.Pretty s
warn s = P.group "⚠️  " <> s

nothingTodo :: IsString s => P.Pretty s -> P.Pretty s
nothingTodo s = P.group "😶  " <> s

type IsOptional = Bool

data InputPattern = InputPattern
  { patternName :: String
  , aliases :: [String]
  , args :: [(IsOptional, ArgumentType)]
  , help :: P.Pretty CT.ColorText
  , parse :: [String] -> Either (P.Pretty CT.ColorText) Input
  }

data ArgumentType = ArgumentType
  { typeName :: String
  , suggestions :: forall m v a . Monad m
                => String
                -> Codebase m v a
                -> Branch
                -> m [Line.Completion]
  }

showPatternHelp :: InputPattern -> P.Pretty CT.ColorText
showPatternHelp i = P.lines [
  P.bold (fromString $ patternName i) <> fromString
    (if not . null $ aliases i
     then " (or " <> intercalate ", " (aliases i) <> ")"
     else ""),
  help i ]

validInputs :: [InputPattern]
validInputs = validPatterns
 where
  commandNames = patternName <$> validPatterns
  commandMap   = Map.fromList (commandNames `zip` validPatterns)
  helpPattern  = InputPattern
    "help"
    ["?"]
    [(True, commandName)]
    "`help` shows general help and `help <cmd>` shows help for one command."
    (\case
      []    -> Left $ intercalateMap "\n\n" showPatternHelp validPatterns
      [cmd] -> case Map.lookup cmd commandMap of
        Nothing ->
          Left . warn $ "I don't know of that command. Try `help`."
        Just pat -> Left $ help pat
      _ -> Left $ warn "Use `help <cmd>` or `help`."
    )
  commandName =
    ArgumentType "command" $ \q _ _ -> pure $ autoComplete q commandNames
  branchArg = ArgumentType "branch" $ \q codebase _ -> do
    branches <- Codebase.branches codebase
    let bs = Text.unpack <$> branches
    pure $ autoComplete q bs
  quit = InputPattern
    "quit"
    ["exit"]
    []
    "Exits the Unison command line interface."
    (\case
      [] -> pure QuitI
      _  -> Left "Use `quit`, `exit`, or <Ctrl-D> to quit."
    )
  validPatterns
    = [ helpPattern
      , InputPattern
        "add"
        []
        []
        (  "`add` adds to the codebase all the definitions from "
        <> "the most recently typechecked file."
        )
        (\ws -> if not $ null ws
          then Left $ warn "`add` doesn't take any arguments."
          else pure AddI
        )
      , InputPattern
        "branch"
        []
        [(True, branchArg)]
        (P.column2 [("`branch`", P.wrap "lists all branches in the codebase.")
                   ,("`branch foo`", P.wrap $ "switches to the branch named 'foo', "
                                  <> "creating it first if it doesn't exist.")]
        )
        (\case
          []  -> pure ListBranchesI
          [b] -> pure . SwitchBranchI $ Text.pack b
          _ ->
            Left . warn . P.wrap $
              "Use `branch` to list all branches " <>
              "or `branch foo` to switch to or create the branch 'foo'."
        )
      , InputPattern
        "fork"
        []
        [(False, branchArg)]
        (  "`fork foo` creates the branch 'foo' "
        <> "as a fork of the current branch."
        )
        (\case
          [b] -> pure . ForkBranchI $ Text.pack b
          _ -> Left . warn . P.wrap $
            "Use `fork foo` to create the branch 'foo'" <>
            "from the current branch."
        )
      , InputPattern
        "list"
        ["ls"]
        []
        (P.column2 [
          ("`list`", P.wrap $ "shows all definitions in the current branch."),
          ("`list foo`", P.wrap $ "shows all definitions with a name similar"
                               <> "to 'foo' in the current branch."),
          ("`list foo bar`", P.wrap $ "shows all definitions with a name similar"
                                   <> "to 'foo' or 'bar' in the current branch.")]
        )
        (pure . SearchByNameI Editor.Fuzzy)
      , InputPattern
        "merge"
        []
        [(False, branchArg)]
        ("`merge foo` merges the branch 'foo' into the current branch.")
        (\case
          [b] -> pure . MergeBranchI $ Text.pack b
          _ -> Left . warn . P.wrap $
            "Use `merge foo` to merge the branch 'foo'" <>
            "into the current branch."
        )
      , quit
      ]

completion :: String -> Line.Completion
completion s = Line.Completion s s True

autoComplete :: String -> [String] -> [Line.Completion]
autoComplete q ss = completion <$> Codebase.sortedApproximateMatches q ss

parseInput :: Map String InputPattern -> [String] -> Either (P.Pretty CT.ColorText) Input
parseInput patterns ss = case ss of
  [] -> Left ""
  command : args -> case Map.lookup command patterns of
    Just pat -> parse pat args
    Nothing -> Left . warn . P.wrap $
      "I don't know how to " <> P.group (fromString command) <>
      ". Type `help` or `?` to get help."

prompt :: String
prompt = "> "

putPrettyLn :: P.Pretty CT.ColorText -> IO ()
putPrettyLn p = do
  width <- getAvailableWidth
  putStrLn . P.toANSI width $ p

getAvailableWidth :: IO Int
getAvailableWidth =
  fromMaybe 80 . fmap (\s -> 100 `min` Terminal.width s) <$> Terminal.size

getUserInput
  :: (MonadIO m, Line.MonadException m)
  => Map String InputPattern
  -> Codebase m v a
  -> Branch
  -> BranchName
  -> m Input
getUserInput patterns codebase branch branchName = Line.runInputT settings $ do
  line <- Line.getInputLine $ Text.unpack branchName <> prompt
  case line of
    Nothing -> pure QuitI
    Just l  -> case parseInput patterns $ words l of
      Left msg -> lift $ do
        liftIO $ putPrettyLn msg
        getUserInput patterns codebase branch branchName
      Right i -> pure i
 where
  settings    = Line.Settings tabComplete (Just ".unisonHistory") True
  tabComplete = Line.completeWordWithPrev Nothing " " $ \prev word ->
    -- User hasn't finished a command name, complete from command names
    if null prev
      then pure $ autoComplete word (Map.keys patterns)
    -- User has finished a command name; use completions for that command
      else case words $ reverse prev of
        h : t -> fromMaybe (pure []) $ do
          p            <- Map.lookup h patterns
          (_, argType) <- listToMaybe $ drop (length t) (args p)
          pure $ suggestions argType word codebase branch
        _ -> pure []

main
  :: forall v
   . Var v
  => FilePath
  -> BranchName
  -> Maybe FilePath
  -> IO (Runtime v)
  -> Codebase IO v Ann
  -> IO ()
main dir currentBranchName _initialFile startRuntime codebase = do
  currentBranch <- Codebase.getBranch codebase currentBranchName
  eventQueue    <- Q.newIO
  currentBranch <- case currentBranch of
    Nothing ->
      Codebase.mergeBranch codebase currentBranchName Codebase.builtinBranch
        <* (  putStrLn
           $  "☝️  I found no branch named '"
           <> Text.unpack currentBranchName
           <> "' so I've created it for you."
           )
    Just b -> pure b
  do
    runtime                  <- startRuntime
    branchRef                <- newIORef (currentBranch, currentBranchName)
    cancelFileSystemWatch    <- watchFileSystem eventQueue dir
    cancelWatchBranchUpdates <- watchBranchUpdates eventQueue codebase
    let patternMap =
          Map.fromList
            $   validInputs
            >>= (\p -> [(patternName p, p)] ++ ((, p) <$> aliases p))
        getInput = do
          (branch, branchName) <- readIORef branchRef
          getUserInput patternMap codebase branch branchName
    let awaitInput = do
          -- Race the user input and file watch.
          Async.race (atomically $ Q.peek eventQueue) getInput >>= \case
            Left _ -> Left <$> atomically (Q.dequeue eventQueue)
            x      -> pure x
        cleanup = do
          Runtime.terminate runtime
          cancelFileSystemWatch
          cancelWatchBranchUpdates
    (`finally` cleanup)
      $ Editor.commandLine awaitInput
                           runtime
                           (\b bn -> writeIORef branchRef (b, bn))
                           (notifyUser dir)
                           codebase
      $ Actions.startLoop currentBranch currentBranchName
