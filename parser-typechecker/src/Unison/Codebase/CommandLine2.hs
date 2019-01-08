{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Unison.Codebase.CommandLine2 where

-- import Debug.Trace
import           Control.Concurrent             (forkIO, killThread)
import qualified Control.Concurrent.Async       as Async
import           Control.Concurrent.STM         (atomically)
import           Control.Exception              (finally)
import           Control.Monad                  (forever, when)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Trans            (lift)
import           Data.Foldable                  (toList, traverse_)
import           Data.IORef
import           Data.List                      (intercalate, isSuffixOf, sort)
import           Data.List.Extra                (nubOrdOn)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe, listToMaybe)
import           Data.String                    (IsString, fromString)
import qualified Data.Text                      as Text
import qualified System.Console.ANSI            as Console
import qualified System.Console.Haskeline       as Line
import qualified System.Console.Terminal.Size   as Terminal
import           System.Directory               (canonicalizePath)
import           System.Random                  (randomRIO)
import           Unison.Codebase                (Codebase)
import qualified Unison.Codebase                as Codebase
import           Unison.Codebase.Branch         (Branch)
import qualified Unison.Codebase.Branch         as Branch
import           Unison.Codebase.Editor         (BranchName, DisplayThing (..),
                                                 Event (..), Input (..),
                                                 Output (..))
import qualified Unison.Codebase.Editor         as Editor
import qualified Unison.Codebase.Editor.Actions as Actions
import           Unison.Codebase.Runtime        (Runtime)
import qualified Unison.Codebase.Runtime        as Runtime
import qualified Unison.Codebase.Watch          as Watch
import qualified Unison.Names                   as Names
import           Unison.Parser                  (Ann)
import qualified Unison.PrettyPrintEnv          as PPE
import           Unison.PrintError              (prettyParseError,
                                                 prettyTypecheckedFile,
                                                 renderNoteAsANSI)
import qualified Unison.Result                  as Result
import qualified Unison.Referent                as Referent
import qualified Unison.Reference               as Reference
import qualified Unison.TypePrinter             as TypePrinter
import qualified Unison.TermPrinter             as TermPrinter
import qualified Unison.UnisonFile              as UF
import qualified Unison.Util.ColorText          as CT
import           Unison.Util.Monoid             (intercalateMap)
import qualified Unison.Util.Pretty             as P
import qualified Unison.Util.Relation           as R
import           Unison.Util.TQueue             (TQueue)
import qualified Unison.Util.TQueue             as Q
import           Unison.Var                     (Var)
import qualified Unison.Var                     as Var

notifyUser :: forall v . Var v => FilePath -> Output v -> IO ()
notifyUser dir o = do
  -- note - even if user's terminal is huge, we restrict available width since
  -- it's hard to read code or text that's super wide.
  width <- fromMaybe 80 . fmap (min 100 . Terminal.width) <$> Terminal.size
  let putPrettyLn = putStrLn . P.toANSI width
  case o of
    Success _    -> putStrLn "Done."
    DisplayDefinitions ppe terms types -> let
      prettyTerms = map go terms
      go (r, dt) =
        let n = PPE.termName ppe (Referent.Ref r) in
        case dt of
          MissingThing r -> missing n r
          BuiltinThing -> builtin n
          RegularThing tm -> P.map fromString $
            TermPrinter.prettyBinding ppe (Var.named n) tm
      prettyTypes = map go2 types
      builtin n = P.wrap $ "--" <> P.text n <> " is built-in."
      missing n r = P.wrap (
        "-- The name " <> P.text n <> " is assigned to the "
        <> "reference " <> fromString (show r ++ ",")
        <> "which is missing from the codebase.")
        <> P.newline
        <> tip "You might need to repair the codebase manually."
      go2 (r, dt) =
        let n = PPE.typeName ppe r in
        case dt of
          MissingThing r -> missing n r
          BuiltinThing -> builtin n
          RegularThing decl -> case decl of
            Left _ability -> P.bold "ability " <> P.text n <> " -- todo"
            Right _d -> P.bold "type " <> P.text n <> " -- todo"
      in putPrettyLn $ P.sep "\n\n" (prettyTerms <> prettyTypes)
    NoUnisonFile -> do
      dir' <- canonicalizePath dir
      putPrettyLn $ P.lines
        [ nothingTodo $ P.wrap "There's nothing for me to add right now."
        , ""
        , P.column2 [(P.bold "Hint:", msg dir')]
        , ""
        ]
     where
      msg dir =
        P.wrap
          $  "I'm currently watching for definitions in .u files under the"
          <> renderFileName dir
          <> "directory. Make sure you've updated something there before using the"
          <> P.bold "`add`"
          <> "command."
    UnknownBranch branchName ->
      putPrettyLn
        .  warn
        .  P.wrap
        $  "I don't know of a branch named "
        <> P.red (P.text branchName)
        <> "."
    UnknownName branchName nameTarget name ->
      putPrettyLn
        .  warn
        .  P.wrap
        $  "I don't know of any "
        <> fromString (Names.renderNameTarget nameTarget)
        <> " named "
        <> P.red (P.text name)
        <> " in the branch "
        <> P.blue (P.text branchName)
        <> "."
    NameAlreadyExists branchName nameTarget name ->
      putPrettyLn
        .  warn
        .  P.wrap
        $  "There's already a "
        <> fromString (Names.renderNameTarget nameTarget)
        <> " named "
        <> P.red (P.text name)
        <> " in the branch "
        <> P.blue (P.text branchName)
        <> "."
    ConflictedName branchName nameTarget name ->
      putPrettyLn
        .  warn
        .  P.wrap
        $  "The name "
        <> P.red (P.text name)
        <> " refers to more than one "
        <> fromString (Names.renderNameTarget nameTarget)
        <> " in the branch "
        <> P.blue (P.text branchName)
        <> "."
    BranchAlreadyExists b ->
      putPrettyLn
        $  warn
             (P.wrap $ "There's already a branch called " <> P.text b <> ".\n\n")
        <> (  tip
           $  "You can switch to that branch via"
           <> backtick ("branch " <> P.text b)
           <> "or delete it via"
           <> backtickEOS ("branch.delete " <> P.text b)
           )
    ListOfBranches current branches ->
      putPrettyLn
        $ let
            go n = if n == current
              then P.bold ("* " <> P.text n)
              else "  " <> P.text n
          in  intercalateMap "\n" go (sort branches)
    ListOfDefinitions branch terms types ->
      let ppe  = Branch.prettyPrintEnv1 branch
          sigs0 = (\(name, _, typ) -> (name, typ)) <$> terms
          sigs = [(name,t) | (name, Just t) <- sigs0 ]
          termsWithMissingTypes =
             [ (name, r) | (name, Referent.Ref (Reference.DerivedId r), Nothing) <- terms ]
          impossible = terms >>= \case
            (name, r, Nothing) -> case r of
              Referent.Ref (Reference.Builtin _) -> [(name,r)]
              _ -> []
            _ -> []
          missingTypes = nubOrdOn snd $
             [ (name, Reference.DerivedPrivate_ r) | (name, _, MissingThing r) <- types ] <>
             [ (name, r) | (name, Referent.Con r _, Nothing) <- terms] <>
             [ (name, r) | (name, Referent.Req r _, Nothing) <- terms]
          typeResults = map go types
          go (name, _, displayDecl) = case displayDecl of
            BuiltinThing -> P.wrap $
              P.bold "type" <> P.text name <> "(built-in)"
            MissingThing _ -> mempty -- these are collected above
            RegularThing decl -> case decl of
              Left _ability -> P.bold "ability " <> P.text name
              Right _d -> P.bold "type " <> P.text name
      in do
        putPrettyLn $ P.lines typeResults
        putPrettyLn $
          fromString <$> TypePrinter.prettySignatures ppe sigs
        when (not $ null impossible) . error $ "Compiler bug, these referents are missing types: " <> show impossible
        when (not $ null termsWithMissingTypes) . putPrettyLn $
          warn "The search returned the following terms for which the type signature is corrupted or missing:" <>
                P.newline <>
                P.column2 [ (P.text name, fromString (show ref))
                          | (name, ref) <- termsWithMissingTypes ]
        when (not $ null missingTypes) . putPrettyLn $
          warn "The search returned the following types which weren't found in the codebase:" <>
                P.newline <>
                P.column2 [ (P.text name, fromString (show ref))
                          | (name, ref) <- missingTypes ]

    AddOutput a -> case a of
      Editor.NothingToAdd -> notifyUser dir (NoUnisonFile @v)
      Editor.Added _ofile _branch adds dupes colls refcolls
        -> let
             Editor.AddOutputComponent addedTypes    addedTerms    = adds
             Editor.AddOutputComponent dupeTypes     dupeTerms     = dupes
             Editor.AddOutputComponent collidedTypes collidedTerms = colls
             addMsg = if not (null addedTypes && null addedTerms)
               then
                 "✓  OK, I added these definitions: "
                 <> P.newline
                 <> P.bulleted (fromVar <$> toList addedTypes)
                 <> P.bulleted (fromVar <$> toList addedTerms)
                 <> P.newline
               else ""
             dupeMsg = if not (null dupeTypes && null dupeTerms)
               then
                 P.wrap
                   (  "\128111\8205\9794\65039  I skipped these definitions"
                   <> " because they already exist in the current branch: "
                   )
                 <> P.lines
                      [ P.bulleted (fromVar <$> toList dupeTypes)
                      , P.bulleted (fromVar <$> toList dupeTerms)
                      ]
                 <> P.newline
               else ""
             collMsg =
               ( P.lines
                 . fmap
                     (\x ->
                       warn
                         .  P.wrap
                         $  "The name "
                         <> P.blue x
                         <> " already has another definition "
                         <> "in the current branch."
                     )
                 . toList
                 $ (  (fromVar <$> toList collidedTypes)
                   <> (fromVar <$> toList collidedTerms)
                   )
                 )
                 <> if not (null collidedTypes && null collidedTerms)
                    then
                      P.newline
                    else
                      ""
             nameCollMsg kind collsOfThatKind =
               P.lines
                 . fmap
                     (\(k, v) ->
                       warn
                         .  P.wrap
                         $  "The "
                         <> kind
                         <> " you added as "
                         <> P.blue (P.text k)
                         <> " already exists with "
                         <> (if length v > 1
                              then "different names. "
                              else "a different name. "
                            )
                         <> "It's defined as "
                         <> P.oxfordCommas (P.green . P.text <$> toList v)
                         <> ". I've added "
                         <> P.blue (P.text k)
                         <> " as a new name for it."
                     )
                 . Map.toList
                 . R.domain
                 $ collsOfThatKind refcolls
             dupeRefMsg     = nameCollMsg "term" Branch.termCollisions
             dupeTypeRefMsg = nameCollMsg "type" Branch.typeCollisions
           in
             putPrettyLn
             $  addMsg
             <> dupeMsg
             <> collMsg
             <> dupeTypeRefMsg
             <> dupeRefMsg
    ParseErrors src es -> do
      Console.setTitle "Unison ☹︎"
      traverse_ (putStrLn . CT.toANSI . prettyParseError (Text.unpack src)) es
    TypeErrors src ppenv notes -> do
      Console.setTitle "Unison ☹︎"
      let showNote =
            intercalateMap "\n\n" (renderNoteAsANSI ppenv (Text.unpack src))
              . map Result.TypeError
      putStrLn . showNote $ notes
    Evaluated names (watches, _term) -> do
      traverse_ (uncurry $ Watch.watchPrinter names) watches
    DisplayConflicts branch -> do
      let terms    = R.dom $ Branch.termNamespace branch
          patterns = R.dom $ Branch.patternNamespace branch
          types    = R.dom $ Branch.typeNamespace branch
      when (not $ null terms) $ do
        putStrLn "🙅 These terms have conflicts: "
        traverse_ (\x -> putStrLn ("  " ++ Text.unpack x)) terms
      when (not $ null patterns) $ do
        putStrLn "🙅 These patterns have conflicts: "
        traverse_ (\x -> putStrLn ("  " ++ Text.unpack x)) patterns
      when (not $ null types) $ do
        putStrLn "🙅 These types have conflicts: "
        traverse_ (\x -> putStrLn ("  " ++ Text.unpack x)) types
      -- TODO: Present conflicting TermEdits and TypeEdits
      -- if we ever allow users to edit hashes directly.
    FileChangeEvent _sourceName _src -> do
      Console.clearScreen
      Console.setCursorPosition 0 0
    Typechecked sourceName errorEnv unisonFile -> do
      Console.setTitle "Unison ☺︎"
      let emoticons = "🌸🌺🌹🌻🌼🌷🌵🌴🍄🌲"
      n <- randomRIO (0, length emoticons - 1)
      let uf         = UF.discardTerm unisonFile
          defs       = prettyTypecheckedFile uf errorEnv
          prettyDefs = CT.toANSI defs
      when (not $ null defs)
        .  putStrLn
        $  "✅ "
        ++ [emoticons !! n]
        ++ "  Found and typechecked the following definitions in "
        ++ (Text.unpack sourceName)
        ++ ":\n"
      putStrLn prettyDefs
      putStrLn $
          "👀  Now evaluating any watch expressions (lines starting with `>`)"
        <> " ...\n"
 where
  renderFileName = P.group . P.blue . fromString
  fromVar        = P.text . Var.name


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
  , aliases     :: [String]
  , args        :: [(IsOptional, ArgumentType)]
  , help        :: P.Pretty CT.ColorText
  , parse       :: [String] -> Either (P.Pretty CT.ColorText) Input
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
  definitionQueryArg = ArgumentType "definition query" $ \q _ b -> do
    let names = Text.unpack <$> toList (Branch.allNames (Branch.head b))
    pure $ autoComplete q names
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
        ( P.wrap $ "`add` adds to the codebase all the definitions from "
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
        (  P.wrap $ "`fork foo` creates the branch 'foo' "
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
        [(True, definitionQueryArg)]
        (P.column2 [
          ("`list`", P.wrap $ "shows all definitions in the current branch."),
          ("`list foo`", P.wrap $ "shows all definitions with a name similar"
                               <> "to 'foo' in the current branch."),
          ("`list foo bar`", P.wrap $ "shows all definitions with a name similar"
                                   <> "to 'foo' or 'bar' in the current branch.")]
        )
        (pure . SearchByNameI)
      , InputPattern
        "merge"
        []
        [(False, branchArg)]
        (P.wrap "`merge foo` merges the branch 'foo' into the current branch.")
        (\case
          [b] -> pure . MergeBranchI $ Text.pack b
          _ -> Left . warn . P.wrap $
            "Use `merge foo` to merge the branch 'foo'" <>
            "into the current branch."
        )
      , InputPattern
        "view"
        []
        [(False, definitionQueryArg)]
        (P.wrap "`view foo` prints the definition of `foo`.")
        (pure . ShowDefinitionI)
      , quit
      ]

completion :: String -> Line.Completion
completion s = Line.Completion s s True

autoComplete :: String -> [String] -> [Line.Completion]
autoComplete q ss = fixup $
  completion <$> (id $ Codebase.sortedApproximateMatches q ss)
  where
  -- workaround for https://github.com/judah/haskeline/issues/100
  -- if the common prefix of all the completions is smaller than
  -- the query, we make all the replacements equal to the query,
  -- which will preserve what the user has typed
  fixup [] = []
  fixup [c] = [c]
  fixup cs@(h:t) = let
    commonPrefix (h1:t1) (h2:t2) | h1 == h2 = h1 : commonPrefix t1 t2
    commonPrefix _ _             = ""
    overallCommonPrefix =
      foldl commonPrefix (Line.replacement h) (Line.replacement <$> t)
    in if length overallCommonPrefix < length q
       then [ c { Line.replacement = q } | c <- cs ]
       else cs

parseInput
  :: Map String InputPattern -> [String] -> Either (P.Pretty CT.ColorText) Input
parseInput patterns ss = case ss of
  []             -> Left ""
  command : args -> case Map.lookup command patterns of
    Just pat -> parse pat args
    Nothing ->
      Left
        .  warn
        .  P.wrap
        $  "I don't know how to "
        <> P.group (fromString command <> ".")
        <> "Type `help` or `?` to get help."

prompt :: String
prompt = "> "

putPrettyLn :: P.Pretty CT.ColorText -> IO ()
putPrettyLn p = do
  width <- getAvailableWidth
  putStrLn . P.toANSI width $ p

getAvailableWidth :: IO Int
getAvailableWidth =
  fromMaybe 80 . fmap (\s -> 120 `min` Terminal.width s) <$> Terminal.size

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
