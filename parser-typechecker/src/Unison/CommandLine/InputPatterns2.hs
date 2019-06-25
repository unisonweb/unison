{-# OPTIONS_GHC -Wno-unused-matches #-} -- todo: remove me later

-- {-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Unison.CommandLine.InputPatterns2 where

-- import Debug.Trace
import Data.Bifunctor (first)
import Data.List (intercalate, sortOn)
import Data.Map (Map)
import Data.String (fromString)
import Unison.Codebase.Editor.Input (Input)
import Unison.Codebase.Editor.RemoteRepo
import Unison.CommandLine2
import Unison.CommandLine.InputPattern2 (ArgumentType (ArgumentType), InputPattern (InputPattern), IsOptional(Optional,Required,ZeroPlus,OnePlus))
import Unison.Util.Monoid (intercalateMap)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.Codebase.Branch2 as Branch
import qualified Unison.Codebase.Editor.Input as Input
import qualified Unison.Codebase.Path as Path
import qualified Unison.CommandLine.InputPattern2 as I
import qualified Unison.HashQualified as HQ
import qualified Unison.Codebase.NameSegment as NameSegment
import qualified Unison.Names as Names
import qualified Unison.Util.ColorText as CT
import qualified Unison.Util.Pretty as P

showPatternHelp :: InputPattern -> P.Pretty CT.ColorText
showPatternHelp i = P.lines [
  P.bold (fromString $ I.patternName i) <> fromString
    (if not . null $ I.aliases i
     then " (or " <> intercalate ", " (I.aliases i) <> ")"
     else ""),
  P.wrap $ I.help i ]

-- `example list ["foo", "bar"]` (haskell) becomes `list foo bar` (pretty)
makeExample :: InputPattern -> [P.Pretty CT.ColorText] -> P.Pretty CT.ColorText
makeExample p args =
  backtick (intercalateMap " " id (fromString (I.patternName p) : args))

makeExample' :: InputPattern -> P.Pretty CT.ColorText
makeExample' p = makeExample p []

makeExampleEOS ::
  InputPattern -> [P.Pretty CT.ColorText] -> P.Pretty CT.ColorText
makeExampleEOS p args = P.group $
  backtick (intercalateMap " " id (fromString (I.patternName p) : args)) <> "."

helpFor :: InputPattern -> Either (P.Pretty CT.ColorText) Input
helpFor p = I.parse help [I.patternName p]

updateBuiltins :: InputPattern
updateBuiltins = InputPattern "builtins.update" [] []
  "Adds all the builtins that are missing from this branch, and deprecate the ones that don't exist in this version of Unison."
  (const . pure $ Input.UpdateBuiltinsI)

todo :: InputPattern
todo = InputPattern
  "todo"
  []
  [(Required, patchPathArg), (Optional, branchPathArg)]
  "`todo` lists the work remaining in the current branch to complete an ongoing refactoring."
  (\case
    patchStr : ws -> first fromString $ do
      patch  <- Path.parseSplit' Path.wordyNameSegment patchStr
      branch <- case ws of
        [pathStr] -> Path.parsePath' pathStr
        _         -> pure Path.relativeEmpty'
      pure $ Input.TodoI patch branch
    [] -> Left $ warn "`todo` takes a patch and an optional path"
  )

add :: InputPattern
add = InputPattern "add" [] [(ZeroPlus, noCompletions)]
 "`add` adds to the codebase all the definitions from the most recently typechecked file."
 (\ws -> pure $ Input.AddI (HQ.fromString <$> ws))

update :: InputPattern
update = InputPattern "update"
  []
  [(Required, patchPathArg)
  ,(ZeroPlus, noCompletions)]
  "`update` works like `add`, except if a definition in the file has the same name as an existing definition, the name gets updated to point to the new definition. If the old definition has any dependents, `update` will add those dependents to a refactoring session."
  (\case
    patchStr : ws -> first fromString $ do
      patch <- Path.parseSplit' Path.wordyNameSegment patchStr
      pure $ Input.UpdateI patch (HQ.fromString <$> ws)
    [] -> Left $ warn "`update` takes a patch and an optional list of definitions")

patch :: InputPattern
patch = InputPattern "patch" [] [(Required, patchPathArg), (Optional, branchPathArg)]
  "`propagate` rewrites any definitions that depend on definitions with type-preserving edits to use the updated versions of these dependencies."
  (\case
    patchStr : ws -> first fromString $ do
      patch <- Path.parseSplit' Path.wordyNameSegment patchStr
      branch <- case ws of
        [pathStr] -> Path.parsePath' pathStr
        _ -> pure Path.relativeEmpty'
      pure $ Input.PatchI patch branch
    [] -> Left $ warn "`todo` takes a patch and an optional path")


view :: InputPattern
view = InputPattern "view" [] [(OnePlus, exactDefinitionQueryArg)]
      "`view foo` prints the definition of `foo`."
      (pure . Input.ShowDefinitionI Input.ConsoleLocation)

undo :: InputPattern
undo = InputPattern "undo" [] []
      "`undo` reverts the most recent change to the codebase."
      (const $ pure Input.UndoI)

viewByPrefix :: InputPattern
viewByPrefix
  = InputPattern "view.recursive" [] [(OnePlus, exactDefinitionQueryArg)]
    "`view.recursive Foo` prints the definitions of `Foo` and `Foo.blah`."
    (pure . Input.ShowDefinitionByPrefixI Input.ConsoleLocation)

find :: InputPattern
find = InputPattern "find" [] [(ZeroPlus, fuzzyDefinitionQueryArg)]
    (P.wrapColumn2
      [ ("`find`"
        , "lists all definitions in the current branch.")
      , ( "`find foo`"
        , "lists all definitions with a name similar to 'foo' in the current branch.")
      , ( "`find foo bar`"
        , "lists all definitions with a name similar to 'foo' or 'bar' in the current branch.")
      , ( "`find -l foo bar`"
        , "lists all definitions with a name similar to 'foo' or 'bar' in the current branch, along with their hashes and aliases.")
      ]
    )
    (pure . Input.SearchByNameI)

findPatch :: InputPattern
findPatch = InputPattern "find.patch" [] []
    (P.wrapColumn2
      [ ("`find`"
        , "lists all patches in the current branch.")
      ]
    )
    (pure . const Input.FindPatchI)

renameTerm :: InputPattern
renameTerm = InputPattern "rename.term" []
    [(Required, exactDefinitionTermQueryArg)
    ,(Required, noCompletions)]
    "`rename.term foo bar` renames `foo` to `bar`."
    (\case
      [oldName, newName] -> first fromString $ do
        src <- Path.parseHQ'Split' oldName
        target <- Path.parseSplit' Path.definitionNameSegment newName
        pure $ Input.MoveTermI src target
      _ -> Left . P.warnCallout $ P.wrap
        "`rename.term` takes two arguments, like `rename.term oldname newname`.")

renameType :: InputPattern
renameType = InputPattern "rename.type" []
    [(Required, exactDefinitionTypeQueryArg)
    ,(Required, noCompletions)]
    "`rename.type foo bar` renames `foo` to `bar`."
    (\case
      [oldName, newName] -> first fromString $ do
        src <- Path.parseHQ'Split' oldName
        target <- Path.parseSplit' Path.definitionNameSegment newName
        pure $ Input.MoveTypeI src target
      _ -> Left . P.warnCallout $ P.wrap
        "`rename.type` takes two arguments, like `rename.type oldname newname`.")

deleteTerm :: InputPattern
deleteTerm = InputPattern "delete.term" []
    [(OnePlus, exactDefinitionTermQueryArg)]
    "`delete.term foo` removes the term name `foo` from the namespace."
    (\case
      [query] -> first fromString $ do
        p <- Path.parseHQ'Split' query
        pure $ Input.DeleteTermI p
      _ -> Left . P.warnCallout $ P.wrap
        "`delete.term` takes one or more arguments, like `delete.term name`."
    )

deleteType :: InputPattern
deleteType = InputPattern "delete.type" []
    [(OnePlus, exactDefinitionTypeQueryArg)]
    "`delete.type foo` removes the type name `foo` from the namespace."
    (\case
      [query] -> first fromString $ do
        p <- Path.parseHQ'Split' query
        pure $ Input.DeleteTypeI p
      _ -> Left . P.warnCallout $ P.wrap
        "`delete.type` takes one or more arguments, like `delete.type name`."
    )

aliasTerm :: InputPattern
aliasTerm = InputPattern "alias.term" []
    [(Required, exactDefinitionTermQueryArg), (Required, noCompletions)]
    "`alias.term foo bar` introduces `bar` with the same definition as `foo`."
    (\case
      [oldName, newName] -> first fromString $ do
        source <- Path.parseHQSplit' oldName
        target <- Path.parseSplit' Path.definitionNameSegment newName
        pure $ Input.AliasTermI source target
      _ -> Left . warn $ P.wrap
        "`alias.term` takes two arguments, like `alias.term oldname newname`."
    )

aliasType :: InputPattern
aliasType = InputPattern "alias.type" []
    [(Required, exactDefinitionTypeQueryArg), (Required, noCompletions)]
    "`alias.type Foo Bar` introduces `Bar` with the same definition as `Foo`."
    (\case
      [oldName, newName] -> first fromString $ do
        source <- Path.parseHQSplit' oldName
        target <- Path.parseSplit' Path.definitionNameSegment newName
        pure $ Input.AliasTypeI source target
      _ -> Left . warn $ P.wrap
        "`alias.type` takes two arguments, like `alias.type oldname newname`."
    )

cd :: InputPattern
cd = InputPattern "cd" [] [(Required, branchPathArg)]
    (P.wrapColumn2
      [ ("`cd foo.bar`",
          "descends into foo.bar from the current path.")
      , ("`cd .cat.dog",
          "sets the current path to the abolute path .cat.dog.") ])
    (\case
      [p] -> first fromString $ do
        p <- Path.parsePath' p
        pure . Input.SwitchBranchI $ p
      _ -> Left (I.help cd)
    )

deleteBranch :: InputPattern
deleteBranch = InputPattern "delete.branch" [] [(OnePlus, branchPathArg)]
  "`delete.branch <foo>` deletes the branch `foo`"
   (\case
        [p] -> first fromString $ do
          p <- Path.parseSplit' Path.wordyNameSegment p
          pure . Input.DeleteBranchI $ p
        _ -> Left (I.help deleteBranch)
      )

renameBranch :: InputPattern
renameBranch = InputPattern "rename.branch"
   []
   [(Required, branchPathArg), (Required, branchPathArg)]
   "`rename.branch foo bar` renames the branch `bar` to `foo`."
    (\case
      [src, dest] -> first fromString $ do
        src <- Path.parseSplit' Path.wordyNameSegment src
        dest <- Path.parseSplit' Path.wordyNameSegment dest
        pure $ Input.MoveBranchI src dest
      _ -> Left (I.help renameBranch)
    )

forkLocal :: InputPattern
forkLocal = InputPattern "fork" [] [(Required, branchPathArg)
                                   ,(Required, branchPathArg)]
    "`fork foo bar` creates the branch `bar` as a fork of `foo`."
    (\case
      [src, dest] -> first fromString $ do
        src <- Path.parsePath' src
        dest <- Path.parsePath' dest
        pure $ Input.ForkLocalBranchI src dest
      _ -> Left (I.help forkLocal)
    )

pull :: InputPattern
pull = InputPattern
  "pull"
  []
  [(Required, gitUrlArg), (Optional, branchPathArg)]
  (P.wrapColumn2
    [ ( "`pull ssh://example.org/path/to/repo.git`"
      , "pulls the contents of the given git url into the current path."
      )
    , ( "`pull ssh://example.org/path/to/repo.git foo.bar`"
      , "pulls the contents of the given git url into `foo.bar` relative "
        <> "to the current path."
      )
    , ( "`pull ssh://example.org/path/to/repo.git .foo.bar`"
      , "pulls the contents of the given git url into into the absolute path "
        <> "`.foo.bar`."
      )
    , ( "`pull ssh://example.org/path/to/repo.git foo bar`"
      , "pulls the contents of the git branch or commit named `bar` from the "
        <> " given git url into the path `bar`."
      )
    ]
  )
  (\case
    [url] -> pure $ Input.PullRemoteBranchI
      (GitRepo (Text.pack url) "master")
      Path.relativeEmpty'
    [url, path] -> do
      p <- first fromString $ Path.parsePath' path
      pure $ Input.PullRemoteBranchI (GitRepo (Text.pack url) "master") p
    [url, path, treeish] -> do
      p <- first fromString $ Path.parsePath' path
      pure $ Input.PullRemoteBranchI
        (GitRepo (Text.pack url) $ Text.pack treeish)
        p
    _ -> Left (I.help pull)
  )

push :: InputPattern
push = InputPattern
  "push"
  []
  [(Required, gitUrlArg), (Optional, branchPathArg)]
  (P.wrapColumn2
    [ ( "`push ssh://example.org/path/to/repo.git`"
      , "pushes the contents of the current path to the given git url."
      )
    , ( "`push ssh://example.org/path/to/repo.git foo.bar`"
      , "pushes the contents of `foo.bar` relative to the current path "
        <> "to the given git url."
      )
    , ( "`push ssh://example.org/path/to/repo.git .foo.bar`"
      , "pushes the contents of the absolute path `.foo.bar` "
        <> "to the given gir url."
      )
    , ( "`push ssh://example.org/path/to/repo.git foo bar`"
      , "pushes the contents of the path `foo` "
        <> "to the git branch `bar` at the given git url."
      )
    ]
  )
  (\case
    [url] -> first fromString . pure $ Input.PushRemoteBranchI
      (GitRepo (Text.pack url) "master")
      Path.relativeEmpty'
    [url, path] -> first fromString $ do
      p <- Path.parsePath' path
      pure $ Input.PushRemoteBranchI (GitRepo (Text.pack url) "master") p
    [url, path, treeish] -> first fromString $ do
      p <- Path.parsePath' path
      pure $ Input.PushRemoteBranchI
        (GitRepo (Text.pack url) $ Text.pack treeish)
        p
    _ -> Left (I.help push)
  )

mergeLocal :: InputPattern
mergeLocal = InputPattern "merge" [] [(Required, branchPathArg)
                                     ,(Optional, branchPathArg)]
 "`merge foo` merges the branch 'foo' into the current branch."
 (\case
      [src] -> first fromString $ do
        src <- Path.parsePath' src
        pure $ Input.MergeLocalBranchI src Path.relativeEmpty'
      [src, dest] -> first fromString $ do
        src <- Path.parsePath' src
        dest <- Path.parsePath' dest
        pure $ Input.MergeLocalBranchI src dest
      _ -> Left (I.help mergeLocal)
 )

-- replace,resolve :: InputPattern
--replace = InputPattern "replace" []
--          [ (Required, exactDefinitionQueryArg)
--          , (Required, exactDefinitionQueryArg) ]
--  (makeExample replace ["foo#abc", "foo#def"] <> "begins a refactor to replace" <> "uses of `foo#abc` with `foo#def`")
--  (const . Left . warn . P.wrap $ "This command hasn't been implemented. 😞")
--
--resolve = InputPattern "resolve" [] [(Required, exactDefinitionQueryArg)]
--  (makeExample resolve ["foo#abc"] <> "sets `foo#abc` as the canonical `foo` in cases of conflict, and begins a refactor to replace references to all other `foo`s to `foo#abc`.")
--  (const . Left . warn . P.wrap $ "This command hasn't been implemented. 😞")

edit :: InputPattern
edit = InputPattern "edit" [] [(OnePlus, exactDefinitionQueryArg)]
  "`edit foo` prepends the definition of `foo` to the top of the most recently saved file."
  (pure . Input.ShowDefinitionI Input.LatestFileLocation)

helpTopics :: Map String (P.Pretty P.ColorText)
helpTopics = Map.fromList [
  ("testcache", testCacheMsg),
  ("filestatus", fileStatusMsg),
  ("topics", topics)
  ]
  where
  topics = P.callout "🌻" $ P.lines [
    "Here's a list of topics I can tell you more about: ",
    "",
    P.indentN 2 $ P.sep "\n" (P.string <$> Map.keys helpTopics),
    "",
    aside "Example" "use `help filestatus` to learn more about that topic."
    ]
  fileStatusMsg = "🚧  Under construction!! Todo: docs here."
  testCacheMsg = P.callout "🎈" . P.lines $ [
    P.wrap $ "Unison caches the results of " <> P.blue "test>"
          <> "watch expressions. Since these expressions are pure and"
          <> "always yield the same result when evaluated, there's no need"
          <> "to run them more than once!",
    "",
    P.wrap $ "A test is rerun only if it has changed, or if one"
          <> "of the definitions it depends on has changed."
    ]

help :: InputPattern
help = InputPattern
    "help" ["?"] [(Optional, commandNameArg)]
    "`help` shows general help and `help <cmd>` shows help for one command."
    (\case
      [] -> Left $ intercalateMap "\n\n" showPatternHelp
        (sortOn I.patternName validInputs)
      [isHelp -> Just msg] -> Left msg
      [cmd] -> case lookup cmd (commandNames `zip` validInputs) of
        Nothing  -> Left . warn $ "I don't know of that command. Try `help`."
        Just pat -> Left $ I.help pat
      _ -> Left $ warn "Use `help <cmd>` or `help`.")
    where
      isHelp s = Map.lookup s helpTopics

quit :: InputPattern
quit = InputPattern "quit" ["exit"] []
  "Exits the Unison command line interface."
  (\case
    [] -> pure Input.QuitI
    _  -> Left "Use `quit`, `exit`, or <Ctrl-D> to quit."
  )

viewPatch :: InputPattern
viewPatch = InputPattern "view.patch" [] [(Required, patchPathArg)]
  "Lists all the edits in the given patch."
  (\case
    patchStr : [] -> first fromString $ do
      patch <- Path.parseSplit' Path.wordyNameSegment patchStr
      Right $ Input.ListEditsI patch
    _ -> Left $ warn "`view.patch` takes a patch and that's it."
   )

link :: InputPattern
link = InputPattern "link" []
  [(Required, exactDefinitionQueryArg),
   (Required, exactDefinitionQueryArg) ]
  "`link src dest` creates a link from `src` to `dest`. Use `links src` or `links src <type>` to view outgoing links, and `unlink src dest` to remove a link."
  (\case
    [src, dest] -> first fromString $ do
      src <- Path.parseHQ'Split' src
      dest <- Path.parseHQSplit' dest
      Right $ Input.LinkI src dest
    _ -> Left (I.help link)
   )

links :: InputPattern
links = InputPattern "links" []
  [(Required, exactDefinitionQueryArg), (Optional, exactDefinitionQueryArg)]
  "`links src` shows all outgoing links from `src`. `link src <type>` shows all links for the given type."
  (\case
    src : rest -> first fromString $ do
      src <- Path.parseHQ'Split' src
      ty <- pure $ case rest of
        [] -> Nothing
        _ -> Just (intercalate " " rest)
      Right $ Input.LinksI src ty
    _ -> Left (I.help links)
  )

unlink :: InputPattern
unlink = InputPattern "unlink" ["delete.link"]
  [(Required, exactDefinitionQueryArg),
   (Required, exactDefinitionQueryArg) ]
  "`unlink src dest` removes a link from `src` to `dest`."
  (\case
    [src, dest] -> first fromString $ do
      src <- Path.parseHQ'Split' src
      dest <- Path.parseHQSplit' dest
      Right $ Input.UnlinkI src dest
    _ -> Left (I.help unlink)
   )

validInputs :: [InputPattern]
validInputs =
  [ help
  , add
  , update
  , forkLocal
  , mergeLocal
  , push
  , pull
  , cd
  , deleteBranch
  , renameBranch
  , find
  , view
  , findPatch
  , viewPatch
  , undo
  , edit
  , renameTerm
  , deleteTerm
  , aliasTerm
  , renameType
  , deleteType
  , aliasType
  , todo
  , patch
  , link
  , unlink
  , links
  , InputPattern "test" [] []
    "`test` runs unit tests for the current branch."
    (const $ pure $ Input.TestI True True)
  , InputPattern "execute" [] []
    "`execute foo` evaluates the Unison expression `foo` of type `()` with access to the `IO` ability."
    (\ws -> if null ws
               then Left $ warn "`execute` needs a Unison language expression."
               else pure . Input.ExecuteI $ unwords ws)
  , quit
  , updateBuiltins
  ]

allTargets :: Set.Set Names.NameTarget
allTargets = Set.fromList [Names.TermName, Names.TypeName]

commandNames :: [String]
commandNames = I.patternName <$> validInputs

commandNameArg :: ArgumentType
commandNameArg =
  ArgumentType "command" $ \q _ _ _ -> pure (fuzzyComplete q (commandNames <> Map.keys helpTopics))

fuzzyDefinitionQueryArg :: ArgumentType
fuzzyDefinitionQueryArg =
  ArgumentType "fuzzy definition query" $ \q _ (Branch.head -> b) _ ->
    pure [] -- fuzzyCompleteHashQualified b q

-- todo: support absolute paths?
exactDefinitionQueryArg :: ArgumentType
exactDefinitionQueryArg =
  ArgumentType "definition query" $ \q _ (Branch.head -> b) _ ->
    pure [] -- autoCompleteHashQualified b q

exactDefinitionTypeQueryArg :: ArgumentType
exactDefinitionTypeQueryArg =
  ArgumentType "term definition query" $ \q _ (Branch.head -> b) _ ->
    pure [] -- autoCompleteHashQualifiedType b q

exactDefinitionTermQueryArg :: ArgumentType
exactDefinitionTermQueryArg =
  ArgumentType "term definition query" $ \q _ (Branch.head -> b) _ ->
    pure [] -- autoCompleteHashQualifiedTerm b q

patchPathArg :: ArgumentType
patchPathArg = noCompletions { I.typeName = "patch" }
  -- todo - better autocomplete provider here
  -- ArgumentType "patch" $ \q ->

branchPathArg :: ArgumentType
branchPathArg = ArgumentType "path" $ \q0 code b currentPath -> pure $ case q0 of
  -- query is just . show the immediate decendents under root
  "." -> [ completion' (Text.unpack $ NameSegment.toText s)
         | s <- Map.keys $ Branch._children (Branch.head b) ]
  -- query is empty, show immediate decendents under current path
  "" ->  [ completion' (Text.unpack $ NameSegment.toText s)
         | s <- Map.keys . Branch._children $ Branch.getAt0 (Path.unabsolute currentPath) (Branch.head b) ]
  -- query ends in . so show immediate dependents under path up to the dot
  (last -> '.') -> case Path.parsePath' (init q0) of
    Left err -> [prettyCompletion' ("", P.red (P.string err))]
    Right p' -> let
      p = Path.unabsolute $ Path.toAbsolutePath currentPath p'
      b0 = Branch.getAt0 p (Branch.head b)
      in [ completion' (q0 <> NameSegment.toString c) | c <- Map.keys $ Branch._children b0 ]
  -- query is foo.ba, so complete from foo children starting with 'ba'
  q0 -> case Path.parseSplit' Path.optionalWordyNameSegment q0 of
    Left err -> [prettyCompletion' ("", P.red (P.string err))]
    Right (init, last) -> let
      p = Path.unabsolute $ Path.toAbsolutePath currentPath init
      b0 = Branch.getAt0 p (Branch.head b)
      matchingChildren = filter (NameSegment.isPrefixOf last) . Map.keys $ Branch._children b0
      n = Text.length (NameSegment.toText last)
      in [ completion' (q0 <> drop n (NameSegment.toString c)) | c <- matchingChildren ]

noCompletions :: ArgumentType
noCompletions = ArgumentType "word" I.noSuggestions

gitUrlArg :: ArgumentType
gitUrlArg = noCompletions { I.typeName = "git-url" }

