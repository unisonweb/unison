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
import Data.String (fromString)
import Unison.Codebase.Editor.Input (Input)
import Unison.CommandLine
import Unison.CommandLine.InputPattern2 (ArgumentType (ArgumentType), InputPattern (InputPattern), IsOptional(Optional,Required,ZeroPlus,OnePlus))
import Unison.Util.Monoid (intercalateMap)
import qualified Data.Set as Set
import qualified Unison.Codebase.Branch2 as Branch
import qualified Unison.Codebase.Editor.Input as Input
import qualified Unison.Codebase.Path as Path
import qualified Unison.CommandLine.InputPattern2 as I
import qualified Unison.HashQualified as HQ
import qualified Unison.Names as Names
import qualified Unison.Util.ColorText as CT
import qualified Unison.Util.Pretty as P
import qualified Data.Text as Text
import Unison.Codebase.Editor.RemoteRepo

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
      pure $ Input.PropagateI patch branch
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
  [(Required, githubOwner), (Required, githubRepo), (Optional, branchPathArg)]
  (P.wrapColumn2
    [ ( "`pull foo bar`"
      , "pulls the contents of the Github repo foo/bar into the current path."
      )
    , ( "`pull foo bar baz.qux`"
      , "pulls the contents of the Github repo foo/bar into baz.qux relative "
        <> "to the current path."
      )
    , ( "`pull foo bar .baz.qux`"
      , "pulls the Github repo foo/bar into the absolute path .baz.qux."
      )
    , ( "`pull foo bar baz qux`"
      , "pulls the contents of the branch or commit named qux from Github repo "
        <> "foo/bar into the path baz."
      )
    ]
  )
  (\case
    [owner, repo] -> pure $ Input.PullRemoteBranchI
      (Github (Text.pack owner) (Text.pack repo) "master")
      Path.relativeEmpty'
    [owner, repo, path] -> do
      p <- first fromString $ Path.parsePath' path
      pure $ Input.PullRemoteBranchI
        (Github (Text.pack owner) (Text.pack repo) "master")
        p
    [owner, repo, path, treeish] -> do
      p <- first fromString $ Path.parsePath' path
      pure $ Input.PullRemoteBranchI
        (Github (Text.pack owner) (Text.pack repo) $ Text.pack treeish)
        p
    _ -> Left (I.help pull)
  )

push :: InputPattern
push = InputPattern
  "push"
  []
  [(Required, githubOwner), (Required, githubRepo), (Optional, branchPathArg)]
  (P.wrapColumn2
    [ ( "`push foo bar`"
      , "pushes the contents of the current path to the Github repo foo/bar."
      )
    , ( "`push foo bar baz.qux`"
      , "pushes the contents of baz.qux relative to the current path "
        <> "to the Github repo foo/bar."
      )
    , ( "`push foo bar .baz.qux`"
      , "pushes the contents of the absolute path .baz.qux "
        <> "to the Github repo foo/bar."
      )
    , ( "`push foo bar baz qux`"
      , "pushes the contents of the path baz "
        <> "to the branch qux at the Github repo foo/bar."
      )
    ]
  )
  (\case
    [owner, repo] -> first fromString . pure $ Input.PushRemoteBranchI
      (Github (Text.pack owner) (Text.pack repo) "master")
      Path.relativeEmpty'
    [owner, repo, path] -> first fromString $ do
      p <- Path.parsePath' path
      pure $ Input.PushRemoteBranchI
        (Github (Text.pack owner) (Text.pack repo) "master")
        p
    [owner, repo, path, treeish] -> first fromString $ do
      p <- Path.parsePath' path
      pure $ Input.PushRemoteBranchI
        (Github (Text.pack owner) (Text.pack repo) $ Text.pack treeish)
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

help :: InputPattern
help = InputPattern
    "help" ["?"] [(Optional, commandNameArg)]
    "`help` shows general help and `help <cmd>` shows help for one command."
    (\case
      [] -> Left $ intercalateMap "\n\n" showPatternHelp
        (sortOn I.patternName validInputs)
      [cmd] -> case lookup cmd (commandNames `zip` validInputs) of
        Nothing  -> Left . warn $ "I don't know of that command. Try `help`."
        Just pat -> Left $ I.help pat
      _ -> Left $ warn "Use `help <cmd>` or `help`.")

quit :: InputPattern
quit = InputPattern "quit" ["exit"] []
  "Exits the Unison command line interface."
  (\case
    [] -> pure Input.QuitI
    _  -> Left "Use `quit`, `exit`, or <Ctrl-D> to quit."
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
  --  , InputPattern "test" [] []
  --    "`test` runs unit tests for the current branch."
  --    (const $ pure $ Input.TestI True True)g
  , InputPattern "execute" [] []
    "`execute foo` evaluates the Unison expression `foo` of type `()` with access to the `IO` ability."
    (\ws -> if null ws
               then Left $ warn "`execute` needs a Unison language expression."
               else pure . Input.ExecuteI $ unwords ws)
  , quit
  , updateBuiltins
 , InputPattern "view.patch" [] [(Required, patchPathArg)]
     "Lists all the edits in the given patch."
     (\case
       patchStr : [] -> first fromString $ do
         patch <- Path.parseSplit' Path.wordyNameSegment patchStr
         Right $ Input.ListEditsI patch
       _ -> Left $ warn "`view.patch` takes a patch and that's it."
       )
  ]

allTargets :: Set.Set Names.NameTarget
allTargets = Set.fromList [Names.TermName, Names.TypeName]

commandNames :: [String]
commandNames = I.patternName <$> validInputs

commandNameArg :: ArgumentType
commandNameArg =
  ArgumentType "command" $ \q _ _ _ -> pure (fuzzyComplete q commandNames)

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
branchPathArg = noCompletions { I.typeName = "branch" }

githubOwner :: ArgumentType
githubOwner = noCompletions { I.typeName = "Github-owner"}

githubRepo :: ArgumentType
githubRepo = noCompletions { I.typeName = "Github-repo"}

noCompletions :: ArgumentType
noCompletions = ArgumentType "word" I.noSuggestions
