{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Unison.CommandLine.InputPatterns where

-- import Debug.Trace
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.List (intercalate, sortOn, isPrefixOf)
import Data.List.Extra (nubOrd)
import Data.Map (Map)
import Data.Set (Set)
import Data.String (fromString)
import Data.Text (Text)
import System.Console.Haskeline.Completion (Completion)
import Unison.Codebase (Codebase)
import Unison.Codebase.Editor.Input (Input)
import Unison.Codebase.Editor.RemoteRepo
import Unison.CommandLine.InputPattern (ArgumentType (ArgumentType), InputPattern (InputPattern), IsOptional(Optional,Required,ZeroPlus,OnePlus))
import Unison.CommandLine
import Unison.Util.Monoid (intercalateMap)
import Data.Either.Combinators (mapLeft)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Editor.Input as Input
import qualified Unison.Codebase.Path as Path
import qualified Unison.CommandLine.InputPattern as I
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import qualified Unison.Name as Name
import qualified Unison.Names2 as Names
import qualified Unison.Util.ColorText as CT
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.Relation as R

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

mergeBuiltins :: InputPattern
mergeBuiltins = InputPattern "builtins.merge" [] []
  "Adds all the builtins to the current path."
  (const . pure $ Input.MergeBuiltinsI)

updateBuiltins :: InputPattern
updateBuiltins = InputPattern "builtins.update" [] []
  "Adds all the builtins that are missing from this branch, and deprecate the ones that don't exist in this version of Unison."
  (const . pure $ Input.UpdateBuiltinsI)

todo :: InputPattern
todo = InputPattern
  "todo"
  []
  [(Optional, patchArg), (Optional, pathArg)]
  (P.wrapColumn2
    [ (makeExample' todo
      , "lists the refactor work remaining in the default patch for the current path.")
    , (makeExample todo ["<patch>"]
      , "lists the refactor work remaining in the given patch in the current path.")
    , (makeExample todo ["<patch>", "[path]"]
      , "lists the refactor work remaining in the given patch in given path.")
    ]
  )
  (\case
    patchStr : ws -> mapLeft (warn . fromString) $ do
      patch  <- Path.parseSplit' Path.wordyNameSegment patchStr
      branch <- case ws of
        []        -> pure Path.relativeEmpty'
        [pathStr] -> Path.parsePath' pathStr
        _         -> Left "`todo` just takes a patch and one optional path"
      Right $ Input.TodoI (Just patch) branch
    [] -> Right $ Input.TodoI Nothing Path.relativeEmpty'
  )

add :: InputPattern
add = InputPattern "add" [] [(ZeroPlus, noCompletions)]
 "`add` adds to the codebase all the definitions from the most recently typechecked file."
 $ \ws -> case traverse HQ'.fromString ws of
  Just ws -> pure $ Input.AddI ws
  Nothing -> Left . warn . P.lines . fmap fromString
            . ("I don't know what these refer to:\n" :)
            $ collectNothings HQ'.fromString ws

update :: InputPattern
update = InputPattern "update"
  []
  [(Optional, patchArg)
  ,(ZeroPlus, noCompletions)]
  (P.wrap (makeExample' update <> "works like"
      <> (P.group $ makeExample' add <> ",")
      <> "except that if a definition in the file has the same name as an"
      <> "existing definition, the name gets updated to point to the new"
      <> "definition. If the old definition has any dependents, `update` will"
      <> "add those dependents to a refactoring session, specified by an"
      <> "optional patch.")
   <> P.wrapColumn2
    [ (makeExample' update
      , "adds all definitions in the .u file, noting replacements in the"
       <> "default patch for the current path.")
    , (makeExample update ["<patch>"]
      , "adds all definitions in the .u file, noting replacements in the"
       <> "specified patch.")
    , (makeExample update ["<patch>", "foo", "bar"]
      , "adds `foo`, `bar`, and their dependents from the .u file, noting"
       <> "any replacements into the specified patch.")
    ]
  )
  (\case
    patchStr : ws -> do
      patch <- first fromString $ Path.parseSplit' Path.wordyNameSegment patchStr
      case traverse HQ'.fromString ws of
        Just ws -> Right $ Input.UpdateI (Just patch) ws
        Nothing ->
          Left . warn . P.lines . fmap fromString .
                ("I don't know what these refer to:\n" :) $
                collectNothings HQ'.fromString ws
    [] -> Right $ Input.UpdateI Nothing [] )

patch :: InputPattern
patch = InputPattern "patch" [] [(Required, patchArg), (Optional, pathArg)]
  (P.wrap $ makeExample' patch <> "rewrites any definitions that depend on "
      <> "definitions with type-preserving edits to use the updated versions of"
      <> "these dependencies.")
  (\case
    patchStr : ws -> first fromString $ do
      patch <- Path.parseSplit' Path.wordyNameSegment patchStr
      branch <- case ws of
        [pathStr] -> Path.parsePath' pathStr
        _ -> pure Path.relativeEmpty'
      pure $ Input.PatchI patch branch
    [] -> Left $ warn $ makeExample' patch <> "takes a patch and an optional path")

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
find = InputPattern "find" ["list", "ls"] [(ZeroPlus, fuzzyDefinitionQueryArg)]
    (P.wrapColumn2
      [ ("`find`"
        , "lists all definitions in the current branch.")
      , ( "`find foo`"
        , "lists all definitions with a name similar to 'foo' in the current branch.")
      , ( "`find foo bar`"
        , "lists all definitions with a name similar to 'foo' or 'bar' in the current branch.")
      ]
    )
    (pure . Input.SearchByNameI False)

findVerbose :: InputPattern
findVerbose = InputPattern "find.verbose" ["list.verbose", "ls.verbose"] [(ZeroPlus, fuzzyDefinitionQueryArg)]
  "`find.verbose` searches for definitions like `find`, but includes hashes and aliases in the results."
  (pure . Input.SearchByNameI True)


findPatch :: InputPattern
findPatch = InputPattern "find.patch" ["list.patch", "ls.patch"] []
    (P.wrapColumn2
      [ ("`find.patch`"
        , "lists all patches in the current branch.")
      ]
    )
    (pure . const Input.FindPatchI)

renameTerm :: InputPattern
renameTerm = InputPattern "move.term" ["rename.term"]
    [(Required, exactDefinitionTermQueryArg)
    ,(Required, noCompletions)]
    "`move.term foo bar` renames `foo` to `bar`."
    (\case
      [oldName, newName] -> first fromString $ do
        src <- Path.parseHQSplit' oldName
        target <- Path.parseSplit' Path.definitionNameSegment newName
        pure $ Input.MoveTermI src target
      _ -> Left . P.warnCallout $ P.wrap
        "`rename.term` takes two arguments, like `rename.term oldname newname`.")

renameType :: InputPattern
renameType = InputPattern "move.type" ["rename.type"]
    [(Required, exactDefinitionTypeQueryArg)
    ,(Required, noCompletions)]
    "`move.type foo bar` renames `foo` to `bar`."
    (\case
      [oldName, newName] -> first fromString $ do
        src <- Path.parseHQSplit' oldName
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
        p <- Path.parseHQSplit' query
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
        p <- Path.parseHQSplit' query
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
cd = InputPattern "path" ["cd", "j"] [(Required, pathArg)]
    (P.wrapColumn2
      [ ("`path foo.bar`",
          "descends into foo.bar from the current path.")
      , ("`path .cat.dog`",
          "sets the current path to the abolute path .cat.dog.") ])
    (\case
      [p] -> first fromString $ do
        p <- Path.parsePath' p
        pure . Input.SwitchBranchI $ p
      _ -> Left (I.help cd)
    )

deleteBranch :: InputPattern
deleteBranch = InputPattern "delete.path" [] [(Required, pathArg)]
  "`delete.path <foo>` deletes the path `foo`"
   (\case
        ["."] -> first fromString .
          pure $ Input.DeleteBranchI Nothing
        [p] -> first fromString $ do
          p <- Path.parseSplit' Path.wordyNameSegment p
          pure . Input.DeleteBranchI $ Just p
        _ -> Left (I.help deleteBranch)
      )

deletePatch :: InputPattern
deletePatch = InputPattern "delete.patch" [] [(Required, patchArg)]
  "`delete.patch <foo>` deletes the patch `foo`"
   (\case
        [p] -> first fromString $ do
          p <- Path.parseSplit' Path.wordyNameSegment p
          pure . Input.DeletePatchI $ p
        _ -> Left (I.help deletePatch)
      )

movePatch :: String -> String -> Either (P.Pretty CT.ColorText) Input
movePatch src dest = first fromString $ do
  src <- Path.parseSplit' Path.wordyNameSegment src
  dest <- Path.parseSplit' Path.wordyNameSegment dest
  pure $ Input.MovePatchI src dest

copyPatch :: InputPattern
copyPatch = InputPattern "copy.patch"
   []
   [(Required, patchArg), (Required, patchArg)]
   "`copy.patch foo bar` copies the patch `bar` to `foo`."
    (\case
      [src, dest] -> movePatch src dest
      _ -> Left (I.help copyPatch)
    )

renamePatch :: InputPattern
renamePatch = InputPattern "move.patch"
   ["rename.patch"]
   [(Required, patchArg), (Required, patchArg)]
   "`move.path foo bar` renames the patch `bar` to `foo`."
    (\case
      [src, dest] -> movePatch src dest
      _ -> Left (I.help renamePatch)
    )

renameBranch :: InputPattern
renameBranch = InputPattern "move.path"
   ["rename.path"]
   [(Required, pathArg), (Required, pathArg)]
   "`move.path foo bar` renames the path `bar` to `foo`."
    (\case
      [".", dest] -> first fromString $ do
        dest <- Path.parseSplit' Path.wordyNameSegment dest
        pure $ Input.MoveBranchI Nothing dest
      [src, dest] -> first fromString $ do
        src <- Path.parseSplit' Path.wordyNameSegment src
        dest <- Path.parseSplit' Path.wordyNameSegment dest
        pure $ Input.MoveBranchI (Just src) dest
      _ -> Left (I.help renameBranch)
    )

forkLocal :: InputPattern
forkLocal = InputPattern "fork" ["copy.path"] [(Required, pathArg)
                                   ,(Required, pathArg)]
    "`fork foo bar` creates the path `bar` as a fork of `foo`."
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
  [(Optional, gitUrlArg), (Optional, pathArg)]
  (P.wrapColumn2
    [ ( "`pull`"
      , "pulls the contents of the git url configured for the current path."
      )
    , ( "`pull url`"
      , "pulls the contents of the git url `url` into the current path."
      )
    , ( "`pull url foo.bar`"
      , "pulls the contents of the git url `url` into `foo.bar` relative "
        <> "to the current path."
      )
    , ( "`pull url .foo.bar`"
      , "pulls the contents of the git url `url` into into the absolute path "
        <> "`.foo.bar`."
      )
    , ( "`pull url foo bar`"
      , "pulls the contents of the git branch or commit named `bar` from the "
        <> " git url `url` into the path `bar`."
      )
    ]
  )
  (\case
    []    -> pure $ Input.PullRemoteBranchI Nothing Path.relativeEmpty'
    [url] -> pure $ Input.PullRemoteBranchI
      (Just $ GitRepo (Text.pack url) "master")
      Path.relativeEmpty'
    [url, path] -> do
      p <- first fromString $ Path.parsePath' path
      pure $ Input.PullRemoteBranchI (Just $ GitRepo (Text.pack url) "master") p
    [url, path, treeish] -> do
      p <- first fromString $ Path.parsePath' path
      pure $ Input.PullRemoteBranchI
        (Just . GitRepo (Text.pack url) $ Text.pack treeish)
        p
    _ -> Left (I.help pull)
  )

push :: InputPattern
push = InputPattern
  "push"
  []
  [(Optional, gitUrlArg), (Optional, pathArg)]
  (P.wrapColumn2
    [ ( "`push`"
      , "pushes the contents of the current path to the configured git url " <>
        "for that path."
      )
    , ( "`push url`"
      , "pushes the contents of the current path to the git url given by `url`."
      )
    , ( "`push url foo.bar`"
      , "pushes the contents of `foo.bar` relative to the current path "
        <> "to the git url given by `url`."
      )
    , ( "`push url .foo.bar`"
      , "pushes the contents of the absolute path `.foo.bar` "
        <> "to the git url given by `url`."
      )
    , ( "`push url foo bar`"
      , "pushes the contents of the path `foo` "
        <> "to the git branch `bar` at the git url `url`."
      )
    ]
  )
  (\case
    [] -> pure $ Input.PushRemoteBranchI Nothing Path.relativeEmpty'
    [url] -> first fromString . pure $ Input.PushRemoteBranchI
      (Just $ GitRepo (Text.pack url) "master")
      Path.relativeEmpty'
    [url, path] -> first fromString $ do
      p <- Path.parsePath' path
      pure $ Input.PushRemoteBranchI (Just $ GitRepo (Text.pack url) "master") p
    [url, path, treeish] -> first fromString $ do
      p <- Path.parsePath' path
      pure $ Input.PushRemoteBranchI
        (Just . GitRepo (Text.pack url) $ Text.pack treeish)
        p
    _ -> Left (I.help push)
  )

mergeLocal :: InputPattern
mergeLocal = InputPattern "merge" [] [(Required, pathArg)
                                     ,(Optional, pathArg)]
 "`merge foo` merges the path 'foo' into the current branch."
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
  ("topics", topics),
  ("messages.disallowedAbsolute", disallowedAbsoluteMsg),
  ("pathnames", pathnamesMsg)
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
  pathnamesMsg = P.callout "\129488" . P.lines $ [
    P.wrap $ "There are two kinds of path names," <> P.group (P.blue "absolute" <> ",")
          <> "such as" <> P.group ("(" <> P.blue ".foo.bar")
          <> "or" <> P.group (P.blue ".base.math.+" <> ")")
          <> "and" <> P.group (P.green "relative" <> ",")
          <> "such as" <> P.group ("(" <> P.green "math.sqrt")
          <> "or" <> P.group (P.green "util.List.++" <> ")."),
    "",
    P.wrap $ "Relative names are converted to absolute names by prepending the current path."
          <> "For example, if your Unison prompt reads:", "",
      P.indentN 2 $ P.blue ".foo.bar>", "",
    "and your .u file looks like:", "",
      P.indentN 2 $ P.green "x" <> " = 41", "",
    P.wrap $
      "then doing an" <> P.blue "add" <>
      "will create the definition with the absolute name" <>
      P.group (P.blue ".foo.bar.x" <> " = 41"),
    "",
    P.wrap $
      "and you can refer to" <> P.green "x" <> "by its absolute name " <>
      P.blue ".foo.bar.x" <> "elsewhere" <> "in your code. For instance:", "",
    P.indentN 2 $
      "answerToLifeTheUniverseAndEverything = " <> P.blue ".foo.bar.x" <> " + 1"
    ]

  disallowedAbsoluteMsg = P.callout "\129302" . P.lines $ [
    P.wrap $
      "Although I can understand absolute (ex: .foo.bar) or" <>
      "relative (ex: util.math.sqrt) references to existing definitions" <>
      P.group ("(" <> P.blue "help pathnames") <> "to learn more)," <>
      "I can't yet handle giving new definitions with absolute names in a .u file.",
    "",
    P.wrap $ "As a workaround, you can give definitions with a relative name"
          <> "temporarily (like `exports.blah.foo`) and then use `move.*` "
          <> "or `merge` commands to move stuff around afterwards."
    ]

help :: InputPattern
help = InputPattern
    "help" ["?"] [(Optional, commandNameArg)]
    "`help` shows general help and `help <cmd>` shows help for one command."
    (\case
      [] -> Left $ intercalateMap "\n\n" showPatternHelp
        (sortOn I.patternName validInputs)
      [isHelp -> Just msg] -> Left msg
      [cmd] -> case Map.lookup cmd commandsByName of
        Nothing  -> Left . warn $ "I don't know of that command. Try `help`."
        Just pat -> Left $ I.help pat
      _ -> Left $ warn "Use `help <cmd>` or `help`.")
    where
      commandsByName = Map.fromList [
        (n, i) | i <- validInputs, n <- I.patternName i : I.aliases i ]
      isHelp s = Map.lookup s helpTopics

quit :: InputPattern
quit = InputPattern "quit" ["exit", ":q"] []
  "Exits the Unison command line interface."
  (\case
    [] -> pure Input.QuitI
    _  -> Left "Use `quit`, `exit`, or <Ctrl-D> to quit."
  )

viewPatch :: InputPattern
viewPatch = InputPattern "view.patch" [] [(Required, patchArg)]
    (P.wrapColumn2
      [ ( makeExample' viewPatch
        , "Lists all the edits in the default patch."
        )
      , ( makeExample viewPatch ["<patch>"]
        , "Lists all the edits in the given patch."
        )
      ]
    )
  (\case
    []         -> Right $ Input.ListEditsI Nothing
    [patchStr] -> mapLeft fromString $ do
      patch <- Path.parseSplit' Path.wordyNameSegment patchStr
      Right $ Input.ListEditsI (Just patch)
    _ -> Left $ warn "`view.patch` takes a patch and that's it."
   )

link :: InputPattern
link = InputPattern "link" []
  [(Required, exactDefinitionQueryArg),
   (Required, exactDefinitionQueryArg) ]
  "`link src dest` creates a link from `src` to `dest`. Use `links src` or `links src <type>` to view outgoing links, and `unlink src dest` to remove a link."
  (\case
    [src, dest] -> first fromString $ do
      src <- Path.parseHQSplit' src
      dest <- Path.parseHQSplit' dest
      Right $ Input.LinkI src dest
    _ -> Left (I.help link)
   )

links :: InputPattern
links = InputPattern
  "links"
  []
  [(Required, exactDefinitionQueryArg), (Optional, exactDefinitionQueryArg)]
  "`links src` shows all outgoing links from `src`. `link src <type>` shows all links for the given type."
  (\case
    src : rest -> first fromString $ do
      src <- Path.parseHQSplit' src
      let ty = case rest of
            [] -> Nothing
            _  -> Just $ unwords rest
       in Right $ Input.LinksI src ty
    _ -> Left (I.help links)
  )

unlink :: InputPattern
unlink = InputPattern "unlink" ["delete.link"]
  [(Required, exactDefinitionQueryArg),
   (Required, exactDefinitionQueryArg) ]
  "`unlink src dest` removes a link from `src` to `dest`."
  (\case
    [src, dest] -> first fromString $ do
      src <- Path.parseHQSplit' src
      dest <- Path.parseHQSplit' dest
      Right $ Input.UnlinkI src dest
    _ -> Left (I.help unlink)
   )

names :: InputPattern
names = InputPattern "names" []
  [(Required, exactDefinitionQueryArg)]
  "`names foo` shows the hash and all known names for `foo`."
  (\case
    [thing] -> case HQ.fromString thing of
      Just hq -> Right $ Input.NamesI hq
      Nothing -> Left $ "I was looking for one of these forms: "
                       <> P.blue "foo .foo.bar foo#abc #abcde .foo.bar#asdf"
    _ -> Left (I.help names)
  )

validInputs :: [InputPattern]
validInputs =
  [ help
  , add
  , update
  , forkLocal
  , mergeLocal
  , names
  , push
  , pull
  , cd
  , deleteBranch
  , renameBranch
  , deletePatch
  , renamePatch
  , copyPatch
  , find
  , findVerbose
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
  , mergeBuiltins
  ]

commandNames :: [String]
commandNames = validInputs >>= \i -> I.patternName i : I.aliases i

commandNameArg :: ArgumentType
commandNameArg =
  ArgumentType "command" $ \q _ _ _ -> pure (exactComplete q (commandNames <> Map.keys helpTopics))

fuzzyDefinitionQueryArg :: ArgumentType
fuzzyDefinitionQueryArg =
  -- todo: improve this
  ArgumentType "fuzzy definition query" $
    bothCompletors (termCompletor fuzzyComplete)
                   (typeCompletor fuzzyComplete)

-- todo: support absolute paths?
exactDefinitionQueryArg :: ArgumentType
exactDefinitionQueryArg =
  ArgumentType "definition query" $
    bothCompletors (termCompletor exactComplete)
                   (typeCompletor exactComplete)

exactDefinitionTypeQueryArg :: ArgumentType
exactDefinitionTypeQueryArg =
  ArgumentType "term definition query" $ typeCompletor exactComplete

exactDefinitionTermQueryArg :: ArgumentType
exactDefinitionTermQueryArg =
  ArgumentType "term definition query" $ termCompletor exactComplete

typeCompletor :: Applicative m
              => (String -> [String] -> [Completion])
              -> String
              -> Codebase m v a
              -> Branch.Branch m
              -> Path.Absolute
              -> m [Completion]
typeCompletor filterQuery = pathCompletor filterQuery go where
  go = Set.map HQ'.toText . R.dom . Names.types . Names.names0ToNames . Branch.toNames0

termCompletor :: Applicative m
              => (String -> [String] -> [Completion])
              -> String
              -> Codebase m v a
              -> Branch.Branch m
              -> Path.Absolute
              -> m [Completion]
termCompletor filterQuery = pathCompletor filterQuery go where
  go = Set.map HQ'.toText . R.dom . Names.terms . Names.names0ToNames . Branch.toNames0

patchArg :: ArgumentType
patchArg = ArgumentType "patch" $
  pathCompletor exactComplete (Set.map Name.toText . Branch.deepEdits)

bothCompletors
  :: (Monad m, Ord a)
  => (t1 -> t2 -> t3 -> t4 -> m [a])
  -> (t1 -> t2 -> t3 -> t4 -> m [a])
  -> t1 -> t2 -> t3 -> t4 -> m [a]
bothCompletors c1 c2 q0 code b currentPath = do
  suggestions1 <- c1 q0 code b currentPath
  suggestions2 <- c2 q0 code b currentPath
  pure . nubOrd $ suggestions1 ++ suggestions2

pathCompletor
  :: Applicative f
  => (String -> [String] -> [Completion])
  -> (Branch.Branch0 m -> Set Text)
  -> String
  -> codebase
  -> Branch.Branch m
  -> Path.Absolute
  -> f [Completion]
pathCompletor filterQuery getNames query _code b p = let
  b0root = Branch.head b
  b0local = Branch.getAt0 (Path.unabsolute p) b0root
  -- todo: if these sets are huge, maybe trim results
  in pure . filterQuery query . map Text.unpack $
       toList (getNames b0local) ++
       if "." `isPrefixOf` query then
         map ("." <>) (toList (getNames b0root))
       else
         []

pathArg :: ArgumentType
pathArg = ArgumentType "path" $
  pathCompletor exactComplete (Set.map Path.toText . Branch.deepPaths)

noCompletions :: ArgumentType
noCompletions = ArgumentType "word" I.noSuggestions

gitUrlArg :: ArgumentType
gitUrlArg = noCompletions { I.typeName = "git-url" }

collectNothings :: (a -> Maybe b) -> [a] -> [a]
collectNothings f as = [ a | (Nothing, a) <- map f as `zip` as ]
