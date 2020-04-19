{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Unison.CommandLine.InputPatterns where

import Unison.Prelude

import qualified Control.Lens.Cons as Cons
import Data.Bifunctor (first, second)
import Data.List (isPrefixOf)
import Data.List.Extra (nubOrdOn)
import qualified System.Console.Haskeline.Completion as Completion
import System.Console.Haskeline.Completion (Completion(Completion))
import Unison.Codebase (Codebase)
import Unison.Codebase.Editor.Input (Input)
import qualified Unison.Codebase.Editor.HelpTopics as HT
import Unison.CommandLine.InputPattern
         ( ArgumentType(..)
         , InputPattern(InputPattern)
         , IsOptional(..)
         )
import Unison.CommandLine
import Unison.Util.Monoid (intercalateMap)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Text.Megaparsec as P
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
import qualified Unison.Codebase.Editor.UriParser as UriParser
import Unison.Codebase.Editor.RemoteRepo (RemoteNamespace)

patternName :: InputPattern -> P.Pretty P.ColorText
patternName = fromString . I.patternName

-- `example list ["foo", "bar"]` (haskell) becomes `list foo bar` (pretty)
makeExample, makeExampleNoBackticks :: InputPattern -> [P.Pretty CT.ColorText] -> P.Pretty CT.ColorText
makeExample p args = P.group . backtick $ makeExampleNoBackticks p args

makeExampleNoBackticks p args =
  P.group $ intercalateMap " " id (P.nonEmpty $ fromString (I.patternName p) : args)

makeExample' :: InputPattern -> P.Pretty CT.ColorText
makeExample' p = makeExample p []

makeExampleEOS ::
  InputPattern -> [P.Pretty CT.ColorText] -> P.Pretty CT.ColorText
makeExampleEOS p args = P.group $
  backtick (intercalateMap " " id (P.nonEmpty $ fromString (I.patternName p) : args)) <> "."

helpFor :: InputPattern -> Either (P.Pretty CT.ColorText) Input
helpFor p = I.parse help [I.patternName p]

mergeBuiltins :: InputPattern
mergeBuiltins = InputPattern "builtins.merge" [] []
  "Adds the builtins to `builtins.` in the current namespace (excluding `io` and misc)."
  (const . pure $ Input.MergeBuiltinsI)

mergeIOBuiltins :: InputPattern
mergeIOBuiltins = InputPattern "builtins.mergeio" [] []
  "Adds all the builtins to `builtins.` in the current namespace, including `io` and misc."
  (const . pure $ Input.MergeIOBuiltinsI)

updateBuiltins :: InputPattern
updateBuiltins = InputPattern
  "builtins.update"
  []
  []
  (  "Adds all the builtins that are missing from this namespace, "
  <> "and deprecate the ones that don't exist in this version of Unison."
  )
  (const . pure $ Input.UpdateBuiltinsI)

todo :: InputPattern
todo = InputPattern
  "todo"
  []
  [(Optional, patchArg), (Optional, pathArg)]
  (P.wrapColumn2
    [ ( makeExample' todo
      , "lists the refactor work remaining in the default patch for the current"
        <> " namespace."
      )
    , ( makeExample todo ["<patch>"]
      , "lists the refactor work remaining in the given patch in the current "
        <> "namespace."
      )
    , ( makeExample todo ["<patch>", "[path]"]
      , "lists the refactor work remaining in the given patch in given namespace."
      )
    ]
  )
  (\case
    patchStr : ws -> mapLeft (warn . fromString) $ do
      patch  <- Path.parseSplit' Path.wordyNameSegment patchStr
      branch <- case ws of
        []        -> pure Path.relativeEmpty'
        [pathStr] -> Path.parsePath' pathStr
        _         -> Left "`todo` just takes a patch and one optional namespace"
      Right $ Input.TodoI (Just patch) branch
    [] -> Right $ Input.TodoI Nothing Path.relativeEmpty'
  )

load :: InputPattern
load = InputPattern
  "load"
  []
  [(Optional, noCompletions)]
  (P.wrapColumn2
    [ ( makeExample' load
      , "parses, typechecks, and evaluates the most recent scratch file."
      )
    , (makeExample load ["<scratch file>"]
      , "parses, typechecks, and evaluates the given scratch file."
      )
    ]
  )
  (\case
    [] -> pure $ Input.LoadI Nothing
    [file] -> pure $ Input.LoadI . Just $ file
    _ -> pure $ Input.HelpI (Just $ I.patternName load) True
  )

add :: InputPattern
add =
  InputPattern
      "add"
      []
      [(ZeroPlus, noCompletions)]
      ("`add` adds to the codebase all the definitions from the most recently "
      <> "typechecked file."
      )
    $ \ws -> case traverse HQ'.fromString ws of
        Just ws -> pure $ Input.AddI ws
        Nothing ->
          Left
            . warn
            . P.lines
            . fmap fromString
            . ("I don't know what these refer to:\n" :)
            $ collectNothings HQ'.fromString ws

previewAdd :: InputPattern
previewAdd =
  InputPattern
      "add.preview"
      []
      [(ZeroPlus, noCompletions)]
      ("`add.preview` previews additions to the codebase from the most recently "
      <> "typechecked file. This command only displays cached typechecking "
      <> "results. Use `load` to reparse & typecheck the file if the context "
      <> "has changed."
      )
    $ \ws -> case traverse HQ'.fromString ws of
        Just ws -> pure $ Input.PreviewAddI ws
        Nothing ->
          Left
            . warn
            . P.lines
            . fmap fromString
            . ("I don't know what these refer to:\n" :)
            $ collectNothings HQ'.fromString ws

update :: InputPattern
update = InputPattern "update"
  []
  [(Optional, patchArg)
  ,(ZeroPlus, noCompletions)]
  (P.wrap (makeExample' update <> "works like"
      <> P.group (makeExample' add <> ",")
      <> "except that if a definition in the file has the same name as an"
      <> "existing definition, the name gets updated to point to the new"
      <> "definition. If the old definition has any dependents, `update` will"
      <> "add those dependents to a refactoring session, specified by an"
      <> "optional patch.")
   <> P.wrapColumn2
    [ (makeExample' update
      , "adds all definitions in the .u file, noting replacements in the"
       <> "default patch for the current namespace.")
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

previewUpdate :: InputPattern
previewUpdate =
  InputPattern
      "update.preview"
      []
      [(ZeroPlus, noCompletions)]
      ("`update.preview` previews updates to the codebase from the most "
      <> "recently typechecked file. This command only displays cached "
      <> "typechecking results. Use `load` to reparse & typecheck the file if "
      <> "the context has changed."
      )
    $ \ws -> case traverse HQ'.fromString ws of
        Just ws -> pure $ Input.PreviewUpdateI ws
        Nothing ->
          Left
            . warn
            . P.lines
            . fmap fromString
            . ("I don't know what these refer to:\n" :)
            $ collectNothings HQ'.fromString ws

patch :: InputPattern
patch = InputPattern
  "patch"
  []
  [(Required, patchArg), (Optional, pathArg)]
  (  P.wrap
  $  makeExample' patch
  <> "rewrites any definitions that depend on "
  <> "definitions with type-preserving edits to use the updated versions of"
  <> "these dependencies."
  )
  (\case
    patchStr : ws -> first fromString $ do
      patch  <- Path.parseSplit' Path.wordyNameSegment patchStr
      branch <- case ws of
        [pathStr] -> Path.parsePath' pathStr
        _         -> pure Path.relativeEmpty'
      pure $ Input.PropagatePatchI patch branch
    [] ->
      Left
        $  warn
        $  makeExample' patch
        <> "takes a patch and an optional namespace."
  )

view :: InputPattern
view = InputPattern
  "view"
  []
  [(OnePlus, definitionQueryArg)]
  "`view foo` prints the definition of `foo`."
  ( fmap (Input.ShowDefinitionI Input.ConsoleLocation)
  . traverse parseHashQualifiedName
  )

display :: InputPattern
display = InputPattern
  "display"
  []
  [(Required, definitionQueryArg)]
  "`display foo` prints a rendered version of the term `foo`."
  (\case
    [s] -> Input.DisplayI Input.ConsoleLocation <$> parseHashQualifiedName s
    _ -> pure $ Input.HelpI (Just $ I.patternName display) True
  )


displayTo :: InputPattern
displayTo = InputPattern
  "display.to"
  []
  [(Required, noCompletions), (Required, definitionQueryArg)]
  (  P.wrap
  $  makeExample displayTo ["<filename>", "foo"]
  <> "prints a rendered version of the term `foo` to the given file."
  )
  (\case
    [file, s] ->
      Input.DisplayI (Input.FileLocation file) <$> parseHashQualifiedName s
    _ -> pure $ Input.HelpI (Just $ I.patternName displayTo) True
  )

docs :: InputPattern
docs = InputPattern "docs" [] [(Required, definitionQueryArg)]
      "`docs foo` shows documentation for the definition `foo`."
      (\case
        [s] -> first fromString $ Input.DocsI <$> Path.parseHQSplit' s
        _ -> pure $ Input.HelpI (Just $ I.patternName docs) True
    )

undo :: InputPattern
undo = InputPattern "undo" [] []
      "`undo` reverts the most recent change to the codebase."
      (const $ pure Input.UndoI)

viewByPrefix :: InputPattern
viewByPrefix = InputPattern
  "view.recursive"
  []
  [(OnePlus, definitionQueryArg)]
  "`view.recursive Foo` prints the definitions of `Foo` and `Foo.blah`."
  ( fmap (Input.ShowDefinitionByPrefixI Input.ConsoleLocation)
  . traverse parseHashQualifiedName
  )

find :: InputPattern
find = InputPattern
  "find"
  []
  [(ZeroPlus, fuzzyDefinitionQueryArg)]
  (P.wrapColumn2
    [ ("`find`", "lists all definitions in the current namespace.")
    , ( "`find foo`"
      , "lists all definitions with a name similar to 'foo' in the current "
        <> "namespace."
      )
    , ( "`find foo bar`"
      , "lists all definitions with a name similar to 'foo' or 'bar' in the "
        <> "current namespace."
      )
    ]
  )
  (pure . Input.SearchByNameI False False)

findShallow :: InputPattern
findShallow = InputPattern
  "list"
  ["ls"]
  [(Optional, pathArg)]
  (P.wrapColumn2
    [ ("`list`", "lists definitions and namespaces at the current level of the current namespace.")
    , ( "`list foo`", "lists the 'foo' namespace." )
    , ( "`list .foo`", "lists the '.foo' namespace." )
    ]
  )
  (\case
    [] -> pure $ Input.FindShallowI Path.relativeEmpty'
    [path] -> first fromString $ do
      p <- Path.parsePath' path
      pure $ Input.FindShallowI p
    _ -> pure $ Input.HelpI (Just $ I.patternName findShallow) True
  )

findVerbose :: InputPattern
findVerbose = InputPattern
  "find.verbose"
  ["list.verbose", "ls.verbose"]
  [(ZeroPlus, fuzzyDefinitionQueryArg)]
  (  "`find.verbose` searches for definitions like `find`, but includes hashes "
  <> "and aliases in the results."
  )
  (pure . Input.SearchByNameI True False)

findPatch :: InputPattern
findPatch = InputPattern
  "find.patch"
  ["list.patch", "ls.patch"]
  []
  (P.wrapColumn2
    [("`find.patch`", "lists all patches in the current namespace.")]
  )
  (pure . const Input.FindPatchI)

renameTerm :: InputPattern
renameTerm = InputPattern "move.term" ["rename.term"]
    [(Required, exactDefinitionTermQueryArg)
    ,(Required, newNameArg)]
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
    ,(Required, newNameArg)]
    "`move.type foo bar` renames `foo` to `bar`."
    (\case
      [oldName, newName] -> first fromString $ do
        src <- Path.parseHQSplit' oldName
        target <- Path.parseSplit' Path.definitionNameSegment newName
        pure $ Input.MoveTypeI src target
      _ -> Left . P.warnCallout $ P.wrap
        "`rename.type` takes two arguments, like `rename.type oldname newname`.")

delete :: InputPattern
delete = InputPattern "delete" []
    [(OnePlus, definitionQueryArg)]
    "`delete foo` removes the term or type name `foo` from the namespace."
    (\case
      [query] -> first fromString $ do
        p <- Path.parseHQSplit' query
        pure $ Input.DeleteI p
      _ -> Left . P.warnCallout $ P.wrap
        "`delete` takes an argument, like `delete name`."
    )

deleteTerm :: InputPattern
deleteTerm = InputPattern "delete.term" []
    [(OnePlus, exactDefinitionTermQueryArg)]
    "`delete.term foo` removes the term name `foo` from the namespace."
    (\case
      [query] -> first fromString $ do
        p <- Path.parseHQSplit' query
        pure $ Input.DeleteTermI p
      _ -> Left . P.warnCallout $ P.wrap
        "`delete.term` takes an argument, like `delete.term name`."
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
        "`delete.type` takes an argument, like `delete.type name`."
    )

deleteTermReplacementCommand :: String
deleteTermReplacementCommand = "delete.term-replacement"

deleteTypeReplacementCommand :: String
deleteTypeReplacementCommand = "delete.type-replacement"

deleteReplacement :: Bool -> InputPattern
deleteReplacement isTerm = InputPattern
  commandName
  []
  [(Required, if isTerm then exactDefinitionTermQueryArg else exactDefinitionTypeQueryArg), (Optional, patchArg)]
  (  P.string
  $  commandName
  <> " <patch>` removes any edit of the "
  <> str
  <> " `foo` "
  <> "from the patch `patch`, or the default patch if none is specified."
  )
  (\case
    query : patch -> do
      patch <-
        first fromString
        . traverse (Path.parseSplit' Path.wordyNameSegment)
        $ listToMaybe patch
      q <- parseHashQualifiedName query
      pure $ input q patch
    _ ->
      Left
        .  P.warnCallout
        .  P.wrapString
        $  commandName
        <> " needs arguments. See `help "
        <> commandName
        <> "`."
  )
 where
  input = if isTerm
    then Input.RemoveTermReplacementI
    else Input.RemoveTypeReplacementI
  str         = if isTerm then "term" else "type"
  commandName = if isTerm
    then deleteTermReplacementCommand
    else deleteTypeReplacementCommand

deleteTermReplacement :: InputPattern
deleteTermReplacement = deleteReplacement True

deleteTypeReplacement :: InputPattern
deleteTypeReplacement = deleteReplacement False

parseHashQualifiedName
  :: String -> Either (P.Pretty CT.ColorText) HQ.HashQualified
parseHashQualifiedName s =
  maybe
      (  Left
      .  P.warnCallout
      .  P.wrap
      $  P.string s
      <> " is not a well-formed name, hash, or hash-qualified name. "
      <> "I expected something like `foo`, `#abc123`, or `foo#abc123`."
      )
      Right
    $ HQ.fromString s

aliasTerm :: InputPattern
aliasTerm = InputPattern "alias.term" []
    [(Required, exactDefinitionTermQueryArg), (Required, newNameArg)]
    "`alias.term foo bar` introduces `bar` with the same definition as `foo`."
    (\case
      [oldName, newName] -> first fromString $ do
        source <- Path.parseShortHashOrHQSplit' oldName
        target <- Path.parseSplit' Path.definitionNameSegment newName
        pure $ Input.AliasTermI source target
      _ -> Left . warn $ P.wrap
        "`alias.term` takes two arguments, like `alias.term oldname newname`."
    )

aliasType :: InputPattern
aliasType = InputPattern "alias.type" []
    [(Required, exactDefinitionTypeQueryArg), (Required, newNameArg)]
    "`alias.type Foo Bar` introduces `Bar` with the same definition as `Foo`."
    (\case
      [oldName, newName] -> first fromString $ do
        source <- Path.parseShortHashOrHQSplit' oldName
        target <- Path.parseSplit' Path.definitionNameSegment newName
        pure $ Input.AliasTypeI source target
      _ -> Left . warn $ P.wrap
        "`alias.type` takes two arguments, like `alias.type oldname newname`."
    )

aliasMany :: InputPattern
aliasMany = InputPattern "alias.many" ["copy"]
  [(Required, definitionQueryArg), (OnePlus, exactDefinitionOrPathArg)]
  (P.group . P.lines $
    [ P.wrap $ P.group (makeExample aliasMany ["<relative1>", "[relative2...]", "<namespace>"])
      <> "creates aliases `relative1`, `relative2`, ... in the namespace `namespace`."
    , P.wrap $ P.group (makeExample aliasMany ["foo.foo", "bar.bar", ".quux"])
      <> "creates aliases `.quux.foo.foo` and `.quux.bar.bar`."
    ])
  (\case
    srcs@(_:_) Cons.:> dest -> first fromString $ do
      sourceDefinitions <- traverse Path.parseHQSplit srcs
      destNamespace <- Path.parsePath' dest
      pure $ Input.AliasManyI sourceDefinitions destNamespace
    _ -> pure $ Input.HelpI (Just $ I.patternName aliasMany) True
  )


cd :: InputPattern
cd = InputPattern "namespace" ["cd", "j"] [(Required, pathArg)]
    (P.wrapColumn2
      [ (makeExample cd ["foo.bar"],
          "descends into foo.bar from the current namespace.")
      , (makeExample cd [".cat.dog"],
          "sets the current namespace to the abolute namespace .cat.dog.") ])
    (\case
      [p] -> first fromString $ do
        p <- Path.parsePath' p
        pure . Input.SwitchBranchI $ p
      _ -> pure $ Input.HelpI (Just $ I.patternName cd) True
    )

back :: InputPattern
back = InputPattern "back" ["popd"] []
    (P.wrapColumn2
      [ (makeExample back [],
          "undoes the last" <> makeExample' cd <> "command.")
      ])
    (\case
      [] -> pure Input.PopBranchI
      _ -> pure $ Input.HelpI (Just $ I.patternName cd) True
    )

deleteBranch :: InputPattern
deleteBranch = InputPattern "delete.namespace" [] [(Required, pathArg)]
  "`delete.namespace <foo>` deletes the namespace `foo`"
   (\case
        ["."] -> first fromString .
          pure $ Input.DeleteBranchI Nothing
        [p] -> first fromString $ do
          p <- Path.parseSplit' Path.wordyNameSegment p
          pure . Input.DeleteBranchI $ Just p
        _ -> pure $ Input.HelpI (Just $ I.patternName deleteBranch) True
      )

deletePatch :: InputPattern
deletePatch = InputPattern "delete.patch" [] [(Required, patchArg)]
  "`delete.patch <foo>` deletes the patch `foo`"
   (\case
        [p] -> first fromString $ do
          p <- Path.parseSplit' Path.wordyNameSegment p
          pure . Input.DeletePatchI $ p
        _ -> pure $ Input.HelpI (Just $ I.patternName deletePatch) True
      )

movePatch :: String -> String -> Either (P.Pretty CT.ColorText) Input
movePatch src dest = first fromString $ do
  src <- Path.parseSplit' Path.wordyNameSegment src
  dest <- Path.parseSplit' Path.wordyNameSegment dest
  pure $ Input.MovePatchI src dest

copyPatch :: InputPattern
copyPatch = InputPattern "copy.patch"
   []
   [(Required, patchArg), (Required, newNameArg)]
   "`copy.patch foo bar` copies the patch `bar` to `foo`."
    (\case
      [src, dest] -> movePatch src dest
      _ -> pure $ Input.HelpI (Just $ I.patternName copyPatch) True
    )

renamePatch :: InputPattern
renamePatch = InputPattern "move.patch"
   ["rename.patch"]
   [(Required, patchArg), (Required, newNameArg)]
   "`move.patch foo bar` renames the patch `bar` to `foo`."
    (\case
      [src, dest] -> movePatch src dest
      _ -> pure $ Input.HelpI (Just $ I.patternName renamePatch) True
    )

renameBranch :: InputPattern
renameBranch = InputPattern "move.namespace"
   ["rename.namespace"]
   [(Required, pathArg), (Required, newNameArg)]
   "`move.namespace foo bar` renames the path `bar` to `foo`."
    (\case
      [".", dest] -> first fromString $ do
        dest <- Path.parseSplit' Path.wordyNameSegment dest
        pure $ Input.MoveBranchI Nothing dest
      [src, dest] -> first fromString $ do
        src <- Path.parseSplit' Path.wordyNameSegment src
        dest <- Path.parseSplit' Path.wordyNameSegment dest
        pure $ Input.MoveBranchI (Just src) dest
      _ -> pure $ Input.HelpI (Just $ I.patternName renameBranch) True
    )

history :: InputPattern
history = InputPattern "history" []
   [(Optional, pathArg)]
   (P.wrapColumn2 [
     (makeExample history [], "Shows the history of the current path."),
     (makeExample history [".foo"], "Shows history of the path .foo."),
     (makeExample history ["#9dndk3kbsk13nbpeu"],
       "Shows the history of the namespace with the given hash." <>
       "The full hash must be provided.")
     ])
    (\case
      [src] -> first fromString $ do
        p <- Input.parseBranchId src
        pure $ Input.HistoryI (Just 10) (Just 10) p
      [] -> pure $ Input.HistoryI (Just 10) (Just 10) (Right Path.currentPath)
      _ -> pure $ Input.HelpI (Just $ I.patternName history) True
    )

forkLocal :: InputPattern
forkLocal = InputPattern "fork" ["copy.namespace"] [(Required, pathArg)
                                   ,(Required, newNameArg)]
    (makeExample forkLocal ["src", "dest"] <> "creates the namespace `dest` as a copy of `src`.")
    (\case
      [src, dest] -> first fromString $ do
        src <- Input.parseBranchId src
        dest <- Path.parsePath' dest
        pure $ Input.ForkLocalBranchI src dest
      _ -> pure $ Input.HelpI (Just $ I.patternName forkLocal) True
    )

resetRoot :: InputPattern
resetRoot = InputPattern "reset-root" [] [(Required, pathArg)]
  (P.wrapColumn2 [
    (makeExample resetRoot [".foo"],
      "Reset the root namespace (along with its history) to that of the `.foo` namespace."),
    (makeExample resetRoot ["#9dndk3kbsk13nbpeu"],
      "Reset the root namespace (along with its history) to that of the namespace with hash `#9dndk3kbsk13nbpeu`.")
    ])
  (\case
    [src] -> first fromString $ do
     src <- Input.parseBranchId src
     pure $ Input.ResetRootI src
    _ -> pure $ Input.HelpI (Just $ I.patternName resetRoot) True
  )

pull :: InputPattern
pull = InputPattern
  "pull"
  []
  [(Optional, gitUrlArg), (Optional, pathArg)]
  (P.lines
    [ P.wrap
      "The `pull` command merges a remote namespace into a local namespace."
    , ""
    , P.wrapColumn2
      [ ( "`pull remote local`"
        , "merges the remote namespace `remote`"
        <>"into the local namespace `local`."
        )
      , ( "`pull remote`"
        , "merges the remote namespace `remote`"
        <>"into the current namespace")
      , ( "`pull`"
        , "merges the remote namespace configured in `.unisonConfig`"
        <> "with the key `GitUrl.ns` where `ns` is the current namespace,"
        <> "into the current namespace")
      ]
    , ""
    , P.wrap "where `remote` is a git repository, optionally followed by `:`"
    <> "and an absolute remote path, such as:"
    , P.indentN 2 . P.lines $
      [P.backticked "https://github.com/org/repo"
      ,P.backticked "https://github.com/org/repo:.some.remote.path"
      ]
    ]
  )
  (\case
    []    -> Right $ Input.PullRemoteBranchI Nothing Path.relativeEmpty'
    [url] -> do
      ns <- first (fromString . show)
              (P.parse UriParser.repoPath "url" (Text.pack url))
      Right $ Input.PullRemoteBranchI (Just ns) Path.relativeEmpty'
    [url, path] -> do
      ns <- first (fromString . show)
              (P.parse UriParser.repoPath "url" (Text.pack url))
      p <- first fromString $ Path.parsePath' path
      pure $ Input.PullRemoteBranchI (Just ns) p
    _ -> pure $ Input.HelpI (Just $ I.patternName pull) True
  )

push :: InputPattern
push = InputPattern
  "push"
  []
  [(Required, gitUrlArg), (Optional, pathArg)]
  (P.lines
    [ P.wrap
      "The `push` command merges a local namespace into a remote namespace."
    , ""
    , P.wrapColumn2
      [ ( "`push remote local`"
        , "merges the contents of the local namespace `local`"
          <>  "into the remote namespace `remote`."
        )
      , ( "`push remote`"
        , "publishes the current namespace into the remote namespace `remote`")
      , ( "`push`"
        , "publishes the current namespace"
        <> "into the remote namespace configured in `.unisonConfig`"
        <> "with the key `GitUrl.ns` where `ns` is the current namespace")
      ]
    , ""
    , P.wrap "where `remote` is a git repository, optionally followed by `:`"
    <> "and an absolute remote path, such as:"
    , P.indentN 2 . P.lines $
      [P.backticked "https://github.com/org/repo"
      ,P.backticked "https://github.com/org/repo:.some.remote.path"
      ]
    ]
  )
  (\case
    []    -> Right $ Input.PushRemoteBranchI Nothing Path.relativeEmpty'
    url : rest -> do
      (repo, sbh, path) <- first (fromString . show)
        (P.parse UriParser.repoPath "url" (Text.pack url))
      when (isJust sbh)
        $ Left "Can't push to a particular remote namespace hash."
      case rest of
        [] -> Right $ Input.PushRemoteBranchI (Just (repo, path)) Path.relativeEmpty'
        [path'] -> first fromString $ Path.parsePath' path' >>= 
          Right . Input.PushRemoteBranchI (Just (repo, path))
        _ -> Right $ Input.HelpI (Just $ I.patternName push) True
  )

createPullRequest :: InputPattern
createPullRequest = InputPattern "pull-request.create" ["pr.create"]
  [(Required, gitUrlArg), (Required, gitUrlArg), (Optional, pathArg)]
  (P.group $ P.lines
    [ P.wrap $ makeExample createPullRequest ["base", "head"]
        <> "will generate a request to merge the remote repo `head`"
        <> "into the remote repo `base`."
    , ""
    , "example: " <>
      makeExampleNoBackticks createPullRequest ["https://github.com/unisonweb/base",
                                                "https://github.com/me/unison:.libs.pr.base" ]
    ])
  (\case
    [baseUrl, headUrl] -> first fromString $ do
      baseRepo <- parseUri "baseRepo" baseUrl
      headRepo <- parseUri "headRepo" headUrl
      pure $ Input.CreatePullRequestI baseRepo headRepo
    _ -> pure $ Input.HelpI (Just $ I.patternName createPullRequest) True
  )

loadPullRequest :: InputPattern
loadPullRequest = InputPattern "pull-request.load" ["pr.load"]
  [(Required, gitUrlArg), (Required, gitUrlArg), (Optional, pathArg)]
  (P.lines
   [P.wrap $ makeExample loadPullRequest ["base", "head"]
    <> "will load a pull request for merging the remote repo `head` into the"
    <> "remote repo `base`, staging each in the current namespace"
    <> "(so make yourself a clean spot to work first)."
   ,P.wrap $ makeExample loadPullRequest ["base", "head", "dest"]
     <> "will load a pull request for merging the remote repo `head` into the"
     <> "remote repo `base`, staging each in `dest`, which must be empty."
   ])
  (\case
    [baseUrl, headUrl] -> first fromString $ do
      baseRepo <- parseUri "baseRepo" baseUrl
      headRepo <- parseUri "topicRepo" headUrl
      pure $ Input.LoadPullRequestI baseRepo headRepo Path.relativeEmpty'
    [baseUrl, headUrl, dest] -> first fromString $ do
      baseRepo <- parseUri "baseRepo" baseUrl
      headRepo <- parseUri "topicRepo" headUrl
      destPath <- Path.parsePath' dest
      pure $ Input.LoadPullRequestI baseRepo headRepo destPath
    _ -> pure $ Input.HelpI (Just $ I.patternName loadPullRequest) True
  )
parseUri :: IsString b => String -> String -> Either b RemoteNamespace
parseUri label input =
  first (fromString . show)
    (P.parse UriParser.repoPath label (Text.pack input))

mergeLocal :: InputPattern
mergeLocal = InputPattern "merge" [] [(Required, pathArg)
                                     ,(Optional, pathArg)]
 (P.column2 [
   ("`merge src`", "merges `src` namespace into the current namespace"),
   ("`merge src dest`", "merges `src` namespace into the `dest` namespace")])
 (\case
      [src] -> first fromString $ do
        src <- Path.parsePath' src
        pure $ Input.MergeLocalBranchI src Path.relativeEmpty'
      [src, dest] -> first fromString $ do
        src <- Path.parsePath' src
        dest <- Path.parsePath' dest
        pure $ Input.MergeLocalBranchI src dest
      _ -> pure $ Input.HelpI (Just $ I.patternName mergeLocal) True
 )

diffNamespace :: InputPattern
diffNamespace = InputPattern
  "diff.namespace"
  []
  [(Required, pathArg), (Required, pathArg)]
  (P.column2
    [ ( "`diff.namespace before after`"
      , P.wrap
        "shows how the namespace `after` differs from the namespace `before`"
      )
    ]
  )
  (\case
    [before, after] -> first fromString $ do
      before <- Path.parsePath' before
      after <- Path.parsePath' after
      pure $ Input.DiffNamespaceI before after
    _ -> pure $ Input.HelpI (Just $ I.patternName diffNamespace) True
  )

previewMergeLocal :: InputPattern
previewMergeLocal = InputPattern
  "merge.preview"
  []
  [(Required, pathArg), (Optional, pathArg)]
  (P.column2
    [ ( "`merge.preview src`"
      , "shows how the current namespace will change after a `merge src`."
      )
    , ( "`merge.preview src dest`"
      , "shows how `dest` namespace will change after a `merge src dest`."
      )
    ]
  )
  (\case
    [src] -> first fromString $ do
      src <- Path.parsePath' src
      pure $ Input.PreviewMergeLocalBranchI src Path.relativeEmpty'
    [src, dest] -> first fromString $ do
      src  <- Path.parsePath' src
      dest <- Path.parsePath' dest
      pure $ Input.PreviewMergeLocalBranchI src dest
    _ -> pure $ Input.HelpI (Just $ I.patternName previewMergeLocal) True
  )

replaceEdit
  :: (HQ.HashQualified -> HQ.HashQualified -> Maybe Input.PatchPath -> Input)
  -> String
  -> InputPattern
replaceEdit f s = self
 where
  self = InputPattern
    ("replace." <> s)
    []
    [ (Required, definitionQueryArg)
    , (Required, definitionQueryArg)
    , (Optional, patchArg)
    ]
    (P.wrapColumn2
      [ ( makeExample self ["<from>", "<to>", "<patch>"]
        , "Replace the "
        <> P.string s
        <> " <from> in the given patch "
        <> "with the "
        <> P.string s
        <> " <to>."
        )
      , ( makeExample self ["<from>", "<to>"]
        , "Replace the "
        <> P.string s
        <> "<from> with <to> in the default patch."
        )
      ]
    )
    (\case
      source : target : patch -> do
        patch <-
          first fromString
          <$> traverse (Path.parseSplit' Path.wordyNameSegment)
          $   listToMaybe patch
        sourcehq <- parseHashQualifiedName source
        targethq <- parseHashQualifiedName target
        pure $ f sourcehq targethq patch
      _ -> pure $ Input.HelpI (Just $ I.patternName self) True
    )

replaceType :: InputPattern
replaceType = replaceEdit Input.ReplaceTypeI "type"

replaceTerm :: InputPattern
replaceTerm = replaceEdit Input.ReplaceTermI "term"

viewReflog :: InputPattern
viewReflog = InputPattern
  "reflog"
  []
  []
  "`reflog` lists the changes that have affected the root namespace"
  (\case
    [] -> pure Input.ShowReflogI
    _  -> Left . warn . P.string
              $ I.patternName viewReflog ++ " doesn't take any arguments.")

edit :: InputPattern
edit = InputPattern
  "edit"
  []
  [(OnePlus, definitionQueryArg)]
  (  "`edit foo` prepends the definition of `foo` to the top of the most "
  <> "recently saved file."
  )
  ( fmap (Input.ShowDefinitionI Input.LatestFileLocation)
  . traverse parseHashQualifiedName
  )

topicNameArg :: ArgumentType
topicNameArg =
  ArgumentType "topic" $ \q _ _ _ -> pure (exactComplete q HT.topics)

helpTopics :: InputPattern
helpTopics = InputPattern
  "help-topics"
  ["help-topic"]
  [(Optional, topicNameArg)]
  ( "`help-topics` lists all topics and `help-topics <topic>` shows an explanation of that topic." )
  (\case
    [] -> Right $ Input.HelpTopicI Nothing
    [topic] -> second (Input.HelpTopicI . Just) $ HT.fromString topic
    _ -> Left $ warn "Use `help-topics <topic>` or `help-topics`."
  )


help :: InputPattern
help = InputPattern
    "help" ["?"] [(Optional, commandNameArg)]
    "`help` shows general help and `help <cmd>` shows help for one command."
    (\case
      [] -> Right $ Input.HelpI Nothing False
      [isHelp -> Right ht] -> Right ht
      [cmd] -> case Map.lookup cmd commandsByName of
        Nothing  -> Left . warn $ "I don't know of that command. Try `help`."
        Just _ -> Right $ Input.HelpI (Just cmd) False
      _ -> Left $ warn "Use `help <cmd>` or `help`.")
    where
      commandsByName = Map.fromList [
        (n, i) | i <- validInputs, n <- I.patternName i : I.aliases i ]
      isHelp topic = second (Input.HelpTopicI . Just) $ HT.fromString topic

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
link = InputPattern
  "link"
  []
  [(Required, definitionQueryArg), (OnePlus, definitionQueryArg)]
  (fromString $ concat
    [ "`link metadata defn` creates a link to `metadata` from `defn`. "
    , "Use `links defn` or `links defn <type>` to view outgoing links, "
    , "and `unlink metadata defn` to remove a link. The `defn` can be either the "
    , "name of a term or type, multiple such names, or a range like `1-4` "
    , "for a range of definitions listed by a prior `find` command."
    ]
  )
  (\case
    md : defs -> first fromString $ do
      md <- case HQ.fromString md of
        Nothing -> Left "Invalid hash qualified identifier for metadata."
        Just hq -> pure hq
      defs <- traverse Path.parseHQSplit' defs
      Right $ Input.LinkI md defs
    _ -> pure $ Input.HelpI (Just $ I.patternName link) True
  )

links :: InputPattern
links = InputPattern
  "links"
  []
  [(Required, definitionQueryArg), (Optional, definitionQueryArg)]
  (P.column2 [
    (makeExample links ["defn"], "shows all outgoing links from `defn`."),
    (makeExample links ["defn", "<type>"], "shows all links of the given type.") ])
  (\case
    src : rest -> first fromString $ do
      src <- Path.parseHQSplit' src
      let ty = case rest of
            [] -> Nothing
            _  -> Just $ unwords rest
       in Right $ Input.LinksI src ty
    _ -> pure $ Input.HelpI (Just $ I.patternName links) True
  )

unlink :: InputPattern
unlink = InputPattern
  "unlink"
  ["delete.link"]
  [(Required, definitionQueryArg), (OnePlus, definitionQueryArg)]
  (fromString $ concat
    [ "`unlink metadata defn` removes a link to `detadata` from `defn`."
    , "The `defn` can be either the "
    , "name of a term or type, multiple such names, or a range like `1-4` "
    , "for a range of definitions listed by a prior `find` command."
    ])
  (\case
    md : defs -> first fromString $ do
      md <- case HQ.fromString md of
        Nothing -> Left "Invalid hash qualified identifier for metadata."
        Just hq -> pure hq
      defs <- traverse Path.parseHQSplit' defs
      Right $ Input.UnlinkI md defs
    _ -> pure $ Input.HelpI (Just $ I.patternName unlink) True
  )

names :: InputPattern
names = InputPattern "names" []
  [(Required, definitionQueryArg)]
  "`names foo` shows the hash and all known names for `foo`."
  (\case
    [thing] -> case HQ.fromString thing of
      Just hq -> Right $ Input.NamesI hq
      Nothing -> Left $ "I was looking for one of these forms: "
                       <> P.blue "foo .foo.bar foo#abc #abcde .foo.bar#asdf"
    _ -> pure $ Input.HelpI (Just $ I.patternName names) True
  )

dependents, dependencies :: InputPattern
dependents = InputPattern "dependents" [] []
  "List the dependents of the specified definition."
  (\case
    [thing] -> fmap Input.ListDependentsI $ parseHashQualifiedName thing
    _ -> pure $ Input.HelpI (Just $ I.patternName dependents) True
  )
dependencies = InputPattern "dependencies" [] []
  "List the dependencies of the specified definition."
  (\case
    [thing] -> fmap Input.ListDependenciesI $ parseHashQualifiedName thing
    _ -> pure $ Input.HelpI (Just $ I.patternName dependencies) True
  )

debugNumberedArgs :: InputPattern
debugNumberedArgs = InputPattern "debug.numberedArgs" [] []
  "Dump the contents of the numbered args state."
  (const $ Right Input.DebugNumberedArgsI)

debugBranchHistory :: InputPattern
debugBranchHistory = InputPattern "debug.history" []
  [(Optional, noCompletions)]
  "Dump codebase history, compatible with bit-booster.com/graph.html"
  (const $ Right Input.DebugBranchHistoryI)
  
debugFileHashes :: InputPattern
debugFileHashes = InputPattern "debug.file" [] []
  "View details about the most recent succesfully typechecked file."
  (const $ Right Input.DebugTypecheckedUnisonFileI)
   
test :: InputPattern
test = InputPattern "test" [] []
    "`test` runs unit tests for the current branch."
    (const $ pure $ Input.TestI True True)

execute :: InputPattern
execute = InputPattern
  "run"
  []
  []
  (P.wrapColumn2
    [ ( "`run mymain`"
      , "Runs `!mymain`, where `mymain` is searched for in the most recent"
        <> "typechecked file, or in the codebase."
      )
    ]
  )
  (\case
    [w] -> pure . Input.ExecuteI $ w
    _   -> pure $ Input.HelpI (Just $ I.patternName execute) True
  )

createAuthor :: InputPattern
createAuthor = InputPattern "create.author" []
  [(Required, noCompletions), (Required, noCompletions)]
  (makeExample createAuthor ["alicecoder", "\"Alice McGee\""]
    <> "creates" <> backtick "alicecoder" <> "values in"
    <> backtick "metadata.authors" <> "and"
    <> backtickEOS "metadata.copyrightHolders")
  (\case
      symbolStr : authorStr@(_:_) -> first fromString $ do
        symbol <- Path.wordyNameSegment symbolStr
        -- let's have a real parser in not too long
        let author :: Text
            author = Text.pack $ case (unwords authorStr) of
              quoted@('"':_) -> (init . tail) quoted
              bare -> bare
        pure $ Input.CreateAuthorI symbol author
      _   -> pure $ Input.HelpI (Just $ I.patternName createAuthor) True
    )
validInputs :: [InputPattern]
validInputs =
  [ help
  , helpTopics
  , load
  , add
  , previewAdd
  , update
  , previewUpdate
  , delete
  , forkLocal
  , mergeLocal
  , previewMergeLocal
  , diffNamespace
  , names
  , push
  , pull
  , createPullRequest
  , loadPullRequest
  , cd
  , back
  , deleteBranch
  , renameBranch
  , deletePatch
  , renamePatch
  , copyPatch
  , find
  , findShallow
  , findVerbose
  , view
  , display
  , displayTo
  , docs
  , findPatch
  , viewPatch
  , undo
  , history
  , edit
  , renameTerm
  , deleteTerm
  , aliasTerm
  , renameType
  , deleteType
  , aliasType
  , aliasMany
  , todo
  , patch
  , link
  , unlink
  , links
  , createAuthor
  , replaceTerm
  , replaceType
  , deleteTermReplacement
  , deleteTypeReplacement
  , test
  , execute
  , viewReflog
  , resetRoot
  , quit
  , updateBuiltins
  , mergeBuiltins
  , mergeIOBuiltins
  , dependents, dependencies
  , debugNumberedArgs
  , debugBranchHistory
  , debugFileHashes
  ]

commandNames :: [String]
commandNames = validInputs >>= \i -> I.patternName i : I.aliases i

commandNameArg :: ArgumentType
commandNameArg =
  ArgumentType "command" $ \q _ _ _ -> pure (exactComplete q (commandNames <> HT.topics))

exactDefinitionOrPathArg :: ArgumentType
exactDefinitionOrPathArg =
  ArgumentType "definition or path" $
    bothCompletors
      (bothCompletors
        (termCompletor exactComplete)
        (typeCompletor exactComplete))
      (pathCompletor exactComplete (Set.map Path.toText . Branch.deepPaths))

fuzzyDefinitionQueryArg :: ArgumentType
fuzzyDefinitionQueryArg =
  -- todo: improve this
  ArgumentType "fuzzy definition query" $
    bothCompletors (termCompletor fuzzyComplete)
                   (typeCompletor fuzzyComplete)

definitionQueryArg :: ArgumentType
definitionQueryArg = fuzzyDefinitionQueryArg { typeName = "definition query" }

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
patchArg = ArgumentType "patch" $ pathCompletor
  exactComplete
  (Set.map Name.toText . Map.keysSet . Branch.deepEdits)

bothCompletors
  :: (Monad m)
  => (String -> t2 -> t3 -> t4 -> m [Completion])
  -> (String -> t2 -> t3 -> t4 -> m [Completion])
  -> String -> t2 -> t3 -> t4 -> m [Completion]
bothCompletors c1 c2 q code b currentPath = do
  suggestions1 <- c1 q code b currentPath
  suggestions2 <- c2 q code b currentPath
  pure . fixupCompletion q
       . nubOrdOn Completion.display
       $ suggestions1 ++ suggestions2

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
pathArg = ArgumentType "namespace" $
  pathCompletor exactComplete (Set.map Path.toText . Branch.deepPaths)

newNameArg :: ArgumentType
newNameArg = ArgumentType "new-name" $
  pathCompletor prefixIncomplete
    (Set.map ((<> ".") . Path.toText) . Branch.deepPaths)

noCompletions :: ArgumentType
noCompletions = ArgumentType "word" I.noSuggestions

-- Arya: I could imagine completions coming from previous git pulls
gitUrlArg :: ArgumentType
gitUrlArg = ArgumentType "git-url" $ \input _ _ _ -> case input of
  "gh" -> complete "https://github.com/"
  "gl" -> complete "https://gitlab.com/"
  "bb" -> complete "https://bitbucket.com/"
  "ghs" -> complete "git@github.com:"
  "gls" -> complete "git@gitlab.com:"
  "bbs" -> complete "git@bitbucket.com:"
  _ -> pure []
  where complete s = pure [Completion s s False]

collectNothings :: (a -> Maybe b) -> [a] -> [a]
collectNothings f as = [ a | (Nothing, a) <- map f as `zip` as ]
