{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Unison.CommandLine.InputPatterns where

import Unison.Prelude

import Data.Bifunctor (first)
import Data.List (intercalate, sortOn, isPrefixOf)
import Data.List.Extra (nubOrdOn)
import qualified System.Console.Haskeline.Completion as Completion
import System.Console.Haskeline.Completion (Completion(Completion))
import Unison.Codebase (Codebase)
import Unison.Codebase.Editor.Input (Input)
import Unison.CommandLine.InputPattern
         ( ArgumentType(..)
         , InputPattern(InputPattern)
         , IsOptional(..)
         )
import Unison.CommandLine
import Unison.Util.Monoid (intercalateMap)
import Unison.ShortHash (ShortHash)
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
import qualified Unison.Codebase.Editor.SlurpResult as SR
import qualified Unison.Codebase.Editor.UriParser as UriParser

showPatternHelp :: InputPattern -> P.Pretty CT.ColorText
showPatternHelp i = P.lines [
  P.bold (fromString $ I.patternName i) <> fromString
    (if not . null $ I.aliases i
     then " (or " <> intercalate ", " (I.aliases i) <> ")"
     else ""),
  P.wrap $ I.help i ]

patternName :: InputPattern -> P.Pretty P.ColorText
patternName = fromString . I.patternName

-- `example list ["foo", "bar"]` (haskell) becomes `list foo bar` (pretty)
makeExample :: InputPattern -> [P.Pretty CT.ColorText] -> P.Pretty CT.ColorText
makeExample p args = P.group $
  backtick (intercalateMap " " id (P.nonEmpty $ fromString (I.patternName p) : args))

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
  "Adds all the builtins to `builtins.` in the current namespace."
  (const . pure $ Input.MergeBuiltinsI)

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
    _ -> Left (I.help load))


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
view = InputPattern "view" [] [(OnePlus, exactDefinitionQueryArg)]
      "`view foo` prints the definition of `foo`."
      (pure . Input.ShowDefinitionI Input.ConsoleLocation)

display :: InputPattern
display = InputPattern "display" [] [(Required, exactDefinitionQueryArg)]
      "`display foo` prints a rendered version of the term `foo`."
      (\case
        [s] -> pure (Input.DisplayI Input.ConsoleLocation s)
        _ -> Left (I.help display))

displayTo :: InputPattern
displayTo = InputPattern "display.to" [] [(Required, noCompletions), (Required, exactDefinitionQueryArg)]
      (P.wrap $ makeExample displayTo ["<filename>", "foo"]
             <> "prints a rendered version of the term `foo` to the given file.")
      (\case
        [file,s] -> pure (Input.DisplayI (Input.FileLocation file) s)
        _ -> Left (I.help displayTo))

docs :: InputPattern
docs = InputPattern "docs" [] [(Required, exactDefinitionQueryArg)]
      ("`docs foo` shows documentation for the definition `foo`.")
      (\case
        [s] -> first fromString $ Input.DocsI <$> Path.parseHQSplit' s
        _ -> Left (I.help docs))

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
    _ -> Left (I.help findShallow)
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
    [(OnePlus, exactDefinitionQueryArg)]
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

aliasTerm :: InputPattern
aliasTerm = InputPattern "alias.term" []
    [(Required, exactDefinitionTermQueryArg), (Required, newNameArg)]
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
    [(Required, exactDefinitionTypeQueryArg), (Required, newNameArg)]
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
      _ -> Left (I.help cd)
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
   [(Required, patchArg), (Required, newNameArg)]
   "`copy.patch foo bar` copies the patch `bar` to `foo`."
    (\case
      [src, dest] -> movePatch src dest
      _ -> Left (I.help copyPatch)
    )

renamePatch :: InputPattern
renamePatch = InputPattern "move.patch"
   ["rename.patch"]
   [(Required, patchArg), (Required, newNameArg)]
   "`move.patch foo bar` renames the patch `bar` to `foo`."
    (\case
      [src, dest] -> movePatch src dest
      _ -> Left (I.help renamePatch)
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
      _ -> Left (I.help renameBranch)
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
      _ -> Left (I.help history)
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
      _ -> Left (I.help forkLocal)
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
    _ -> Left (I.help resetRoot))

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
      , ( "`push`"
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
    _ -> Left (I.help pull)
  )

push :: InputPattern
push = InputPattern
  "push"
  []
  [(Optional, gitUrlArg), (Optional, pathArg)]
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
      p <- case rest of
        [] -> Right Path.relativeEmpty'
        [path] -> first fromString $ Path.parsePath' path
        _ -> Left (I.help push)
      Right $ Input.PushRemoteBranchI (Just (repo, path)) p
  )

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
      _ -> Left (I.help mergeLocal)
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
    _ -> Left $ I.help diffNamespace
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
    _ -> Left (I.help previewMergeLocal)
  )

replaceEdit
  :: (ShortHash -> ShortHash -> Maybe Input.PatchPath -> Input)
  -> String
  -> InputPattern
replaceEdit f s = self
 where
  self = InputPattern
    ("replace." <> s)
    []
    [ (Required, exactDefinitionQueryArg)
    , (Required, exactDefinitionQueryArg)
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
      source : target : patch -> first fromString $ do
        src   <- Path.parseShortHashOrHQSplit' source
        dest  <- Path.parseShortHashOrHQSplit' target
        patch <- traverse (Path.parseSplit' Path.wordyNameSegment)
          $ listToMaybe patch
        sourceH <- maybe (Left (source <> " is not a valid hash."))
                         Right
                         (toHash src)
        targetH <- maybe (Left (target <> " is not a valid hash."))
                         Right
                         (toHash dest)
        pure $ f sourceH targetH patch
      _ -> Left $ I.help self
    )
  toHash :: Either ShortHash Path.HQSplit' -> Maybe ShortHash
  toHash (Left  h      ) = Just h
  toHash (Right (_, hq)) = HQ'.toHash hq

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
  [(OnePlus, exactDefinitionQueryArg)]
  (  "`edit foo` prepends the definition of `foo` to the top of the most "
  <> "recently saved file."
  )
  (pure . Input.ShowDefinitionI Input.LatestFileLocation)

topicNameArg :: ArgumentType
topicNameArg =
  ArgumentType "topic" $ \q _ _ _ -> pure (exactComplete q $ Map.keys helpTopicsMap)

helpTopics :: InputPattern
helpTopics = InputPattern
  "help-topics"
  ["help-topic"]
  [(Optional, topicNameArg)]
  ( "`help-topics` lists all topics and `help-topics <topic>` shows an explanation of that topic." )
  (\case
    [] -> Left topics
    [topic] -> case Map.lookup topic helpTopicsMap of
       Nothing -> Left . warn $ "I don't know of that topic. Try `help-topics`."
       Just t -> Left t
    _ -> Left $ warn "Use `help-topics <topic>` or `help-topics`."
  )
  where
    topics = P.callout "🌻" $ P.lines [
      "Here's a list of topics I can tell you more about: ",
      "",
      P.indentN 2 $ P.sep "\n" (P.string <$> Map.keys helpTopicsMap),
      "",
      aside "Example" "use `help filestatus` to learn more about that topic."
      ]

helpTopicsMap :: Map String (P.Pretty P.ColorText)
helpTopicsMap = Map.fromList [
  ("testcache", testCacheMsg),
  ("filestatus", fileStatusMsg),
  ("messages.disallowedAbsolute", disallowedAbsoluteMsg),
  ("namespaces", pathnamesMsg)
  ]
  where
  blankline = ("","")
  fileStatusMsg = P.callout "📓" . P.lines $ [
    P.wrap $ "Here's a list of possible status messages you might see"
          <> "for definitions in a .u file.", "",
    P.wrapColumn2 [
      (P.bold $ SR.prettyStatus SR.Collision,
       "A definition with the same name as an existing definition. Doing" <>
       "`update` instead of `add` will turn this failure into a successful" <>
       "update."),
      blankline,
      (P.bold $ SR.prettyStatus SR.Conflicted,
       "A definition with the same name as an existing definition." <>
       "Resolving the conflict and then trying an `update` again will" <>
       "turn this into a successful update."),
      blankline,
      (P.bold $ SR.prettyStatus SR.TermExistingConstructorCollision,
       "A definition with the same name as an existing constructor for " <>
       "some data type. Rename your definition or the data type before" <>
       "trying again to `add` or `update`."),
      blankline,
      (P.bold $ SR.prettyStatus SR.ConstructorExistingTermCollision,
       "A type defined in the file has a constructor that's named the" <>
       "same as an existing term. Rename that term or your constructor" <>
       "before trying again to `add` or `update`."),
      blankline,
      (P.bold $ SR.prettyStatus SR.Alias,
       "A definition in the file already has another name." <>
       "You can use the `alias.term` or `alias.type` commands" <>
       "to create new names for existing definitions."),
      blankline,
      (P.bold $ SR.prettyStatus SR.BlockedDependency,
       "This definition was blocked because it dependended on " <>
       "a definition with a failed status."),
      blankline,
      (P.bold $ SR.prettyStatus SR.ExtraDefinition,
       "This definition was added because it was a dependency of" <>
       "a definition explicitly selected.")
      ]
   ]
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
    P.wrap $ "There are two kinds of namespaces," <> P.group (P.blue "absolute" <> ",")
          <> "such as" <> P.group ("(" <> P.blue ".foo.bar")
          <> "or" <> P.group (P.blue ".base.math.+" <> ")")
          <> "and" <> P.group (P.green "relative" <> ",")
          <> "such as" <> P.group ("(" <> P.green "math.sqrt")
          <> "or" <> P.group (P.green "util.List.++" <> ")."),
    "",
    P.wrap $ "Relative names are converted to absolute names by prepending the current namespace."
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
      P.group ("(" <> P.blue "help namespaces") <> "to learn more)," <>
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
        Just pat -> Left $ showPatternHelp pat
      _ -> Left $ warn "Use `help <cmd>` or `help`.")
    where
      commandsByName = Map.fromList [
        (n, i) | i <- validInputs, n <- I.patternName i : I.aliases i ]
      isHelp s = Map.lookup s helpTopicsMap

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
  (P.column2 [
    (makeExample links ["src"], "shows all outgoing links from `src`."),
    (makeExample links ["src", "<type>"], "shows all links for the given type.") ])
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

debugNumberedArgs :: InputPattern 
debugNumberedArgs = InputPattern "debug.numberedArgs" [] []
  "Dump the contents of the numbered args state."
  (const $ Right Input.DebugNumberedArgsI)

debugBranchHistory :: InputPattern
debugBranchHistory = InputPattern "debug.history" []
  [(Optional, noCompletions)]
  "Dump codebase history, compatible with bit-booster.com/graph.html"
  (const $ Right Input.DebugBranchHistoryI)

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
    _   -> Left $ showPatternHelp execute
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
  , cd
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
  , todo
  , patch
  , link
  , unlink
  , links
  , replaceTerm
  , replaceType
  , test
  , execute
  , viewReflog
  , resetRoot
  , quit
  , updateBuiltins
  , mergeBuiltins
  , debugNumberedArgs
  , debugBranchHistory
  ]

commandNames :: [String]
commandNames = validInputs >>= \i -> I.patternName i : I.aliases i

commandNameArg :: ArgumentType
commandNameArg =
  ArgumentType "command" $ \q _ _ _ -> pure (exactComplete q (commandNames <> Map.keys helpTopicsMap))

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
