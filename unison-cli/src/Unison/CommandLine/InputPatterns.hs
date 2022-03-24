{-
   This module defines 'InputPattern' values for every supported input command.
-}
module Unison.CommandLine.InputPatterns where

import qualified Control.Lens.Cons as Cons
import Data.Bifunctor (Bifunctor (bimap), first)
import Data.List (intercalate, isPrefixOf)
import Data.List.Extra (nubOrdOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Void (Void)
import System.Console.Haskeline.Completion (Completion (Completion))
import qualified System.Console.Haskeline.Completion as Completion
import qualified Text.Megaparsec as P
import Unison.Codebase (Codebase)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Merge as Branch
import qualified Unison.Codebase.Branch.Names as Branch
import Unison.Codebase.Editor.Input (Input)
import qualified Unison.Codebase.Editor.Input as Input
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace, WriteRemotePath, WriteRepo)
import qualified Unison.Codebase.Editor.SlurpResult as SR
import qualified Unison.Codebase.Editor.UriParser as UriParser
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Path.Parse as Path
import qualified Unison.Codebase.PushBehavior as PushBehavior
import qualified Unison.Codebase.SyncMode as SyncMode
import Unison.Codebase.Verbosity (Verbosity)
import qualified Unison.Codebase.Verbosity as Verbosity
import Unison.CommandLine
import qualified Unison.CommandLine.Globbing as Globbing
import Unison.CommandLine.InputPattern
  ( ArgumentType (..),
    InputPattern (InputPattern),
    IsOptional (..),
  )
import qualified Unison.CommandLine.InputPattern as I
import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (NameSegment))
import qualified Unison.Names as Names
import Unison.Prelude
import qualified Unison.Util.ColorText as CT
import Unison.Util.Monoid (intercalateMap)
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.Relation as R

showPatternHelp :: InputPattern -> P.Pretty CT.ColorText
showPatternHelp i =
  P.lines
    [ P.bold (fromString $ I.patternName i)
        <> fromString
          ( if not . null $ I.aliases i
              then " (or " <> intercalate ", " (I.aliases i) <> ")"
              else ""
          ),
      P.wrap $ I.help i
    ]

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
makeExampleEOS p args =
  P.group $
    backtick (intercalateMap " " id (P.nonEmpty $ fromString (I.patternName p) : args)) <> "."

helpFor :: InputPattern -> Either (P.Pretty CT.ColorText) Input
helpFor p = I.parse help [I.patternName p]

mergeBuiltins :: InputPattern
mergeBuiltins =
  InputPattern
    "builtins.merge"
    []
    []
    "Adds the builtins to `builtins.` in the current namespace (excluding `io` and misc)."
    (const . pure $ Input.MergeBuiltinsI)

mergeIOBuiltins :: InputPattern
mergeIOBuiltins =
  InputPattern
    "builtins.mergeio"
    []
    []
    "Adds all the builtins to `builtins.` in the current namespace, including `io` and misc."
    (const . pure $ Input.MergeIOBuiltinsI)

updateBuiltins :: InputPattern
updateBuiltins =
  InputPattern
    "builtins.update"
    []
    []
    ( "Adds all the builtins that are missing from this namespace, "
        <> "and deprecate the ones that don't exist in this version of Unison."
    )
    (const . pure $ Input.UpdateBuiltinsI)

todo :: InputPattern
todo =
  InputPattern
    "todo"
    []
    [(Optional, patchArg), (Optional, namespaceArg)]
    ( P.wrapColumn2
        [ ( makeExample' todo,
            "lists the refactor work remaining in the default patch for the current"
              <> " namespace."
          ),
          ( makeExample todo ["<patch>"],
            "lists the refactor work remaining in the given patch in the current "
              <> "namespace."
          ),
          ( makeExample todo ["<patch>", "[path]"],
            "lists the refactor work remaining in the given patch in given namespace."
          )
        ]
    )
    ( \case
        patchStr : ws -> mapLeft (warn . fromString) $ do
          patch <- Path.parseSplit' Path.definitionNameSegment patchStr
          branch <- case ws of
            [] -> pure Path.relativeEmpty'
            [pathStr] -> Path.parsePath' pathStr
            _ -> Left "`todo` just takes a patch and one optional namespace"
          Right $ Input.TodoI (Just patch) branch
        [] -> Right $ Input.TodoI Nothing Path.relativeEmpty'
    )

load :: InputPattern
load =
  InputPattern
    "load"
    []
    [(Optional, noCompletions)]
    ( P.wrapColumn2
        [ ( makeExample' load,
            "parses, typechecks, and evaluates the most recent scratch file."
          ),
          ( makeExample load ["<scratch file>"],
            "parses, typechecks, and evaluates the given scratch file."
          )
        ]
    )
    ( \case
        [] -> pure $ Input.LoadI Nothing
        [file] -> pure $ Input.LoadI . Just $ file
        _ -> Left (I.help load)
    )

add :: InputPattern
add =
  InputPattern
    "add"
    []
    [(ZeroPlus, noCompletions)]
    ( "`add` adds to the codebase all the definitions from the most recently "
        <> "typechecked file."
    )
    $ \ws -> pure $ Input.AddI (Set.fromList $ map Name.unsafeFromString ws)

previewAdd :: InputPattern
previewAdd =
  InputPattern
    "add.preview"
    []
    [(ZeroPlus, noCompletions)]
    ( "`add.preview` previews additions to the codebase from the most recently "
        <> "typechecked file. This command only displays cached typechecking "
        <> "results. Use `load` to reparse & typecheck the file if the context "
        <> "has changed."
    )
    $ \ws -> pure $ Input.PreviewAddI (Set.fromList $ map Name.unsafeFromString ws)

updateNoPatch :: InputPattern
updateNoPatch =
  InputPattern
    "update.nopatch"
    ["un"]
    [(ZeroPlus, noCompletions)]
    ( P.wrap
        ( makeExample' updateNoPatch
            <> "works like"
            <> P.group (makeExample' update <> ",")
            <> "except it doesn't add a patch entry for any updates. "
            <> "Use this when you want to make changes to definitions without "
            <> "pushing those changes to dependents beyond your codebase. "
            <> "An example is when updating docs, or when updating a term you "
            <> "just added."
        )
        <> P.wrapColumn2
          [ ( makeExample' updateNoPatch,
              "updates all definitions in the .u file."
            ),
            ( makeExample updateNoPatch ["foo", "bar"],
              "updates `foo`, `bar`, and their dependents from the .u file."
            )
          ]
    )
    ( \case
        ws -> do
          pure $
            Input.UpdateI
              Input.NoPatch
              (Set.fromList $ map Name.unsafeFromString ws)
    )

update :: InputPattern
update =
  InputPattern
    "update"
    []
    [(Optional, patchArg), (ZeroPlus, noCompletions)]
    ( P.wrap
        ( makeExample' update
            <> "works like"
            <> P.group (makeExample' add <> ",")
            <> "except that if a definition in the file has the same name as an"
            <> "existing definition, the name gets updated to point to the new"
            <> "definition. If the old definition has any dependents, `update` will"
            <> "add those dependents to a refactoring session, specified by an"
            <> "optional patch."
        )
        <> P.wrapColumn2
          [ ( makeExample' update,
              "adds all definitions in the .u file, noting replacements in the"
                <> "default patch for the current namespace."
            ),
            ( makeExample update ["<patch>"],
              "adds all definitions in the .u file, noting replacements in the"
                <> "specified patch."
            ),
            ( makeExample update ["<patch>", "foo", "bar"],
              "adds `foo`, `bar`, and their dependents from the .u file, noting"
                <> "any replacements into the specified patch."
            )
          ]
    )
    ( \case
        patchStr : ws -> do
          patch <-
            first fromString $
              Path.parseSplit' Path.definitionNameSegment patchStr
          pure $
            Input.UpdateI
              (Input.UsePatch patch)
              (Set.fromList $ map Name.unsafeFromString ws)
        [] -> Right $ Input.UpdateI Input.DefaultPatch mempty
    )

previewUpdate :: InputPattern
previewUpdate =
  InputPattern
    "update.preview"
    []
    [(ZeroPlus, noCompletions)]
    ( "`update.preview` previews updates to the codebase from the most "
        <> "recently typechecked file. This command only displays cached "
        <> "typechecking results. Use `load` to reparse & typecheck the file if "
        <> "the context has changed."
    )
    $ \ws -> pure $ Input.PreviewUpdateI (Set.fromList $ map Name.unsafeFromString ws)

patch :: InputPattern
patch =
  InputPattern
    "patch"
    []
    [(Required, patchArg), (Optional, namespaceArg)]
    ( P.wrap $
        makeExample' patch
          <> "rewrites any definitions that depend on "
          <> "definitions with type-preserving edits to use the updated versions of"
          <> "these dependencies."
    )
    ( \case
        patchStr : ws -> first fromString $ do
          patch <- Path.parseSplit' Path.definitionNameSegment patchStr
          branch <- case ws of
            [pathStr] -> Path.parsePath' pathStr
            _ -> pure Path.relativeEmpty'
          pure $ Input.PropagatePatchI patch branch
        [] ->
          Left $
            warn $
              makeExample' patch
                <> "takes a patch and an optional namespace."
    )

view :: InputPattern
view =
  InputPattern
    "view"
    []
    [(ZeroPlus, definitionQueryArg)]
    ( P.lines
        [ "`view foo` prints the definition of `foo`.",
          "`view` without arguments invokes a search to select definitions to view, which requires that `fzf` can be found within your PATH."
        ]
    )
    ( fmap (Input.ShowDefinitionI Input.ConsoleLocation)
        . traverse parseHashQualifiedName
    )

display :: InputPattern
display =
  InputPattern
    "display"
    []
    [(ZeroPlus, definitionQueryArg)]
    ( P.lines
        [ "`display foo` prints a rendered version of the term `foo`.",
          "`display` without arguments invokes a search to select a definition to display, which requires that `fzf` can be found within your PATH."
        ]
    )
    ( \xs -> Input.DisplayI Input.ConsoleLocation <$> (traverse parseHashQualifiedName xs)
    )

displayTo :: InputPattern
displayTo =
  InputPattern
    "display.to"
    []
    [(Required, noCompletions), (ZeroPlus, definitionQueryArg)]
    ( P.wrap $
        makeExample displayTo ["<filename>", "foo"]
          <> "prints a rendered version of the term `foo` to the given file."
    )
    ( \case
        (file : xs) ->
          Input.DisplayI (Input.FileLocation file) <$> traverse parseHashQualifiedName xs
        _ -> Left (I.help displayTo)
    )

docs :: InputPattern
docs =
  InputPattern
    "docs"
    []
    [(ZeroPlus, definitionQueryArg)]
    ( P.lines
        [ "`docs foo` shows documentation for the definition `foo`.",
          "`docs` without arguments invokes a search to select which definition to view documentation for, which requires that `fzf` can be found within your PATH."
        ]
    )
    (bimap fromString Input.DocsI . traverse Path.parseHQSplit')

api :: InputPattern
api =
  InputPattern
    "api"
    []
    []
    "`api` provides details about the API."
    (const $ pure Input.ApiI)

ui :: InputPattern
ui =
  InputPattern
    "ui"
    []
    []
    "`ui` opens the Codebase UI in the default browser."
    (const $ pure Input.UiI)

undo :: InputPattern
undo =
  InputPattern
    "undo"
    []
    []
    "`undo` reverts the most recent change to the codebase."
    (const $ pure Input.UndoI)

viewByPrefix :: InputPattern
viewByPrefix =
  InputPattern
    "view.recursive"
    []
    [(OnePlus, definitionQueryArg)]
    "`view.recursive Foo` prints the definitions of `Foo` and `Foo.blah`."
    ( fmap (Input.ShowDefinitionByPrefixI Input.ConsoleLocation)
        . traverse parseHashQualifiedName
    )

find :: InputPattern
find =
  InputPattern
    "find"
    []
    [(ZeroPlus, fuzzyDefinitionQueryArg)]
    ( P.wrapColumn2
        [ ("`find`", "lists all definitions in the current namespace."),
          ( "`find foo`",
            "lists all definitions with a name similar to 'foo' in the current "
              <> "namespace."
          ),
          ( "`find foo bar`",
            "lists all definitions with a name similar to 'foo' or 'bar' in the "
              <> "current namespace."
          )
        ]
    )
    (pure . Input.SearchByNameI False False)

findShallow :: InputPattern
findShallow =
  InputPattern
    "list"
    ["ls", "dir"]
    [(Optional, namespaceArg)]
    ( P.wrapColumn2
        [ ("`list`", "lists definitions and namespaces at the current level of the current namespace."),
          ("`list foo`", "lists the 'foo' namespace."),
          ("`list .foo`", "lists the '.foo' namespace.")
        ]
    )
    ( \case
        [] -> pure $ Input.FindShallowI Path.relativeEmpty'
        [path] -> first fromString $ do
          p <- Path.parsePath' path
          pure $ Input.FindShallowI p
        _ -> Left (I.help findShallow)
    )

findVerbose :: InputPattern
findVerbose =
  InputPattern
    "find.verbose"
    ["list.verbose", "ls.verbose"]
    [(ZeroPlus, fuzzyDefinitionQueryArg)]
    ( "`find.verbose` searches for definitions like `find`, but includes hashes "
        <> "and aliases in the results."
    )
    (pure . Input.SearchByNameI True False)

findPatch :: InputPattern
findPatch =
  InputPattern
    "find.patch"
    ["list.patch", "ls.patch"]
    []
    ( P.wrapColumn2
        [("`find.patch`", "lists all patches in the current namespace.")]
    )
    (pure . const Input.FindPatchI)

renameTerm :: InputPattern
renameTerm =
  InputPattern
    "move.term"
    ["rename.term"]
    [ (Required, exactDefinitionTermQueryArg),
      (Required, newNameArg)
    ]
    "`move.term foo bar` renames `foo` to `bar`."
    ( \case
        [oldName, newName] -> first fromString $ do
          src <- Path.parseHQSplit' oldName
          target <- Path.parseSplit' Path.definitionNameSegment newName
          pure $ Input.MoveTermI src target
        _ ->
          Left . P.warnCallout $
            P.wrap
              "`rename.term` takes two arguments, like `rename.term oldname newname`."
    )

renameType :: InputPattern
renameType =
  InputPattern
    "move.type"
    ["rename.type"]
    [ (Required, exactDefinitionTypeQueryArg),
      (Required, newNameArg)
    ]
    "`move.type foo bar` renames `foo` to `bar`."
    ( \case
        [oldName, newName] -> first fromString $ do
          src <- Path.parseHQSplit' oldName
          target <- Path.parseSplit' Path.definitionNameSegment newName
          pure $ Input.MoveTypeI src target
        _ ->
          Left . P.warnCallout $
            P.wrap
              "`rename.type` takes two arguments, like `rename.type oldname newname`."
    )

delete :: InputPattern
delete =
  InputPattern
    "delete"
    []
    [(OnePlus, definitionQueryArg)]
    "`delete foo` removes the term or type name `foo` from the namespace."
    ( \case
        [query] -> first fromString $ do
          p <- Path.parseHQSplit' query
          pure $ Input.DeleteI p
        _ ->
          Left . P.warnCallout $
            P.wrap
              "`delete` takes an argument, like `delete name`."
    )

deleteTerm :: InputPattern
deleteTerm =
  InputPattern
    "delete.term"
    []
    [(OnePlus, exactDefinitionTermQueryArg)]
    "`delete.term foo` removes the term name `foo` from the namespace."
    ( \case
        [query] -> first fromString $ do
          p <- Path.parseHQSplit' query
          pure $ Input.DeleteTermI p
        _ ->
          Left . P.warnCallout $
            P.wrap
              "`delete.term` takes an argument, like `delete.term name`."
    )

deleteType :: InputPattern
deleteType =
  InputPattern
    "delete.type"
    []
    [(OnePlus, exactDefinitionTypeQueryArg)]
    "`delete.type foo` removes the type name `foo` from the namespace."
    ( \case
        [query] -> first fromString $ do
          p <- Path.parseHQSplit' query
          pure $ Input.DeleteTypeI p
        _ ->
          Left . P.warnCallout $
            P.wrap
              "`delete.type` takes an argument, like `delete.type name`."
    )

deleteTermReplacementCommand :: String
deleteTermReplacementCommand = "delete.term-replacement"

deleteTypeReplacementCommand :: String
deleteTypeReplacementCommand = "delete.type-replacement"

deleteReplacement :: Bool -> InputPattern
deleteReplacement isTerm =
  InputPattern
    commandName
    []
    [(Required, if isTerm then exactDefinitionTermQueryArg else exactDefinitionTypeQueryArg), (Optional, patchArg)]
    ( P.string $
        commandName
          <> " <foo> <patch>` removes any edit of the "
          <> str
          <> " `foo` from the patch `patch`, "
          <> "or from the default patch if none is specified.  Note that `foo` refers to the "
          <> "original name for the "
          <> str
          <> " - not the one in place after the edit."
    )
    ( \case
        query : patch -> do
          patch <-
            first fromString
              . traverse (Path.parseSplit' Path.definitionNameSegment)
              $ listToMaybe patch
          q <- parseHashQualifiedName query
          pure $ input q patch
        _ ->
          Left
            . P.warnCallout
            . P.wrapString
            $ commandName
              <> " needs arguments. See `help "
              <> commandName
              <> "`."
    )
  where
    input =
      if isTerm
        then Input.RemoveTermReplacementI
        else Input.RemoveTypeReplacementI
    str = if isTerm then "term" else "type"
    commandName =
      if isTerm
        then deleteTermReplacementCommand
        else deleteTypeReplacementCommand

deleteTermReplacement :: InputPattern
deleteTermReplacement = deleteReplacement True

deleteTypeReplacement :: InputPattern
deleteTypeReplacement = deleteReplacement False

parseHashQualifiedName ::
  String -> Either (P.Pretty CT.ColorText) (HQ.HashQualified Name)
parseHashQualifiedName s =
  maybe
    ( Left
        . P.warnCallout
        . P.wrap
        $ P.string s
          <> " is not a well-formed name, hash, or hash-qualified name. "
          <> "I expected something like `foo`, `#abc123`, or `foo#abc123`."
    )
    Right
    $ HQ.fromString s

aliasTerm :: InputPattern
aliasTerm =
  InputPattern
    "alias.term"
    []
    [(Required, exactDefinitionTermQueryArg), (Required, newNameArg)]
    "`alias.term foo bar` introduces `bar` with the same definition as `foo`."
    ( \case
        [oldName, newName] -> first fromString $ do
          source <- Path.parseShortHashOrHQSplit' oldName
          target <- Path.parseSplit' Path.definitionNameSegment newName
          pure $ Input.AliasTermI source target
        _ ->
          Left . warn $
            P.wrap
              "`alias.term` takes two arguments, like `alias.term oldname newname`."
    )

aliasType :: InputPattern
aliasType =
  InputPattern
    "alias.type"
    []
    [(Required, exactDefinitionTypeQueryArg), (Required, newNameArg)]
    "`alias.type Foo Bar` introduces `Bar` with the same definition as `Foo`."
    ( \case
        [oldName, newName] -> first fromString $ do
          source <- Path.parseShortHashOrHQSplit' oldName
          target <- Path.parseSplit' Path.definitionNameSegment newName
          pure $ Input.AliasTypeI source target
        _ ->
          Left . warn $
            P.wrap
              "`alias.type` takes two arguments, like `alias.type oldname newname`."
    )

aliasMany :: InputPattern
aliasMany =
  InputPattern
    "alias.many"
    ["copy"]
    [(Required, definitionQueryArg), (OnePlus, exactDefinitionOrPathArg)]
    ( P.group . P.lines $
        [ P.wrap $
            P.group (makeExample aliasMany ["<relative1>", "[relative2...]", "<namespace>"])
              <> "creates aliases `relative1`, `relative2`, ... in the namespace `namespace`.",
          P.wrap $
            P.group (makeExample aliasMany ["foo.foo", "bar.bar", ".quux"])
              <> "creates aliases `.quux.foo.foo` and `.quux.bar.bar`."
        ]
    )
    ( \case
        srcs@(_ : _) Cons.:> dest -> first fromString $ do
          sourceDefinitions <- traverse Path.parseHQSplit srcs
          destNamespace <- Path.parsePath' dest
          pure $ Input.AliasManyI sourceDefinitions destNamespace
        _ -> Left (I.help aliasMany)
    )

up :: InputPattern
up =
  InputPattern
    "up"
    []
    []
    (P.wrapColumn2 [(makeExample up [], "move current path up one level")])
    ( \case
        [] -> Right Input.UpI
        _ -> Left (I.help up)
    )

cd :: InputPattern
cd =
  InputPattern
    "namespace"
    ["cd", "j"]
    [(Required, namespaceArg)]
    ( P.lines
        [ "Moves your perspective to a different namespace.",
          "",
          P.wrapColumn2
            [ ( makeExample cd ["foo.bar"],
                "descends into foo.bar from the current namespace."
              ),
              ( makeExample cd [".cat.dog"],
                "sets the current namespace to the abolute namespace .cat.dog."
              ),
              ( makeExample cd [".."],
                "moves to the parent of the current namespace. E.g. moves from '.cat.dog' to '.cat'"
              ),
              ( makeExample cd [],
                "invokes a search to select which namespace to move to, which requires that `fzf` can be found within your PATH."
              )
            ]
        ]
    )
    ( \case
        [".."] -> Right Input.UpI
        [p] -> first fromString $ do
          p <- Path.parsePath' p
          pure . Input.SwitchBranchI $ Just p
        -- No args will trigger a fuzzy find when handled.
        [] -> pure (Input.SwitchBranchI Nothing)
        _ -> Left (I.help cd)
    )

back :: InputPattern
back =
  InputPattern
    "back"
    ["popd"]
    []
    ( P.wrapColumn2
        [ ( makeExample back [],
            "undoes the last" <> makeExample' cd <> "command."
          )
        ]
    )
    ( \case
        [] -> pure Input.PopBranchI
        _ -> Left (I.help cd)
    )

deleteNamespace :: InputPattern
deleteNamespace =
  InputPattern
    "delete.namespace"
    []
    [(Required, namespaceArg)]
    "`delete.namespace <foo>` deletes the namespace `foo`"
    (deleteNamespaceParser (I.help deleteNamespace) Input.Try)

deleteNamespaceForce :: InputPattern
deleteNamespaceForce =
  InputPattern
    "delete.namespace.force"
    []
    [(Required, namespaceArg)]
    ( "`delete.namespace.force <foo>` deletes the namespace `foo`,"
        <> "deletion will proceed even if other code depends on definitions in foo."
    )
    (deleteNamespaceParser (I.help deleteNamespaceForce) Input.Force)

deleteNamespaceParser :: P.Pretty CT.ColorText -> Input.Insistence -> [String] -> Either (P.Pretty CT.ColorText) Input
deleteNamespaceParser helpText insistence =
  ( \case
      ["."] ->
        first fromString
          . pure
          $ Input.DeleteBranchI insistence Nothing
      [p] -> first fromString $ do
        p <- Path.parseSplit' Path.definitionNameSegment p
        pure . Input.DeleteBranchI insistence $ Just p
      _ -> Left helpText
  )

deletePatch :: InputPattern
deletePatch =
  InputPattern
    "delete.patch"
    []
    [(Required, patchArg)]
    "`delete.patch <foo>` deletes the patch `foo`"
    ( \case
        [p] -> first fromString $ do
          p <- Path.parseSplit' Path.definitionNameSegment p
          pure . Input.DeletePatchI $ p
        _ -> Left (I.help deletePatch)
    )

movePatch :: String -> String -> Either (P.Pretty CT.ColorText) Input
movePatch src dest = first fromString $ do
  src <- Path.parseSplit' Path.definitionNameSegment src
  dest <- Path.parseSplit' Path.definitionNameSegment dest
  pure $ Input.MovePatchI src dest

copyPatch' :: String -> String -> Either (P.Pretty CT.ColorText) Input
copyPatch' src dest = first fromString $ do
  src <- Path.parseSplit' Path.definitionNameSegment src
  dest <- Path.parseSplit' Path.definitionNameSegment dest
  pure $ Input.CopyPatchI src dest

copyPatch :: InputPattern
copyPatch =
  InputPattern
    "copy.patch"
    []
    [(Required, patchArg), (Required, newNameArg)]
    "`copy.patch foo bar` copies the patch `foo` to `bar`."
    ( \case
        [src, dest] -> copyPatch' src dest
        _ -> Left (I.help copyPatch)
    )

renamePatch :: InputPattern
renamePatch =
  InputPattern
    "move.patch"
    ["rename.patch"]
    [(Required, patchArg), (Required, newNameArg)]
    "`move.patch foo bar` renames the patch `foo` to `bar`."
    ( \case
        [src, dest] -> movePatch src dest
        _ -> Left (I.help renamePatch)
    )

renameBranch :: InputPattern
renameBranch =
  InputPattern
    "move.namespace"
    ["rename.namespace"]
    [(Required, namespaceArg), (Required, newNameArg)]
    "`move.namespace foo bar` renames the path `bar` to `foo`."
    ( \case
        [".", dest] -> first fromString $ do
          dest <- Path.parseSplit' Path.definitionNameSegment dest
          pure $ Input.MoveBranchI Nothing dest
        [src, dest] -> first fromString $ do
          src <- Path.parseSplit' Path.definitionNameSegment src
          dest <- Path.parseSplit' Path.definitionNameSegment dest
          pure $ Input.MoveBranchI (Just src) dest
        _ -> Left (I.help renameBranch)
    )

history :: InputPattern
history =
  InputPattern
    "history"
    []
    [(Optional, namespaceArg)]
    ( P.wrapColumn2
        [ (makeExample history [], "Shows the history of the current path."),
          (makeExample history [".foo"], "Shows history of the path .foo."),
          ( makeExample history ["#9dndk3kbsk13nbpeu"],
            "Shows the history of the namespace with the given hash."
              <> "The full hash must be provided."
          )
        ]
    )
    ( \case
        [src] -> first fromString $ do
          p <- Input.parseBranchId src
          pure $ Input.HistoryI (Just 10) (Just 10) p
        [] -> pure $ Input.HistoryI (Just 10) (Just 10) (Right Path.currentPath)
        _ -> Left (I.help history)
    )

forkLocal :: InputPattern
forkLocal =
  InputPattern
    "fork"
    ["copy.namespace"]
    [ (Required, namespaceArg),
      (Required, newNameArg)
    ]
    (makeExample forkLocal ["src", "dest"] <> "creates the namespace `dest` as a copy of `src`.")
    ( \case
        [src, dest] -> first fromString $ do
          src <- Input.parseBranchId src
          dest <- Path.parsePath' dest
          pure $ Input.ForkLocalBranchI src dest
        _ -> Left (I.help forkLocal)
    )

resetRoot :: InputPattern
resetRoot =
  InputPattern
    "reset-root"
    []
    [(Required, namespaceArg)]
    ( P.wrapColumn2
        [ ( makeExample resetRoot [".foo"],
            "Reset the root namespace (along with its history) to that of the `.foo` namespace."
          ),
          ( makeExample resetRoot ["#9dndk3kbsk13nbpeu"],
            "Reset the root namespace (along with its history) to that of the namespace with hash `#9dndk3kbsk13nbpeu`."
          )
        ]
    )
    ( \case
        [src] -> first fromString $ do
          src <- Input.parseBranchId src
          pure $ Input.ResetRootI src
        _ -> Left (I.help resetRoot)
    )

pullSilent :: InputPattern
pullSilent =
  pullImpl "pull.silent" Verbosity.Silent Input.PullWithHistory "without listing the merged entities"

pull :: InputPattern
pull = pullImpl "pull" Verbosity.Default Input.PullWithHistory ""

pullWithoutHistory :: InputPattern
pullWithoutHistory =
  pullImpl
    "pull.without-history"
    Verbosity.Default
    Input.PullWithoutHistory
    "without including the remote's history. This usually results in smaller codebase sizes."

pullImpl :: String -> Verbosity -> Input.PullMode -> P.Pretty CT.ColorText -> InputPattern
pullImpl name verbosity pullMode addendum = do
  self
  where
    self =
      InputPattern
        name
        []
        [(Optional, gitUrlArg), (Optional, namespaceArg)]
        ( P.lines
            [ P.wrap
                "The"
                <> makeExample' self
                <> "command merges a remote namespace into a local namespace"
                <> addendum,
              "",
              P.wrapColumn2
                [ ( makeExample self ["remote", "local"],
                    "merges the remote namespace `remote`"
                      <> "into the local namespace `local"
                  ),
                  ( makeExample self ["remote"],
                    "merges the remote namespace `remote`"
                      <> "into the current namespace"
                  ),
                  ( makeExample' self,
                    "merges the remote namespace configured in `.unisonConfig`"
                      <> "with the key `GitUrl.ns` where `ns` is the current namespace,"
                      <> "into the current namespace"
                  )
                ],
              "",
              explainRemote
            ]
        )
        ( \case
            [] ->
              Right $ Input.PullRemoteBranchI Nothing Path.relativeEmpty' SyncMode.ShortCircuit pullMode verbosity
            [url] -> do
              ns <- parseUri "url" url
              Right $ Input.PullRemoteBranchI (Just ns) Path.relativeEmpty' SyncMode.ShortCircuit pullMode verbosity
            [url, path] -> do
              ns <- parseUri "url" url
              p <- first fromString $ Path.parsePath' path
              Right $ Input.PullRemoteBranchI (Just ns) p SyncMode.ShortCircuit pullMode verbosity
            _ -> Left (I.help self)
        )

pullExhaustive :: InputPattern
pullExhaustive =
  InputPattern
    "debug.pull-exhaustive"
    []
    [(Required, gitUrlArg), (Optional, namespaceArg)]
    ( P.lines
        [ P.wrap $
            "The " <> makeExample' pullExhaustive <> "command can be used in place of"
              <> makeExample' pull
              <> "to complete namespaces"
              <> "which were pulled incompletely due to a bug in UCM"
              <> "versions M1l and earlier.  It may be extra slow!"
        ]
    )
    ( \case
        [] ->
          Right $ Input.PullRemoteBranchI Nothing Path.relativeEmpty' SyncMode.Complete Input.PullWithHistory Verbosity.Default
        [url] -> do
          ns <- parseUri "url" url
          Right $ Input.PullRemoteBranchI (Just ns) Path.relativeEmpty' SyncMode.Complete Input.PullWithHistory Verbosity.Default
        [url, path] -> do
          ns <- parseUri "url" url
          p <- first fromString $ Path.parsePath' path
          Right $ Input.PullRemoteBranchI (Just ns) p SyncMode.Complete Input.PullWithHistory Verbosity.Default
        _ -> Left (I.help pull)
    )

push :: InputPattern
push =
  InputPattern
    "push"
    []
    [(Required, gitUrlArg), (Optional, namespaceArg)]
    ( P.lines
        [ P.wrap
            "The `push` command merges a local namespace into a remote namespace.",
          "",
          P.wrapColumn2
            [ ( "`push remote local`",
                "merges the contents of the local namespace `local`"
                  <> "into the remote namespace `remote`."
              ),
              ( "`push remote`",
                "publishes the current namespace into the remote namespace `remote`"
              ),
              ( "`push`",
                "publishes the current namespace"
                  <> "into the remote namespace configured in `.unisonConfig`"
                  <> "with the key `GitUrl.ns` where `ns` is the current namespace"
              )
            ],
          "",
          explainRemote
        ]
    )
    ( \case
        [] ->
          Right $ Input.PushRemoteBranchI Nothing Path.relativeEmpty' PushBehavior.RequireNonEmpty SyncMode.ShortCircuit
        url : rest -> do
          (repo, path) <- parsePushPath "url" url
          p <- case rest of
            [] -> Right Path.relativeEmpty'
            [path] -> first fromString $ Path.parsePath' path
            _ -> Left (I.help push)
          Right $ Input.PushRemoteBranchI (Just (repo, path)) p PushBehavior.RequireNonEmpty SyncMode.ShortCircuit
    )

pushCreate :: InputPattern
pushCreate =
  InputPattern
    "push.create"
    []
    [(Required, gitUrlArg), (Optional, namespaceArg)]
    ( P.lines
        [ P.wrap
            "The `push.create` command pushes a local namespace to an empty remote namespace.",
          "",
          P.wrapColumn2
            [ ( "`push.create remote local`",
                "pushes the contents of the local namespace `local`"
                  <> "into the empty remote namespace `remote`."
              ),
              ( "`push remote`",
                "publishes the current namespace into the empty remote namespace `remote`"
              ),
              ( "`push`",
                "publishes the current namespace"
                  <> "into the empty remote namespace configured in `.unisonConfig`"
                  <> "with the key `GitUrl.ns` where `ns` is the current namespace"
              )
            ],
          "",
          explainRemote
        ]
    )
    ( \case
        [] ->
          Right $ Input.PushRemoteBranchI Nothing Path.relativeEmpty' PushBehavior.RequireEmpty SyncMode.ShortCircuit
        url : rest -> do
          (repo, path) <- parsePushPath "url" url
          p <- case rest of
            [] -> Right Path.relativeEmpty'
            [path] -> first fromString $ Path.parsePath' path
            _ -> Left (I.help push)
          Right $ Input.PushRemoteBranchI (Just (repo, path)) p PushBehavior.RequireEmpty SyncMode.ShortCircuit
    )

pushExhaustive :: InputPattern
pushExhaustive =
  InputPattern
    "debug.push-exhaustive"
    []
    [(Required, gitUrlArg), (Optional, namespaceArg)]
    ( P.lines
        [ P.wrap $
            "The " <> makeExample' pushExhaustive <> "command can be used in place of"
              <> makeExample' push
              <> "to repair remote namespaces"
              <> "which were pushed incompletely due to a bug in UCM"
              <> "versions M1l and earlier. It may be extra slow!"
        ]
    )
    ( \case
        [] ->
          Right $ Input.PushRemoteBranchI Nothing Path.relativeEmpty' PushBehavior.RequireNonEmpty SyncMode.Complete
        url : rest -> do
          (repo, path) <- parsePushPath "url" url
          p <- case rest of
            [] -> Right Path.relativeEmpty'
            [path] -> first fromString $ Path.parsePath' path
            _ -> Left (I.help push)
          Right $ Input.PushRemoteBranchI (Just (repo, path)) p PushBehavior.RequireNonEmpty SyncMode.Complete
    )

createPullRequest :: InputPattern
createPullRequest =
  InputPattern
    "pull-request.create"
    ["pr.create"]
    [(Required, gitUrlArg), (Required, gitUrlArg), (Optional, namespaceArg)]
    ( P.group $
        P.lines
          [ P.wrap $
              makeExample createPullRequest ["base", "head"]
                <> "will generate a request to merge the remote repo `head`"
                <> "into the remote repo `base`.",
            "",
            "example: "
              <> makeExampleNoBackticks
                createPullRequest
                [ "https://github.com/unisonweb/base:.trunk",
                  "https://github.com/me/unison:.prs.base._myFeature"
                ]
          ]
    )
    ( \case
        [baseUrl, headUrl] -> do
          baseRepo <- parseUri "baseRepo" baseUrl
          headRepo <- parseUri "headRepo" headUrl
          pure $ Input.CreatePullRequestI baseRepo headRepo
        _ -> Left (I.help createPullRequest)
    )

loadPullRequest :: InputPattern
loadPullRequest =
  InputPattern
    "pull-request.load"
    ["pr.load"]
    [(Required, gitUrlArg), (Required, gitUrlArg), (Optional, namespaceArg)]
    ( P.lines
        [ P.wrap $
            makeExample loadPullRequest ["base", "head"]
              <> "will load a pull request for merging the remote repo `head` into the"
              <> "remote repo `base`, staging each in the current namespace"
              <> "(so make yourself a clean spot to work first).",
          P.wrap $
            makeExample loadPullRequest ["base", "head", "dest"]
              <> "will load a pull request for merging the remote repo `head` into the"
              <> "remote repo `base`, staging each in `dest`, which must be empty."
        ]
    )
    ( \case
        [baseUrl, headUrl] -> do
          baseRepo <- parseUri "baseRepo" baseUrl
          headRepo <- parseUri "topicRepo" headUrl
          pure $ Input.LoadPullRequestI baseRepo headRepo Path.relativeEmpty'
        [baseUrl, headUrl, dest] -> do
          baseRepo <- parseUri "baseRepo" baseUrl
          headRepo <- parseUri "topicRepo" headUrl
          destPath <- first fromString $ Path.parsePath' dest
          pure $ Input.LoadPullRequestI baseRepo headRepo destPath
        _ -> Left (I.help loadPullRequest)
    )

parseUri :: String -> String -> Either (P.Pretty P.ColorText) ReadRemoteNamespace
parseUri label input =
  let printError err = P.lines [P.string "I couldn't parse the repository address given above.", prettyPrintParseError input err]
   in first printError (P.parse UriParser.repoPath label (Text.pack input))

prettyPrintParseError :: String -> P.ParseError Char Void -> P.Pretty P.ColorText
prettyPrintParseError input = \case
  P.TrivialError sp ue ee ->
    P.lines
      [ printLocation sp,
        P.newline,
        printTrivial ue ee
      ]
  P.FancyError sp ee ->
    let errors = foldMap (P.string . mappend "\n" . P.showErrorComponent) ee
     in P.lines
          [ printLocation sp,
            errors
          ]
  where
    printLocation :: NE.NonEmpty P.SourcePos -> P.Pretty P.ColorText
    printLocation sp =
      let col = (P.unPos $ P.sourceColumn $ NE.head sp) - 1
          row = (P.unPos $ P.sourceLine $ NE.head sp) - 1
          errorLine = lines input !! row
       in P.lines
            [ P.newline,
              P.string errorLine,
              P.string $ replicate col ' ' <> "^-- This is where I gave up."
            ]

    printTrivial :: (Maybe (P.ErrorItem Char)) -> (Set (P.ErrorItem Char)) -> P.Pretty P.ColorText
    printTrivial ue ee =
      let expected = "I expected " <> foldMap (P.singleQuoted . P.string . P.showErrorComponent) ee
          found = P.string . mappend "I found " . P.showErrorComponent <$> ue
          message = [expected] <> catMaybes [found]
       in P.oxfordCommasWith "." message

parseWriteRepo :: String -> String -> Either (P.Pretty P.ColorText) WriteRepo
parseWriteRepo label input = do
  first
    (fromString . show) -- turn any parsing errors into a Pretty.
    (P.parse UriParser.writeRepo label (Text.pack input))

parsePushPath :: String -> String -> Either (P.Pretty P.ColorText) WriteRemotePath
parsePushPath label input = do
  first
    (fromString . show) -- turn any parsing errors into a Pretty.
    (P.parse UriParser.writeRepoPath label (Text.pack input))

squashMerge :: InputPattern
squashMerge =
  InputPattern
    "merge.squash"
    ["squash"]
    [(Required, namespaceArg), (Required, namespaceArg)]
    ( P.wrap $
        makeExample squashMerge ["src", "dest"]
          <> "merges `src` namespace into `dest`,"
          <> "discarding the history of `src` in the process."
          <> "The resulting `dest` will have (at most) 1"
          <> "additional history entry."
    )
    ( \case
        [src, dest] -> first fromString $ do
          src <- Path.parsePath' src
          dest <- Path.parsePath' dest
          pure $ Input.MergeLocalBranchI src dest Branch.SquashMerge
        _ -> Left (I.help squashMerge)
    )

mergeLocal :: InputPattern
mergeLocal =
  InputPattern
    "merge"
    []
    [ (Required, namespaceArg),
      (Optional, namespaceArg)
    ]
    ( P.column2
        [ ("`merge src`", "merges `src` namespace into the current namespace"),
          ("`merge src dest`", "merges `src` namespace into the `dest` namespace")
        ]
    )
    ( \case
        [src] -> first fromString $ do
          src <- Path.parsePath' src
          pure $ Input.MergeLocalBranchI src Path.relativeEmpty' Branch.RegularMerge
        [src, dest] -> first fromString $ do
          src <- Path.parsePath' src
          dest <- Path.parsePath' dest
          pure $ Input.MergeLocalBranchI src dest Branch.RegularMerge
        _ -> Left (I.help mergeLocal)
    )

diffNamespace :: InputPattern
diffNamespace =
  InputPattern
    "diff.namespace"
    []
    [(Required, namespaceArg), (Optional, namespaceArg)]
    ( P.column2
        [ ( "`diff.namespace before after`",
            P.wrap
              "shows how the namespace `after` differs from the namespace `before`"
          ),
          ( "`diff.namespace before`",
            P.wrap
              "shows how the current namespace differs from the namespace `before`"
          )
        ]
    )
    ( \case
        [before, after] -> first fromString $ do
          before <- Input.parseBranchId before
          after <- Input.parseBranchId after
          pure $ Input.DiffNamespaceI before after
        [before] -> first fromString $ do
          before <- Input.parseBranchId before
          pure $ Input.DiffNamespaceI before (Right Path.currentPath)
        _ -> Left $ I.help diffNamespace
    )

previewMergeLocal :: InputPattern
previewMergeLocal =
  InputPattern
    "merge.preview"
    []
    [(Required, namespaceArg), (Optional, namespaceArg)]
    ( P.column2
        [ ( "`merge.preview src`",
            "shows how the current namespace will change after a `merge src`."
          ),
          ( "`merge.preview src dest`",
            "shows how `dest` namespace will change after a `merge src dest`."
          )
        ]
    )
    ( \case
        [src] -> first fromString $ do
          src <- Path.parsePath' src
          pure $ Input.PreviewMergeLocalBranchI src Path.relativeEmpty'
        [src, dest] -> first fromString $ do
          src <- Path.parsePath' src
          dest <- Path.parsePath' dest
          pure $ Input.PreviewMergeLocalBranchI src dest
        _ -> Left (I.help previewMergeLocal)
    )

replaceEdit ::
  ( HQ.HashQualified Name ->
    HQ.HashQualified Name ->
    Maybe Input.PatchPath ->
    Input
  ) ->
  InputPattern
replaceEdit f = self
  where
    self =
      InputPattern
        "replace"
        []
        [ (Required, definitionQueryArg),
          (Required, definitionQueryArg),
          (Optional, patchArg)
        ]
        ( P.wrapColumn2
            [ ( makeExample self ["<from>", "<to>", "<patch>"],
                "Replace the term/type <from> in the given patch with the term/type <to>."
              ),
              ( makeExample self ["<from>", "<to>"],
                "Replace the term/type <from> with <to> in the default patch."
              )
            ]
        )
        ( \case
            source : target : patch -> do
              patch <-
                first fromString
                  <$> traverse (Path.parseSplit' Path.definitionNameSegment)
                  $ listToMaybe patch
              sourcehq <- parseHashQualifiedName source
              targethq <- parseHashQualifiedName target
              pure $ f sourcehq targethq patch
            _ -> Left $ I.help self
        )

replace :: InputPattern
replace = replaceEdit Input.ReplaceI

viewReflog :: InputPattern
viewReflog =
  InputPattern
    "reflog"
    []
    []
    "`reflog` lists the changes that have affected the root namespace"
    ( \case
        [] -> pure Input.ShowReflogI
        _ ->
          Left . warn . P.string $
            I.patternName viewReflog ++ " doesn't take any arguments."
    )

edit :: InputPattern
edit =
  InputPattern
    "edit"
    []
    [(OnePlus, definitionQueryArg)]
    ( P.lines
        [ "`edit foo` prepends the definition of `foo` to the top of the most "
            <> "recently saved file.",
          "`edit` without arguments invokes a search to select a definition for editing, which requires that `fzf` can be found within your PATH."
        ]
    )
    ( fmap (Input.ShowDefinitionI Input.LatestFileLocation)
        . traverse parseHashQualifiedName
    )

topicNameArg :: ArgumentType
topicNameArg =
  ArgumentType
    { typeName = "topic",
      suggestions = \q _ _ _ -> pure (exactComplete q $ Map.keys helpTopicsMap),
      globTargets = mempty
    }

helpTopics :: InputPattern
helpTopics =
  InputPattern
    "help-topics"
    ["help-topic"]
    [(Optional, topicNameArg)]
    ("`help-topics` lists all topics and `help-topics <topic>` shows an explanation of that topic.")
    ( \case
        [] -> Left topics
        [topic] -> case Map.lookup topic helpTopicsMap of
          Nothing -> Left . warn $ "I don't know of that topic. Try `help-topics`."
          Just t -> Left t
        _ -> Left $ warn "Use `help-topics <topic>` or `help-topics`."
    )
  where
    topics =
      P.callout "🌻" $
        P.lines
          [ "Here's a list of topics I can tell you more about: ",
            "",
            P.indentN 2 $ P.sep "\n" (P.string <$> Map.keys helpTopicsMap),
            "",
            aside "Example" "use `help filestatus` to learn more about that topic."
          ]

helpTopicsMap :: Map String (P.Pretty P.ColorText)
helpTopicsMap =
  Map.fromList
    [ ("testcache", testCacheMsg),
      ("filestatus", fileStatusMsg),
      ("messages.disallowedAbsolute", disallowedAbsoluteMsg),
      ("namespaces", pathnamesMsg)
    ]
  where
    blankline = ("", "")
    fileStatusMsg =
      P.callout "📓" . P.lines $
        [ P.wrap $
            "Here's a list of possible status messages you might see"
              <> "for definitions in a .u file.",
          "",
          P.wrapColumn2
            [ ( P.bold $ SR.prettyStatus SR.Collision,
                "A definition with the same name as an existing definition. Doing"
                  <> "`update` instead of `add` will turn this failure into a successful"
                  <> "update."
              ),
              blankline,
              ( P.bold $ SR.prettyStatus SR.Conflicted,
                "A definition with the same name as an existing definition."
                  <> "Resolving the conflict and then trying an `update` again will"
                  <> "turn this into a successful update."
              ),
              blankline,
              ( P.bold $ SR.prettyStatus SR.TermExistingConstructorCollision,
                "A definition with the same name as an existing constructor for "
                  <> "some data type. Rename your definition or the data type before"
                  <> "trying again to `add` or `update`."
              ),
              blankline,
              ( P.bold $ SR.prettyStatus SR.ConstructorExistingTermCollision,
                "A type defined in the file has a constructor that's named the"
                  <> "same as an existing term. Rename that term or your constructor"
                  <> "before trying again to `add` or `update`."
              ),
              blankline,
              ( P.bold $ SR.prettyStatus SR.BlockedDependency,
                "This definition was blocked because it dependended on "
                  <> "a definition with a failed status."
              ),
              blankline,
              ( P.bold $ SR.prettyStatus SR.ExtraDefinition,
                "This definition was added because it was a dependency of"
                  <> "a definition explicitly selected."
              )
            ]
        ]
    testCacheMsg =
      P.callout "🎈" . P.lines $
        [ P.wrap $
            "Unison caches the results of " <> P.blue "test>"
              <> "watch expressions. Since these expressions are pure and"
              <> "always yield the same result when evaluated, there's no need"
              <> "to run them more than once!",
          "",
          P.wrap $
            "A test is rerun only if it has changed, or if one"
              <> "of the definitions it depends on has changed."
        ]
    pathnamesMsg =
      P.callout "\129488" . P.lines $
        [ P.wrap $
            "There are two kinds of namespaces," <> P.group (P.blue "absolute" <> ",")
              <> "such as"
              <> P.group ("(" <> P.blue ".foo.bar")
              <> "or"
              <> P.group (P.blue ".base.math.+" <> ")")
              <> "and"
              <> P.group (P.green "relative" <> ",")
              <> "such as"
              <> P.group ("(" <> P.green "math.sqrt")
              <> "or"
              <> P.group (P.green "util.List.++" <> ")."),
          "",
          P.wrap $
            "Relative names are converted to absolute names by prepending the current namespace."
              <> "For example, if your Unison prompt reads:",
          "",
          P.indentN 2 $ P.blue ".foo.bar>",
          "",
          "and your .u file looks like:",
          "",
          P.indentN 2 $ P.green "x" <> " = 41",
          "",
          P.wrap $
            "then doing an" <> P.blue "add"
              <> "will create the definition with the absolute name"
              <> P.group (P.blue ".foo.bar.x" <> " = 41"),
          "",
          P.wrap $
            "and you can refer to" <> P.green "x" <> "by its absolute name "
              <> P.blue ".foo.bar.x"
              <> "elsewhere"
              <> "in your code. For instance:",
          "",
          P.indentN 2 $
            "answerToLifeTheUniverseAndEverything = " <> P.blue ".foo.bar.x" <> " + 1"
        ]

    disallowedAbsoluteMsg =
      P.callout "\129302" . P.lines $
        [ P.wrap $
            "Although I can understand absolute (ex: .foo.bar) or"
              <> "relative (ex: util.math.sqrt) references to existing definitions"
              <> P.group ("(" <> P.blue "help namespaces")
              <> "to learn more),"
              <> "I can't yet handle giving new definitions with absolute names in a .u file.",
          "",
          P.wrap $
            "As a workaround, you can give definitions with a relative name"
              <> "temporarily (like `exports.blah.foo`) and then use `move.*` "
              <> "or `merge` commands to move stuff around afterwards."
        ]

help :: InputPattern
help =
  InputPattern
    "help"
    ["?"]
    [(Optional, commandNameArg)]
    "`help` shows general help and `help <cmd>` shows help for one command."
    ( \case
        [] ->
          Left $
            intercalateMap
              "\n\n"
              showPatternHelp
              (sortOn I.patternName validInputs)
        [isHelp -> Just msg] -> Left msg
        [cmd] -> case Map.lookup cmd commandsByName of
          Nothing -> Left . warn $ "I don't know of that command. Try `help`."
          Just pat -> Left $ showPatternHelp pat
        _ -> Left $ warn "Use `help <cmd>` or `help`."
    )
  where
    commandsByName =
      Map.fromList
        [ (n, i) | i <- validInputs, n <- I.patternName i : I.aliases i
        ]
    isHelp s = Map.lookup s helpTopicsMap

quit :: InputPattern
quit =
  InputPattern
    "quit"
    ["exit", ":q"]
    []
    "Exits the Unison command line interface."
    ( \case
        [] -> pure Input.QuitI
        _ -> Left "Use `quit`, `exit`, or <Ctrl-D> to quit."
    )

viewPatch :: InputPattern
viewPatch =
  InputPattern
    "view.patch"
    []
    [(Required, patchArg)]
    ( P.wrapColumn2
        [ ( makeExample' viewPatch,
            "Lists all the edits in the default patch."
          ),
          ( makeExample viewPatch ["<patch>"],
            "Lists all the edits in the given patch."
          )
        ]
    )
    ( \case
        [] -> Right $ Input.ListEditsI Nothing
        [patchStr] -> mapLeft fromString $ do
          patch <- Path.parseSplit' Path.definitionNameSegment patchStr
          Right $ Input.ListEditsI (Just patch)
        _ -> Left $ warn "`view.patch` takes a patch and that's it."
    )

link :: InputPattern
link =
  InputPattern
    "link"
    []
    [(Required, definitionQueryArg), (OnePlus, definitionQueryArg)]
    ( fromString $
        concat
          [ "`link metadata defn` creates a link to `metadata` from `defn`. ",
            "Use `links defn` or `links defn <type>` to view outgoing links, ",
            "and `unlink metadata defn` to remove a link. The `defn` can be either the ",
            "name of a term or type, multiple such names, or a range like `1-4` ",
            "for a range of definitions listed by a prior `find` command."
          ]
    )
    ( \case
        md : defs -> first fromString $ do
          md <- case HQ.fromString md of
            Nothing -> Left "Invalid hash qualified identifier for metadata."
            Just hq -> pure hq
          defs <- traverse Path.parseHQSplit' defs
          Right $ Input.LinkI md defs
        _ -> Left (I.help link)
    )

links :: InputPattern
links =
  InputPattern
    "links"
    []
    [(Required, definitionQueryArg), (Optional, definitionQueryArg)]
    ( P.column2
        [ (makeExample links ["defn"], "shows all outgoing links from `defn`."),
          (makeExample links ["defn", "<type>"], "shows all links of the given type.")
        ]
    )
    ( \case
        src : rest -> first fromString $ do
          src <- Path.parseHQSplit' src
          let ty = case rest of
                [] -> Nothing
                _ -> Just $ unwords rest
           in Right $ Input.LinksI src ty
        _ -> Left (I.help links)
    )

unlink :: InputPattern
unlink =
  InputPattern
    "unlink"
    ["delete.link"]
    [(Required, definitionQueryArg), (OnePlus, definitionQueryArg)]
    ( fromString $
        concat
          [ "`unlink metadata defn` removes a link to `metadata` from `defn`.",
            "The `defn` can be either the ",
            "name of a term or type, multiple such names, or a range like `1-4` ",
            "for a range of definitions listed by a prior `find` command."
          ]
    )
    ( \case
        md : defs -> first fromString $ do
          md <- case HQ.fromString md of
            Nothing -> Left "Invalid hash qualified identifier for metadata."
            Just hq -> pure hq
          defs <- traverse Path.parseHQSplit' defs
          Right $ Input.UnlinkI md defs
        _ -> Left (I.help unlink)
    )

names :: InputPattern
names =
  InputPattern
    "names"
    []
    [(Required, definitionQueryArg)]
    "`names foo` shows the hash and all known names for `foo`."
    ( \case
        [thing] -> case HQ.fromString thing of
          Just hq -> Right $ Input.NamesI hq
          Nothing ->
            Left $
              "I was looking for one of these forms: "
                <> P.blue "foo .foo.bar foo#abc #abcde .foo.bar#asdf"
        _ -> Left (I.help names)
    )

dependents, dependencies :: InputPattern
dependents =
  InputPattern
    "dependents"
    []
    []
    "List the named dependents of the specified definition."
    ( \case
        [thing] -> fmap Input.ListDependentsI $ parseHashQualifiedName thing
        _ -> Left (I.help dependents)
    )
dependencies =
  InputPattern
    "dependencies"
    []
    []
    "List the dependencies of the specified definition."
    ( \case
        [thing] -> fmap Input.ListDependenciesI $ parseHashQualifiedName thing
        _ -> Left (I.help dependencies)
    )

namespaceDependencies :: InputPattern
namespaceDependencies =
  InputPattern
    "namespace.dependencies"
    []
    [(Optional, namespaceArg)]
    "List the external dependencies of the specified namespace."
    ( \case
        [p] -> first fromString $ do
          p <- Path.parsePath' p
          pure $ Input.NamespaceDependenciesI (Just p)
        [] -> pure (Input.NamespaceDependenciesI Nothing)
        _ -> Left (I.help namespaceDependencies)
    )

debugNumberedArgs :: InputPattern
debugNumberedArgs =
  InputPattern
    "debug.numberedArgs"
    []
    []
    "Dump the contents of the numbered args state."
    (const $ Right Input.DebugNumberedArgsI)

debugFileHashes :: InputPattern
debugFileHashes =
  InputPattern
    "debug.file"
    []
    []
    "View details about the most recent succesfully typechecked file."
    (const $ Right Input.DebugTypecheckedUnisonFileI)

debugDumpNamespace :: InputPattern
debugDumpNamespace =
  InputPattern
    "debug.dump-namespace"
    []
    [(Required, noCompletions)]
    "Dump the namespace to a text file"
    (const $ Right Input.DebugDumpNamespacesI)

debugDumpNamespaceSimple :: InputPattern
debugDumpNamespaceSimple =
  InputPattern
    "debug.dump-namespace-simple"
    []
    [(Required, noCompletions)]
    "Dump the namespace to a text file"
    (const $ Right Input.DebugDumpNamespaceSimpleI)

debugClearWatchCache :: InputPattern
debugClearWatchCache =
  InputPattern
    "debug.clear-cache"
    []
    [(Required, noCompletions)]
    "Clear the watch expression cache"
    (const $ Right Input.DebugClearWatchI)

test :: InputPattern
test =
  InputPattern
    "test"
    []
    []
    "`test` runs unit tests for the current branch."
    (const $ pure $ Input.TestI True True)

docsToHtml :: InputPattern
docsToHtml =
  InputPattern
    "docs.to-html"
    []
    []
    ( P.wrapColumn2
        [ ( "`docs.to-html .path.to.namespace ~/path/to/file/output`",
            "Render all docs contained within a namespace, no matter how deep,"
              <> "to html files on a file path"
          )
        ]
    )
    ( \case
        [namespacePath, destinationFilePath] -> first fromString $ do
          np <- Path.parsePath' namespacePath
          pure $ Input.DocsToHtmlI np destinationFilePath
        _ -> Left $ showPatternHelp docsToHtml
    )

execute :: InputPattern
execute =
  InputPattern
    "run"
    []
    [(Required, exactDefinitionTermQueryArg), (ZeroPlus, noCompletions)]
    ( P.wrapColumn2
        [ ( "`run mymain args...`",
            "Runs `!mymain`, where `mymain` is searched for in the most recent"
              <> "typechecked file, or in the codebase."
              <> "Any provided arguments will be passed as program arguments as though they were"
              <> "provided at the command line when running mymain as an executable."
          )
        ]
    )
    ( \case
        [w] -> pure $ Input.ExecuteI w []
        (w : ws) -> pure $ Input.ExecuteI w ws
        _ -> Left $ showPatternHelp execute
    )

ioTest :: InputPattern
ioTest =
  InputPattern
    "io.test"
    []
    []
    ( P.wrapColumn2
        [ ( "`io.test mytest`",
            "Runs `!mytest`, where `mytest` is searched for in the most recent"
              <> "typechecked file, or in the codebase."
          )
        ]
    )
    ( \case
        [thing] -> fmap Input.IOTestI $ parseHashQualifiedName thing
        _ -> Left $ showPatternHelp ioTest
    )

makeStandalone :: InputPattern
makeStandalone =
  InputPattern
    "compile"
    ["compile.output"]
    [(Required, exactDefinitionTermQueryArg), (Required, noCompletions)]
    ( P.wrapColumn2
        [ ( "`compile main file`",
            "Outputs a stand alone file that can be directly loaded and"
              <> "executed by unison. Said execution will have the effect of"
              <> "running `!main`."
          )
        ]
    )
    ( \case
        [main, file] ->
          Input.MakeStandaloneI file <$> parseHashQualifiedName main
        _ -> Left $ showPatternHelp makeStandalone
    )

createAuthor :: InputPattern
createAuthor =
  InputPattern
    "create.author"
    []
    [(Required, noCompletions), (Required, noCompletions)]
    ( makeExample createAuthor ["alicecoder", "\"Alice McGee\""]
        <> "creates"
        <> backtick "alicecoder"
        <> "values in"
        <> backtick "metadata.authors"
        <> "and"
        <> backtick "metadata.copyrightHolders"
        <> "."
    )
    ( \case
        symbolStr : authorStr@(_ : _) -> first fromString $ do
          symbol <- Path.definitionNameSegment symbolStr
          -- let's have a real parser in not too long
          let author :: Text
              author = Text.pack $ case (unwords authorStr) of
                quoted@('"' : _) -> (init . tail) quoted
                bare -> bare
          pure $ Input.CreateAuthorI symbol author
        _ -> Left $ showPatternHelp createAuthor
    )

gist :: InputPattern
gist =
  InputPattern
    "push.gist"
    ["gist"]
    [(Required, gitUrlArg)]
    ( P.lines
        [ "Publish the current namespace.",
          "",
          P.wrapColumn2
            [ ( "`gist remote`",
                "publishes the contents of the current namespace into the repo `remote`."
              )
            ]
        ]
    )
    ( \case
        [repoString] -> do
          repo <- parseWriteRepo "repo" repoString
          pure (Input.GistI (Input.GistInput repo))
        _ -> Left (showPatternHelp gist)
    )

authLogin :: InputPattern
authLogin =
  InputPattern
    "auth.login"
    []
    []
    ("Obtain an authentication session with Unison Share")
    ( \case
        [] -> Right Input.AuthLoginI
        _ -> Left (showPatternHelp authLogin)
    )

validInputs :: [InputPattern]
validInputs =
  [ help,
    helpTopics,
    load,
    add,
    previewAdd,
    update,
    previewUpdate,
    updateNoPatch,
    delete,
    forkLocal,
    mergeLocal,
    squashMerge,
    previewMergeLocal,
    diffNamespace,
    names,
    push,
    pushCreate,
    pull,
    pullWithoutHistory,
    pullSilent,
    pushExhaustive,
    pullExhaustive,
    createPullRequest,
    loadPullRequest,
    cd,
    up,
    back,
    deleteNamespace,
    deleteNamespaceForce,
    renameBranch,
    deletePatch,
    renamePatch,
    copyPatch,
    find,
    findShallow,
    findVerbose,
    view,
    display,
    displayTo,
    api,
    ui,
    docs,
    docsToHtml,
    findPatch,
    viewPatch,
    undo,
    history,
    edit,
    renameTerm,
    deleteTerm,
    aliasTerm,
    renameType,
    deleteType,
    aliasType,
    aliasMany,
    todo,
    patch,
    link,
    unlink,
    links,
    createAuthor,
    replace,
    deleteTermReplacement,
    deleteTypeReplacement,
    test,
    ioTest,
    execute,
    viewReflog,
    resetRoot,
    quit,
    updateBuiltins,
    makeStandalone,
    mergeBuiltins,
    mergeIOBuiltins,
    dependents,
    dependencies,
    namespaceDependencies,
    debugNumberedArgs,
    debugFileHashes,
    debugDumpNamespace,
    debugDumpNamespaceSimple,
    debugClearWatchCache,
    gist,
    authLogin
  ]

commandNames :: [String]
commandNames = validInputs >>= \i -> I.patternName i : I.aliases i

commandNameArg :: ArgumentType
commandNameArg =
  ArgumentType
    { typeName = "command",
      suggestions = \q _ _ _ -> pure (exactComplete q (commandNames <> Map.keys helpTopicsMap)),
      globTargets = mempty
    }

exactDefinitionOrPathArg :: ArgumentType
exactDefinitionOrPathArg =
  ArgumentType
    { typeName = "definition or path",
      suggestions =
        allCompletors
          [ termCompletor exactComplete,
            typeCompletor exactComplete,
            pathCompletor exactComplete (Set.map Path.toText . Branch.deepPaths)
          ],
      globTargets = Set.fromList [Globbing.Term, Globbing.Type, Globbing.Namespace]
    }

-- todo: improve this
fuzzyDefinitionQueryArg :: ArgumentType
fuzzyDefinitionQueryArg =
  ArgumentType
    { typeName = "fuzzy definition query",
      suggestions =
        bothCompletors
          (termCompletor fuzzyComplete)
          (typeCompletor fuzzyComplete),
      globTargets = Set.fromList [Globbing.Term, Globbing.Type]
    }

definitionQueryArg :: ArgumentType
definitionQueryArg = fuzzyDefinitionQueryArg {typeName = "definition query"}

exactDefinitionTypeQueryArg :: ArgumentType
exactDefinitionTypeQueryArg =
  ArgumentType
    { typeName = "type definition query",
      suggestions = typeCompletor exactComplete,
      globTargets = Set.fromList [Globbing.Type]
    }

exactDefinitionTermQueryArg :: ArgumentType
exactDefinitionTermQueryArg =
  ArgumentType
    { typeName = "term definition query",
      suggestions = termCompletor exactComplete,
      globTargets = Set.fromList [Globbing.Term]
    }

typeCompletor ::
  Applicative m =>
  (String -> [String] -> [Completion]) ->
  String ->
  Codebase m v a ->
  Branch.Branch m ->
  Path.Absolute ->
  m [Completion]
typeCompletor filterQuery = pathCompletor filterQuery go
  where
    go = Set.map HQ.toText . R.dom . Names.hashQualifyTypesRelation . Names.types . Branch.toNames

termCompletor ::
  Applicative m =>
  (String -> [String] -> [Completion]) ->
  String ->
  Codebase m v a ->
  Branch.Branch m ->
  Path.Absolute ->
  m [Completion]
termCompletor filterQuery = pathCompletor filterQuery go
  where
    go = Set.map HQ.toText . R.dom . Names.hashQualifyTermsRelation . Names.terms . Branch.toNames

patchArg :: ArgumentType
patchArg =
  ArgumentType
    { typeName = "patch",
      suggestions =
        pathCompletor
          exactComplete
          (Set.map Name.toText . Map.keysSet . Branch.deepEdits),
      globTargets = Set.fromList []
    }

allCompletors ::
  Monad m =>
  ( [String -> Codebase m v a -> Branch.Branch m -> Path.Absolute -> m [Completion]] ->
    (String -> Codebase m v a -> Branch.Branch m -> Path.Absolute -> m [Completion])
  )
allCompletors = foldl' bothCompletors I.noSuggestions

bothCompletors ::
  (Monad m) =>
  (String -> t2 -> t3 -> t4 -> m [Completion]) ->
  (String -> t2 -> t3 -> t4 -> m [Completion]) ->
  String ->
  t2 ->
  t3 ->
  t4 ->
  m [Completion]
bothCompletors c1 c2 q code b currentPath = do
  suggestions1 <- c1 q code b currentPath
  suggestions2 <- c2 q code b currentPath
  pure . fixupCompletion q
    . nubOrdOn Completion.display
    $ suggestions1 ++ suggestions2

-- | A completer for namespace paths.
pathCompletor ::
  Applicative f =>
  -- | Turns a query and list of possible completions into a 'Completion'.
  (String -> [String] -> [Completion]) ->
  -- | Construct completions given ucm's current branch context, or the root namespace if
  -- the query is absolute.
  (Branch.Branch0 m -> Set Text) ->
  -- | The portion of this arg that the user has already typed.
  String ->
  codebase ->
  Branch.Branch m ->
  Path.Absolute ->
  f [Completion]
pathCompletor filterQuery getNames query _code b p =
  let b0root = Branch.head b
      b0local = Branch.getAt0 (Path.unabsolute p) b0root
   in -- todo: if these sets are huge, maybe trim results
      pure . filterQuery query . map Text.unpack $
        toList (getNames b0local)
          ++ if "." `isPrefixOf` query
            then map ("." <>) (toList (getNames b0root))
            else []

namespaceArg :: ArgumentType
namespaceArg =
  ArgumentType
    { typeName = "namespace",
      suggestions = pathCompletor completeWithinQueryNamespace (Set.fromList . allSubNamespaces),
      globTargets = Set.fromList [Globbing.Namespace]
    }

-- | Recursively collects all names of namespaces which are children of the branch.
allSubNamespaces :: Branch.Branch0 m -> [Text]
allSubNamespaces b =
  flip Map.foldMapWithKey (Branch.nonEmptyChildren b) $
    \(NameSegment k) (Branch.head -> b') ->
      (k : fmap (\sn -> k <> "." <> sn) (allSubNamespaces b'))

newNameArg :: ArgumentType
newNameArg =
  ArgumentType
    { typeName = "new-name",
      suggestions =
        pathCompletor
          prefixIncomplete
          (Set.map ((<> ".") . Path.toText) . Branch.deepPaths),
      globTargets = mempty
    }

noCompletions :: ArgumentType
noCompletions =
  ArgumentType
    { typeName = "word",
      suggestions = I.noSuggestions,
      globTargets = mempty
    }

-- Arya: I could imagine completions coming from previous git pulls
gitUrlArg :: ArgumentType
gitUrlArg =
  ArgumentType
    { typeName = "git-url",
      suggestions =
        let complete s = pure [Completion s s False]
         in \input _ _ _ -> case input of
              "gh" -> complete "https://github.com/"
              "gl" -> complete "https://gitlab.com/"
              "bb" -> complete "https://bitbucket.com/"
              "ghs" -> complete "git@github.com:"
              "gls" -> complete "git@gitlab.com:"
              "bbs" -> complete "git@bitbucket.com:"
              _ -> pure [],
      globTargets = mempty
    }

collectNothings :: (a -> Maybe b) -> [a] -> [a]
collectNothings f as = [a | (Nothing, a) <- map f as `zip` as]

explainRemote :: P.Pretty CT.ColorText
explainRemote =
  P.lines
    [ P.wrap "where `remote` is a git repository, optionally followed by `:`"
        <> "and an absolute remote path, a branch, or both, such as:",
      P.indentN 2 . P.lines $
        [ P.backticked "https://github.com/org/repo",
          P.backticked "https://github.com/org/repo:.some.remote.path",
          P.backticked "https://github.com/org/repo:some-branch:.some.remote.path",
          P.backticked "https://github.com/org/repo:some-branch"
        ]
    ]
