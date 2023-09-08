{-
   This module defines 'InputPattern' values for every supported input command.
-}
module Unison.CommandLine.InputPatterns where

import Control.Lens (preview, (^.))
import Control.Lens.Cons qualified as Cons
import Data.List (intercalate)
import Data.List.Extra qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These (These (..))
import Network.URI qualified as URI
import System.Console.Haskeline.Completion (Completion (Completion))
import System.Console.Haskeline.Completion qualified as Haskeline
import Text.Megaparsec qualified as P
import U.Codebase.Sqlite.DbId (ProjectBranchId, ProjectId)
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Pretty (prettyProjectName, prettyProjectNameSlash, prettySlashProjectBranchName, prettyURI)
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Merge qualified as Branch
import Unison.Codebase.Editor.Input (DeleteOutput (..), DeleteTarget (..), Input)
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Editor.Output.PushPull (PushPull (Pull, Push))
import Unison.Codebase.Editor.Output.PushPull qualified as PushPull
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace, WriteGitRepo, WriteRemoteNamespace)
import Unison.Codebase.Editor.SlurpResult qualified as SR
import Unison.Codebase.Editor.UriParser (readRemoteNamespaceParser)
import Unison.Codebase.Editor.UriParser qualified as UriParser
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Path.Parse qualified as Path
import Unison.Codebase.PushBehavior qualified as PushBehavior
import Unison.Codebase.SyncMode qualified as SyncMode
import Unison.Codebase.Verbosity (Verbosity)
import Unison.Codebase.Verbosity qualified as Verbosity
import Unison.CommandLine
import Unison.CommandLine.Completion
import Unison.CommandLine.Globbing qualified as Globbing
import Unison.CommandLine.InputPattern (ArgumentType (..), InputPattern (InputPattern), IsOptional (..))
import Unison.CommandLine.InputPattern qualified as I
import Unison.HashQualified qualified as HQ
import Unison.JitInfo qualified as JitInfo
import Unison.Name (Name)
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectAndBranchNames (..), ProjectBranchName, ProjectBranchNameOrLatestRelease (..), ProjectBranchSpecifier (..), ProjectName, Semver)
import Unison.Syntax.HashQualified qualified as HQ (fromString)
import Unison.Syntax.Name qualified as Name (fromText, unsafeFromString)
import Unison.Util.ColorText qualified as CT
import Unison.Util.Monoid (intercalateMap)
import Unison.Util.Pretty qualified as P

showPatternHelp :: InputPattern -> P.Pretty CT.ColorText
showPatternHelp i =
  P.lines
    [ P.bold (fromString $ I.patternName i)
        <> fromString
          ( if not . null $ I.aliases i
              then " (or " <> intercalate ", " (I.aliases i) <> ")"
              else ""
          ),
      I.help i
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

helpFor :: InputPattern -> P.Pretty CT.ColorText
helpFor p = I.help p

mergeBuiltins :: InputPattern
mergeBuiltins =
  InputPattern
    "builtins.merge"
    []
    I.Hidden
    []
    "Adds the builtins to `builtins.` in the current namespace (excluding `io` and misc)."
    (const . pure $ Input.MergeBuiltinsI)

mergeIOBuiltins :: InputPattern
mergeIOBuiltins =
  InputPattern
    "builtins.mergeio"
    []
    I.Hidden
    []
    "Adds all the builtins to `builtins.` in the current namespace, including `io` and misc."
    (const . pure $ Input.MergeIOBuiltinsI)

updateBuiltins :: InputPattern
updateBuiltins =
  InputPattern
    "builtins.update"
    []
    I.Visible
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
    I.Visible
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
    I.Visible
    [(Optional, noCompletionsArg)]
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

clear :: InputPattern
clear =
  InputPattern
    "clear"
    []
    I.Visible
    []
    ( P.wrapColumn2
        [ ( makeExample' clear,
            "Clears the screen."
          )
        ]
    )
    ( \case
        [] -> pure $ Input.ClearI
        _ -> Left (I.help clear)
    )

add :: InputPattern
add =
  InputPattern
    "add"
    []
    I.Visible
    [(ZeroPlus, noCompletionsArg)]
    ( "`add` adds to the codebase all the definitions from the most recently "
        <> "typechecked file."
    )
    $ \ws -> pure $ Input.AddI (Set.fromList $ map Name.unsafeFromString ws)

previewAdd :: InputPattern
previewAdd =
  InputPattern
    "add.preview"
    []
    I.Visible
    [(ZeroPlus, noCompletionsArg)]
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
    I.Visible
    [(ZeroPlus, noCompletionsArg)]
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
    I.Visible
    [(Optional, patchArg), (ZeroPlus, noCompletionsArg)]
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
    I.Visible
    [(ZeroPlus, noCompletionsArg)]
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
    I.Visible
    [(Required, patchArg), (Optional, namespaceArg)]
    ( P.lines
        [ P.wrap $
            makeExample' patch
              <> "rewrites any definitions that depend on "
              <> "definitions with type-preserving edits to use the updated versions of"
              <> "these dependencies.",
          "",
          P.wrapColumn2
            [ ( makeExample patch ["<patch>", "[path]"],
                "applies the given patch"
                  <> "to the given namespace"
              ),
              ( makeExample patch ["<patch>"],
                "applies the given patch"
                  <> "to the current namespace"
              )
            ]
        ]
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
    I.Visible
    [(ZeroPlus, definitionQueryArg)]
    ( P.lines
        [ P.wrap $ makeExample view ["foo"] <> "shows definitions named `foo` within your current namespace.",
          P.wrap $ makeExample view [] <> "without arguments invokes a search to select definitions to view, which requires that `fzf` can be found within your PATH.",
          " ", -- hmm, this blankline seems to be ignored by pretty printer
          P.wrap $
            "Supports glob syntax, where ? acts a wildcard, so"
              <> makeExample view ["List.?"]
              <> "will show `List.map`, `List.filter`, etc, but "
              <> "not `List.map.doc` (since ? only matches 1 name segment)."
        ]
    )
    ( fmap (Input.ShowDefinitionI Input.ConsoleLocation Input.ShowDefinitionLocal)
        . traverse parseHashQualifiedName
    )

viewGlobal :: InputPattern
viewGlobal =
  InputPattern
    "view.global"
    []
    I.Visible
    [(ZeroPlus, definitionQueryArg)]
    ( P.lines
        [ "`view.global foo` prints definitions of `foo` within your codebase.",
          "`view.global` without arguments invokes a search to select definitions to view, which requires that `fzf` can be found within your PATH."
        ]
    )
    ( fmap (Input.ShowDefinitionI Input.ConsoleLocation Input.ShowDefinitionGlobal)
        . traverse parseHashQualifiedName
    )

display :: InputPattern
display =
  InputPattern
    "display"
    []
    I.Visible
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
    I.Visible
    [(Required, noCompletionsArg), (ZeroPlus, definitionQueryArg)]
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
    I.Visible
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
    I.Visible
    []
    "`api` provides details about the API."
    (const $ pure Input.ApiI)

ui :: InputPattern
ui =
  InputPattern
    { patternName = "ui",
      aliases = [],
      visibility = I.Visible,
      argTypes = [(Optional, namespaceOrDefinitionArg)],
      help = P.wrap "`ui` opens the Local UI in the default browser.",
      parse = \case
        [] -> pure $ Input.UiI Path.relativeEmpty'
        [path] -> first fromString $ do
          p <- Path.parsePath' path
          pure $ Input.UiI p
        _ -> Left (I.help ui)
    }

undo :: InputPattern
undo =
  InputPattern
    "undo"
    []
    I.Visible
    []
    "`undo` reverts the most recent change to the codebase."
    (const $ pure Input.UndoI)

viewByPrefix :: InputPattern
viewByPrefix =
  InputPattern
    "view.recursive"
    []
    I.Visible
    [(OnePlus, definitionQueryArg)]
    "`view.recursive Foo` prints the definitions of `Foo` and `Foo.blah`."
    ( fmap (Input.ShowDefinitionByPrefixI Input.ConsoleLocation)
        . traverse parseHashQualifiedName
    )

sfind :: InputPattern
sfind =
  InputPattern "rewrite.find" ["sfind"] I.Visible [(Required, definitionQueryArg)] msg parse
  where
    parse [q] = Input.StructuredFindI Input.FindLocal <$> parseHashQualifiedName q
    parse _ = Left "expected exactly one argument"
    msg =
      P.lines
        [ P.wrap $
            makeExample sfind ["rule1"]
              <> " finds definitions that match any of the left side(s) of `rule`"
              <> "in the current namespace.",
          "",
          P.wrap $
            "The argument `rule1` must refer to a `@rewrite` block or a function that immediately returns"
              <> "a `@rewrite` block."
              <> "It can be in the codebase or scratch file. An example:",
          "",
          "    -- right of ==> is ignored by this command",
          "    rule1 x = @rewrite term x + 1 ==> ()",
          "",
          P.wrap $
            "Here, `x` will stand in for any expression,"
              <> "so this rule will match "
              <> P.backticked' "(42+10+11) + 1" ".",
          "",
          "See https://unison-lang.org/learn/structured-find to learn more.",
          "",
          P.wrap ("Also see the related command" <> makeExample sfindReplace [])
        ]

sfindReplace :: InputPattern
sfindReplace =
  InputPattern "rewrite" ["sfind.replace"] I.Visible [(Required, definitionQueryArg)] msg parse
  where
    parse [q] = Input.StructuredFindReplaceI <$> parseHashQualifiedName q
    parse _ = Left "expected exactly one argument"
    msg :: P.Pretty CT.ColorText
    msg =
      P.lines
        [ makeExample sfindReplace ["rule1"] <> " rewrites definitions in the latest scratch file.",
          "",
          P.wrap $
            "The argument `rule1` must refer to a `@rewrite` block or a function that immediately returns"
              <> "a `@rewrite` block."
              <> "It can be in the codebase or scratch file. An example:",
          "",
          "    rule1 x = @rewrite term x + 1 ==> Nat.increment x",
          "",
          P.wrap $
            "Here, `x` will stand in for any expression wherever this rewrite is applied,"
              <> "so this rule will match "
              <> P.backticked "(42+10+11) + 1"
              <> "and replace it with"
              <> P.backticked' "Nat.increment (42+10+11)" ".",
          "",
          "See https://unison-lang.org/learn/structured-find to learn more.",
          "",
          P.wrap ("Also see the related command" <> makeExample sfind [])
        ]

find :: InputPattern
find = find' "find" Input.FindLocal

findAll :: InputPattern
findAll = find' "find.all" Input.FindLocalAndDeps

findGlobal :: InputPattern
findGlobal = find' "find.global" Input.FindGlobal

find' :: String -> Input.FindScope -> InputPattern
find' cmd fscope =
  InputPattern
    cmd
    []
    I.Visible
    [(ZeroPlus, exactDefinitionArg)]
    ( P.wrapColumn2
        [ ("`find`", "lists all definitions in the current namespace."),
          ( "`find foo`",
            "lists all definitions with a name similar to 'foo' in the current "
              <> "namespace (excluding those under 'lib')."
          ),
          ( "`find foo bar`",
            "lists all definitions with a name similar to 'foo' or 'bar' in the "
              <> "current namespace (excluding those under 'lib')."
          ),
          ( "find.all foo",
            "lists all definitions with a name similar to 'foo' in the current "
              <> "namespace (including one level of 'lib')."
          ),
          ( "find.global foo",
            "lists all definitions with a name similar to 'foo' in any namespace"
          )
        ]
    )
    (pure . Input.FindI False fscope)

findShallow :: InputPattern
findShallow =
  InputPattern
    "list"
    ["ls", "dir"]
    I.Visible
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
    []
    I.Visible
    [(ZeroPlus, exactDefinitionArg)]
    ( "`find.verbose` searches for definitions like `find`, but includes hashes "
        <> "and aliases in the results."
    )
    (pure . Input.FindI True Input.FindLocal)

findVerboseAll :: InputPattern
findVerboseAll =
  InputPattern
    "find.all.verbose"
    []
    I.Visible
    [(ZeroPlus, exactDefinitionArg)]
    ( "`find.all.verbose` searches for definitions like `find.all`, but includes hashes "
        <> "and aliases in the results."
    )
    (pure . Input.FindI True Input.FindLocalAndDeps)

findPatch :: InputPattern
findPatch =
  InputPattern
    "find.patch"
    ["list.patch", "ls.patch"]
    I.Visible
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
    I.Visible
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
    I.Visible
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

deleteGen :: Maybe String -> String -> ([Path.HQSplit'] -> DeleteTarget) -> InputPattern
deleteGen suffix target mkTarget =
  let cmd = maybe "delete" ("delete." <>) suffix
      info =
        P.wrapColumn2
          [ ( P.sep
                " "
                [ backtick (P.sep " " [P.string cmd, "foo"]),
                  "removes the",
                  P.string target,
                  "name `foo` from the namespace."
                ],
              ""
            ),
            ( P.sep
                " "
                [ backtick (P.sep " " [P.string cmd, "foo bar"]),
                  "removes the",
                  P.string target,
                  "name `foo` and `bar` from the namespace."
                ],
              ""
            )
          ]
      warn =
        P.sep
          " "
          [ backtick (P.string cmd),
            "takes an argument, like",
            backtick (P.sep " " [P.string cmd, "name"]) <> "."
          ]
   in InputPattern
        cmd
        []
        I.Visible
        [(OnePlus, exactDefinitionTermQueryArg)]
        info
        ( \case
            [] -> Left . P.warnCallout $ P.wrap warn
            queries -> first fromString $ do
              paths <- traverse Path.parseHQSplit' queries
              pure $ Input.DeleteI (mkTarget paths)
        )

delete :: InputPattern
delete = deleteGen Nothing "term or type" (DeleteTarget'TermOrType DeleteOutput'NoDiff)

deleteVerbose :: InputPattern
deleteVerbose = deleteGen (Just "verbose") "term or type" (DeleteTarget'TermOrType DeleteOutput'Diff)

deleteTerm :: InputPattern
deleteTerm = deleteGen (Just "term") "term" (DeleteTarget'Term DeleteOutput'NoDiff)

deleteTermVerbose :: InputPattern
deleteTermVerbose = deleteGen (Just "term.verbose") "term" (DeleteTarget'Term DeleteOutput'Diff)

deleteType :: InputPattern
deleteType = deleteGen (Just "type") "type" (DeleteTarget'Type DeleteOutput'NoDiff)

deleteTypeVerbose :: InputPattern
deleteTypeVerbose = deleteGen (Just "type.verbose") "type" (DeleteTarget'Type DeleteOutput'Diff)

deleteTermReplacementCommand :: String
deleteTermReplacementCommand = "delete.term-replacement"

deleteTypeReplacementCommand :: String
deleteTypeReplacementCommand = "delete.type-replacement"

deleteReplacement :: Bool -> InputPattern
deleteReplacement isTerm =
  InputPattern
    commandName
    []
    I.Visible
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

deleteProject :: InputPattern
deleteProject =
  InputPattern
    { patternName = "delete.project",
      aliases = ["project.delete"],
      visibility = I.Visible,
      argTypes = [(Required, projectNameArg)],
      help =
        P.wrapColumn2
          [ ("`delete.project foo`", "deletes the local project `foo`")
          ],
      parse = \case
        [name]
          | Right project <- tryInto @ProjectName (Text.pack name) ->
              Right (Input.DeleteI (DeleteTarget'Project project))
        _ -> Left (showPatternHelp deleteProject)
    }

deleteBranch :: InputPattern
deleteBranch =
  InputPattern
    { patternName = "delete.branch",
      aliases = ["branch.delete"],
      visibility = I.Visible,
      argTypes = [(Required, projectAndBranchNamesArg True)],
      help =
        P.wrapColumn2
          [ ("`delete.branch foo/bar`", "deletes the branch `bar` in the project `foo`"),
            ("`delete.branch /bar`", "deletes the branch `bar` in the current project")
          ],
      parse = \case
        [name] ->
          case tryInto @(ProjectAndBranch (Maybe ProjectName) ProjectBranchName) (Text.pack name) of
            Left _ -> Left (showPatternHelp deleteBranch)
            Right projectAndBranch -> Right (Input.DeleteI (DeleteTarget'ProjectBranch projectAndBranch))
        _ -> Left (showPatternHelp deleteBranch)
    }

deleteTermReplacement :: InputPattern
deleteTermReplacement = deleteReplacement True

deleteTypeReplacement :: InputPattern
deleteTypeReplacement = deleteReplacement False

aliasTerm :: InputPattern
aliasTerm =
  InputPattern
    "alias.term"
    []
    I.Visible
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
    I.Visible
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
    I.Visible
    [(Required, definitionQueryArg), (OnePlus, exactDefinitionArg)]
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
    I.Visible
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
    I.Visible
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
    I.Visible
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
    I.Visible
    [(Required, namespaceArg)]
    "`delete.namespace <foo>` deletes the namespace `foo`"
    (deleteNamespaceParser (I.help deleteNamespace) Input.Try)

deleteNamespaceForce :: InputPattern
deleteNamespaceForce =
  InputPattern
    "delete.namespace.force"
    []
    I.Visible
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
          $ Input.DeleteI (DeleteTarget'Namespace insistence Nothing)
      [p] -> first fromString $ do
        p <- Path.parseSplit' Path.definitionNameSegment p
        pure $ Input.DeleteI (DeleteTarget'Namespace insistence (Just p))
      _ -> Left helpText
  )

deletePatch :: InputPattern
deletePatch =
  InputPattern
    "delete.patch"
    []
    I.Visible
    [(Required, patchArg)]
    "`delete.patch <foo>` deletes the patch `foo`"
    ( \case
        [p] -> first fromString $ do
          p <- Path.parseSplit' Path.definitionNameSegment p
          pure . Input.DeleteI $ DeleteTarget'Patch p
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
    I.Visible
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
    I.Visible
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
    I.Visible
    [(Required, namespaceArg), (Required, newNameArg)]
    "`move.namespace foo bar` renames the path `foo` to `bar`."
    ( \case
        [src, dest] -> first fromString $ do
          src <- Path.parsePath' src
          dest <- Path.parsePath' dest
          pure $ Input.MoveBranchI src dest
        _ -> Left (I.help renameBranch)
    )

history :: InputPattern
history =
  InputPattern
    "history"
    []
    I.Visible
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
    I.Visible
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

reset :: InputPattern
reset =
  InputPattern
    "reset"
    []
    I.Visible
    [ (Required, namespaceArg),
      (Optional, namespaceArg)
    ]
    ( P.wrapColumn2
        [ ("`reset #pvfd222s8n`", "reset the current namespace to the causal `#pvfd222s8n`"),
          ("`reset foo`", "reset the current namespace to that of the `foo` namespace."),
          ("`reset foo bar`", "reset the namespace `bar` to that of the `foo` namespace."),
          ("`reset #pvfd222s8n /topic`", "reset the branch `topic` of the current project to the causal `#pvfd222s8n`.")
        ]
    )
    ( maybeToEither (I.help reset) . \case
        arg0 : restArgs -> do
          arg0 <- branchIdOrProject arg0
          arg1 <- case restArgs of
            [] -> pure Nothing
            arg1 : [] -> Just <$> parseLooseCodeOrProject arg1
            _ -> Nothing
          Just (Input.ResetI arg0 arg1)
        _ -> Nothing
    )
  where
    branchIdOrProject ::
      String ->
      Maybe
        ( These
            Input.BranchId
            (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
        )
    branchIdOrProject str =
      let branchIdRes = Input.parseBranchId str
          projectRes = tryInto @(ProjectAndBranch (Maybe ProjectName) ProjectBranchName) (Text.pack str)
       in case (branchIdRes, projectRes) of
            (Left _, Left _) -> Nothing
            (Left _, Right pr) -> Just (That pr)
            (Right bid, Left _) -> Just (This bid)
            (Right bid, Right pr) -> Just (These bid pr)

-- asBranch = tryInto @(ProjectAndBranch (Maybe ProjectName) ProjectBranchName) (Text.pack inputString)

resetRoot :: InputPattern
resetRoot =
  InputPattern
    "reset-root"
    []
    I.Visible
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

pull :: InputPattern
pull =
  pullImpl "pull" ["pull.silent"] Verbosity.Silent Input.PullWithHistory "without listing the merged entities"

pullVerbose :: InputPattern
pullVerbose = pullImpl "pull.verbose" [] Verbosity.Verbose Input.PullWithHistory "and lists the merged entities"

pullWithoutHistory :: InputPattern
pullWithoutHistory =
  pullImpl
    "pull.without-history"
    []
    Verbosity.Silent
    Input.PullWithoutHistory
    "without including the remote's history. This usually results in smaller codebase sizes."

pullImpl :: String -> [String] -> Verbosity -> Input.PullMode -> P.Pretty CT.ColorText -> InputPattern
pullImpl name aliases verbosity pullMode addendum = do
  self
  where
    self =
      InputPattern
        { patternName = name,
          aliases = aliases,
          visibility = I.Visible,
          argTypes = [(Optional, remoteNamespaceArg), (Optional, namespaceArg)],
          help =
            P.lines
              [ P.wrap
                  "The"
                  <> makeExample' self
                  <> "command merges a remote namespace into a local namespace"
                  <> addendum,
                "",
                P.wrapColumn2
                  [ ( makeExample self ["@unison/base/main"],
                      "merges the branch `main`"
                        <> "of the Unison Share hosted project `@unison/base`"
                        <> "into the current namespace"
                    ),
                    ( makeExample self ["@unison/base/main", "my-base/topic"],
                      "merges the branch `main`"
                        <> "of the Unison Share hosted project `@unison/base`"
                        <> "into the branch `topic` of the local `my-base` project"
                    ),
                    ( makeExample self ["remote", "local"],
                      "merges the remote namespace `remote`"
                        <> "into the local namespace `local"
                    ),
                    ( makeExample self ["remote"],
                      "merges the remote namespace `remote`"
                        <> "into the current namespace"
                    ),
                    ( makeExample' self,
                      "merges the remote namespace configured in `.unisonConfig`"
                        <> "at the key `RemoteMappings.<namespace>` where `<namespace>` is the current namespace,"
                    )
                  ],
                "",
                explainRemote Pull
              ],
          parse =
            maybeToEither (I.help self) . \case
              [] -> Just $ Input.PullRemoteBranchI Input.PullSourceTarget0 SyncMode.ShortCircuit pullMode verbosity
              [sourceString] -> do
                source <- parsePullSource (Text.pack sourceString)
                Just $ Input.PullRemoteBranchI (Input.PullSourceTarget1 source) SyncMode.ShortCircuit pullMode verbosity
              [sourceString, targetString] -> do
                source <- parsePullSource (Text.pack sourceString)
                target <- parseLooseCodeOrProject targetString
                Just $
                  Input.PullRemoteBranchI
                    (Input.PullSourceTarget2 source target)
                    SyncMode.ShortCircuit
                    pullMode
                    verbosity
              _ -> Nothing
        }

pullExhaustive :: InputPattern
pullExhaustive =
  InputPattern
    "debug.pull-exhaustive"
    []
    I.Hidden
    [(Required, remoteNamespaceArg), (Optional, namespaceArg)]
    ( P.lines
        [ P.wrap $
            "The "
              <> makeExample' pullExhaustive
              <> "command can be used in place of"
              <> makeExample' pullVerbose
              <> "to complete namespaces"
              <> "which were pulled incompletely due to a bug in UCM"
              <> "versions M1l and earlier.  It may be extra slow!"
        ]
    )
    ( maybeToEither (I.help pullExhaustive) . \case
        [] ->
          Just $
            Input.PullRemoteBranchI
              Input.PullSourceTarget0
              SyncMode.Complete
              Input.PullWithHistory
              Verbosity.Verbose
        [sourceString] -> do
          source <- parsePullSource (Text.pack sourceString)
          Just $
            Input.PullRemoteBranchI
              (Input.PullSourceTarget1 source)
              SyncMode.Complete
              Input.PullWithHistory
              Verbosity.Verbose
        [sourceString, targetString] -> do
          source <- parsePullSource (Text.pack sourceString)
          target <- parseLooseCodeOrProject targetString
          Just $
            Input.PullRemoteBranchI
              (Input.PullSourceTarget2 source target)
              SyncMode.Complete
              Input.PullWithHistory
              Verbosity.Verbose
        _ -> Nothing
    )

debugTabCompletion :: InputPattern
debugTabCompletion =
  InputPattern
    "debug.tab-complete"
    []
    I.Hidden
    [(ZeroPlus, noCompletionsArg)]
    ( P.lines
        [ P.wrap $ "This command can be used to test and debug ucm's tab-completion within transcripts.",
          P.wrap $ "Completions which are finished are prefixed with a *"
        ]
    )
    ( \inputs ->
        Right $ Input.DebugTabCompletionI inputs
    )

push :: InputPattern
push =
  InputPattern
    "push"
    []
    I.Visible
    [(Required, remoteNamespaceArg), (Optional, namespaceArg)]
    ( P.lines
        [ P.wrap
            "The `push` command merges a local project or namespace into a remote project or namespace.",
          "",
          P.wrapColumn2
            [ ( "`push <remote> <local>`",
                "publishes the contents of a local namespace or branch"
                  <> "into a remote namespace or branch."
              ),
              ( "`push <remote>`",
                "publishes the current namespace or branch into a remote namespace or branch"
              ),
              ( "`push`",
                "publishes the current namespace or branch. Remote mappings for namespaces are configured in"
                  <> "your `.unisonConfig` at the key `RemoteMappings.<namespace>` where `<namespace>` is the "
                  <> "current namespace. Remote mappings for branches default to the branch that you cloned from"
                  <> "or pushed to initially. Otherwise, it is pushed to"
                  <> P.group "@<user handle>/<local project name>"
              )
            ],
          "",
          explainRemote Push
        ]
    )
    \args -> do
      sourceTarget <-
        case args of
          [] -> Right Input.PushSourceTarget0
          [targetStr] -> do
            target <- parsePushTarget targetStr
            Right (Input.PushSourceTarget1 target)
          [targetStr, sourceStr] -> do
            target <- parsePushTarget targetStr
            source <- parsePushSource sourceStr
            Right (Input.PushSourceTarget2 source target)
          _ -> Left (I.help push)
      Right $
        Input.PushRemoteBranchI
          Input.PushRemoteBranchInput
            { sourceTarget,
              pushBehavior = PushBehavior.RequireNonEmpty,
              syncMode = SyncMode.ShortCircuit
            }

pushCreate :: InputPattern
pushCreate =
  InputPattern
    "push.create"
    []
    I.Visible
    [(Required, remoteNamespaceArg), (Optional, namespaceArg)]
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
                "publishes the current namespace into the remote namespace configured in your `.unisonConfig`"
                  <> "at the key `RemoteMappings.<namespace>` where `<namespace>` is the current namespace,"
                  <> "then publishes the current namespace to that location."
              )
            ],
          "",
          explainRemote Push
        ]
    )
    \args -> do
      sourceTarget <-
        case args of
          [] -> Right Input.PushSourceTarget0
          [targetStr] -> do
            target <- parsePushTarget targetStr
            Right (Input.PushSourceTarget1 target)
          [targetStr, sourceStr] -> do
            target <- parsePushTarget targetStr
            source <- parsePushSource sourceStr
            Right (Input.PushSourceTarget2 source target)
          _ -> Left (I.help pushForce)
      Right $
        Input.PushRemoteBranchI
          Input.PushRemoteBranchInput
            { sourceTarget,
              pushBehavior = PushBehavior.RequireEmpty,
              syncMode = SyncMode.ShortCircuit
            }

pushForce :: InputPattern
pushForce =
  InputPattern
    "unsafe.force-push"
    []
    I.Hidden
    [(Required, remoteNamespaceArg), (Optional, namespaceArg)]
    (P.wrap "Like `push`, but overwrites any remote namespace.")
    \args -> do
      sourceTarget <-
        case args of
          [] -> Right Input.PushSourceTarget0
          [targetStr] -> do
            target <- parsePushTarget targetStr
            Right (Input.PushSourceTarget1 target)
          [targetStr, sourceStr] -> do
            target <- parsePushTarget targetStr
            source <- parsePushSource sourceStr
            Right (Input.PushSourceTarget2 source target)
          _ -> Left (I.help pushForce)
      Right $
        Input.PushRemoteBranchI
          Input.PushRemoteBranchInput
            { sourceTarget,
              pushBehavior = PushBehavior.ForcePush,
              syncMode = SyncMode.ShortCircuit
            }

pushExhaustive :: InputPattern
pushExhaustive =
  InputPattern
    "debug.push-exhaustive"
    []
    I.Hidden
    [(Required, remoteNamespaceArg), (Optional, namespaceArg)]
    ( P.lines
        [ P.wrap $
            "The "
              <> makeExample' pushExhaustive
              <> "command can be used in place of"
              <> makeExample' push
              <> "to repair remote namespaces"
              <> "which were pushed incompletely due to a bug in UCM"
              <> "versions M1l and earlier. It may be extra slow!"
        ]
    )
    \args -> do
      sourceTarget <-
        case args of
          [] -> Right Input.PushSourceTarget0
          [targetStr] -> do
            target <- parsePushTarget targetStr
            Right (Input.PushSourceTarget1 target)
          [targetStr, sourceStr] -> do
            target <- parsePushTarget targetStr
            source <- parsePushSource sourceStr
            Right (Input.PushSourceTarget2 source target)
          _ -> Left (I.help pushExhaustive)
      Right $
        Input.PushRemoteBranchI
          Input.PushRemoteBranchInput
            { sourceTarget,
              pushBehavior = PushBehavior.RequireNonEmpty,
              syncMode = SyncMode.Complete
            }

squashMerge :: InputPattern
squashMerge =
  InputPattern
    { patternName = "merge.squash",
      aliases = ["squash"],
      visibility = I.Visible,
      argTypes = [(Required, namespaceArg), (Required, namespaceArg)],
      help =
        P.wrap $
          makeExample squashMerge ["src", "dest"]
            <> "merges `src` namespace into `dest`,"
            <> "discarding the history of `src` in the process."
            <> "The resulting `dest` will have (at most) 1"
            <> "additional history entry.",
      parse =
        maybeToEither (I.help squashMerge) . \case
          [src, dest] -> do
            src <- parseLooseCodeOrProject src
            dest <- parseLooseCodeOrProject dest
            Just $ Input.MergeLocalBranchI src dest Branch.SquashMerge
          _ -> Nothing
    }

mergeLocal :: InputPattern
mergeLocal =
  InputPattern
    "merge"
    []
    I.Visible
    [ (Required, namespaceArg),
      (Optional, namespaceArg)
    ]
    ( P.column2
        [ ( "`merge foo/bar baz/qux`",
            "merges the `foo/bar` branch into the `baz/qux` branch"
          ),
          ( "`merge /topic /main`",
            "merges the branch `topic` of the current project into the `main` branch of the current project"
          ),
          ( "`merge foo/topic /main`",
            "merges the branch `topic` of the project `foo` into the `main` branch of the current project"
          ),
          ( "`merge /topic foo/main`",
            "merges the branch `topic` of the current project into the `main` branch of the project 'foo`"
          ),
          ( "`merge .src`",
            "merges `.src` namespace into the current namespace"
          ),
          ( "`merge .src .dest`",
            "merges `.src` namespace into the `dest` namespace"
          )
        ]
    )
    ( maybeToEither (I.help mergeLocal) . \case
        [src] -> do
          src <- parseLooseCodeOrProject src
          Just $ Input.MergeLocalBranchI src (This Path.relativeEmpty') Branch.RegularMerge
        [src, dest] -> do
          src <- parseLooseCodeOrProject src
          dest <- parseLooseCodeOrProject dest
          Just $ Input.MergeLocalBranchI src dest Branch.RegularMerge
        _ -> Nothing
    )

parseLooseCodeOrProject :: String -> Maybe Input.LooseCodeOrProject
parseLooseCodeOrProject inputString =
  case (asLooseCode, asBranch) of
    (Right path, Left _) -> Just (This path)
    (Left _, Right branch) -> Just (That branch)
    (Right path, Right branch) -> Just (These path branch)
    (Left _, Left _) -> Nothing
  where
    asLooseCode = Path.parsePath' inputString
    asBranch = tryInto @(ProjectAndBranch (Maybe ProjectName) ProjectBranchName) (Text.pack inputString)

diffNamespace :: InputPattern
diffNamespace =
  InputPattern
    "diff.namespace"
    []
    I.Visible
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
    I.Visible
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
    ( maybeToEither (I.help previewMergeLocal) . \case
        [src] -> do
          src <- parseLooseCodeOrProject src
          pure $ Input.PreviewMergeLocalBranchI src (This Path.relativeEmpty')
        [src, dest] -> do
          src <- parseLooseCodeOrProject src
          dest <- parseLooseCodeOrProject dest
          pure $ Input.PreviewMergeLocalBranchI src dest
        _ -> Nothing
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
        I.Visible
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
    I.Visible
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
    { patternName = "edit",
      aliases = [],
      visibility = I.Visible,
      argTypes = [(OnePlus, definitionQueryArg)],
      help =
        P.lines
          [ "`edit foo` prepends the definition of `foo` to the top of the most "
              <> "recently saved file.",
            "`edit` without arguments invokes a search to select a definition for editing, which requires that `fzf` can be found within your PATH."
          ],
      parse =
        fmap (Input.ShowDefinitionI Input.LatestFileLocation Input.ShowDefinitionLocal)
          . traverse parseHashQualifiedName
    }

topicNameArg :: ArgumentType
topicNameArg =
  ArgumentType
    { typeName = "topic",
      suggestions = \q _ _ _ -> pure (exactComplete q $ Map.keys helpTopicsMap),
      globTargets = mempty
    }

codebaseServerNameArg :: ArgumentType
codebaseServerNameArg =
  ArgumentType
    { typeName = "codebase-server",
      suggestions = \q _ _ _ -> pure (exactComplete q $ Map.keys helpTopicsMap),
      globTargets = mempty
    }

helpTopics :: InputPattern
helpTopics =
  InputPattern
    "help-topics"
    ["help-topic"]
    I.Visible
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
      ("remotes", remotesMsg),
      ("namespaces", pathnamesMsg),
      ("projects", projectsMsg)
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
            "Unison caches the results of "
              <> P.blue "test>"
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
            "There are two kinds of namespaces,"
              <> P.group (P.blue "absolute" <> ",")
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
            "then doing an"
              <> P.blue "add"
              <> "will create the definition with the absolute name"
              <> P.group (P.blue ".foo.bar.x" <> " = 41"),
          "",
          P.wrap $
            "and you can refer to"
              <> P.green "x"
              <> "by its absolute name "
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
    remotesMsg =
      P.callout "\129302" . P.lines $
        [ P.wrap $
            "Local projects may be associated with at most one remote project on Unison Share."
              <> "When this relationship is established, it becomes the default argument for a"
              <> "number of share commands. For example, running `push` or `pull` in a project"
              <> "with no arguments will push to or pull from the associated remote, if it exists.",
          "",
          P.wrap $
            "This association is created automatically on when a project is created by `clone`."
              <> "If the project was created locally then the relationship will be established on"
              <> "the first `push`."
        ]
    projectsMsg =
      P.lines $
        [ P.wrap $
            "A project is a versioned collection of code that can be edited, published, and depended on other projects."
              <> "Unison projects are analogous to Git repositories.",
          "",
          P.column2
            [ (patternName projectCreate, "create a new project"),
              (patternName projectsInputPattern, "list all your projects"),
              (patternName branchInputPattern, "create a new workstream"),
              (patternName branchesInputPattern, "list all your branches"),
              (patternName mergeLocal, "merge one branch into another"),
              (patternName projectSwitch, "switch to a project or branch"),
              (patternName push, "upload your changes to Unison Share"),
              (patternName pull, "download code(/changes/updates) from Unison Share"),
              (patternName clone, "download a Unison Share project or branch for contribution")
            ],
          "",
          tip ("Use" <> makeExample help [patternName projectCreate] <> "to learn more."),
          "",
          P.wrap $
            "For full documentation, see"
              <> prettyURI (fromJust (URI.parseURI "https://unison-lang.org/learn/projects"))
        ]

help :: InputPattern
help =
  InputPattern
    "help"
    ["?"]
    I.Visible
    [(Optional, commandNameArg)]
    "`help` shows general help and `help <cmd>` shows help for one command."
    ( \case
        [] ->
          Left $
            intercalateMap
              "\n\n"
              showPatternHelp
              visibleInputs
        [cmd] ->
          case (Map.lookup cmd commandsByName, isHelp cmd) of
            (Nothing, Just msg) -> Left msg
            (Nothing, Nothing) -> Left . warn $ "I don't know of that command. Try `help`."
            (Just pat, Nothing) -> Left $ showPatternHelp pat
            -- If we have a command and a help topic with the same name (like "projects"), then append a tip to the
            -- command's help that suggests running `help-topic command`
            (Just pat, Just _) ->
              Left $
                showPatternHelp pat
                  <> P.newline
                  <> P.newline
                  <> ( tip $
                         "To read more about"
                           <> P.group (P.string cmd <> ",")
                           <> "use"
                           <> makeExample helpTopics [P.string cmd]
                     )
        _ -> Left $ warn "Use `help <cmd>` or `help`."
    )
  where
    commandsByName =
      Map.fromList $ do
        input@I.InputPattern {I.patternName, I.aliases} <- validInputs
        name <- patternName : aliases
        pure (name, input)
    isHelp s = Map.lookup s helpTopicsMap

quit :: InputPattern
quit =
  InputPattern
    "quit"
    ["exit", ":q"]
    I.Visible
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
    I.Visible
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
    I.Visible
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
    I.Visible
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
    I.Visible
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

names :: Input.IsGlobal -> InputPattern
names isGlobal =
  InputPattern
    cmdName
    []
    I.Visible
    [(Required, definitionQueryArg)]
    (P.wrap $ makeExample (names isGlobal) ["foo"] <> " shows the hash and all known names for `foo`.")
    ( \case
        [thing] -> case HQ.fromString thing of
          Just hq -> Right $ Input.NamesI isGlobal hq
          Nothing ->
            Left $
              "I was looking for one of these forms: "
                <> P.blue "foo .foo.bar foo#abc #abcde .foo.bar#asdf"
        _ -> Left (I.help (names isGlobal))
    )
  where
    cmdName = if isGlobal then "names.global" else "names"

dependents, dependencies :: InputPattern
dependents =
  InputPattern
    "dependents"
    []
    I.Visible
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
    I.Visible
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
    I.Visible
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
    I.Visible
    []
    "Dump the contents of the numbered args state."
    (const $ Right Input.DebugNumberedArgsI)

debugFileHashes :: InputPattern
debugFileHashes =
  InputPattern
    "debug.file"
    []
    I.Visible
    []
    "View details about the most recent succesfully typechecked file."
    (const $ Right Input.DebugTypecheckedUnisonFileI)

debugDumpNamespace :: InputPattern
debugDumpNamespace =
  InputPattern
    "debug.dump-namespace"
    []
    I.Visible
    [(Required, noCompletionsArg)]
    "Dump the namespace to a text file"
    (const $ Right Input.DebugDumpNamespacesI)

debugDumpNamespaceSimple :: InputPattern
debugDumpNamespaceSimple =
  InputPattern
    "debug.dump-namespace-simple"
    []
    I.Visible
    [(Required, noCompletionsArg)]
    "Dump the namespace to a text file"
    (const $ Right Input.DebugDumpNamespaceSimpleI)

debugClearWatchCache :: InputPattern
debugClearWatchCache =
  InputPattern
    "debug.clear-cache"
    []
    I.Visible
    [(Required, noCompletionsArg)]
    "Clear the watch expression cache"
    (const $ Right Input.DebugClearWatchI)

debugDoctor :: InputPattern
debugDoctor =
  InputPattern
    "debug.doctor"
    []
    I.Visible
    []
    ( P.wrap "Analyze your codebase for errors and inconsistencies."
    )
    ( \case
        [] -> Right $ Input.DebugDoctorI
        _ -> Left (showPatternHelp debugDoctor)
    )

debugNameDiff :: InputPattern
debugNameDiff =
  InputPattern
    { patternName = "debug.name-diff",
      aliases = [],
      visibility = I.Hidden,
      argTypes = [(Required, namespaceArg), (Required, namespaceArg)],
      help = P.wrap "List all name changes between two causal hashes. Does not detect patch or metadata changes.",
      parse =
        ( \case
            [from, to] -> first fromString $ do
              fromSCH <- Input.parseShortCausalHash from
              toSCH <- Input.parseShortCausalHash to
              pure $ Input.DebugNameDiffI fromSCH toSCH
            _ -> Left (I.help debugNameDiff)
        )
    }

test :: InputPattern
test =
  InputPattern
    "test"
    []
    I.Visible
    []
    "`test` runs unit tests for the current branch."
    ( const $
        pure $
          Input.TestI
            Input.TestInput
              { includeLibNamespace = False,
                showFailures = True,
                showSuccesses = True
              }
    )

testAll :: InputPattern
testAll =
  InputPattern
    "test.all"
    []
    I.Visible
    []
    "`test.all` runs unit tests for the current branch (including the `lib` namespace)."
    ( const $
        pure $
          Input.TestI
            Input.TestInput
              { includeLibNamespace = True,
                showFailures = True,
                showSuccesses = True
              }
    )

docsToHtml :: InputPattern
docsToHtml =
  InputPattern
    "docs.to-html"
    []
    I.Visible
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

docToMarkdown :: InputPattern
docToMarkdown =
  InputPattern
    "debug.doc-to-markdown"
    []
    I.Visible
    []
    ( P.wrapColumn2
        [ ( "`debug.doc-to-markdown term.doc`",
            "Render a doc to markdown."
          )
        ]
    )
    ( \case
        [docNameText] -> first fromString $ do
          docName <- maybeToEither "Invalid name" . Name.fromText . Text.pack $ docNameText
          pure $ Input.DocToMarkdownI docName
        _ -> Left $ showPatternHelp docToMarkdown
    )

execute :: InputPattern
execute =
  InputPattern
    "run"
    []
    I.Visible
    [(Required, exactDefinitionTermQueryArg), (ZeroPlus, noCompletionsArg)]
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

saveExecuteResult :: InputPattern
saveExecuteResult =
  InputPattern
    "add.run"
    []
    I.Visible
    [(Required, newNameArg)]
    ( "`add.run name` adds to the codebase the result of the most recent `run` command"
        <> "as `name`."
    )
    ( \case
        [w] -> pure $ Input.SaveExecuteResultI (Name.unsafeFromString w)
        _ -> Left $ showPatternHelp saveExecuteResult
    )

ioTest :: InputPattern
ioTest =
  InputPattern
    { patternName = "io.test",
      aliases = ["test.io"],
      visibility = I.Visible,
      argTypes = [(Required, exactDefinitionTermQueryArg)],
      help =
        P.wrapColumn2
          [ ( "`io.test mytest`",
              "Runs `!mytest`, where `mytest` is a delayed test that can use the `IO` and `Exception` abilities."
            )
          ],
      parse = \case
        [thing] -> fmap Input.IOTestI $ parseHashQualifiedName thing
        _ -> Left $ showPatternHelp ioTest
    }

makeStandalone :: InputPattern
makeStandalone =
  InputPattern
    "compile"
    ["compile.output"]
    I.Visible
    [(Required, exactDefinitionTermQueryArg), (Required, noCompletionsArg)]
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

runScheme :: InputPattern
runScheme =
  InputPattern
    "run.native"
    []
    I.Visible
    [(Required, exactDefinitionTermQueryArg)]
    ( P.wrapColumn2
        [ ( makeExample runScheme ["main", "args"],
            "Executes !main using native compilation via scheme."
          )
        ]
    )
    ( \case
        (main : args) ->
          flip Input.ExecuteSchemeI args <$> parseHashQualifiedName main
        _ -> Left $ showPatternHelp runScheme
    )

compileScheme :: InputPattern
compileScheme =
  InputPattern
    "compile.native"
    []
    I.Visible
    [(Required, exactDefinitionTermQueryArg), (Required, noCompletionsArg)]
    ( P.wrapColumn2
        [ ( makeExample compileScheme ["main", "file"],
            "Creates stand alone executable via compilation to"
              <> "scheme. The created executable will have the effect"
              <> "of running `!main`."
          )
        ]
    )
    ( \case
        [main, file] ->
          Input.CompileSchemeI file <$> parseHashQualifiedName main
        _ -> Left $ showPatternHelp compileScheme
    )

schemeLibgen :: InputPattern
schemeLibgen =
  InputPattern
    "compile.native.genlibs"
    []
    I.Visible
    []
    ( P.wrapColumn2
        [ ( makeExample schemeLibgen [],
            "Generates libraries necessary for scheme compilation.\n\n\
            \There is no need to run this before"
              <> P.group (makeExample compileScheme [])
              <> "as\
                 \ the latter will check if the libraries are missing and\
                 \ auto-generate them. However, this will generate the\
                 \ libraries even if their files already exist, so if the\
                 \ compiler has been upgraded, this can be used to ensure\
                 \ the generated libraries are up to date."
          )
        ]
    )
    ( \case
        [] -> pure Input.GenSchemeLibsI
        _ -> Left $ showPatternHelp schemeLibgen
    )

fetchScheme :: InputPattern
fetchScheme =
  InputPattern
    "compile.native.fetch"
    []
    I.Visible
    []
    ( P.wrapColumn2
        [ ( makeExample fetchScheme [],
            P.lines . fmap P.wrap $
              [ "Fetches the unison library for compiling to scheme.",
                "This is done automatically when"
                  <> P.group (makeExample compileScheme [])
                  <> "is run if the library is not already in the\
                     \ standard location (unison.internal). However,\
                     \ this command will force a pull even if the\
                     \ library already exists.",
                "You can also specify a user and branch name to pull\
                \ from in order to use an alternate version of the\
                \ unison compiler (for development purposes, for\
                \ example).",
                "The default user is `unison`. The default branch\
                \ for the `unison` user is a specified latest\
                \ version of the compiler for stability. The\
                \ default branch for other uses is `main`. The\
                \ command fetches code from a project:",
                P.indentN 2 ("@user/internal/branch")
              ]
          )
        ]
    )
    ( \case
        [] -> pure (Input.FetchSchemeCompilerI "unison" JitInfo.currentRelease)
        [name] -> pure (Input.FetchSchemeCompilerI name branch)
          where
            branch
              | name == "unison" = JitInfo.currentRelease
              | otherwise = "main"
        [name, branch] -> pure (Input.FetchSchemeCompilerI name branch)
        _ -> Left $ showPatternHelp fetchScheme
    )

createAuthor :: InputPattern
createAuthor =
  InputPattern
    "create.author"
    []
    I.Visible
    [(Required, noCompletionsArg), (Required, noCompletionsArg)]
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
    I.Visible
    [(Required, gitUrlArg)]
    ( P.lines
        [ "Publish the current namespace.",
          "",
          P.wrapColumn2
            [ ( "`gist git(git@github.com:user/repo)`",
                "publishes the contents of the current namespace into the specified git repo."
              )
            ],
          "",
          P.indentN 2 . P.wrap $
            "Note: Gists are not yet supported on Unison Share, though you can just do a normal"
              <> "`push.create` of the current namespace to your Unison Share codebase wherever you like!"
        ]
    )
    ( \case
        [repoString] -> do
          repo <- parseWriteGitRepo "gist git repo" repoString
          pure (Input.GistI (Input.GistInput repo))
        _ -> Left (showPatternHelp gist)
    )

authLogin :: InputPattern
authLogin =
  InputPattern
    "auth.login"
    []
    I.Hidden
    []
    ( P.lines
        [ P.wrap "Obtain an authentication session with Unison Share.",
          makeExample authLogin []
            <> "authenticates ucm with Unison Share."
        ]
    )
    ( \case
        [] -> Right $ Input.AuthLoginI
        _ -> Left (showPatternHelp authLogin)
    )

printVersion :: InputPattern
printVersion =
  InputPattern
    "version"
    []
    I.Visible
    []
    ( P.wrap "Print the version of unison you're running"
    )
    ( \case
        [] -> Right $ Input.VersionI
        _ -> Left (showPatternHelp printVersion)
    )

diffNamespaceToPatch :: InputPattern
diffNamespaceToPatch =
  InputPattern
    { patternName = "diff.namespace.to-patch",
      aliases = [],
      visibility = I.Visible,
      argTypes = [],
      help = P.wrap "Create a patch from a namespace diff.",
      parse = \case
        [branchId1, branchId2, patch] ->
          mapLeft fromString do
            branchId1 <- Input.parseBranchId branchId1
            branchId2 <- Input.parseBranchId branchId2
            patch <- Path.parseSplit' Path.definitionNameSegment patch
            pure (Input.DiffNamespaceToPatchI Input.DiffNamespaceToPatchInput {branchId1, branchId2, patch})
        _ -> Left (showPatternHelp diffNamespaceToPatch)
    }

projectCreate :: InputPattern
projectCreate =
  InputPattern
    { patternName = "project.create",
      aliases = ["create.project"],
      visibility = I.Visible,
      argTypes = [],
      help =
        P.wrapColumn2
          [ ("`project.create`", "creates a project with a random name"),
            ("`project.create foo`", "creates a project named `foo`")
          ],
      parse = \case
        [name] ->
          case tryInto @ProjectName (Text.pack name) of
            Left _ -> Left "Invalid project name."
            Right name1 -> Right (Input.ProjectCreateI True (Just name1))
        _ -> Right (Input.ProjectCreateI True Nothing)
    }

projectCreateEmptyInputPattern :: InputPattern
projectCreateEmptyInputPattern =
  InputPattern
    { patternName = "project.create-empty",
      aliases = ["create.empty-project"],
      visibility = I.Hidden,
      argTypes = [],
      help =
        P.wrapColumn2
          [ ("`project.create-empty`", "creates an empty project with a random name"),
            ("`project.create-empty foo`", "creates an empty project named `foo`")
          ],
      parse = \case
        [name] ->
          case tryInto @ProjectName (Text.pack name) of
            Left _ -> Left "Invalid project name."
            Right name1 -> Right (Input.ProjectCreateI False (Just name1))
        _ -> Right (Input.ProjectCreateI False Nothing)
    }

projectRenameInputPattern :: InputPattern
projectRenameInputPattern =
  InputPattern
    { patternName = "project.rename",
      aliases = ["rename.project"],
      visibility = I.Visible,
      argTypes = [],
      help =
        P.wrapColumn2
          [ ("`project.rename foo`", "renames the current project to `foo`")
          ],
      parse = \case
        [nameString] | Right name <- tryInto (Text.pack nameString) -> Right (Input.ProjectRenameI name)
        _ -> Left (showPatternHelp projectRenameInputPattern)
    }

projectSwitch :: InputPattern
projectSwitch =
  InputPattern
    { patternName = "switch",
      aliases = [],
      visibility = I.Visible,
      argTypes = [(Optional, projectAndBranchNamesArg False)],
      help =
        P.wrapColumn2
          [ ("`switch foo/bar`", "switches to the branch `bar` in the project `foo`"),
            ("`switch foo/`", "switches to the last branch you visited in the project `foo`"),
            ("`switch /bar`", "switches to the branch `bar` in the current project")
          ],
      parse = \case
        [] -> Right (Input.ProjectSwitchI Nothing)
        [name] ->
          case tryInto @ProjectAndBranchNames (Text.pack name) of
            Left _ -> Left (showPatternHelp projectSwitch)
            Right projectAndBranch -> Right (Input.ProjectSwitchI $ Just projectAndBranch)
        _ -> Left (showPatternHelp projectSwitch)
    }

projectsInputPattern :: InputPattern
projectsInputPattern =
  InputPattern
    { patternName = "projects",
      aliases = ["list.project", "ls.project", "project.list"],
      visibility = I.Visible,
      argTypes = [],
      help = P.wrap "List projects.",
      parse = \_ -> Right Input.ProjectsI
    }

branchesInputPattern :: InputPattern
branchesInputPattern =
  InputPattern
    { patternName = "branches",
      aliases = ["list.branch", "ls.branch", "branch.list"],
      visibility = I.Visible,
      argTypes = [],
      help =
        P.wrapColumn2
          [ ("`branches`", "lists all branches in the current project"),
            ("`branches foo", "lists all branches in the project `foo`")
          ],
      parse = \case
        [] -> Right (Input.BranchesI Nothing)
        [nameString] | Right name <- tryFrom (Text.pack nameString) -> Right (Input.BranchesI (Just name))
        _ -> Left (showPatternHelp branchesInputPattern)
    }

branchInputPattern :: InputPattern
branchInputPattern =
  InputPattern
    { patternName = "branch",
      aliases = ["branch.create", "create.branch"],
      visibility = I.Visible,
      argTypes = [],
      help =
        P.wrapColumn2
          [ ("`branch foo`", "forks the current project branch to a new branch `foo`"),
            ("`branch /bar foo`", "forks the branch `bar` of the current project to a new branch `foo`"),
            ("`branch .bar foo`", "forks the path `.bar` of the current project to a new branch `foo`")
          ],
      parse =
        maybeToEither (showPatternHelp branchInputPattern) . \case
          [source0, name] -> do
            source <- parseLooseCodeOrProject source0
            projectAndBranch <-
              Text.pack name
                & tryInto @(ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
                & eitherToMaybe
            Just (Input.BranchI (Input.BranchSourceI'LooseCodeOrProject source) projectAndBranch)
          [name] -> do
            projectAndBranch <-
              Text.pack name
                & tryInto @(ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
                & eitherToMaybe
            Just (Input.BranchI Input.BranchSourceI'CurrentContext projectAndBranch)
          _ -> Nothing
    }

branchEmptyInputPattern :: InputPattern
branchEmptyInputPattern =
  InputPattern
    { patternName = "branch.empty",
      aliases = ["branch.create-empty", "create.empty-branch"],
      visibility = I.Visible,
      argTypes = [],
      help = P.wrap "Create a new empty branch.",
      parse = \case
        [name] ->
          first (\_ -> showPatternHelp branchEmptyInputPattern) do
            projectAndBranch <- tryInto @(ProjectAndBranch (Maybe ProjectName) ProjectBranchName) (Text.pack name)
            Right (Input.BranchI Input.BranchSourceI'Empty projectAndBranch)
        _ -> Left (showPatternHelp branchEmptyInputPattern)
    }

branchRenameInputPattern :: InputPattern
branchRenameInputPattern =
  InputPattern
    { patternName = "branch.rename",
      aliases = ["rename.branch"],
      visibility = I.Visible,
      argTypes = [],
      help =
        P.wrapColumn2
          [ ("`branch.rename foo`", "renames the current branch to `foo`")
          ],
      parse = \case
        [nameString] | Right name <- tryInto (Text.pack nameString) -> Right (Input.BranchRenameI name)
        _ -> Left (showPatternHelp branchRenameInputPattern)
    }

clone :: InputPattern
clone =
  InputPattern
    { patternName = "clone",
      aliases = [],
      visibility = I.Visible,
      argTypes = [],
      help =
        P.wrapColumn2
          [ ( "`clone @unison/json/topic json/my-topic`",
              "creates `json/my-topic` from the remote branch `@unison/json/topic`"
            ),
            ( "`clone @unison/base base/`",
              "creates `base/main` from the remote branch `@unison/base/main`"
            ),
            ( "`clone @unison/base /main2`",
              "creates the branch `main2` in the current project from the remote branch `@unison/base/main`"
            ),
            ( "`clone /main /main2`",
              "creates the branch `main2` in the current project from the remote branch `main` of the current project's associated remote"
                <> "(see"
                <> P.group (makeExample helpTopics ["remotes"] <> ")")
            ),
            ( "`clone /main my-fork/`",
              "creates `my-fork/main` from the branch `main` of the current project's associated remote"
                <> "(see"
                <> P.group (makeExample helpTopics ["remotes"] <> ")")
            )
          ],
      parse =
        maybe (Left (showPatternHelp clone)) Right . \case
          [remoteNamesString] -> do
            remoteNames <- eitherToMaybe (tryInto @ProjectAndBranchNames (Text.pack remoteNamesString))
            Just (Input.CloneI remoteNames Nothing)
          [remoteNamesString, localNamesString] -> do
            remoteNames <- eitherToMaybe (tryInto @ProjectAndBranchNames (Text.pack remoteNamesString))
            localNames <- eitherToMaybe (tryInto @ProjectAndBranchNames (Text.pack localNamesString))
            Just (Input.CloneI remoteNames (Just localNames))
          _ -> Nothing
    }

releaseDraft :: InputPattern
releaseDraft =
  InputPattern
    { patternName = "release.draft",
      aliases = ["draft.release"],
      visibility = I.Visible,
      argTypes = [],
      help = P.wrap "Draft a release.",
      parse = \case
        [tryInto @Semver . Text.pack -> Right semver] -> Right (Input.ReleaseDraftI semver)
        _ -> Left (showPatternHelp releaseDraft)
    }

validInputs :: [InputPattern]
validInputs =
  sortOn
    I.patternName
    [ add,
      aliasMany,
      aliasTerm,
      aliasType,
      api,
      authLogin,
      back,
      branchEmptyInputPattern,
      branchInputPattern,
      branchRenameInputPattern,
      branchesInputPattern,
      cd,
      clear,
      clone,
      compileScheme,
      copyPatch,
      createAuthor,
      debugClearWatchCache,
      debugDoctor,
      debugDumpNamespace,
      debugDumpNamespaceSimple,
      debugFileHashes,
      debugNameDiff,
      debugNumberedArgs,
      debugTabCompletion,
      delete,
      deleteBranch,
      deleteProject,
      deleteNamespace,
      deleteNamespaceForce,
      deletePatch,
      deleteTerm,
      deleteTermReplacement,
      deleteTermVerbose,
      deleteType,
      deleteTypeReplacement,
      deleteTypeVerbose,
      deleteVerbose,
      dependencies,
      dependents,
      diffNamespace,
      diffNamespaceToPatch,
      display,
      displayTo,
      docToMarkdown,
      docs,
      docsToHtml,
      edit,
      execute,
      fetchScheme,
      find,
      findAll,
      findGlobal,
      findPatch,
      findShallow,
      findVerbose,
      findVerboseAll,
      sfind,
      sfindReplace,
      forkLocal,
      gist,
      help,
      helpTopics,
      history,
      ioTest,
      link,
      links,
      load,
      makeStandalone,
      mergeBuiltins,
      mergeIOBuiltins,
      mergeLocal,
      names False, -- names
      names True, -- names.global
      namespaceDependencies,
      patch,
      previewAdd,
      previewMergeLocal,
      previewUpdate,
      printVersion,
      projectCreate,
      projectCreateEmptyInputPattern,
      projectRenameInputPattern,
      projectSwitch,
      projectsInputPattern,
      pull,
      pullExhaustive,
      pullVerbose,
      pullWithoutHistory,
      push,
      pushCreate,
      pushExhaustive,
      pushForce,
      quit,
      releaseDraft,
      renameBranch,
      renamePatch,
      renameTerm,
      renameType,
      replace,
      reset,
      resetRoot,
      runScheme,
      saveExecuteResult,
      schemeLibgen,
      squashMerge,
      test,
      testAll,
      todo,
      ui,
      undo,
      unlink,
      up,
      update,
      updateBuiltins,
      updateNoPatch,
      view,
      viewGlobal,
      viewPatch,
      viewReflog
    ]

-- | A map of all command patterns by pattern name or alias.
patternMap :: Map String InputPattern
patternMap =
  Map.fromList $
    validInputs
      >>= (\p -> (I.patternName p, p) : ((,p) <$> I.aliases p))

visibleInputs :: [InputPattern]
visibleInputs = filter ((== I.Visible) . I.visibility) validInputs

commandNames :: [String]
commandNames = visibleInputs >>= \i -> I.patternName i : I.aliases i

commandNameArg :: ArgumentType
commandNameArg =
  ArgumentType
    { typeName = "command",
      suggestions = \q _ _ _ -> pure (exactComplete q (commandNames <> Map.keys helpTopicsMap)),
      globTargets = mempty
    }

exactDefinitionArg :: ArgumentType
exactDefinitionArg =
  ArgumentType
    { typeName = "definition",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteTermOrType q p),
      globTargets = Set.fromList [Globbing.Term, Globbing.Type]
    }

fuzzyDefinitionQueryArg :: ArgumentType
fuzzyDefinitionQueryArg =
  ArgumentType
    { typeName = "fuzzy definition query",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteTermOrType q p),
      globTargets = Set.fromList [Globbing.Term, Globbing.Type]
    }

definitionQueryArg :: ArgumentType
definitionQueryArg = exactDefinitionArg {typeName = "definition query"}

exactDefinitionTypeQueryArg :: ArgumentType
exactDefinitionTypeQueryArg =
  ArgumentType
    { typeName = "type definition query",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteType q p),
      globTargets = Set.fromList [Globbing.Type]
    }

exactDefinitionTermQueryArg :: ArgumentType
exactDefinitionTermQueryArg =
  ArgumentType
    { typeName = "term definition query",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteTerm q p),
      globTargets = Set.fromList [Globbing.Term]
    }

patchArg :: ArgumentType
patchArg =
  ArgumentType
    { typeName = "patch",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompletePatch q p),
      globTargets = Set.fromList []
    }

namespaceArg :: ArgumentType
namespaceArg =
  ArgumentType
    { typeName = "namespace",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteNamespace q p),
      globTargets = Set.fromList [Globbing.Namespace]
    }

namespaceOrDefinitionArg :: ArgumentType
namespaceOrDefinitionArg =
  ArgumentType
    { typeName = "term, type, or namespace",
      suggestions = \q cb _http p -> Codebase.runTransaction cb do
        namespaces <- prefixCompleteNamespace q p
        termsTypes <- prefixCompleteTermOrType q p
        pure (List.nubOrd $ namespaces <> termsTypes),
      globTargets = Set.fromList [Globbing.Namespace, Globbing.Term, Globbing.Type]
    }

-- | Names of child branches of the branch, only gives options for one 'layer' deeper at a time.
childNamespaceNames :: Branch.Branch0 m -> [Text]
childNamespaceNames b = NameSegment.toText <$> Map.keys (Branch.nonEmptyChildren b)

newNameArg :: ArgumentType
newNameArg =
  ArgumentType
    { typeName = "new-name",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteNamespace q p),
      globTargets = mempty
    }

noCompletionsArg :: ArgumentType
noCompletionsArg =
  ArgumentType
    { typeName = "word",
      suggestions = noCompletions,
      globTargets = mempty
    }

-- Arya: I could imagine completions coming from previous pulls
gitUrlArg :: ArgumentType
gitUrlArg =
  ArgumentType
    { typeName = "git-url",
      suggestions =
        let complete s = pure [Completion s s False]
         in \input _ _ _ -> case input of
              "gh" -> complete "git(https://github.com/"
              "gl" -> complete "git(https://gitlab.com/"
              "bb" -> complete "git(https://bitbucket.com/"
              "ghs" -> complete "git(git@github.com:"
              "gls" -> complete "git(git@gitlab.com:"
              "bbs" -> complete "git(git@bitbucket.com:"
              _ -> pure [],
      globTargets = mempty
    }

-- | Refers to a namespace on some remote code host.
remoteNamespaceArg :: ArgumentType
remoteNamespaceArg =
  ArgumentType
    { typeName = "remote-namespace",
      suggestions =
        let complete s = pure [Completion s s False]
         in \input _cb http _p -> case input of
              "gh" -> complete "git(https://github.com/"
              "gl" -> complete "git(https://gitlab.com/"
              "bb" -> complete "git(https://bitbucket.com/"
              "ghs" -> complete "git(git@github.com:"
              "gls" -> complete "git(git@gitlab.com:"
              "bbs" -> complete "git(git@bitbucket.com:"
              _ -> do
                sharePathCompletion http input,
      globTargets = mempty
    }

-- | A project name, branch name, or both.
projectAndBranchNamesArg :: Bool -> ArgumentType
projectAndBranchNamesArg includeCurrentBranch =
  ArgumentType
    { typeName = "project-and-branch-names",
      suggestions = \(Text.strip . Text.pack -> input) codebase _httpClient path ->
        case Text.uncons input of
          -- Things like "/foo" would be parsed as unambiguous branches in the logic below, except we also want to
          -- handle "/<TAB>" and "/@<TAB>" inputs, which aren't valid branch names, but are valid branch prefixes. So,
          -- if the input begins with a forward slash, just rip it off and treat the rest as the branch prefix.
          Just ('/', input1) -> handleBranchesComplete input1 codebase path
          _ ->
            case tryFrom input of
              -- This case handles inputs like "", "@", and possibly other things that don't look like a valid project
              -- or branch, but are a valid prefix of one
              Left _err -> handleAmbiguousComplete input codebase path
              Right (ProjectAndBranchNames'Ambiguous _ _) -> handleAmbiguousComplete input codebase path
              -- Here we assume that if we've unambiguously parsed a project, it ended in a forward slash, so we're ready
              -- to suggest branches in that project as autocompletions.
              --
              -- Conceivably, with some other syntax, it may be possible to unambiguously parse a project name, while
              -- still wanting to suggest full project names (e.g. I type "PROJECT=foo<tab>" to get a list of projects
              -- that begin with "foo"), but because that's not how our syntax works today, we don't inspect the input
              -- string for a trailing forward slash.
              Right (ProjectAndBranchNames'Unambiguous (This projectName)) -> do
                branches <-
                  Codebase.runTransaction codebase do
                    Queries.loadProjectByName projectName >>= \case
                      Nothing -> pure []
                      Just project -> do
                        let projectId = project ^. #projectId
                        fmap (filterOutCurrentBranch path projectId) do
                          Queries.loadAllProjectBranchesBeginningWith projectId Nothing
                pure (map (projectBranchToCompletion projectName) branches)
              -- This branch is probably dead due to intercepting inputs that begin with "/" above
              Right (ProjectAndBranchNames'Unambiguous (That branchName)) ->
                handleBranchesComplete (into @Text branchName) codebase path
              Right (ProjectAndBranchNames'Unambiguous (These projectName branchName)) -> do
                branches <-
                  Codebase.runTransaction codebase do
                    Queries.loadProjectByName projectName >>= \case
                      Nothing -> pure []
                      Just project -> do
                        let projectId = project ^. #projectId
                        fmap (filterOutCurrentBranch path projectId) do
                          Queries.loadAllProjectBranchesBeginningWith projectId (Just $ into @Text branchName)
                pure (map (projectBranchToCompletion projectName) branches),
      globTargets = Set.empty
    }
  where
    handleAmbiguousComplete ::
      MonadIO m =>
      Text ->
      Codebase m v a ->
      Path.Absolute ->
      m [Completion]
    handleAmbiguousComplete input codebase path = do
      (branches, projects) <-
        Codebase.runTransaction codebase do
          branches <-
            case preview ProjectUtils.projectBranchPathPrism path of
              Nothing -> pure []
              Just (ProjectAndBranch currentProjectId _, _) ->
                fmap (filterOutCurrentBranch path currentProjectId) do
                  Queries.loadAllProjectBranchesBeginningWith currentProjectId (Just input)
          projects <- Queries.loadAllProjectsBeginningWith (Just input)
          pure (branches, projects)
      let branchCompletions = map currentProjectBranchToCompletion branches
      let projectCompletions = map projectToCompletion projects
      -- There's one final wibble to deal with here at the eleventh hour. You might think we can just append
      -- branchCompletions to projectCompletions and call it a day, *however*...!
      --
      -- Say we have two branches "bar" and "biz". These branches are rendered (and completed) with leading forward
      -- slashes.
      --
      --   > switch b<TAB>
      --   /bar /biz
      --
      --   > switch ba<TAB>
      --   > switch /bar -- the completion
      --
      -- Now say we repeat the above, but with a project "bongo".
      --
      --   > switch <TAB>
      --   /bar /biz bongo
      --
      -- If the user types a prefix that's common to both a branch and a project, like "b", their input will simply
      -- disappear. Wtf, haskeline?!
      --
      --   > switch b<TAB>
      --   > switch -- the completion
      --
      -- Well, it makes sense: we tell haskeline that we have three completions, "/bar", "/biz", and "bongo", with
      -- partial input "b". The longest common prefix here is the empty string "".
      --
      -- So, we have this final check. If there are indeed matching projects *and* matching branches, and the user
      -- has input at least one character (i.e. they aren't just tab-completing like "switch <TAB>" to see
      -- everything), then we pretend (for the sake of tab-completion) that there are only matching projects. This
      -- makes the back-and-forth with the tab completer much more intuitive:
      --
      --   > switch <TAB>
      --   /bar /biz bongo
      --   > switch b<TAB>
      --   > switch bongo -- the completion
      --
      -- A more optimal interface would not hide branches at all, even though their tab-completions end up prefixing
      -- a forward-slash:
      --
      --   > switch <TAB>
      --   /bar /biz bongo
      --   > switch b<TAB>
      --   /bar /biz bongo
      --   > switch ba<TAB>
      --   > switch /bar -- the completion
      --
      -- However, that simly doesn't seem possible with haskeline. Another sub-optimal point in the design space
      -- would be to *not* actually tab-complete branch names with leading forward slashes, even though they are
      -- rendered as such in the tab-completion options. For example,
      --
      --   > switch <TAB>
      --   /bar /biz
      --   > switch ba<TAB>
      --   > switch bar -- the completion
      --
      -- However, this has the unfortunate disadvantage of tab-completing a possibly ambiguous thing for the user,
      -- as in the case when there's both a branch and project with the same name:
      --
      --   > switch <TAB>
      --   /bar /biz bar
      --   > switch ba<TAB>
      --   > switch bar -- the completion
      --
      --   Ambiguous! Try `switch /bar` or `switch bar/`
      pure
        if not (null branchCompletions) && not (null projectCompletions) && not (Text.null input)
          then projectCompletions
          else branchCompletions ++ projectCompletions

    handleBranchesComplete :: MonadIO m => Text -> Codebase m v a -> Path.Absolute -> m [Completion]
    handleBranchesComplete branchName codebase path = do
      branches <-
        case preview ProjectUtils.projectBranchPathPrism path of
          Nothing -> pure []
          Just (ProjectAndBranch currentProjectId _, _) ->
            Codebase.runTransaction codebase do
              fmap (filterOutCurrentBranch path currentProjectId) do
                Queries.loadAllProjectBranchesBeginningWith currentProjectId (Just branchName)
      pure (map currentProjectBranchToCompletion branches)

    filterOutCurrentBranch :: Path.Absolute -> ProjectId -> [(ProjectBranchId, a)] -> [(ProjectBranchId, a)]
    filterOutCurrentBranch path projectId =
      case (includeCurrentBranch, preview ProjectUtils.projectBranchPathPrism path) of
        (False, Just (ProjectAndBranch currentProjectId currentBranchId, _))
          | projectId == currentProjectId -> filter (\(branchId, _) -> branchId /= currentBranchId)
        _ -> id

    currentProjectBranchToCompletion :: (ProjectBranchId, ProjectBranchName) -> Completion
    currentProjectBranchToCompletion (_, branchName) =
      Completion
        { replacement = '/' : Text.unpack (into @Text branchName),
          display = P.toAnsiUnbroken (prettySlashProjectBranchName branchName),
          isFinished = False
        }

    projectBranchToCompletion :: ProjectName -> (ProjectBranchId, ProjectBranchName) -> Completion
    projectBranchToCompletion projectName (_, branchName) =
      Completion
        { replacement = Text.unpack (into @Text (ProjectAndBranch projectName branchName)),
          display = P.toAnsiUnbroken (prettySlashProjectBranchName branchName),
          isFinished = False
        }

    projectToCompletion :: Sqlite.Project -> Completion
    projectToCompletion project =
      Completion
        { replacement = stringProjectName,
          display = P.toAnsiUnbroken (prettyProjectNameSlash (project ^. #name)),
          isFinished = False
        }
      where
        stringProjectName = Text.unpack (into @Text (project ^. #name) <> "/")

-- | A project branch name.
projectBranchNameArg :: ArgumentType
projectBranchNameArg =
  ArgumentType
    { typeName = "project-branch-name",
      suggestions = \_ _ _ _ -> pure [],
      globTargets = Set.empty
    }

-- [project/]branch
projectBranchNameWithOptionalProjectNameArg :: ArgumentType
projectBranchNameWithOptionalProjectNameArg =
  ArgumentType
    { typeName = "project-branch-name-with-optional-project-name",
      suggestions = \_ _ _ _ -> pure [],
      globTargets = Set.empty
    }

-- | A project name.
projectNameArg :: ArgumentType
projectNameArg =
  ArgumentType
    { typeName = "project-name",
      suggestions = \(Text.strip . Text.pack -> input) codebase _httpClient _path -> do
        projects <-
          Codebase.runTransaction codebase do
            Queries.loadAllProjectsBeginningWith (Just input)
        pure $ map projectToCompletion projects,
      globTargets = Set.empty
    }
  where
    projectToCompletion :: Sqlite.Project -> Completion
    projectToCompletion project =
      Completion
        { replacement = Text.unpack (into @Text (project ^. #name)),
          display = P.toAnsiUnbroken (prettyProjectName (project ^. #name)),
          isFinished = False
        }

parsePullSource :: Text -> Maybe (ReadRemoteNamespace (These ProjectName ProjectBranchNameOrLatestRelease))
parsePullSource =
  P.parseMaybe (readRemoteNamespaceParser ProjectBranchSpecifier'NameOrLatestRelease)

-- | Parse a 'Input.PushSource'.
parsePushSource :: String -> Either (P.Pretty CT.ColorText) Input.PushSource
parsePushSource sourceStr =
  case tryFrom (Text.pack sourceStr) of
    Left _ ->
      case Path.parsePath' sourceStr of
        Left _ -> Left (I.help push)
        Right path -> Right (Input.PathySource path)
    Right branch -> Right (Input.ProjySource branch)

-- | Parse a push target.
parsePushTarget :: String -> Either (P.Pretty CT.ColorText) (WriteRemoteNamespace (These ProjectName ProjectBranchName))
parsePushTarget target =
  case P.parseMaybe UriParser.writeRemoteNamespace (Text.pack target) of
    Nothing -> Left (I.help push)
    Just path -> Right path

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

parseWriteGitRepo :: String -> String -> Either (P.Pretty P.ColorText) WriteGitRepo
parseWriteGitRepo label input = do
  first
    (fromString . show) -- turn any parsing errors into a Pretty.
    (P.parse (UriParser.writeGitRepo <* P.eof) label (Text.pack input))

collectNothings :: (a -> Maybe b) -> [a] -> [a]
collectNothings f as = [a | (Nothing, a) <- map f as `zip` as]

explainRemote :: PushPull -> P.Pretty CT.ColorText
explainRemote pushPull =
  P.group $
    P.lines
      [ P.wrap $ "where `remote` is a hosted codebase, such as:",
        P.indentN 2 . P.column2 $
          [ ("Unison Share", P.backticked "user.public.some.remote.path"),
            ("Git + root", P.backticked $ "git(" <> gitRepo <> "user/repo)"),
            ("Git + path", P.backticked $ "git(" <> gitRepo <> "user/repo).some.remote.path"),
            ("Git + branch", P.backticked $ "git(" <> gitRepo <> "user/repo:some-branch)"),
            ("Git + branch + path", P.backticked $ "git(" <> gitRepo <> "user/repo:some-branch).some.remote.path")
          ]
      ]
  where
    gitRepo = PushPull.fold @(P.Pretty P.ColorText) "git@github.com:" "https://github.com/" pushPull

showErrorFancy :: (P.ShowErrorComponent e) => P.ErrorFancy e -> String
showErrorFancy (P.ErrorFail msg) = msg
showErrorFancy (P.ErrorIndentation ord ref actual) =
  "incorrect indentation (got "
    <> show (P.unPos actual)
    <> ", should be "
    <> p
    <> show (P.unPos ref)
    <> ")"
  where
    p = case ord of
      LT -> "less than "
      EQ -> "equal to "
      GT -> "greater than "
showErrorFancy (P.ErrorCustom a) = P.showErrorComponent a

showErrorItem :: P.ErrorItem (P.Token Text) -> String
showErrorItem (P.Tokens ts) = P.showTokens (Proxy @Text) ts
showErrorItem (P.Label label) = NE.toList label
showErrorItem P.EndOfInput = "end of input"
