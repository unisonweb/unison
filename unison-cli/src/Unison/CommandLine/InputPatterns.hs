{-
   This module defines 'InputPattern' values for every supported input command.
-}

module Unison.CommandLine.InputPatterns
  ( -- * Input commands
    add,
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
    debugFormat,
    debugFuzzyOptions,
    debugLSPFoldRanges,
    debugNameDiff,
    debugNumberedArgs,
    debugTabCompletion,
    debugTerm,
    debugTermVerbose,
    debugType,
    delete,
    deleteBranch,
    deleteNamespace,
    deleteNamespaceForce,
    deletePatch,
    deleteProject,
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
    editNamespace,
    execute,
    find,
    findAll,
    findGlobal,
    findIn,
    findInAll,
    findPatch,
    findShallow,
    findVerbose,
    findVerboseAll,
    forkLocal,
    gist,
    help,
    helpTopics,
    history,
    ioTest,
    ioTestAll,
    libInstallInputPattern,
    load,
    makeStandalone,
    mergeBuiltins,
    mergeIOBuiltins,
    mergeInputPattern,
    mergeOldInputPattern,
    mergeOldPreviewInputPattern,
    mergeOldSquashInputPattern,
    moveAll,
    names,
    namespaceDependencies,
    patch,
    previewAdd,
    previewUpdate,
    printVersion,
    projectCreate,
    projectCreateEmptyInputPattern,
    projectRenameInputPattern,
    projectSwitch,
    projectsInputPattern,
    pull,
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
    sfind,
    sfindReplace,
    test,
    testAll,
    todo,
    ui,
    undo,
    up,
    update,
    updateBuiltins,
    updateOld,
    updateOldNoPatch,
    upgrade,
    view,
    viewGlobal,
    viewPatch,
    viewReflog,

    -- * Misc
    deleteTermReplacementCommand,
    deleteTypeReplacementCommand,
    helpFor,
    makeExample',
    makeExample,
    makeExampleEOS,
    makeExampleNoBackticks,
    patternMap,
    patternName,
    showPatternHelp,
    validInputs,
  )
where

import Control.Lens (preview, review, (^.))
import Control.Lens.Cons qualified as Cons
import Data.List (intercalate)
import Data.List.Extra qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These (These (..))
import Network.URI qualified as URI
import System.Console.Haskeline.Completion (Completion (Completion))
import System.Console.Haskeline.Completion qualified as Haskeline
import System.Console.Haskeline.Completion qualified as Line
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Internal qualified as Megaparsec (withParsecT)
import U.Codebase.Sqlite.DbId (ProjectBranchId)
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Cli.Pretty (prettyProjectAndBranchName, prettyProjectName, prettyProjectNameSlash, prettySlashProjectBranchName, prettyURI)
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch.Merge qualified as Branch
import Unison.Codebase.Editor.Input (DeleteOutput (..), DeleteTarget (..), Input)
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Editor.Output.PushPull (PushPull (Pull, Push))
import Unison.Codebase.Editor.Output.PushPull qualified as PushPull
import Unison.Codebase.Editor.RemoteRepo (WriteGitRepo, WriteRemoteNamespace)
import Unison.Codebase.Editor.SlurpResult qualified as SR
import Unison.Codebase.Editor.UriParser (readRemoteNamespaceParser)
import Unison.Codebase.Editor.UriParser qualified as UriParser
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Path.Parse qualified as Path
import Unison.Codebase.PushBehavior qualified as PushBehavior
import Unison.CommandLine
import Unison.CommandLine.BranchRelativePath (parseBranchRelativePath, parseIncrementalBranchRelativePath)
import Unison.CommandLine.BranchRelativePath qualified as BranchRelativePath
import Unison.CommandLine.Completion
import Unison.CommandLine.FZFResolvers (IncludeLibFZF (..))
import Unison.CommandLine.FZFResolvers qualified as Resolvers
import Unison.CommandLine.InputPattern (ArgumentType (..), InputPattern (InputPattern), IsOptional (..), unionSuggestions)
import Unison.CommandLine.InputPattern qualified as I
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.Project
  ( ProjectAndBranch (..),
    ProjectAndBranchNames (..),
    ProjectBranchName,
    ProjectBranchNameOrLatestRelease (..),
    ProjectBranchSpecifier (..),
    ProjectName,
    Semver,
    branchWithOptionalProjectParser,
  )
import Unison.Project.Util (ProjectContext (..), projectContextFromPath)
import Unison.Syntax.HashQualified qualified as HQ (parseText)
import Unison.Syntax.Name qualified as Name (parseText, unsafeParseText)
import Unison.Syntax.NameSegment qualified as NameSegment (renderParseErr, segmentP)
import Unison.Util.ColorText qualified as CT
import Unison.Util.Monoid (intercalateMap)
import Unison.Util.Pretty qualified as P
import Unison.Util.Pretty.MegaParsec (prettyPrintParseError)

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

makeExampleEOS :: InputPattern -> [P.Pretty CT.ColorText] -> P.Pretty CT.ColorText
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
    [("namespace", Optional, namespaceArg)]
    "Adds the builtins (excluding `io` and misc) to the specified namespace. Defaults to `builtin.`"
    \case
      [] -> pure . Input.MergeBuiltinsI $ Nothing
      [p] -> first P.text do
        p <- Path.parsePath p
        pure . Input.MergeBuiltinsI $ Just p
      _ -> Left (I.help mergeBuiltins)

mergeIOBuiltins :: InputPattern
mergeIOBuiltins =
  InputPattern
    "builtins.mergeio"
    []
    I.Hidden
    [("namespace", Optional, namespaceArg)]
    "Adds all the builtins, including `io` and misc., to the specified namespace. Defaults to `builtin.`"
    \case
      [] -> pure . Input.MergeIOBuiltinsI $ Nothing
      [p] -> first P.text do
        p <- Path.parsePath p
        pure . Input.MergeIOBuiltinsI $ Just p
      _ -> Left (I.help mergeBuiltins)

updateBuiltins :: InputPattern
updateBuiltins =
  InputPattern
    "builtins.update"
    []
    I.Hidden
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
    [("patch", Optional, patchArg), ("namespace", Optional, namespaceArg)]
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
        patchStr : ws -> mapLeft (warn . P.text) $ do
          patch <- Path.parseSplit' patchStr
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
    [("scratch file", Optional, filePathArg)]
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
    [("definition", ZeroPlus, noCompletionsArg)]
    ( "`add` adds to the codebase all the definitions from the most recently "
        <> "typechecked file."
    )
    \ws -> pure $ Input.AddI (Set.fromList $ map (Name.unsafeParseText . Text.pack) ws)

previewAdd :: InputPattern
previewAdd =
  InputPattern
    "add.preview"
    []
    I.Visible
    [("definition", ZeroPlus, noCompletionsArg)]
    ( "`add.preview` previews additions to the codebase from the most recently "
        <> "typechecked file. This command only displays cached typechecking "
        <> "results. Use `load` to reparse & typecheck the file if the context "
        <> "has changed."
    )
    \ws -> pure $ Input.PreviewAddI (Set.fromList $ map (Name.unsafeParseText . Text.pack) ws)

update :: InputPattern
update =
  InputPattern
    { patternName = "update",
      aliases = [],
      visibility = I.Visible,
      args = [],
      help =
        P.wrap $
          "Adds everything in the most recently typechecked file to the namespace,"
            <> "replacing existing definitions having the same name, and attempts to update all the existing dependents accordingly. If the process"
            <> "can't be completed automatically, the dependents will be added back to the scratch file"
            <> "for your review.",
      parse =
        maybeToEither (I.help update) . \case
          [] -> Just Input.Update2I
          _ -> Nothing
    }

updateOldNoPatch :: InputPattern
updateOldNoPatch =
  InputPattern
    "update.old.nopatch"
    []
    I.Visible
    [("definition", ZeroPlus, noCompletionsArg)]
    ( P.wrap
        ( makeExample' updateOldNoPatch
            <> "works like"
            <> P.group (makeExample' updateOld <> ",")
            <> "except it doesn't add a patch entry for any updates. "
            <> "Use this when you want to make changes to definitions without "
            <> "pushing those changes to dependents beyond your codebase. "
            <> "An example is when updating docs, or when updating a term you "
            <> "just added."
        )
        <> P.wrapColumn2
          [ ( makeExample' updateOldNoPatch,
              "updates all definitions in the .u file."
            ),
            ( makeExample updateOldNoPatch ["foo", "bar"],
              "updates `foo`, `bar`, and their dependents from the .u file."
            )
          ]
    )
    ( \case
        ws -> do
          pure $
            Input.UpdateI
              Input.NoPatch
              (Set.fromList $ map (Name.unsafeParseText . Text.pack) ws)
    )

updateOld :: InputPattern
updateOld =
  InputPattern
    "update.old"
    []
    I.Visible
    [("patch", Optional, patchArg), ("definition", ZeroPlus, noCompletionsArg)]
    ( P.wrap
        ( makeExample' updateOld
            <> "works like"
            <> P.group (makeExample' add <> ",")
            <> "except that if a definition in the file has the same name as an"
            <> "existing definition, the name gets updated to point to the new"
            <> "definition. If the old definition has any dependents, `update` will"
            <> "add those dependents to a refactoring session, specified by an"
            <> "optional patch."
        )
        <> P.wrapColumn2
          [ ( makeExample' updateOld,
              "adds all definitions in the .u file, noting replacements in the"
                <> "default patch for the current namespace."
            ),
            ( makeExample updateOld ["<patch>"],
              "adds all definitions in the .u file, noting replacements in the"
                <> "specified patch."
            ),
            ( makeExample updateOld ["<patch>", "foo", "bar"],
              "adds `foo`, `bar`, and their dependents from the .u file, noting"
                <> "any replacements into the specified patch."
            )
          ]
    )
    \case
      patchStr : ws -> do
        patch <- first P.text $ Path.parseSplit' patchStr
        pure $
          Input.UpdateI
            (Input.UsePatch patch)
            (Set.fromList $ map (Name.unsafeParseText . Text.pack) ws)
      [] -> Right $ Input.UpdateI Input.DefaultPatch mempty

previewUpdate :: InputPattern
previewUpdate =
  InputPattern
    "update.old.preview"
    []
    I.Visible
    [("definition", ZeroPlus, noCompletionsArg)]
    ( "`update.old.preview` previews updates to the codebase from the most "
        <> "recently typechecked file. This command only displays cached "
        <> "typechecking results. Use `load` to reparse & typecheck the file if "
        <> "the context has changed."
    )
    \ws -> pure $ Input.PreviewUpdateI (Set.fromList $ map (Name.unsafeParseText . Text.pack) ws)

patch :: InputPattern
patch =
  InputPattern
    "patch"
    []
    I.Visible
    [("patch", Required, patchArg), ("namespace", Optional, namespaceArg)]
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
    \case
      patchStr : ws -> first P.text do
        patch <- Path.parseSplit' patchStr
        branch <- case ws of
          [pathStr] -> Path.parsePath' pathStr
          _ -> pure Path.relativeEmpty'
        pure $ Input.PropagatePatchI patch branch
      [] ->
        Left $
          warn $
            makeExample' patch
              <> "takes a patch and an optional namespace."

view :: InputPattern
view =
  InputPattern
    "view"
    []
    I.Visible
    [("definition to view", OnePlus, definitionQueryArg IncludeDepsFZF)]
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
    ( \case
        (x : xs) ->
          (x NE.:| xs)
            & traverse parseHashQualifiedName
            & fmap (Input.ShowDefinitionI Input.ConsoleLocation Input.ShowDefinitionLocal)
        _ -> Left (I.help view)
    )

viewGlobal :: InputPattern
viewGlobal =
  InputPattern
    "view.global"
    []
    I.Visible
    [("definition to view", ZeroPlus, definitionQueryArg IncludeDepsFZF)]
    ( P.lines
        [ "`view.global foo` prints definitions of `foo` within your codebase.",
          "`view.global` without arguments invokes a search to select definitions to view, which requires that `fzf` can be found within your PATH."
        ]
    )
    ( \case
        (x : xs) ->
          (x NE.:| xs)
            & traverse parseHashQualifiedName
            & fmap (Input.ShowDefinitionI Input.ConsoleLocation Input.ShowDefinitionGlobal)
        _ -> Left (I.help viewGlobal)
    )

display :: InputPattern
display =
  InputPattern
    "display"
    []
    I.Visible
    [("definition to display", OnePlus, definitionQueryArg IncludeDepsFZF)]
    ( P.lines
        [ "`display foo` prints a rendered version of the term `foo`.",
          "`display` without arguments invokes a search to select a definition to display, which requires that `fzf` can be found within your PATH."
        ]
    )
    \case
      (x : xs) ->
        (x NE.:| xs)
          & traverse parseHashQualifiedName
          <&> Input.DisplayI Input.ConsoleLocation
      _ -> Left (I.help display)

displayTo :: InputPattern
displayTo =
  InputPattern
    "display.to"
    []
    I.Visible
    [("destination file name", Required, filePathArg), ("definition to display", OnePlus, definitionQueryArg IncludeDepsFZF)]
    ( P.wrap $
        makeExample displayTo ["<filename>", "foo"]
          <> "prints a rendered version of the term `foo` to the given file."
    )
    \case
      file : (x : xs) ->
        (x NE.:| xs)
          & traverse parseHashQualifiedName
          <&> Input.DisplayI (Input.FileLocation file)
      _ -> Left (I.help displayTo)

docs :: InputPattern
docs =
  InputPattern
    "docs"
    []
    I.Visible
    [("definition", OnePlus, definitionQueryArg IncludeDepsFZF)]
    ( P.lines
        [ "`docs foo` shows documentation for the definition `foo`.",
          "`docs` without arguments invokes a search to select which definition to view documentation for, which requires that `fzf` can be found within your PATH."
        ]
    )
    ( \case
        x : xs ->
          (x NE.:| xs)
            & traverse Path.parseHQSplit'
            & bimap P.text Input.DocsI
        _ -> Left (I.help docs)
    )

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
      args = [("definition to load", Optional, namespaceOrDefinitionArg IncludeDepsFZF)],
      help = P.wrap "`ui` opens the Local UI in the default browser.",
      parse = \case
        [] -> pure $ Input.UiI Path.relativeEmpty'
        [path] -> first P.text $ do
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

sfind :: InputPattern
sfind =
  InputPattern "rewrite.find" ["sfind"] I.Visible [("rewrite-rule definition", Required, definitionQueryArg ExcludeDepsFZF)] msg parse
  where
    parse [q] = Input.StructuredFindI (Input.FindLocal Path.empty) <$> parseHashQualifiedName q
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
  InputPattern "rewrite" ["sfind.replace"] I.Visible [("rewrite-rule definition", Required, definitionQueryArg ExcludeDepsFZF)] msg parse
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
find = find' "find" (Input.FindLocal Path.empty)

findAll :: InputPattern
findAll = find' "find.all" (Input.FindLocalAndDeps Path.empty)

findGlobal :: InputPattern
findGlobal = find' "find.global" Input.FindGlobal

findIn, findInAll :: InputPattern
findIn = findIn' "find-in" Input.FindLocal
findInAll = findIn' "find-in.all" Input.FindLocalAndDeps

findIn' :: String -> (Path.Path -> Input.FindScope) -> InputPattern
findIn' cmd mkfscope =
  InputPattern
    cmd
    []
    I.Visible
    [("namespace", Required, namespaceArg), ("query", ZeroPlus, exactDefinitionArg IncludeDepsFZF)]
    findHelp
    \case
      p : args -> first P.text do
        p <- Path.parsePath p
        pure (Input.FindI False (mkfscope p) args)
      _ -> Left findHelp

findHelp :: P.Pretty CT.ColorText
findHelp =
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
        ( "`find-in namespace`",
          "lists all definitions in the specified subnamespace."
        ),
        ( "`find-in namespace foo bar`",
          "lists all definitions with a name similar to 'foo' or 'bar' in the "
            <> "specified subnamespace."
        ),
        ( "find.all foo",
          "lists all definitions with a name similar to 'foo' in the current "
            <> "namespace (including one level of 'lib')."
        ),
        ( "`find-in.all namespace`",
          "lists all definitions in the specified subnamespace (including one level of its 'lib')."
        ),
        ( "`find-in.all namespace foo bar`",
          "lists all definitions with a name similar to 'foo' or 'bar' in the "
            <> "specified subnamespace (including one level of its 'lib')."
        ),
        ( "find.global foo",
          "lists all definitions with a name similar to 'foo' in any namespace"
        )
      ]
  )

find' :: String -> Input.FindScope -> InputPattern
find' cmd fscope =
  InputPattern
    cmd
    []
    I.Visible
    [("query", ZeroPlus, exactDefinitionArg IncludeDepsFZF)]
    findHelp
    (pure . Input.FindI False fscope)

findShallow :: InputPattern
findShallow =
  InputPattern
    "list"
    ["ls", "dir"]
    I.Visible
    [("namespace", Optional, namespaceArg)]
    ( P.wrapColumn2
        [ ("`list`", "lists definitions and namespaces at the current level of the current namespace."),
          ("`list foo`", "lists the 'foo' namespace."),
          ("`list .foo`", "lists the '.foo' namespace.")
        ]
    )
    ( \case
        [] -> pure $ Input.FindShallowI Path.relativeEmpty'
        [path] -> first P.text $ do
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
    [("query", ZeroPlus, exactDefinitionArg IncludeDepsFZF)]
    ( "`find.verbose` searches for definitions like `find`, but includes hashes "
        <> "and aliases in the results."
    )
    (pure . Input.FindI True (Input.FindLocal Path.empty))

findVerboseAll :: InputPattern
findVerboseAll =
  InputPattern
    "find.all.verbose"
    []
    I.Visible
    [("query", ZeroPlus, exactDefinitionArg IncludeDepsFZF)]
    ( "`find.all.verbose` searches for definitions like `find.all`, but includes hashes "
        <> "and aliases in the results."
    )
    (pure . Input.FindI True (Input.FindLocalAndDeps Path.empty))

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
    [ ("definition to move", Required, exactDefinitionTermQueryArg ExcludeDepsFZF),
      ("new location", Required, newNameArg)
    ]
    "`move.term foo bar` renames `foo` to `bar`."
    ( \case
        [oldName, newName] -> first P.text do
          src <- Path.parseHQSplit' oldName
          target <- Path.parseSplit' newName
          pure $ Input.MoveTermI src target
        _ ->
          Left . P.warnCallout $
            P.wrap
              "`rename.term` takes two arguments, like `rename.term oldname newname`."
    )

moveAll :: InputPattern
moveAll =
  InputPattern
    "move"
    ["rename"]
    I.Visible
    [ ("definition to move", Required, namespaceOrDefinitionArg ExcludeDepsFZF),
      ("new location", Required, newNameArg)
    ]
    "`move foo bar` renames the term, type, and namespace foo to bar."
    ( \case
        [oldName, newName] -> first P.text $ do
          src <- Path.parsePath' oldName
          target <- Path.parsePath' newName
          pure $ Input.MoveAllI src target
        _ ->
          Left . P.warnCallout $
            P.wrap
              "`move` takes two arguments, like `move oldname newname`."
    )

renameType :: InputPattern
renameType =
  InputPattern
    "move.type"
    ["rename.type"]
    I.Visible
    [ ("type to move", Required, exactDefinitionTypeQueryArg ExcludeDepsFZF),
      ("new location", Required, newNameArg)
    ]
    "`move.type foo bar` renames `foo` to `bar`."
    ( \case
        [oldName, newName] -> first P.text do
          src <- Path.parseHQSplit' oldName
          target <- Path.parseSplit' newName
          pure $ Input.MoveTypeI src target
        _ ->
          Left . P.warnCallout $
            P.wrap
              "`rename.type` takes two arguments, like `rename.type oldname newname`."
    )

deleteGen :: Maybe String -> ArgumentType -> String -> ([Path.HQSplit'] -> DeleteTarget) -> InputPattern
deleteGen suffix queryCompletionArg target mkTarget =
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
        [("definition to delete", OnePlus, queryCompletionArg)]
        info
        ( \case
            [] -> Left . P.warnCallout $ P.wrap warn
            queries -> first P.text do
              paths <- traverse Path.parseHQSplit' queries
              pure $ Input.DeleteI (mkTarget paths)
        )

delete :: InputPattern
delete = deleteGen Nothing (exactDefinitionTypeOrTermQueryArg ExcludeDepsFZF) "term or type" (DeleteTarget'TermOrType DeleteOutput'NoDiff)

deleteVerbose :: InputPattern
deleteVerbose = deleteGen (Just "verbose") (exactDefinitionTypeOrTermQueryArg ExcludeDepsFZF) "term or type" (DeleteTarget'TermOrType DeleteOutput'Diff)

deleteTerm :: InputPattern
deleteTerm = deleteGen (Just "term") (exactDefinitionTermQueryArg ExcludeDepsFZF) "term" (DeleteTarget'Term DeleteOutput'NoDiff)

deleteTermVerbose :: InputPattern
deleteTermVerbose = deleteGen (Just "term.verbose") (exactDefinitionTermQueryArg ExcludeDepsFZF) "term" (DeleteTarget'Term DeleteOutput'Diff)

deleteType :: InputPattern
deleteType = deleteGen (Just "type") (exactDefinitionTypeQueryArg ExcludeDepsFZF) "type" (DeleteTarget'Type DeleteOutput'NoDiff)

deleteTypeVerbose :: InputPattern
deleteTypeVerbose = deleteGen (Just "type.verbose") (exactDefinitionTypeQueryArg ExcludeDepsFZF) "type" (DeleteTarget'Type DeleteOutput'Diff)

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
    [("definition", Required, if isTerm then (exactDefinitionTermQueryArg ExcludeDepsFZF) else (exactDefinitionTypeQueryArg ExcludeDepsFZF)), ("patch", Optional, patchArg)]
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
          patch <- first P.text . traverse Path.parseSplit' $ listToMaybe patch
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
      args = [("project to delete", Required, projectNameArg)],
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
      args = [("branch to delete", Required, projectBranchNameArg suggestionsConfig)],
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
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = OnlyWithinCurrentProject,
          branchInclusion = AllBranches
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
    [("term to alias", Required, exactDefinitionTermQueryArg IncludeDepsFZF), ("alias name", Required, newNameArg)]
    "`alias.term foo bar` introduces `bar` with the same definition as `foo`."
    \case
      [oldName, newName] -> first P.text do
        source <- Path.parseShortHashOrHQSplit' oldName
        target <- Path.parseSplit' newName
        pure $ Input.AliasTermI source target
      _ ->
        Left . warn $
          P.wrap
            "`alias.term` takes two arguments, like `alias.term oldname newname`."

aliasType :: InputPattern
aliasType =
  InputPattern
    "alias.type"
    []
    I.Visible
    [("type to alias", Required, exactDefinitionTypeQueryArg IncludeDepsFZF), ("alias name", Required, newNameArg)]
    "`alias.type Foo Bar` introduces `Bar` with the same definition as `Foo`."
    \case
      [oldName, newName] -> first P.text do
        source <- Path.parseShortHashOrHQSplit' oldName
        target <- Path.parseSplit' newName
        pure $ Input.AliasTypeI source target
      _ ->
        Left . warn $
          P.wrap
            "`alias.type` takes two arguments, like `alias.type oldname newname`."

aliasMany :: InputPattern
aliasMany =
  InputPattern
    "alias.many"
    ["copy"]
    I.Visible
    [("definition to alias", Required, definitionQueryArg ExcludeDepsFZF), ("alias names", OnePlus, exactDefinitionArg ExcludeDepsFZF)]
    ( P.group . P.lines $
        [ P.wrap $
            P.group (makeExample aliasMany ["<relative1>", "[relative2...]", "<namespace>"])
              <> "creates aliases `relative1`, `relative2`, ... in the namespace `namespace`.",
          P.wrap $
            P.group (makeExample aliasMany ["foo.foo", "bar.bar", ".quux"])
              <> "creates aliases `.quux.foo.foo` and `.quux.bar.bar`."
        ]
    )
    \case
      srcs@(_ : _) Cons.:> dest -> first P.text do
        sourceDefinitions <- traverse Path.parseHQSplit srcs
        destNamespace <- Path.parsePath' dest
        pure $ Input.AliasManyI sourceDefinitions destNamespace
      _ -> Left (I.help aliasMany)

up :: InputPattern
up =
  InputPattern
    "deprecated.up"
    []
    I.Hidden
    []
    (P.wrapColumn2 [(makeExample up [], "move current path up one level (deprecated)")])
    ( \case
        [] -> Right Input.UpI
        _ -> Left (I.help up)
    )

cd :: InputPattern
cd =
  InputPattern
    "deprecated.cd"
    ["deprecated.namespace"]
    I.Visible
    [("namespace", Required, namespaceArg)]
    ( P.lines
        [ "Moves your perspective to a different namespace. Deprecated for now because too many important things depend on your perspective selection.",
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
    \case
      [".."] -> Right Input.UpI
      [p] -> first P.text do
        p <- Path.parsePath' p
        pure . Input.SwitchBranchI $ p
      _ -> Left (I.help cd)

back :: InputPattern
back =
  InputPattern
    "back"
    ["popd"]
    I.Visible
    []
    ( P.wrapColumn2
        [ ( makeExample back [],
            "undoes the last" <> makeExample' projectSwitch <> "command."
          )
        ]
    )
    \case
      [] -> pure Input.PopBranchI
      _ -> Left (I.help cd)

deleteNamespace :: InputPattern
deleteNamespace =
  InputPattern
    "delete.namespace"
    []
    I.Visible
    [("namespace to delete", Required, namespaceArg)]
    "`delete.namespace <foo>` deletes the namespace `foo`"
    (deleteNamespaceParser (I.help deleteNamespace) Input.Try)

deleteNamespaceForce :: InputPattern
deleteNamespaceForce =
  InputPattern
    "delete.namespace.force"
    []
    I.Visible
    [("namespace to delete", Required, namespaceArg)]
    ( "`delete.namespace.force <foo>` deletes the namespace `foo`,"
        <> "deletion will proceed even if other code depends on definitions in foo."
    )
    (deleteNamespaceParser (I.help deleteNamespaceForce) Input.Force)

deleteNamespaceParser :: P.Pretty CT.ColorText -> Input.Insistence -> [String] -> Either (P.Pretty CT.ColorText) Input
deleteNamespaceParser helpText insistence = \case
  ["."] ->
    first fromString
      . pure
      $ Input.DeleteI (DeleteTarget'Namespace insistence Nothing)
  [p] -> first P.text do
    p <- Path.parseSplit' p
    pure $ Input.DeleteI (DeleteTarget'Namespace insistence (Just p))
  _ -> Left helpText

deletePatch :: InputPattern
deletePatch =
  InputPattern
    "delete.patch"
    []
    I.Visible
    [("patch to delete", Required, patchArg)]
    "`delete.patch <foo>` deletes the patch `foo`"
    \case
      [p] -> first P.text do
        p <- Path.parseSplit' p
        pure . Input.DeleteI $ DeleteTarget'Patch p
      _ -> Left (I.help deletePatch)

movePatch :: String -> String -> Either (P.Pretty CT.ColorText) Input
movePatch src dest = first P.text do
  src <- Path.parseSplit' src
  dest <- Path.parseSplit' dest
  pure $ Input.MovePatchI src dest

copyPatch' :: String -> String -> Either (P.Pretty CT.ColorText) Input
copyPatch' src dest = first P.text do
  src <- Path.parseSplit' src
  dest <- Path.parseSplit' dest
  pure $ Input.CopyPatchI src dest

copyPatch :: InputPattern
copyPatch =
  InputPattern
    "copy.patch"
    []
    I.Visible
    [("patch to copy", Required, patchArg), ("copy destination", Required, newNameArg)]
    "`copy.patch foo bar` copies the patch `foo` to `bar`."
    \case
      [src, dest] -> copyPatch' src dest
      _ -> Left (I.help copyPatch)

renamePatch :: InputPattern
renamePatch =
  InputPattern
    "move.patch"
    ["rename.patch"]
    I.Visible
    [("patch", Required, patchArg), ("new location", Required, newNameArg)]
    "`move.patch foo bar` renames the patch `foo` to `bar`."
    \case
      [src, dest] -> movePatch src dest
      _ -> Left (I.help renamePatch)

renameBranch :: InputPattern
renameBranch =
  InputPattern
    "move.namespace"
    ["rename.namespace"]
    I.Visible
    [("namespace to move", Required, namespaceArg), ("new location", Required, newNameArg)]
    "`move.namespace foo bar` renames the path `foo` to `bar`."
    \case
      [src, dest] -> first P.text do
        src <- Path.parsePath' src
        dest <- Path.parsePath' dest
        pure $ Input.MoveBranchI src dest
      _ -> Left (I.help renameBranch)

history :: InputPattern
history =
  InputPattern
    "history"
    []
    I.Visible
    [("namespace", Optional, namespaceArg)]
    ( P.wrapColumn2
        [ (makeExample history [], "Shows the history of the current path."),
          (makeExample history [".foo"], "Shows history of the path .foo."),
          ( makeExample history ["#9dndk3kbsk13nbpeu"],
            "Shows the history of the namespace with the given hash."
              <> "The full hash must be provided."
          )
        ]
    )
    \case
      [src] -> first P.text do
        p <- Input.parseBranchId src
        pure $ Input.HistoryI (Just 10) (Just 10) p
      [] -> pure $ Input.HistoryI (Just 10) (Just 10) (Right Path.currentPath)
      _ -> Left (I.help history)

forkLocal :: InputPattern
forkLocal =
  InputPattern
    "fork"
    ["copy.namespace"]
    I.Visible
    [ ("source location", Required, branchRelativePathArg),
      ("dest location", Required, branchRelativePathArg)
    ]
    ( P.wrapColumn2
        [ ( makeExample forkLocal ["src", "dest"],
            "creates the namespace `dest` as a copy of `src`."
          ),
          ( makeExample forkLocal ["project0/branch0:a.path", "project1/branch1:foo"],
            "creates the namespace `foo` in `branch1` of `project1` as a copy of `a.path` in `project0/branch0`."
          ),
          ( makeExample forkLocal ["srcproject/srcbranch", "dest"],
            "creates the namespace `dest` as a copy of the branch `srcbranch` of `srcproject`."
          )
        ]
    )
    \case
      [src, dest] -> do
        src <- Input.parseBranchId2 src
        dest <- parseBranchRelativePath dest
        pure $ Input.ForkLocalBranchI src dest
      _ -> Left (I.help forkLocal)

libInstallInputPattern :: InputPattern
libInstallInputPattern =
  InputPattern
    { patternName = "lib.install",
      aliases = ["install.lib"],
      visibility = I.Visible,
      args = [],
      help =
        P.lines
          [ P.wrap $
              "The"
                <> makeExample' libInstallInputPattern
                <> "command installs a dependency into the `lib` namespace.",
            "",
            P.wrapColumn2
              [ ( makeExample libInstallInputPattern ["@unison/base/releases/latest"],
                  "installs the latest release of `@unison/base`"
                ),
                ( makeExample libInstallInputPattern ["@unison/base/releases/3.0.0"],
                  "installs version 3.0.0 of `@unison/base`"
                ),
                ( makeExample libInstallInputPattern ["@unison/base/topic"],
                  "installs the `topic` branch of `@unison/base`"
                )
              ]
          ],
      parse = \args ->
        maybe (Left (I.help libInstallInputPattern)) Right do
          [arg] <- Just args
          libdep <-
            eitherToMaybe $
              tryInto @(ProjectAndBranch ProjectName (Maybe ProjectBranchNameOrLatestRelease)) (Text.pack arg)
          Just (Input.LibInstallI libdep)
    }

reset :: InputPattern
reset =
  InputPattern
    "reset"
    []
    I.Visible
    [ ("namespace, hash, or branch to reset to", Required, namespaceOrProjectBranchArg config),
      ("namespace to be reset", Optional, namespaceOrProjectBranchArg config)
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
    config =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

-- asBranch = tryInto @(ProjectAndBranch (Maybe ProjectName) ProjectBranchName) (Text.pack inputString)

resetRoot :: InputPattern
resetRoot =
  InputPattern
    "reset-root"
    []
    I.Hidden
    [("namespace or hash to reset to", Required, namespaceArg)]
    ( P.lines
        [ "Deprecated because it's incompatible with projects. ⚠️ Warning, this command can cause codebase corruption.",
          P.wrapColumn2
            [ ( makeExample resetRoot [".foo"],
                "Reset the root namespace (along with its history) to that of the `.foo` namespace. Deprecated"
              ),
              ( makeExample resetRoot ["#9dndk3kbsk13nbpeu"],
                "Reset the root namespace (along with its history) to that of the namespace with hash `#9dndk3kbsk13nbpeu`."
              )
            ]
        ]
    )
    \case
      [src] -> first P.text $ do
        src <- Input.parseBranchId src
        pure $ Input.ResetRootI src
      _ -> Left (I.help resetRoot)

pull :: InputPattern
pull =
  pullImpl "pull" [] Input.PullWithHistory ""

pullWithoutHistory :: InputPattern
pullWithoutHistory =
  pullImpl
    "pull.without-history"
    []
    Input.PullWithoutHistory
    "without including the remote's history. This usually results in smaller codebase sizes."

pullImpl :: String -> [String] -> Input.PullMode -> P.Pretty CT.ColorText -> InputPattern
pullImpl name aliases pullMode addendum = do
  self
  where
    self =
      InputPattern
        { patternName = name,
          aliases = aliases,
          visibility = I.Visible,
          args =
            [ ("remote namespace to pull", Optional, remoteNamespaceArg),
              ( "destination branch",
                Optional,
                projectBranchNameArg
                  ProjectBranchSuggestionsConfig
                    { showProjectCompletions = False,
                      projectInclusion = AllProjects,
                      branchInclusion = AllBranches
                    }
              )
            ],
          help =
            P.lines
              [ P.wrap $
                  "The"
                    <> makeExample' self
                    <> "command merges a remote namespace into a local branch"
                    <> addendum,
                "",
                P.wrapColumn2
                  [ ( makeExample self ["@unison/base/main"],
                      "merges the branch `main`"
                        <> "of the Unison Share hosted project `@unison/base`"
                        <> "into the current branch"
                    ),
                    ( makeExample self ["@unison/base/main", "my-base/topic"],
                      "merges the branch `main`"
                        <> "of the Unison Share hosted project `@unison/base`"
                        <> "into the branch `topic` of the local `my-base` project"
                    )
                  ],
                "",
                explainRemote Pull
              ],
          parse = \case
            -- maybeToEither (I.help self) . \case
            [] -> Right $ Input.PullI Input.PullSourceTarget0 pullMode
            [sourceString] -> do
              source <-
                sourceString
                  & Text.pack
                  & megaparse (readRemoteNamespaceParser ProjectBranchSpecifier'NameOrLatestRelease)
                  & mapLeft (\err -> I.help self <> P.newline <> err)
              Right $ Input.PullI (Input.PullSourceTarget1 source) pullMode
            [sourceString, targetString] -> do
              source <-
                sourceString
                  & Text.pack
                  & megaparse (readRemoteNamespaceParser ProjectBranchSpecifier'NameOrLatestRelease)
                  & mapLeft (\err -> I.help self <> P.newline <> err)
              target <-
                targetString
                  & Text.pack
                  & megaparse branchWithOptionalProjectParser
                  & mapLeft
                    ( \err ->
                        -- You used to be able to pull into a path. So if target parsing fails, but path parsing succeeds,
                        -- explain that the command has changed. Furthermore, in the special case that the user is trying to
                        -- pull into the `lib` namespace, suggest using `lib.install`.
                        case Path.parsePath' targetString of
                          Left _ -> I.help self <> P.newline <> err
                          Right path ->
                            I.help self
                              <> P.newline
                              <> P.newline
                              <> P.newline
                              <> let pullingIntoLib =
                                       case path of
                                         Path.RelativePath'
                                           ( Path.Relative
                                               (Path.toList -> lib : _)
                                             ) -> lib == NameSegment.libSegment
                                         _ -> False
                                  in P.wrap $
                                       "You may only"
                                         <> makeExample' pull
                                         <> "into a branch."
                                         <> if pullingIntoLib
                                           then
                                             "Did you mean to run"
                                               <> P.group (makeExample libInstallInputPattern [P.string sourceString] <> "?")
                                           else mempty
                    )
              Right $ Input.PullI (Input.PullSourceTarget2 source target) pullMode
            _ -> Left (I.help self)
        }

debugTabCompletion :: InputPattern
debugTabCompletion =
  InputPattern
    "debug.tab-complete"
    []
    I.Hidden
    [("command arguments", ZeroPlus, noCompletionsArg)]
    ( P.lines
        [ P.wrap $ "This command can be used to test and debug ucm's tab-completion within transcripts.",
          P.wrap $ "Completions which are finished are prefixed with a * represent finished completions."
        ]
    )
    ( \inputs ->
        Right $ Input.DebugTabCompletionI inputs
    )

debugFuzzyOptions :: InputPattern
debugFuzzyOptions =
  InputPattern
    "debug.fuzzy-options"
    []
    I.Hidden
    [("command arguments", OnePlus, noCompletionsArg)]
    ( P.lines
        [ P.wrap $ "This command can be used to test and debug ucm's fuzzy-options within transcripts.",
          P.wrap $ "Write a command invocation with _ for any args you'd like to see completion options for.",
          P.wrap $ "We use _ instead of ! because ! will be expanded by the input parser before it hits the command itself.",
          P.wrap $ "E.g. `debug.fuzzy-options view _`",
          P.wrap $ "or `debug.fuzzy-options merge - _`"
        ]
    )
    \case
      (cmd : args) ->
        Right $ Input.DebugFuzzyOptionsI cmd args
      _ -> Left (I.help debugFuzzyOptions)

debugFormat :: InputPattern
debugFormat =
  InputPattern
    "debug.format"
    []
    I.Hidden
    [("source-file", Optional, filePathArg)]
    ( P.lines
        [ P.wrap $ "This command can be used to test ucm's file formatter on the latest typechecked file.",
          makeExample' debugFormat
        ]
    )
    ( \case
        [] -> Right Input.DebugFormatI
        _ -> Left (I.help debugFormat)
    )

push :: InputPattern
push =
  InputPattern
    "push"
    []
    I.Visible
    [("remote destination", Optional, remoteNamespaceArg), ("local target", Optional, namespaceOrProjectBranchArg suggestionsConfig)]
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
              pushBehavior = PushBehavior.RequireNonEmpty
            }
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

pushCreate :: InputPattern
pushCreate =
  InputPattern
    "push.create"
    []
    I.Visible
    [("remote destination", Optional, remoteNamespaceArg), ("local target", Optional, namespaceOrProjectBranchArg suggestionsConfig)]
    ( P.lines
        [ P.wrap
            "The `push.create` command pushes a local namespace to an empty remote namespace.",
          "",
          P.wrapColumn2
            [ ( "`push.create remote local`",
                "pushes the contents of the local namespace `local`"
                  <> "into the empty remote namespace `remote`."
              ),
              ( "`push.create remote`",
                "publishes the current namespace into the empty remote namespace `remote`"
              ),
              ( "`push.create`",
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
              pushBehavior = PushBehavior.RequireEmpty
            }
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

pushForce :: InputPattern
pushForce =
  InputPattern
    "unsafe.force-push"
    []
    I.Hidden
    [("remote destination", Optional, remoteNamespaceArg), ("local source", Optional, namespaceOrProjectBranchArg suggestionsConfig)]
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
              pushBehavior = PushBehavior.ForcePush
            }
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

pushExhaustive :: InputPattern
pushExhaustive =
  InputPattern
    "debug.push-exhaustive"
    []
    I.Hidden
    [("remote destination", Optional, remoteNamespaceArg), ("local target", Optional, namespaceOrProjectBranchArg suggestionsConfig)]
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
              pushBehavior = PushBehavior.RequireNonEmpty
            }
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

mergeOldSquashInputPattern :: InputPattern
mergeOldSquashInputPattern =
  InputPattern
    { patternName = "merge.old.squash",
      aliases = ["squash.old"],
      visibility = I.Hidden,
      args =
        [ ("namespace or branch to be squashed", Required, namespaceOrProjectBranchArg suggestionsConfig),
          ("merge destination", Required, namespaceOrProjectBranchArg suggestionsConfig)
        ],
      help =
        P.wrap $
          makeExample mergeOldSquashInputPattern ["src", "dest"]
            <> "merges `src` namespace or branch into the `dest` namespace or branch,"
            <> "discarding the history of `src` in the process."
            <> "The resulting `dest` will have (at most) 1"
            <> "additional history entry.",
      parse =
        maybeToEither (I.help mergeOldSquashInputPattern) . \case
          [src, dest] -> do
            src <- parseLooseCodeOrProject src
            dest <- parseLooseCodeOrProject dest
            Just $ Input.MergeLocalBranchI src dest Branch.SquashMerge
          _ -> Nothing
    }
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

mergeOldInputPattern :: InputPattern
mergeOldInputPattern =
  InputPattern
    "merge.old"
    []
    I.Hidden
    [ ("branch or namespace to merge", Required, namespaceOrProjectBranchArg config),
      ("merge destination", Optional, namespaceOrProjectBranchArg config)
    ]
    ( P.column2
        [ ( makeExample mergeOldInputPattern ["foo/bar", "baz/qux"],
            "merges the `foo/bar` branch into the `baz/qux` branch"
          ),
          ( makeExample mergeOldInputPattern ["/topic", "/main"],
            "merges the branch `topic` of the current project into the `main` branch of the current project"
          ),
          ( makeExample mergeOldInputPattern ["foo/topic", "/main"],
            "merges the branch `topic` of the project `foo` into the `main` branch of the current project"
          ),
          ( makeExample mergeOldInputPattern ["/topic", "foo/main"],
            "merges the branch `topic` of the current project into the `main` branch of the project 'foo`"
          ),
          ( makeExample mergeOldInputPattern [".src"],
            "merges `.src` namespace into the current namespace"
          ),
          ( makeExample mergeOldInputPattern [".src", ".dest"],
            "merges `.src` namespace into the `dest` namespace"
          )
        ]
    )
    ( maybeToEither (I.help mergeOldInputPattern) . \case
        [src] -> do
          src <- parseLooseCodeOrProject src
          Just $ Input.MergeLocalBranchI src (This Path.relativeEmpty') Branch.RegularMerge
        [src, dest] -> do
          src <- parseLooseCodeOrProject src
          dest <- parseLooseCodeOrProject dest
          Just $ Input.MergeLocalBranchI src dest Branch.RegularMerge
        _ -> Nothing
    )
  where
    config =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

mergeInputPattern :: InputPattern
mergeInputPattern =
  InputPattern
    { patternName = "merge",
      aliases = [],
      visibility = I.Visible,
      args =
        [ ( "branch to merge",
            Required,
            projectBranchNameArg
              ProjectBranchSuggestionsConfig
                { showProjectCompletions = True,
                  projectInclusion = AllProjects,
                  branchInclusion = ExcludeCurrentBranch
                }
          )
        ],
      help = P.wrap $ makeExample mergeInputPattern ["/branch"] <> "merges `branch` into the current branch",
      parse =
        \args ->
          maybeToEither (I.help mergeInputPattern) do
            [branchString] <- Just args
            branch <-
              eitherToMaybe $
                tryInto
                  @(ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
                  (Text.pack branchString)
            pure (Input.MergeI branch)
    }

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
    [("before namespace", Required, namespaceOrProjectBranchArg suggestionsConfig), ("after namespace", Optional, namespaceOrProjectBranchArg suggestionsConfig)]
    ( P.column2
        [ ( "`diff.namespace before after`",
            P.wrap "shows how the namespace `after` differs from the namespace `before`"
          ),
          ( "`diff.namespace before`",
            P.wrap "shows how the current namespace differs from the namespace `before`"
          )
        ]
    )
    ( \case
        [before, after] -> first P.text do
          before <- Input.parseBranchId before
          after <- Input.parseBranchId after
          pure $ Input.DiffNamespaceI before after
        [before] -> first P.text do
          before <- Input.parseBranchId before
          pure $ Input.DiffNamespaceI before (Right Path.currentPath)
        _ -> Left $ I.help diffNamespace
    )
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

mergeOldPreviewInputPattern :: InputPattern
mergeOldPreviewInputPattern =
  InputPattern
    "merge.old.preview"
    []
    I.Hidden
    [("branch or namespace to merge", Required, namespaceOrProjectBranchArg suggestionsConfig), ("merge destination", Optional, namespaceOrProjectBranchArg suggestionsConfig)]
    ( P.column2
        [ ( makeExample mergeOldPreviewInputPattern ["src"],
            "shows how the current namespace will change after a " <> makeExample mergeOldInputPattern ["src"]
          ),
          ( makeExample mergeOldPreviewInputPattern ["src", "dest"],
            "shows how `dest` namespace will change after a " <> makeExample mergeOldInputPattern ["src", "dest"]
          )
        ]
    )
    ( maybeToEither (I.help mergeOldPreviewInputPattern) . \case
        [src] -> do
          src <- parseLooseCodeOrProject src
          pure $ Input.PreviewMergeLocalBranchI src (This Path.relativeEmpty')
        [src, dest] -> do
          src <- parseLooseCodeOrProject src
          dest <- parseLooseCodeOrProject dest
          pure $ Input.PreviewMergeLocalBranchI src dest
        _ -> Nothing
    )
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

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
        [ ("definition to replace", Required, definitionQueryArg ExcludeDepsFZF),
          ("definition replacement", Required, definitionQueryArg ExcludeDepsFZF),
          ("patch", Optional, patchArg)
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
              patch <- first P.text <$> traverse Path.parseSplit' $ listToMaybe patch
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
      args = [("definition to edit", OnePlus, definitionQueryArg ExcludeDepsFZF)],
      help =
        P.lines
          [ "`edit foo` prepends the definition of `foo` to the top of the most "
              <> "recently saved file.",
            "`edit` without arguments invokes a search to select a definition for editing, which requires that `fzf` can be found within your PATH."
          ],
      parse =
        \case
          (x : xs) ->
            (x NE.:| xs)
              & traverse parseHashQualifiedName
              <&> (Input.ShowDefinitionI Input.LatestFileLocation Input.ShowDefinitionLocal)
          [] -> Left (I.help edit)
    }

editNamespace :: InputPattern
editNamespace =
  InputPattern
    { patternName = "edit.namespace",
      aliases = [],
      visibility = I.Visible,
      args = [("namespace to load definitions from", ZeroPlus, namespaceArg)],
      help =
        P.lines
          [ "`edit.namespace` will load all terms and types contained within the current namespace into your scratch file. This includes definitions in namespaces, but excludes libraries.",
            "`edit.namespace ns1 ns2 ...` loads the terms and types contained within the provided namespaces."
          ],
      parse = Right . Input.EditNamespaceI . fmap (Path.unsafeParseText . Text.pack)
    }

topicNameArg :: ArgumentType
topicNameArg =
  let topics = Map.keys helpTopicsMap
   in ArgumentType
        { typeName = "topic",
          suggestions = \q _ _ _ -> pure (exactComplete q $ topics),
          fzfResolver = Just $ Resolvers.fuzzySelectFromList (Text.pack <$> topics)
        }

helpTopics :: InputPattern
helpTopics =
  InputPattern
    "help-topics"
    ["help-topic"]
    I.Visible
    [("topic", Optional, topicNameArg)]
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
              <> "temporarily (like `exports.blah.foo`) and then use `move.*`."
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
              (patternName mergeInputPattern, "merge one branch into another"),
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
    [("command", Optional, commandNameArg)]
    "`help` shows general help and `help <cmd>` shows help for one command."
    \case
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
    \case
      [] -> pure Input.QuitI
      _ -> Left "Use `quit`, `exit`, or <Ctrl-D> to quit."

viewPatch :: InputPattern
viewPatch =
  InputPattern
    "view.patch"
    []
    I.Visible
    [("patch", Optional, patchArg)]
    ( P.wrapColumn2
        [ ( makeExample' viewPatch,
            "Lists all the edits in the default patch."
          ),
          ( makeExample viewPatch ["<patch>"],
            "Lists all the edits in the given patch."
          )
        ]
    )
    \case
      [] -> Right $ Input.ListEditsI Nothing
      [patchStr] -> mapLeft P.text do
        patch <- Path.parseSplit' patchStr
        Right $ Input.ListEditsI (Just patch)
      _ -> Left $ warn "`view.patch` takes a patch and that's it."

names :: Input.IsGlobal -> InputPattern
names isGlobal =
  InputPattern
    cmdName
    []
    I.Visible
    [("name or hash", Required, definitionQueryArg ExcludeDepsFZF)]
    (P.wrap $ makeExample (names isGlobal) ["foo"] <> " shows the hash and all known names for `foo`.")
    \case
      [thing] -> case HQ.parseText (Text.pack thing) of
        Just hq -> Right $ Input.NamesI isGlobal hq
        Nothing ->
          Left $
            "I was looking for one of these forms: "
              <> P.blue "foo .foo.bar foo#abc #abcde .foo.bar#asdf"
      _ -> Left (I.help (names isGlobal))
  where
    cmdName = if isGlobal then "names.global" else "names"

dependents, dependencies :: InputPattern
dependents =
  InputPattern
    "dependents"
    []
    I.Visible
    [("definition", Required, definitionQueryArg IncludeDepsFZF)]
    "List the named dependents of the specified definition."
    \case
      [thing] -> fmap Input.ListDependentsI $ parseHashQualifiedName thing
      _ -> Left (I.help dependents)
dependencies =
  InputPattern
    "dependencies"
    []
    I.Visible
    [("definition", Required, definitionQueryArg IncludeDepsFZF)]
    "List the dependencies of the specified definition."
    \case
      [thing] -> fmap Input.ListDependenciesI $ parseHashQualifiedName thing
      _ -> Left (I.help dependencies)

namespaceDependencies :: InputPattern
namespaceDependencies =
  InputPattern
    "namespace.dependencies"
    []
    I.Visible
    [("namespace", Optional, namespaceArg)]
    "List the external dependencies of the specified namespace."
    \case
      [p] -> first P.text do
        p <- Path.parsePath' p
        pure $ Input.NamespaceDependenciesI (Just p)
      [] -> pure (Input.NamespaceDependenciesI Nothing)
      _ -> Left (I.help namespaceDependencies)

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
    []
    "Dump the namespace to a text file"
    (const $ Right Input.DebugDumpNamespacesI)

debugDumpNamespaceSimple :: InputPattern
debugDumpNamespaceSimple =
  InputPattern
    "debug.dump-namespace-simple"
    []
    I.Visible
    []
    "Dump the namespace to a text file"
    (const $ Right Input.DebugDumpNamespaceSimpleI)

debugTerm :: InputPattern
debugTerm =
  InputPattern
    "debug.term.abt"
    []
    I.Hidden
    [("term", Required, exactDefinitionTermQueryArg IncludeDepsFZF)]
    "View debugging information for a given term."
    ( \case
        [thing] -> fmap (Input.DebugTermI False) $ parseHashQualifiedName thing
        _ -> Left (I.help debugTerm)
    )

debugTermVerbose :: InputPattern
debugTermVerbose =
  InputPattern
    "debug.term.abt.verbose"
    []
    I.Hidden
    [("term", Required, exactDefinitionTermQueryArg IncludeDepsFZF)]
    "View verbose debugging information for a given term."
    ( \case
        [thing] -> fmap (Input.DebugTermI True) $ parseHashQualifiedName thing
        _ -> Left (I.help debugTermVerbose)
    )

debugType :: InputPattern
debugType =
  InputPattern
    "debug.type.abt"
    []
    I.Hidden
    [("type", Required, exactDefinitionTypeQueryArg IncludeDepsFZF)]
    "View debugging information for a given type."
    ( \case
        [thing] -> fmap (Input.DebugTypeI) $ parseHashQualifiedName thing
        _ -> Left (I.help debugType)
    )

debugLSPFoldRanges :: InputPattern
debugLSPFoldRanges =
  InputPattern
    "debug.lsp.fold-ranges"
    []
    I.Hidden
    []
    "Output the source from the most recently parsed file, but annotated with the computed fold ranges."
    (const $ Right Input.DebugLSPFoldRangesI)

debugClearWatchCache :: InputPattern
debugClearWatchCache =
  InputPattern
    "debug.clear-cache"
    []
    I.Visible
    []
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
      args = [("before namespace", Required, namespaceArg), ("after namespace", Required, namespaceArg)],
      help = P.wrap "List all name changes between two causal hashes. Does not detect patch changes.",
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
    { patternName = "test",
      aliases = [],
      visibility = I.Visible,
      args = [("namespace", Optional, namespaceArg)],
      help =
        P.wrapColumn2
          [ ("`test`", "runs unit tests for the current branch"),
            ("`test foo`", "runs unit tests for the current branch defined in namespace `foo`")
          ],
      parse = \args ->
        maybe (Left (I.help test)) Right do
          path <-
            case args of
              [] -> Just Path.empty
              [pathString] -> eitherToMaybe $ Path.parsePath pathString
              _ -> Nothing
          Just $
            Input.TestI
              Input.TestInput
                { includeLibNamespace = False,
                  path,
                  showFailures = True,
                  showSuccesses = True
                }
    }

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
                path = Path.empty,
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
    [("namespace", Required, namespaceArg), ("", Required, filePathArg)]
    ( P.wrapColumn2
        [ ( "`docs.to-html .path.to.namespace ~/path/to/file/output`",
            "Render all docs contained within a namespace, no matter how deep,"
              <> "to html files on a file path"
          )
        ]
    )
    \case
      [namespacePath, destinationFilePath] -> first P.text do
        np <- Path.parsePath' namespacePath
        pure $ Input.DocsToHtmlI np destinationFilePath
      _ -> Left $ showPatternHelp docsToHtml

docToMarkdown :: InputPattern
docToMarkdown =
  InputPattern
    "debug.doc-to-markdown"
    []
    I.Visible
    [("doc to render", Required, exactDefinitionTermQueryArg ExcludeDepsFZF)]
    ( P.wrapColumn2
        [ ( "`debug.doc-to-markdown term.doc`",
            "Render a doc to markdown."
          )
        ]
    )
    \case
      [docNameText] -> first fromString $ do
        docName <- maybeToEither "Invalid name" . Name.parseText . Text.pack $ docNameText
        pure $ Input.DocToMarkdownI docName
      _ -> Left $ showPatternHelp docToMarkdown

execute :: InputPattern
execute =
  InputPattern
    "run"
    []
    I.Visible
    [("definition to execute", Required, exactDefinitionTermQueryArg ExcludeDepsFZF), ("argument", ZeroPlus, noCompletionsArg)]
    ( P.wrapColumn2
        [ ( "`run mymain args...`",
            "Runs `!mymain`, where `mymain` is searched for in the most recent"
              <> "typechecked file, or in the codebase."
              <> "Any provided arguments will be passed as program arguments as though they were"
              <> "provided at the command line when running mymain as an executable."
          )
        ]
    )
    \case
      [w] -> pure $ Input.ExecuteI (Text.pack w) []
      w : ws -> pure $ Input.ExecuteI (Text.pack w) ws
      _ -> Left $ showPatternHelp execute

saveExecuteResult :: InputPattern
saveExecuteResult =
  InputPattern
    "add.run"
    []
    I.Visible
    [("new name", Required, newNameArg)]
    ( "`add.run name` adds to the codebase the result of the most recent `run` command"
        <> "as `name`."
    )
    \case
      [w] -> pure $ Input.SaveExecuteResultI (Name.unsafeParseText (Text.pack w))
      _ -> Left $ showPatternHelp saveExecuteResult

ioTest :: InputPattern
ioTest =
  InputPattern
    { patternName = "io.test",
      aliases = ["test.io"],
      visibility = I.Visible,
      args = [("test to run", Required, exactDefinitionTermQueryArg ExcludeDepsFZF)],
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

ioTestAll :: InputPattern
ioTestAll =
  InputPattern
    { patternName = "io.test.all",
      aliases = ["test.io.all"],
      visibility = I.Visible,
      args = [],
      help =
        P.wrapColumn2
          [ ( "`io.test.all`",
              "runs unit tests for the current branch that use IO"
            )
          ],
      parse = \case
        [] -> Right Input.IOTestAllI
        _ -> Left $ showPatternHelp ioTest
    }

makeStandalone :: InputPattern
makeStandalone =
  InputPattern
    "compile"
    ["compile.output"]
    I.Visible
    [("definition to compile", Required, exactDefinitionTermQueryArg ExcludeDepsFZF), ("output file", Required, filePathArg)]
    ( P.wrapColumn2
        [ ( "`compile main file`",
            "Outputs a stand alone file that can be directly loaded and"
              <> "executed by unison. Said execution will have the effect of"
              <> "running `!main`."
          )
        ]
    )
    \case
      [main, file] ->
        Input.MakeStandaloneI file <$> parseHashQualifiedName main
      _ -> Left $ showPatternHelp makeStandalone

runScheme :: InputPattern
runScheme =
  InputPattern
    "run.native"
    []
    I.Visible
    [("definition to run", Required, exactDefinitionTermQueryArg ExcludeDepsFZF), ("arguments", ZeroPlus, noCompletionsArg)]
    ( P.wrapColumn2
        [ ( makeExample runScheme ["main", "args"],
            "Executes !main using native compilation via scheme."
          )
        ]
    )
    \case
      main : args -> Right $ Input.ExecuteSchemeI (Text.pack main) args
      _ -> Left $ showPatternHelp runScheme

compileScheme :: InputPattern
compileScheme =
  InputPattern
    "compile.native"
    []
    I.Hidden
    [("definition to compile", Required, exactDefinitionTermQueryArg ExcludeDepsFZF), ("output file", Required, filePathArg)]
    ( P.wrapColumn2
        [ ( makeExample compileScheme ["main", "file"],
            "Creates stand alone executable via compilation to"
              <> "scheme. The created executable will have the effect"
              <> "of running `!main`."
          )
        ]
    )
    \case
      [main, file] ->
        Input.CompileSchemeI (Text.pack file) <$> parseHashQualifiedName main
      _ -> Left $ showPatternHelp compileScheme

createAuthor :: InputPattern
createAuthor =
  InputPattern
    "create.author"
    []
    I.Visible
    [("definition name", Required, noCompletionsArg), ("author name", Required, noCompletionsArg)]
    ( makeExample createAuthor ["alicecoder", "\"Alice McGee\""]
        <> " "
        <> P.wrap
          ( " creates "
              <> backtick "alicecoder"
              <> "values in"
              <> backtick "metadata.authors"
              <> "and"
              <> backtick (P.group ("metadata.copyrightHolders" <> "."))
          )
    )
    ( \case
        symbolStr : authorStr@(_ : _) -> first P.text do
          symbol <-
            Megaparsec.runParser (Megaparsec.withParsecT (fmap NameSegment.renderParseErr) NameSegment.segmentP <* Megaparsec.eof) "" symbolStr
              & mapLeft (Text.pack . Megaparsec.errorBundlePretty)
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
    [("repository", Required, gitUrlArg)]
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
      args = [],
      help = P.wrap "Create a patch from a namespace diff.",
      parse = \case
        [branchId1, branchId2, patch] ->
          mapLeft P.text do
            branchId1 <- Input.parseBranchId branchId1
            branchId2 <- Input.parseBranchId branchId2
            patch <- Path.parseSplit' patch
            pure (Input.DiffNamespaceToPatchI Input.DiffNamespaceToPatchInput {branchId1, branchId2, patch})
        _ -> Left (showPatternHelp diffNamespaceToPatch)
    }

projectCreate :: InputPattern
projectCreate =
  InputPattern
    { patternName = "project.create",
      aliases = ["create.project"],
      visibility = I.Visible,
      args = [],
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
      args = [],
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
      args = [("new name", Required, projectNameArg)],
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
      args = [("project or branch to switch to", Required, projectAndBranchNamesArg suggestionsConfig)],
      help =
        P.wrapColumn2
          [ ("`switch`", "opens an interactive selector to pick a project and branch"),
            ("`switch foo/bar`", "switches to the branch `bar` in the project `foo`"),
            ("`switch foo/`", "switches to the last branch you visited in the project `foo`"),
            ("`switch /bar`", "switches to the branch `bar` in the current project")
          ],
      parse = \case
        [name] ->
          case tryInto @ProjectAndBranchNames (Text.pack name) of
            Left _ -> Left (showPatternHelp projectSwitch)
            Right projectAndBranch -> Right (Input.ProjectSwitchI projectAndBranch)
        _ -> Left (showPatternHelp projectSwitch)
    }
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = True,
          projectInclusion = AllProjects,
          branchInclusion = ExcludeCurrentBranch
        }

projectsInputPattern :: InputPattern
projectsInputPattern =
  InputPattern
    { patternName = "projects",
      aliases = ["list.project", "ls.project", "project.list"],
      visibility = I.Visible,
      args = [],
      help = P.wrap "List projects.",
      parse = \_ -> Right Input.ProjectsI
    }

branchesInputPattern :: InputPattern
branchesInputPattern =
  InputPattern
    { patternName = "branches",
      aliases = ["list.branch", "ls.branch", "branch.list"],
      visibility = I.Visible,
      args = [("project", Optional, projectNameArg)],
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
      args =
        [ ("branch", Required, projectBranchNameArg suggestionsConfig),
          ("branch", Optional, newBranchNameArg)
        ],
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
  where
    newBranchNameArg =
      ArgumentType
        { typeName = "new-branch",
          suggestions = \_ _ _ _ -> pure [],
          fzfResolver = Nothing
        }
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

branchEmptyInputPattern :: InputPattern
branchEmptyInputPattern =
  InputPattern
    { patternName = "branch.empty",
      aliases = ["branch.create-empty", "create.empty-branch"],
      visibility = I.Visible,
      args = [],
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
      args = [],
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
      args = [],
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
      args = [],
      help = P.wrap "Draft a release.",
      parse = \case
        [tryInto @Semver . Text.pack -> Right semver] -> Right (Input.ReleaseDraftI semver)
        _ -> Left (showPatternHelp releaseDraft)
    }

upgrade :: InputPattern
upgrade =
  InputPattern
    { patternName = "upgrade",
      aliases = [],
      visibility = I.Visible,
      args = [("dependency to upgrade", Required, dependencyArg), ("dependency to upgrade to", Required, dependencyArg)],
      help =
        P.wrap $
          "`upgrade old new` upgrades library dependency `lib.old` to `lib.new`, and, if successful, deletes `lib.old`.",
      parse =
        maybeToEither (I.help upgrade) . \args -> do
          [oldString, newString] <- Just args
          old <- parseRelativeNameSegment oldString
          new <- parseRelativeNameSegment newString
          Just (Input.UpgradeI old new)
    }
  where
    parseRelativeNameSegment :: String -> Maybe NameSegment
    parseRelativeNameSegment string = do
      name <- Name.parseText (Text.pack string)
      guard (Name.isRelative name)
      segment NE.:| [] <- Just (Name.reverseSegments name)
      Just segment

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
      debugTerm,
      debugTermVerbose,
      debugType,
      debugLSPFoldRanges,
      debugFileHashes,
      debugNameDiff,
      debugNumberedArgs,
      debugTabCompletion,
      debugFuzzyOptions,
      debugFormat,
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
      editNamespace,
      execute,
      find,
      findIn,
      findAll,
      findInAll,
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
      ioTestAll,
      libInstallInputPattern,
      load,
      makeStandalone,
      mergeBuiltins,
      mergeIOBuiltins,
      mergeOldInputPattern,
      mergeOldPreviewInputPattern,
      mergeOldSquashInputPattern,
      mergeInputPattern,
      names False, -- names
      names True, -- names.global
      namespaceDependencies,
      patch,
      previewAdd,
      previewUpdate,
      printVersion,
      projectCreate,
      projectCreateEmptyInputPattern,
      projectRenameInputPattern,
      projectSwitch,
      projectsInputPattern,
      pull,
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
      moveAll,
      replace,
      reset,
      resetRoot,
      runScheme,
      saveExecuteResult,
      test,
      testAll,
      todo,
      ui,
      undo,
      up,
      update,
      updateBuiltins,
      updateOld,
      updateOldNoPatch,
      upgrade,
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
  let options = commandNames <> Map.keys helpTopicsMap
   in ArgumentType
        { typeName = "command",
          suggestions = \q _ _ _ -> pure (exactComplete q options),
          fzfResolver = Just $ Resolvers.fuzzySelectFromList (Text.pack <$> options)
        }

exactDefinitionArg :: Resolvers.IncludeLibFZF -> ArgumentType
exactDefinitionArg includeLib =
  ArgumentType
    { typeName = "definition",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteTermOrType q p),
      fzfResolver = Just $ Resolvers.definitionResolver includeLib
    }

definitionQueryArg :: Resolvers.IncludeLibFZF -> ArgumentType
definitionQueryArg includeLib = (exactDefinitionArg includeLib) {typeName = "definition query"}

exactDefinitionTypeQueryArg :: IncludeLibFZF -> ArgumentType
exactDefinitionTypeQueryArg includeLib =
  ArgumentType
    { typeName = "type definition query",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteType q p),
      fzfResolver = Just $ Resolvers.typeDefinitionResolver includeLib
    }

exactDefinitionTypeOrTermQueryArg :: IncludeLibFZF -> ArgumentType
exactDefinitionTypeOrTermQueryArg includeLib =
  ArgumentType
    { typeName = "type or term definition query",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteTermOrType q p),
      fzfResolver = Just $ Resolvers.definitionResolver includeLib
    }

exactDefinitionTermQueryArg :: IncludeLibFZF -> ArgumentType
exactDefinitionTermQueryArg includeLib =
  ArgumentType
    { typeName = "term definition query",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteTerm q p),
      fzfResolver = Just $ Resolvers.termDefinitionResolver includeLib
    }

patchArg :: ArgumentType
patchArg =
  ArgumentType
    { typeName = "patch",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompletePatch q p),
      fzfResolver = Nothing
    }

namespaceArg :: ArgumentType
namespaceArg =
  ArgumentType
    { typeName = "namespace",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteNamespace q p),
      fzfResolver = Just Resolvers.namespaceResolver
    }

-- | Usually you'll want one or the other, but some commands support both right now.
namespaceOrProjectBranchArg :: ProjectBranchSuggestionsConfig -> ArgumentType
namespaceOrProjectBranchArg config =
  ArgumentType
    { typeName = "namespace or branch",
      suggestions =
        let namespaceSuggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteNamespace q p)
         in unionSuggestions
              [ projectAndOrBranchSuggestions config,
                namespaceSuggestions
              ],
      fzfResolver = Just Resolvers.projectOrBranchResolver
    }

namespaceOrDefinitionArg :: IncludeLibFZF -> ArgumentType
namespaceOrDefinitionArg includeLib =
  ArgumentType
    { typeName = "term, type, or namespace",
      suggestions = \q cb _http p -> Codebase.runTransaction cb do
        namespaces <- prefixCompleteNamespace q p
        termsTypes <- prefixCompleteTermOrType q p
        pure (List.nubOrd $ namespaces <> termsTypes),
      fzfResolver =
        Just $ Resolvers.namespaceOrDefinitionResolver includeLib
    }

-- | A dependency name. E.g. if your project has `lib.base`, `base` would be a dependency
-- name.
dependencyArg :: ArgumentType
dependencyArg =
  ArgumentType
    { typeName = "project dependency",
      suggestions = \q cb _http p -> Codebase.runTransaction cb do
        prefixCompleteNamespace q (p Path.:> NameSegment.libSegment),
      fzfResolver = Just Resolvers.projectDependencyResolver
    }

newNameArg :: ArgumentType
newNameArg =
  ArgumentType
    { typeName = "new-name",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteNamespace q p),
      fzfResolver = Nothing
    }

noCompletionsArg :: ArgumentType
noCompletionsArg =
  ArgumentType
    { typeName = "word",
      suggestions = noCompletions,
      fzfResolver = Nothing
    }

filePathArg :: ArgumentType
filePathArg =
  ArgumentType
    { typeName = "file-path",
      suggestions = noCompletions,
      fzfResolver = Nothing
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
      fzfResolver = Nothing
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
              _ -> sharePathCompletion http input,
      fzfResolver = Nothing
    }

data ProjectInclusion = OnlyWithinCurrentProject | OnlyOutsideCurrentProject | AllProjects
  deriving stock (Eq, Ord, Show)

data BranchInclusion = ExcludeCurrentBranch | AllBranches
  deriving stock (Eq, Ord, Show)

data ProjectBranchSuggestionsConfig = ProjectBranchSuggestionsConfig
  { -- Whether projects (without branches) should be considered possible completions.
    showProjectCompletions :: Bool,
    -- Whether to include projects/branches within the current project, only outside the
    -- current project, or either.
    projectInclusion :: ProjectInclusion,
    -- Whether to include the current branch as a possible completion.
    branchInclusion :: BranchInclusion
  }

projectAndOrBranchSuggestions ::
  (MonadIO m) =>
  ProjectBranchSuggestionsConfig ->
  String ->
  Codebase m v a ->
  AuthenticatedHttpClient ->
  Path.Absolute -> -- Current path
  m [Line.Completion]
projectAndOrBranchSuggestions config inputStr codebase _httpClient path = do
  case Text.uncons input of
    -- Things like "/foo" would be parsed as unambiguous branches in the logic below, except we also want to
    -- handle "/<TAB>" and "/@<TAB>" inputs, which aren't valid branch names, but are valid branch prefixes. So,
    -- if the input begins with a forward slash, just rip it off and treat the rest as the branch prefix.
    Just ('/', input1) -> handleBranchesComplete input1 codebase path
    _ ->
      case tryInto @ProjectAndBranchNames input of
        -- This case handles inputs like "", "@", and possibly other things that don't look like a valid project
        -- or branch, but are a valid prefix of one
        Left _err -> handleAmbiguousComplete input codebase
        Right (ProjectAndBranchNames'Ambiguous _ _) -> handleAmbiguousComplete input codebase
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
                  fmap (filterBranches config path) do
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
                  fmap (filterBranches config path) do
                    Queries.loadAllProjectBranchesBeginningWith projectId (Just $ into @Text branchName)
          pure (map (projectBranchToCompletion projectName) branches)
  where
    input = Text.strip . Text.pack $ inputStr

    (mayCurrentProjectId, _mayCurrentBranchId) = case projectContextFromPath path of
      LooseCodePath {} -> (Nothing, Nothing)
      ProjectBranchPath projectId branchId _ -> (Just projectId, Just branchId)

    handleAmbiguousComplete ::
      MonadIO m =>
      Text ->
      Codebase m v a ->
      m [Completion]
    handleAmbiguousComplete input codebase = do
      (branches, projects) <-
        Codebase.runTransaction codebase do
          branches <-
            case mayCurrentProjectId of
              Nothing -> pure []
              Just currentProjectId ->
                fmap (filterBranches config path) do
                  Queries.loadAllProjectBranchesBeginningWith currentProjectId (Just input)
          projects <- case (projectInclusion config, mayCurrentProjectId) of
            (OnlyWithinCurrentProject, Just currentProjectId) -> Queries.loadProject currentProjectId <&> maybeToList
            (OnlyWithinCurrentProject, Nothing) -> pure []
            _ -> Queries.loadAllProjectsBeginningWith (Just input) <&> filterProjects
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
              fmap (filterBranches config path) do
                Queries.loadAllProjectBranchesBeginningWith currentProjectId (Just branchName)
      pure (map currentProjectBranchToCompletion branches)

    filterProjects :: [Sqlite.Project] -> [Sqlite.Project]
    filterProjects projects =
      case (mayCurrentProjectId, projectInclusion config) of
        (_, AllProjects) -> projects
        (Nothing, _) -> projects
        (Just currentProjId, OnlyOutsideCurrentProject) -> projects & filter (\Sqlite.Project {projectId} -> projectId /= currentProjId)
        (Just currentBranchId, OnlyWithinCurrentProject) ->
          projects
            & List.find (\Sqlite.Project {projectId} -> projectId == currentBranchId)
            & maybeToList

projectToCompletion :: Sqlite.Project -> Completion
projectToCompletion project =
  Completion
    { replacement = stringProjectName,
      display = P.toAnsiUnbroken (prettyProjectNameSlash (project ^. #name)),
      isFinished = False
    }
  where
    stringProjectName = Text.unpack (into @Text (project ^. #name) <> "/")

projectBranchToCompletion :: ProjectName -> (ProjectBranchId, ProjectBranchName) -> Completion
projectBranchToCompletion projectName (_, branchName) =
  Completion
    { replacement = Text.unpack (into @Text (ProjectAndBranch projectName branchName)),
      display = P.toAnsiUnbroken (prettySlashProjectBranchName branchName),
      isFinished = False
    }

handleBranchesComplete ::
  MonadIO m =>
  ProjectBranchSuggestionsConfig ->
  Text ->
  Codebase m v a ->
  Path.Absolute ->
  m [Completion]
handleBranchesComplete config branchName codebase path = do
  branches <-
    case preview ProjectUtils.projectBranchPathPrism path of
      Nothing -> pure []
      Just (ProjectAndBranch currentProjectId _, _) ->
        Codebase.runTransaction codebase do
          fmap (filterBranches config path) do
            Queries.loadAllProjectBranchesBeginningWith currentProjectId (Just branchName)
  pure (map currentProjectBranchToCompletion branches)

filterBranches :: ProjectBranchSuggestionsConfig -> Path.Absolute -> [(ProjectBranchId, a)] -> [(ProjectBranchId, a)]
filterBranches config path branches =
  case (mayCurrentBranchId, branchInclusion config) of
    (_, AllBranches) -> branches
    (Nothing, _) -> branches
    (Just currentBranchId, ExcludeCurrentBranch) -> branches & filter (\(branchId, _) -> branchId /= currentBranchId)
  where
    (_mayCurrentProjectId, mayCurrentBranchId) = case projectContextFromPath path of
      LooseCodePath {} -> (Nothing, Nothing)
      ProjectBranchPath projectId branchId _ -> (Just projectId, Just branchId)

currentProjectBranchToCompletion :: (ProjectBranchId, ProjectBranchName) -> Completion
currentProjectBranchToCompletion (_, branchName) =
  Completion
    { replacement = '/' : Text.unpack (into @Text branchName),
      display = P.toAnsiUnbroken (prettySlashProjectBranchName branchName),
      isFinished = False
    }

branchRelativePathSuggestions ::
  MonadIO m =>
  ProjectBranchSuggestionsConfig ->
  String ->
  Codebase m v a ->
  AuthenticatedHttpClient ->
  Path.Absolute -> -- Current path
  m [Line.Completion]
branchRelativePathSuggestions config inputStr codebase _httpClient currentPath = do
  case parseIncrementalBranchRelativePath inputStr of
    Left _ -> pure []
    Right ibrp -> case ibrp of
      BranchRelativePath.ProjectOrRelative _txt _path -> do
        namespaceSuggestions <- Codebase.runTransaction codebase (prefixCompleteNamespace inputStr currentPath)
        projectSuggestions <- projectNameSuggestions WithSlash inputStr codebase
        pure (namespaceSuggestions ++ projectSuggestions)
      BranchRelativePath.LooseCode _path ->
        Codebase.runTransaction codebase (prefixCompleteNamespace inputStr currentPath)
      BranchRelativePath.IncompleteProject _proj ->
        projectNameSuggestions WithSlash inputStr codebase
      BranchRelativePath.IncompleteBranch mproj mbranch -> case mproj of
        Nothing -> map suffixPathSep <$> handleBranchesComplete config (maybe "" into mbranch) codebase currentPath
        Just projectName -> do
          branches <-
            Codebase.runTransaction codebase do
              Queries.loadProjectByName projectName >>= \case
                Nothing -> pure []
                Just project -> do
                  let projectId = project ^. #projectId
                  fmap (filterBranches config currentPath) do
                    Queries.loadAllProjectBranchesBeginningWith projectId (into @Text <$> mbranch)
          pure (map (projectBranchToCompletionWithSep projectName) branches)
      BranchRelativePath.PathRelativeToCurrentBranch relPath -> Codebase.runTransaction codebase do
        mprojectBranch <- runMaybeT do
          (projectId, branchId) <- MaybeT (pure $ (,) <$> mayCurrentProjectId <*> mayCurrentBranchId)
          MaybeT (Queries.loadProjectBranch projectId branchId)
        case mprojectBranch of
          Nothing -> pure []
          Just projectBranch -> do
            let branchPath = review ProjectUtils.projectBranchPathPrism (projectAndBranch, mempty)
                projectAndBranch = ProjectAndBranch (projectBranch ^. #projectId) (projectBranch ^. #branchId)
            map prefixPathSep <$> prefixCompleteNamespace (Path.convert relPath) branchPath
      BranchRelativePath.IncompletePath projStuff mpath -> do
        Codebase.runTransaction codebase do
          mprojectBranch <- runMaybeT do
            case projStuff of
              Left names@(ProjectAndBranch projectName branchName) -> do
                (,Left names) <$> MaybeT (Queries.loadProjectBranchByNames projectName branchName)
              Right branchName -> do
                currentProjectId <- MaybeT (pure mayCurrentProjectId)
                projectBranch <- MaybeT (Queries.loadProjectBranchByName currentProjectId branchName)
                pure (projectBranch, Right (projectBranch ^. #name))
          case mprojectBranch of
            Nothing -> pure []
            Just (projectBranch, prefix) -> do
              let branchPath = review ProjectUtils.projectBranchPathPrism (projectAndBranch, mempty)
                  projectAndBranch = ProjectAndBranch (projectBranch ^. #projectId) (projectBranch ^. #branchId)
              map (addBranchPrefix prefix) <$> prefixCompleteNamespace (maybe "" Path.convert mpath) branchPath
  where
    (mayCurrentProjectId, mayCurrentBranchId) = case projectContextFromPath currentPath of
      LooseCodePath {} -> (Nothing, Nothing)
      ProjectBranchPath projectId branchId _ -> (Just projectId, Just branchId)

    projectBranchToCompletionWithSep :: ProjectName -> (ProjectBranchId, ProjectBranchName) -> Completion
    projectBranchToCompletionWithSep projectName (_, branchName) =
      Completion
        { replacement = Text.unpack (into @Text (ProjectAndBranch projectName branchName) <> branchPathSep),
          display = P.toAnsiUnbroken (prettySlashProjectBranchName branchName <> branchPathSepPretty),
          isFinished = False
        }

    prefixPathSep :: Completion -> Completion
    prefixPathSep c =
      c
        { Line.replacement = branchPathSep <> Line.replacement c,
          Line.display = P.toAnsiUnbroken branchPathSepPretty <> Line.display c
        }

    suffixPathSep :: Completion -> Completion
    suffixPathSep c =
      c
        { Line.replacement = Line.replacement c <> branchPathSep,
          Line.display = Line.display c <> P.toAnsiUnbroken branchPathSepPretty
        }

    addBranchPrefix ::
      Either (ProjectAndBranch ProjectName ProjectBranchName) ProjectBranchName ->
      Completion ->
      Completion
    addBranchPrefix eproj =
      let (prefixText, prefixPretty) = case eproj of
            Left pb ->
              ( into @Text pb,
                prettyProjectAndBranchName pb
              )
            Right branch ->
              ( "/" <> into @Text branch,
                prettySlashProjectBranchName branch
              )
       in \c ->
            c
              { Line.replacement = Text.unpack prefixText <> branchPathSep <> Line.replacement c,
                Line.display = P.toAnsiUnbroken (prefixPretty <> branchPathSepPretty) <> Line.display c
              }

    branchPathSepPretty = P.hiBlack branchPathSep

    branchPathSep :: IsString s => s
    branchPathSep = ":"

-- | A project name, branch name, or both.
projectAndBranchNamesArg :: ProjectBranchSuggestionsConfig -> ArgumentType
projectAndBranchNamesArg config =
  ArgumentType
    { typeName = "project-and-branch-names",
      suggestions = projectAndOrBranchSuggestions config,
      fzfResolver = Just Resolvers.projectAndOrBranchArg
    }

-- | A project branch name.
projectBranchNameArg :: ProjectBranchSuggestionsConfig -> ArgumentType
projectBranchNameArg config =
  ArgumentType
    { typeName = "project-branch-name",
      suggestions = projectAndOrBranchSuggestions config,
      fzfResolver = Just Resolvers.projectBranchResolver
    }

branchRelativePathArg :: ArgumentType
branchRelativePathArg =
  ArgumentType
    { typeName = "branch-relative-path",
      suggestions = branchRelativePathSuggestions config,
      fzfResolver = Nothing
    }
  where
    config =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = True,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

-- | A project name.
projectNameArg :: ArgumentType
projectNameArg =
  ArgumentType
    { typeName = "project-name",
      suggestions = \input codebase _httpClient _path -> projectNameSuggestions NoSlash input codebase,
      fzfResolver = Just $ Resolvers.multiResolver [Resolvers.projectNameOptions]
    }

data OptionalSlash
  = WithSlash
  | NoSlash

projectNameSuggestions ::
  MonadIO m =>
  OptionalSlash ->
  String ->
  Codebase m v a ->
  m [Line.Completion]
projectNameSuggestions slash (Text.strip . Text.pack -> input) codebase = do
  projects <-
    Codebase.runTransaction codebase do
      Queries.loadAllProjectsBeginningWith (Just input)
  pure $ map projectToCompletion projects
  where
    projectToCompletion :: Sqlite.Project -> Completion
    projectToCompletion =
      let toPretty = case slash of
            NoSlash -> prettyProjectName
            WithSlash -> prettyProjectNameSlash
          toText project = case slash of
            NoSlash -> into @Text (project ^. #name)
            WithSlash -> Text.snoc (into @Text (project ^. #name)) '/'
       in \project ->
            Completion
              { replacement = Text.unpack (toText project),
                display = P.toAnsiUnbroken (toPretty (project ^. #name)),
                isFinished = False
              }

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
  case Megaparsec.parseMaybe UriParser.writeRemoteNamespace (Text.pack target) of
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
    $ HQ.parseText (Text.pack s)

parseWriteGitRepo :: String -> String -> Either (P.Pretty P.ColorText) WriteGitRepo
parseWriteGitRepo label input = do
  first
    (fromString . show) -- turn any parsing errors into a Pretty.
    (Megaparsec.parse (UriParser.writeGitRepo <* Megaparsec.eof) label (Text.pack input))

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

megaparse :: Megaparsec.Parsec Void Text a -> Text -> Either (P.Pretty P.ColorText) a
megaparse parser input =
  input
    & Megaparsec.parse (parser <* Megaparsec.eof) ""
    & mapLeft (prettyPrintParseError (Text.unpack input))
