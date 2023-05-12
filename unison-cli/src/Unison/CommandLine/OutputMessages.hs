{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Unison.CommandLine.OutputMessages where

import Control.Lens hiding (at)
import Control.Monad.State
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Writer.CPS
import Data.Bifunctor (first, second)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import Data.List (stripPrefix)
import qualified Data.List as List
import Data.List.Extra (notNull, nubOrd, nubOrdOn)
import qualified Data.List.NonEmpty as NEList
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.Human (HumanTimeLocale (..), defaultHumanTimeLocale, humanReadableTimeI18N')
import Data.Tuple (swap)
import Data.Tuple.Extra (dupe)
import Data.Void (absurd)
import qualified Network.HTTP.Types as Http
import Network.URI (URI)
import qualified Network.URI as URI
import qualified Network.URI.Encode as URI
import qualified Servant.Client as Servant
import qualified System.Console.ANSI as ANSI
import qualified System.Console.Haskeline.Completion as Completion
import System.Directory
  ( canonicalizePath,
    doesFileExist,
    getHomeDirectory,
  )
import U.Codebase.Branch (NamespaceStats (..))
import U.Codebase.Branch.Diff (NameChanges (..))
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Sqlite.DbId (SchemaVersion (SchemaVersion))
import qualified U.Codebase.Sqlite.Project as Sqlite
import qualified U.Codebase.Sqlite.ProjectBranch as Sqlite
import U.Util.Base32Hex (Base32Hex)
import qualified U.Util.Base32Hex as Base32Hex
import qualified Unison.ABT as ABT
import qualified Unison.Auth.Types as Auth
import qualified Unison.Builtin.Decls as DD
import qualified Unison.Cli.Share.Projects.Types as Share
import Unison.Codebase.Editor.DisplayObject (DisplayObject (BuiltinObject, MissingObject, UserObject))
import qualified Unison.Codebase.Editor.Input as Input
import Unison.Codebase.Editor.Output
  ( CreatedProjectBranchFrom (..),
    DisplayDefinitionsOutput (..),
    NumberedArgs,
    NumberedOutput (..),
    Output (..),
    ShareError (..),
    TestReportStats (CachedTests, NewlyComputed),
    UndoFailureReason (CantUndoPastMerge, CantUndoPastStart),
    WhichBranchEmpty (..),
  )
import qualified Unison.Codebase.Editor.Output as E
import qualified Unison.Codebase.Editor.Output.BranchDiff as OBD
import qualified Unison.Codebase.Editor.Output.PushPull as PushPull
import Unison.Codebase.Editor.RemoteRepo
  ( ReadGitRepo,
    ReadRemoteNamespace,
    ShareUserHandle (..),
    WriteGitRepo,
    WriteRemoteNamespace (..),
    WriteShareRemoteNamespace (..),
    shareUserHandleToText,
  )
import qualified Unison.Codebase.Editor.RemoteRepo as RemoteRepo
import qualified Unison.Codebase.Editor.SlurpResult as SlurpResult
import qualified Unison.Codebase.Editor.TodoOutput as TO
import Unison.Codebase.GitError
import Unison.Codebase.IntegrityCheck (IntegrityResult (..), prettyPrintIntegrityErrors)
import Unison.Codebase.Patch (Patch (..))
import qualified Unison.Codebase.Patch as Patch
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.PushBehavior as PushBehavior
import qualified Unison.Codebase.Runtime as Runtime
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import qualified Unison.Codebase.ShortCausalHash as SCH
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import Unison.Codebase.SqliteCodebase.GitError
  ( GitSqliteCodebaseError (..),
  )
import qualified Unison.Codebase.TermEdit as TermEdit
import Unison.Codebase.Type (GitError (GitCodebaseError, GitProtocolError, GitSqliteCodebaseError))
import qualified Unison.Codebase.TypeEdit as TypeEdit
import Unison.CommandLine (bigproblem, note, tip)
import Unison.CommandLine.InputPattern (InputPattern)
import Unison.CommandLine.InputPatterns (makeExample')
import qualified Unison.CommandLine.InputPatterns as IP
import Unison.ConstructorReference (GConstructorReference (..))
import qualified Unison.ConstructorType as CT
import Unison.Core.Project (ProjectBranchName (UnsafeProjectBranchName))
import qualified Unison.DataDeclaration as DD
import qualified Unison.Hash as Hash
import Unison.Hash32 (Hash32)
import qualified Unison.Hash32 as Hash32
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import Unison.LabeledDependency as LD
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (..))
import qualified Unison.NameSegment as NameSegment
import Unison.Names (Names (..))
import qualified Unison.Names as Names
import qualified Unison.NamesWithHistory as Names
import Unison.Parser.Ann (Ann, startingLine)
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnv.Util as PPE
import qualified Unison.PrettyPrintEnvDecl as PPED
import Unison.PrettyTerminal
  ( clearCurrentLine,
    putPretty',
  )
import Unison.PrintError
  ( prettyParseError,
    prettyResolutionFailures,
    printNoteWithSource,
    renderCompilerBug,
  )
import Unison.Project (ProjectAndBranch (..), ProjectName, Semver (..))
import Unison.Reference (Reference, TermReference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Referent' as Referent
import qualified Unison.Result as Result
import Unison.Server.Backend (ShallowListEntry (..), TypeEntry (..))
import qualified Unison.Server.Backend as Backend
import qualified Unison.Server.SearchResult' as SR'
import qualified Unison.Share.Sync as Share
import Unison.Share.Sync.Types (CodeserverTransportError (..))
import qualified Unison.ShortHash as SH
import qualified Unison.ShortHash as ShortHash
import Unison.Symbol (Symbol)
import qualified Unison.Sync.Types as Share
import qualified Unison.Syntax.DeclPrinter as DeclPrinter
import qualified Unison.Syntax.HashQualified as HQ (toString, toText, unsafeFromVar)
import qualified Unison.Syntax.Name as Name (toString, toText)
import Unison.Syntax.NamePrinter
  ( SyntaxText,
    prettyHashQualified,
    prettyHashQualified',
    prettyLabeledDependency,
    prettyName,
    prettyNamedReference,
    prettyNamedReferent,
    prettyReference,
    prettyReferent,
    prettyShortHash,
    styleHashQualified,
    styleHashQualified',
  )
import qualified Unison.Syntax.TermPrinter as TermPrinter
import qualified Unison.Syntax.TypePrinter as TypePrinter
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.List as List
import Unison.Util.Monoid (intercalateMap)
import qualified Unison.Util.Monoid as Monoid
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.Relation as R
import Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.WatchKind as WK
import Witch (unsafeFrom)

type Pretty = P.Pretty P.ColorText

shortenDirectory :: FilePath -> IO FilePath
shortenDirectory dir = do
  home <- getHomeDirectory
  pure $ case stripPrefix home dir of
    Just d -> "~" <> d
    Nothing -> dir

renderFileName :: FilePath -> IO Pretty
renderFileName dir = P.group . P.blue . fromString <$> shortenDirectory dir

notifyNumbered :: NumberedOutput -> (Pretty, NumberedArgs)
notifyNumbered = \case
  ShowDiffNamespace oldPrefix newPrefix ppe diffOutput ->
    showDiffNamespace ShowNumbers ppe oldPrefix newPrefix diffOutput
  ShowDiffAfterDeleteDefinitions ppe diff ->
    first
      ( \p ->
          P.lines
            [ p,
              "",
              undoTip
            ]
      )
      (showDiffNamespace ShowNumbers ppe (absPathToBranchId Path.absoluteEmpty) (absPathToBranchId Path.absoluteEmpty) diff)
  ShowDiffAfterDeleteBranch bAbs ppe diff ->
    first
      ( \p ->
          P.lines
            [ p,
              "",
              undoTip
            ]
      )
      (showDiffNamespace ShowNumbers ppe (absPathToBranchId bAbs) (absPathToBranchId bAbs) diff)
  ShowDiffAfterModifyBranch b' _ _ (OBD.isEmpty -> True) ->
    (P.wrap $ "Nothing changed in" <> prettyPath' b' <> ".", mempty)
  ShowDiffAfterModifyBranch b' bAbs ppe diff ->
    first
      ( \p ->
          P.lines
            [ P.wrap $ "Here's what changed in" <> prettyPath' b' <> ":",
              "",
              p,
              "",
              undoTip
            ]
      )
      (showDiffNamespace ShowNumbers ppe (absPathToBranchId bAbs) (absPathToBranchId bAbs) diff)
  ShowDiffAfterMerge _ _ _ (OBD.isEmpty -> True) ->
    (P.wrap $ "Nothing changed as a result of the merge.", mempty)
  ShowDiffAfterMerge dest' destAbs ppe diffOutput ->
    first
      ( \p ->
          P.lines
            [ P.wrap $ "Here's what's changed in " <> prettyPathOrProjectAndBranchName dest' <> "after the merge:",
              "",
              p,
              "",
              tip $
                "You can use "
                  <> IP.makeExample' IP.todo
                  <> "to see if this generated any work to do in this namespace"
                  <> "and "
                  <> IP.makeExample' IP.test
                  <> "to run the tests."
                  <> "Or you can use"
                  <> IP.makeExample' IP.undo
                  <> " or"
                  <> IP.makeExample' IP.viewReflog
                  <> " to undo the results of this merge."
            ]
      )
      (showDiffNamespace ShowNumbers ppe (absPathToBranchId destAbs) (absPathToBranchId destAbs) diffOutput)
  ShowDiffAfterMergePropagate dest' destAbs patchPath' ppe diffOutput ->
    first
      ( \p ->
          P.lines
            [ P.wrap $
                "Here's what's changed in "
                  <> prettyPathOrProjectAndBranchName dest'
                  <> "after applying the patch at "
                  <> P.group (prettyPath' patchPath' <> ":"),
              "",
              p,
              "",
              tip $
                "You can use "
                  <> IP.makeExample IP.todo [prettyPath' patchPath', prettyPathOrProjectAndBranchName dest']
                  <> "to see if this generated any work to do in this namespace"
                  <> "and "
                  <> IP.makeExample' IP.test
                  <> "to run the tests."
                  <> "Or you can use"
                  <> IP.makeExample' IP.undo
                  <> " or"
                  <> IP.makeExample' IP.viewReflog
                  <> " to undo the results of this merge."
            ]
      )
      (showDiffNamespace ShowNumbers ppe (absPathToBranchId destAbs) (absPathToBranchId destAbs) diffOutput)
  ShowDiffAfterMergePreview dest' destAbs ppe diffOutput ->
    first
      ( \p ->
          P.lines
            [ P.wrap $ "Here's what would change in " <> prettyPathOrProjectAndBranchName dest' <> "after the merge:",
              "",
              p
            ]
      )
      (showDiffNamespace ShowNumbers ppe (absPathToBranchId destAbs) (absPathToBranchId destAbs) diffOutput)
  ShowDiffAfterUndo ppe diffOutput ->
    first
      (\p -> P.lines ["Here are the changes I undid", "", p])
      (showDiffNamespace ShowNumbers ppe (absPathToBranchId Path.absoluteEmpty) (absPathToBranchId Path.absoluteEmpty) diffOutput)
  ShowDiffAfterPull dest' destAbs ppe diff ->
    if OBD.isEmpty diff
      then ("✅  Looks like " <> prettyPath' dest' <> " is up to date.", mempty)
      else
        first
          ( \p ->
              P.lines
                [ P.wrap $ "Here's what's changed in " <> prettyPath' dest' <> "after the pull:",
                  "",
                  p,
                  "",
                  undoTip
                ]
          )
          (showDiffNamespace ShowNumbers ppe (absPathToBranchId destAbs) (absPathToBranchId destAbs) diff)
  -- todo: these numbers aren't going to work,
  --  since the content isn't necessarily here.
  -- Should we have a mode with no numbers? :P

  ShowDiffAfterCreateAuthor authorNS authorPath' bAbs ppe diff ->
    first
      ( \p ->
          P.lines
            [ p,
              "",
              tip $
                "Add"
                  <> prettyName "License"
                  <> "values for"
                  <> prettyName (Name.fromSegment authorNS)
                  <> "under"
                  <> P.group (prettyPath' authorPath' <> ".")
            ]
      )
      (showDiffNamespace ShowNumbers ppe (absPathToBranchId bAbs) (absPathToBranchId bAbs) diff)
  TodoOutput names todo -> todoOutput names todo
  CantDeleteDefinitions ppeDecl endangerments ->
    ( P.warnCallout $
        P.lines
          [ P.wrap "I didn't delete the following definitions because they are still in use:",
            "",
            endangeredDependentsTable ppeDecl endangerments
          ],
      numberedArgsForEndangerments ppeDecl endangerments
    )
  CantDeleteNamespace ppeDecl endangerments ->
    ( P.warnCallout $
        P.lines
          [ P.wrap "I didn't delete the namespace because the following definitions are still in use.",
            "",
            endangeredDependentsTable ppeDecl endangerments,
            "",
            P.wrap ("If you want to proceed anyways and leave those definitions without names, use " <> IP.patternName IP.deleteNamespaceForce)
          ],
      numberedArgsForEndangerments ppeDecl endangerments
    )
  History _cap schLength history tail ->
    let (tailMsg, tailHashes) = handleTail (length history + 1)
        msg :: Pretty
        msg =
          P.lines
            [ note $ "The most recent namespace hash is immediately below this message.",
              "",
              P.sep "\n\n" [go i (toSCH h) diff | (i, (h, diff)) <- zip [1 ..] reversedHistory],
              "",
              tailMsg
            ]
        branchHashes :: [CausalHash]
        branchHashes = (fst <$> reversedHistory) <> tailHashes
     in (msg, displayBranchHash <$> branchHashes)
    where
      toSCH :: CausalHash -> ShortCausalHash
      toSCH h = SCH.fromHash schLength h
      reversedHistory = reverse history
      showNum :: Int -> Pretty
      showNum n = P.shown n <> ". "
      handleTail :: Int -> (Pretty, [CausalHash])
      handleTail n = case tail of
        E.EndOfLog h ->
          ( P.lines
              [ "□ " <> showNum n <> prettySCH (toSCH h) <> " (start of history)"
              ],
            [h]
          )
        E.MergeTail h hs ->
          ( P.lines
              [ P.wrap $ "This segment of history starts with a merge." <> ex,
                "",
                "⊙ " <> showNum n <> prettySCH (toSCH h),
                "⑃",
                P.lines (hs & imap \i h -> showNum (n + 1 + i) <> prettySCH (toSCH h))
              ],
            h : hs
          )
        E.PageEnd h _n ->
          ( P.lines
              [ P.wrap $ "There's more history before the versions shown here." <> ex,
                "",
                dots,
                "",
                "⊙ " <> showNum n <> prettySCH (toSCH h),
                ""
              ],
            [h]
          )
      dots = "⠇"
      go i sch diff =
        P.lines
          [ "⊙ " <> showNum i <> prettySCH sch,
            "",
            P.indentN 2 $ prettyDiff diff
          ]
      ex =
        "Use"
          <> IP.makeExample IP.history ["#som3n4m3space"]
          <> "to view history starting from a given namespace hash."
  DeletedDespiteDependents ppeDecl endangerments ->
    ( P.warnCallout $
        P.lines
          [ P.wrap "Of the things I deleted, the following are still used in the following definitions. They now contain un-named references.",
            "",
            endangeredDependentsTable ppeDecl endangerments
          ],
      numberedArgsForEndangerments ppeDecl endangerments
    )
  ListEdits patch ppe -> showListEdits patch ppe
  ListProjects projects ->
    ( P.numberedList (map (prettyProjectName . view #name) projects),
      map (Text.unpack . into @Text . view #name) projects
    )
  ListBranches projectName branches ->
    ( P.columnNHeader
        ["", "Branch", "Remote branch"]
        ( do
            (i, (branchName, remoteBranches0)) <- zip [(1 :: Int) ..] branches
            case uncons remoteBranches0 of
              Nothing -> pure [P.hiBlack (P.shown i <> "."), prettyProjectBranchName branchName, ""]
              Just (firstRemoteBranch, remoteBranches) ->
                [ P.hiBlack (P.shown i <> "."),
                  prettyProjectBranchName branchName,
                  prettyRemoteBranchInfo firstRemoteBranch
                ]
                  : map (\branch -> ["", "", prettyRemoteBranchInfo branch]) remoteBranches
        ),
      map (\(branchName, _) -> Text.unpack (into @Text (ProjectAndBranch projectName branchName))) branches
    )
  BothLocalProjectAndProjectBranchExist project (ProjectAndBranch currentProject branch) ->
    ( P.wrap
        ( "I'm not sure if you wanted to switch to the branch"
            <> prettyProjectAndBranchName (ProjectAndBranch currentProject branch)
            <> "or the project"
            <> P.group (prettyProjectName project <> ".")
            <> "Could you be more specific?"
        )
        <> P.newline
        <> P.newline
        <> P.numberedList
          [ prettySlashProjectBranchName branch <> " (the branch " <> prettyProjectBranchName branch <> " in the current project)",
            prettyProjectNameSlash project <> " (the project " <> prettyProjectName project <> ", with the branch left unspecified)"
          ]
        <> P.newline
        <> P.newline
        <> tip
          ( "use "
              <> switch ["1"]
              <> " or "
              <> switch ["2"]
              <> " to pick one of these."
          ),
      [ Text.unpack (Text.cons '/' (into @Text branch)),
        Text.unpack (into @Text (ProjectAndBranch project (UnsafeProjectBranchName "main")))
      ]
    )
    where
      switch = IP.makeExample IP.projectSwitch
  where
    absPathToBranchId = Right

undoTip :: P.Pretty P.ColorText
undoTip =
  tip $
    "You can use"
      <> IP.makeExample' IP.undo
      <> "or"
      <> IP.makeExample' IP.viewReflog
      <> "to undo this change."

showListEdits :: Patch -> PPE.PrettyPrintEnv -> (P.Pretty P.ColorText, NumberedArgs)
showListEdits patch ppe =
  ( P.sepNonEmpty
      "\n\n"
      [ if null types
          then mempty
          else
            "Edited Types:"
              `P.hang` P.column2 typeOutputs,
        if null terms
          then mempty
          else
            "Edited Terms:"
              `P.hang` P.column2 termOutputs,
        if null types && null terms
          then "This patch is empty."
          else
            tip . P.string $
              "To remove entries from a patch, use "
                <> IP.deleteTermReplacementCommand
                <> " or "
                <> IP.deleteTypeReplacementCommand
                <> ", as appropriate."
      ],
    numberedArgsCol1 <> numberedArgsCol2
  )
  where
    typeOutputs, termOutputs :: [(Pretty, Pretty)]
    numberedArgsCol1, numberedArgsCol2 :: NumberedArgs
    -- We use the output of the first column's count as the first number in the second
    -- column's count. Laziness allows this since they're used independently of one another.
    (((typeOutputs, termOutputs), (lastNumberInFirstColumn, _)), (numberedArgsCol1, numberedArgsCol2)) =
      runWriter . flip runStateT (1, lastNumberInFirstColumn) $ do
        typeOutputs <- traverse prettyTypeEdit types
        termOutputs <- traverse prettyTermEdit terms
        pure (typeOutputs, termOutputs)
    types :: [(Reference, TypeEdit.TypeEdit)]
    types = R.toList $ Patch._typeEdits patch
    terms :: [(Reference, TermEdit.TermEdit)]
    terms = R.toList $ Patch._termEdits patch
    showNum :: Int -> Pretty
    showNum n = P.hiBlack (P.shown n <> ". ")

    prettyTermEdit ::
      (Reference.TermReference, TermEdit.TermEdit) ->
      StateT (Int, Int) (Writer (NumberedArgs, NumberedArgs)) (Pretty, Pretty)
    prettyTermEdit (lhsRef, termEdit) = do
      n1 <- gets fst <* modify (first succ)
      let lhsTermName = PPE.termName ppe (Referent.Ref lhsRef)
      -- We use the shortHash of the lhs rather than its name for numbered args,
      -- since its name is likely to be "historical", and won't work if passed to a ucm command.
      let lhsHash = ShortHash.toString . Reference.toShortHash $ lhsRef
      case termEdit of
        TermEdit.Deprecate -> do
          lift $ tell ([lhsHash], [])
          pure
            ( showNum n1 <> (P.syntaxToColor . prettyHashQualified $ lhsTermName),
              "-> (deprecated)"
            )
        TermEdit.Replace rhsRef _typing -> do
          n2 <- gets snd <* modify (second succ)
          let rhsTermName = PPE.termName ppe (Referent.Ref rhsRef)
          lift $ tell ([lhsHash], [HQ.toString rhsTermName])
          pure
            ( showNum n1 <> (P.syntaxToColor . prettyHashQualified $ lhsTermName),
              "-> " <> showNum n2 <> (P.syntaxToColor . prettyHashQualified $ rhsTermName)
            )

    prettyTypeEdit ::
      (Reference, TypeEdit.TypeEdit) ->
      StateT (Int, Int) (Writer (NumberedArgs, NumberedArgs)) (Pretty, Pretty)
    prettyTypeEdit (lhsRef, typeEdit) = do
      n1 <- gets fst <* modify (first succ)
      let lhsTypeName = PPE.typeName ppe lhsRef
      -- We use the shortHash of the lhs rather than its name for numbered args,
      -- since its name is likely to be "historical", and won't work if passed to a ucm command.
      let lhsHash = ShortHash.toString . Reference.toShortHash $ lhsRef
      case typeEdit of
        TypeEdit.Deprecate -> do
          lift $ tell ([lhsHash], [])
          pure
            ( showNum n1 <> (P.syntaxToColor . prettyHashQualified $ lhsTypeName),
              "-> (deprecated)"
            )
        TypeEdit.Replace rhsRef -> do
          n2 <- gets snd <* modify (second succ)
          let rhsTypeName = PPE.typeName ppe rhsRef
          lift $ tell ([lhsHash], [HQ.toString rhsTypeName])
          pure
            ( showNum n1 <> (P.syntaxToColor . prettyHashQualified $ lhsTypeName),
              "-> " <> showNum n2 <> (P.syntaxToColor . prettyHashQualified $ rhsTypeName)
            )

prettyURI :: URI -> Pretty
prettyURI = P.bold . P.blue . P.shown

prettyReadRemoteNamespace :: ReadRemoteNamespace Share.RemoteProjectBranch -> Pretty
prettyReadRemoteNamespace =
  prettyReadRemoteNamespaceWith \remoteProjectBranch ->
    into @Text (ProjectAndBranch (remoteProjectBranch ^. #projectName) (remoteProjectBranch ^. #branchName))

prettyReadRemoteNamespaceWith :: (a -> Text) -> ReadRemoteNamespace a -> Pretty
prettyReadRemoteNamespaceWith printProject =
  P.group . P.blue . P.text . RemoteRepo.printReadRemoteNamespace printProject

prettyWriteRemoteNamespace :: WriteRemoteNamespace (ProjectAndBranch ProjectName ProjectBranchName) -> Pretty
prettyWriteRemoteNamespace =
  P.group . P.blue . P.text . RemoteRepo.printWriteRemoteNamespace

notifyUser :: FilePath -> Output -> IO Pretty
notifyUser dir = \case
  SaveTermNameConflict name ->
    pure
      . P.warnCallout
      . P.wrap
      $ "Cannot save the last run result into"
        <> P.backticked (P.string (Name.toString name))
        <> "because that name conflicts with a name in the scratch file."
  NoLastRunResult ->
    pure
      . P.warnCallout
      . P.wrap
      $ "There is no previous evaluation to save."
        <> "Use"
        <> P.backticked "run"
        <> "to evaluate something before attempting"
        <> "to save it."
  Success -> pure $ P.bold "Done."
  PrintMessage pretty -> do
    pure pretty
  CouldntLoadBranch h ->
    pure . P.fatalCallout . P.wrap $
      "I have reason to believe that"
        <> P.shown h
        <> "exists in the codebase, but there was a failure"
        <> "when I tried to load it."
  NamespaceEmpty p ->
    case p of
      (p0 NEList.:| []) ->
        pure
          . P.warnCallout
          $ "The namespace "
            <> prettyBranchId p0
            <> " is empty. Was there a typo?"
      ps ->
        pure
          . P.warnCallout
          $ "The namespaces "
            <> P.commas (prettyBranchId <$> ps)
            <> " are empty. Was there a typo?"
  WarnIncomingRootBranch current hashes ->
    pure $
      if null hashes
        then
          P.wrap $
            "Please let someone know I generated an empty IncomingRootBranch"
              <> " event, which shouldn't be possible!"
        else
          P.lines
            [ P.wrap $
                (if length hashes == 1 then "A" else "Some")
                  <> "codebase"
                  <> P.plural hashes "root"
                  <> "appeared unexpectedly"
                  <> "with"
                  <> P.group (P.plural hashes "hash" <> ":"),
              "",
              (P.indentN 2 . P.oxfordCommas)
                (map prettySCH $ toList hashes),
              "",
              P.wrap $
                "and I'm not sure what to do about it."
                  <> "The last root namespace hash that I knew about was:",
              "",
              P.indentN 2 $ prettySCH current,
              "",
              P.wrap $ "Now might be a good time to make a backup of your codebase. 😬",
              "",
              P.wrap $
                "After that, you might try using the"
                  <> makeExample' IP.forkLocal
                  <> "command to inspect the namespaces listed above, and decide which"
                  <> "one you want as your root."
                  <> "You can also use"
                  <> makeExample' IP.viewReflog
                  <> "to see the"
                  <> "last few root namespace hashes on record.",
              "",
              P.wrap $
                "Once you find one you like, you can use the"
                  <> makeExample' IP.resetRoot
                  <> "command to set it."
            ]
  LoadPullRequest baseNS headNS basePath headPath mergedPath squashedPath ->
    pure $
      P.lines
        [ P.wrap $ "I checked out" <> prettyReadRemoteNamespaceWith absurd baseNS <> "to" <> P.group (prettyPath' basePath <> "."),
          P.wrap $ "I checked out" <> prettyReadRemoteNamespaceWith absurd headNS <> "to" <> P.group (prettyPath' headPath <> "."),
          "",
          P.wrap $ "The merged result is in" <> P.group (prettyPath' mergedPath <> "."),
          P.wrap $ "The (squashed) merged result is in" <> P.group (prettyPath' squashedPath <> "."),
          P.wrap $
            "Use"
              <> IP.makeExample
                IP.diffNamespace
                [prettyPath' basePath, prettyPath' mergedPath]
              <> "or"
              <> IP.makeExample
                IP.diffNamespace
                [prettyPath' basePath, prettyPath' squashedPath]
              <> "to see what's been updated.",
          P.wrap $
            "Use"
              <> IP.makeExample
                IP.todo
                [ prettyPath' (snoc mergedPath "patch"),
                  prettyPath' mergedPath
                ]
              <> "to see what work is remaining for the merge.",
          P.wrap $
            "Use"
              <> IP.makeExample
                IP.push
                [prettyReadRemoteNamespaceWith absurd baseNS, prettyPath' mergedPath]
              <> "or"
              <> IP.makeExample
                IP.push
                [prettyReadRemoteNamespaceWith absurd baseNS, prettyPath' squashedPath]
              <> "to push the changes."
        ]
  DisplayDefinitions output -> displayDefinitions output
  DisplayRendered outputLoc pp ->
    displayRendered outputLoc pp
  TestResults stats ppe _showSuccess _showFailures oks fails -> case stats of
    CachedTests 0 _ -> pure . P.callout "😶" $ "No tests to run."
    CachedTests n n'
      | n == n' ->
          pure $
            P.lines [cache, "", displayTestResults True ppe oks fails]
    CachedTests _n m ->
      pure $
        if m == 0
          then "✅  "
          else
            P.indentN 2 $
              P.lines ["", cache, "", displayTestResults False ppe oks fails, "", "✅  "]
    NewlyComputed -> do
      clearCurrentLine
      pure $
        P.lines
          [ "  " <> P.bold "New test results:",
            "",
            displayTestResults True ppe oks fails
          ]
    where
      cache = P.bold "Cached test results " <> "(`help testcache` to learn more)"
  TestIncrementalOutputStart ppe (n, total) r _src -> do
    putPretty' $
      P.shown (total - n)
        <> " tests left to run, current test: "
        <> P.syntaxToColor (prettyHashQualified (PPE.termName ppe $ Referent.Ref r))
    pure mempty
  TestIncrementalOutputEnd _ppe (_n, _total) _r result -> do
    clearCurrentLine
    if isTestOk result
      then putPretty' "  ✅  "
      else putPretty' "  🚫  "
    pure mempty
  TermMissingType ref ->
    pure . P.fatalCallout . P.lines $
      [ P.wrap $
          "The type signature for reference "
            <> P.blue (P.text (Reference.toText ref))
            <> " is missing from the codebase! This means something might be wrong "
            <> " with the codebase, or the term was deleted just now "
            <> " by someone else. Trying your command again might fix it."
      ]
  MetadataMissingType ppe ref ->
    pure . P.fatalCallout . P.lines $
      [ P.wrap $
          "The metadata value "
            <> P.red (prettyTermName ppe ref)
            <> "is missing a type signature in the codebase.",
        "",
        P.wrap $
          "This might be due to pulling an incomplete"
            <> "or invalid codebase, or because files inside the codebase"
            <> "are being deleted external to UCM."
      ]
  MetadataAmbiguous hq _ppe [] ->
    pure
      . P.warnCallout
      . P.wrap
      $ "I couldn't find any metadata matching "
        <> P.syntaxToColor (prettyHashQualified hq)
  MetadataAmbiguous _ ppe refs ->
    pure . P.warnCallout . P.lines $
      [ P.wrap $
          "I'm not sure which metadata value you're referring to"
            <> "since there are multiple matches:",
        "",
        P.indentN 2 $ P.spaced (P.blue . prettyTermName ppe <$> refs),
        "",
        tip "Try again and supply one of the above definitions explicitly."
      ]
  EvaluationFailure err -> pure err
  TypeTermMismatch typeName termName ->
    pure $
      P.warnCallout "I was expecting either two types or two terms but was given a type "
        <> P.syntaxToColor (prettyHashQualified typeName)
        <> " and a term "
        <> P.syntaxToColor (prettyHashQualified termName)
        <> "."
  SearchTermsNotFound hqs | null hqs -> pure mempty
  SearchTermsNotFound hqs ->
    pure $
      P.warnCallout "The following names were not found in the codebase. Check your spelling."
        <> P.newline
        <> (P.syntaxToColor $ P.indent "  " (P.lines (prettyHashQualified <$> hqs)))
  SearchTermsNotFoundDetailed wasTerm hqMisses otherHits ->
    pure (missMsg <> hitMsg)
    where
      typeOrTermMsg =
        if wasTerm
          then "I was expecting the following names to be terms, though I found types instead."
          else "I was expecting the following names to be types, though I found terms instead."
      missMsg = case null hqMisses of
        True -> mempty
        False ->
          P.warnCallout "The following names were not found in the codebase. Check your spelling."
            <> P.newline
            <> P.syntaxToColor (P.indent "  " (P.lines (prettyHashQualified <$> hqMisses)))
      hitMsg = case null otherHits of
        True -> mempty
        False ->
          P.warnCallout typeOrTermMsg
            <> P.newline
            <> P.syntaxToColor (P.indent "  " (P.lines (prettyHashQualified <$> otherHits)))
  PatchNotFound _ ->
    pure . P.warnCallout $ "I don't know about that patch."
  NameNotFound _ ->
    pure . P.warnCallout $ "I don't know about that name."
  NamesNotFound hqs ->
    pure $
      P.warnCallout "The following names were not found in the codebase. Check your spelling."
        <> P.newline
        <> (P.syntaxToColor $ P.indent "  " (P.lines (fmap prettyName hqs)))
  TermNotFound _ ->
    pure . P.warnCallout $ "I don't know about that term."
  TypeNotFound _ ->
    pure . P.warnCallout $ "I don't know about that type."
  TermAlreadyExists _ _ ->
    pure . P.warnCallout $ "A term by that name already exists."
  TypeAlreadyExists _ _ ->
    pure . P.warnCallout $ "A type by that name already exists."
  PatchAlreadyExists _ ->
    pure . P.warnCallout $ "A patch by that name already exists."
  BranchEmpty b ->
    pure . P.warnCallout . P.wrap $
      P.group (prettyWhichBranchEmpty b) <> "is an empty namespace."
  CantUndo reason -> case reason of
    CantUndoPastStart -> pure . P.warnCallout $ "Nothing more to undo."
    CantUndoPastMerge -> pure . P.warnCallout $ "Sorry, I can't undo a merge (not implemented yet)."
  NoMainFunction main ppe ts ->
    pure . P.callout "😶" $
      P.lines
        [ P.wrap $
            "I looked for a function"
              <> P.backticked (P.string main)
              <> "in the most recently typechecked file and codebase but couldn't find one. It has to have the type:",
          "",
          P.indentN 2 $ P.lines [P.string main <> " : " <> TypePrinter.pretty ppe t | t <- ts]
        ]
  BadMainFunction what main ty ppe ts ->
    pure . P.callout "😶" $
      P.lines
        [ P.string "I found this function:",
          "",
          P.indentN 2 $ P.string main <> " : " <> TypePrinter.pretty ppe ty,
          "",
          P.wrap $ P.string "but in order for me to" <> P.backticked (P.string what) <> "it needs be a subtype of:",
          "",
          P.indentN 2 $ P.lines [P.string main <> " : " <> TypePrinter.pretty ppe t | t <- ts]
        ]
  NoUnisonFile -> do
    dir' <- canonicalizePath dir
    fileName <- renderFileName dir'
    pure . P.callout "😶" $
      P.lines
        [ P.wrap "There's nothing for me to add right now.",
          "",
          P.column2 [(P.bold "Hint:", msg fileName)]
        ]
    where
      msg dir =
        P.wrap $
          "I'm currently watching for definitions in .u files under the"
            <> dir
            <> "directory. Make sure you've updated something there before using the"
            <> makeExample' IP.add
            <> "or"
            <> makeExample' IP.update
            <> "commands, or use"
            <> makeExample' IP.load
            <> "to load a file explicitly."
  InvalidSourceName name ->
    pure . P.callout "😶" $
      P.wrap $
        "The file "
          <> P.blue (P.shown name)
          <> " does not exist or is not a valid source file."
  SourceLoadFailed name ->
    pure . P.callout "😶" $
      P.wrap $
        "The file "
          <> P.blue (P.shown name)
          <> " could not be loaded."
  BadNamespace msg path ->
    pure . P.warnCallout $ "Invalid namespace " <> P.blue (P.string path) <> ", " <> P.string msg
  BranchNotFound b ->
    pure . P.warnCallout $ "The namespace " <> P.blue (P.shown b) <> " doesn't exist."
  EmptyLooseCodePush b ->
    pure . P.warnCallout $ "The namespace " <> P.blue (P.shown b) <> " is empty. There is nothing to push."
  EmptyProjectBranchPush projectAndBranch ->
    pure . P.warnCallout . P.wrap $
      prettyProjectAndBranchName projectAndBranch <> "is empty. There is nothing to push."
  CreatedNewBranch path ->
    pure $
      "☝️  The namespace " <> P.blue (P.shown path) <> " is empty."
  -- RenameOutput rootPath oldName newName r -> do
  --   nameChange "rename" "renamed" oldName newName r
  -- AliasOutput rootPath existingName newName r -> do
  --   nameChange "alias" "aliased" existingName newName r
  DeletedEverything ->
    pure . P.wrap . P.lines $
      [ "Okay, I deleted everything except the history.",
        "Use "
          <> IP.makeExample' IP.undo
          <> " to undo, or "
          <> IP.makeExample' IP.mergeBuiltins
          <> " to restore the absolute "
          <> "basics to the current path."
      ]
  DeleteEverythingConfirmation ->
    pure . P.warnCallout . P.lines $
      [ "Are you sure you want to clear away everything?",
        "You could use "
          <> IP.makeExample' IP.cd
          <> " to switch to a new namespace instead."
      ]
  DeleteBranchConfirmation _uniqueDeletions -> error "todo"
  -- let
  --   pretty (branchName, (ppe, results)) =
  --     header $ listOfDefinitions' ppe False results
  --     where
  --     header = plural uniqueDeletions id ((P.text branchName <> ":") `P.hang`)
  --
  -- in putPrettyLn . P.warnCallout
  --   $ P.wrap ("The"
  --   <> plural uniqueDeletions "namespace contains" "namespaces contain"
  --   <> "definitions that don't exist in any other branches:")
  --   <> P.border 2 (mconcat (fmap pretty uniqueDeletions))
  --   <> P.newline
  --   <> P.wrap "Please repeat the same command to confirm the deletion."
  MoveRootBranchConfirmation ->
    pure . P.warnCallout . P.lines $
      [ "Moves which affect the root branch cannot be undone, are you sure?",
        "Re-run the same command to proceed."
      ]
  MovedOverExistingBranch dest' ->
    pure . P.warnCallout . P.lines $
      [ P.wrap $ "A branch existed at the destination:" <> prettyPath' dest' <> "so I over-wrote it.",
        "",
        undoTip
      ]
  ListOfDefinitions fscope ppe detailed results ->
    listOfDefinitions fscope ppe detailed results
  ListOfLinks ppe results ->
    listOfLinks ppe [(name, tm) | (name, _ref, tm) <- results]
  ListNames global len types terms ->
    if null types && null terms
      then
        pure . P.callout "😶" $
          P.sepNonEmpty "\n\n" $
            [ P.wrap "I couldn't find anything by that name.",
              globalTip
            ]
      else
        pure . P.sepNonEmpty "\n\n" $
          [ formatTypes types,
            formatTerms terms,
            globalTip
          ]
    where
      globalTip =
        if global
          then mempty
          else (tip $ "Use " <> IP.makeExample (IP.names True) [] <> " to see more results.")
      formatTerms tms =
        P.lines . P.nonEmpty $ P.plural tms (P.blue "Term") : List.intersperse "" (go <$> tms)
        where
          go (ref, hqs) =
            P.column2
              [ ("Hash:", P.syntaxToColor (prettyReferent len ref)),
                ( "Names: ",
                  P.group $
                    P.spaced $
                      P.bold . P.syntaxToColor . prettyHashQualified' <$> List.sortBy Name.compareAlphabetical hqs
                )
              ]
      formatTypes types =
        P.lines . P.nonEmpty $ P.plural types (P.blue "Type") : List.intersperse "" (go <$> types)
        where
          go (ref, hqs) =
            P.column2
              [ ("Hash:", P.syntaxToColor (prettyReference len ref)),
                ( "Names:",
                  P.group $
                    P.spaced $
                      P.bold . P.syntaxToColor . prettyHashQualified' <$> List.sortBy Name.compareAlphabetical hqs
                )
              ]
  -- > names foo
  --   Terms:
  --     Hash: #asdflkjasdflkjasdf
  --     Names: .util.frobnicate foo blarg.mcgee
  --
  --   Term (with hash #asldfkjsdlfkjsdf): .util.frobnicate, foo, blarg.mcgee
  --   Types (with hash #hsdflkjsdfsldkfj): Optional, Maybe, foo
  ListShallow buildPPE entries -> do
    let needPPE =
          entries
            & any \case
              ShallowTermEntry {} -> True
              _ -> False
    ppe <-
      if needPPE
        then buildPPE
        else pure PPE.empty
    -- todo: make a version of prettyNumberedResult to support 3-columns
    pure $
      if null entries
        then P.lit "nothing to show"
        else numberedEntries ppe entries
    where
      numberedEntries :: (Var v) => PPE.PrettyPrintEnv -> [ShallowListEntry v a] -> Pretty
      numberedEntries ppe entries =
        (P.column3 . fmap f) ([(1 :: Integer) ..] `zip` fmap (formatEntry ppe) entries)
        where
          f (i, (p1, p2)) = (P.hiBlack . fromString $ show i <> ".", p1, p2)
      formatEntry :: (Var v) => PPE.PrettyPrintEnv -> ShallowListEntry v a -> (Pretty, Pretty)
      formatEntry ppe = \case
        ShallowTermEntry termEntry ->
          ( P.syntaxToColor . prettyHashQualified' . fmap Name.fromSegment . Backend.termEntryHQName $ termEntry,
            P.lit "(" <> maybe "type missing" (TypePrinter.pretty ppe) (Backend.termEntryType termEntry) <> P.lit ")"
          )
        ShallowTypeEntry typeEntry ->
          ( P.syntaxToColor . prettyHashQualified' . fmap Name.fromSegment . Backend.typeEntryHQName $ typeEntry,
            isBuiltin (typeEntryReference typeEntry)
          )
        ShallowBranchEntry ns _ (NamespaceStats {numContainedTerms, numContainedTypes}) ->
          ( (P.syntaxToColor . prettyName . Name.fromSegment) ns <> "/",
            case catMaybes [formatCount "term" numContainedTerms, formatCount "type" numContainedTypes] of
              [] -> ""
              counts -> P.hiBlack $ "(" <> intercalateMap ", " id counts <> ")"
          )
        ShallowPatchEntry ns ->
          ( (P.syntaxToColor . prettyName . Name.fromSegment) ns,
            P.lit "(patch)"
          )
      formatCount :: Pretty -> Int -> Maybe Pretty
      formatCount _thing 0 = Nothing
      formatCount thing 1 = Just $ "1 " <> thing
      formatCount thing n = Just $ P.shown n <> " " <> thing <> "s"
      isBuiltin = \case
        Reference.Builtin {} -> P.lit "(builtin type)"
        Reference.DerivedId {} -> P.lit "(type)"
  SlurpOutput input ppe s ->
    let isPast = case input of
          Input.AddI {} -> True
          Input.UpdateI {} -> True
          Input.SaveExecuteResultI {} -> True
          _ -> False
     in pure $ SlurpResult.pretty isPast ppe s
  FindNoLocalMatches ->
    pure . P.callout "☝️" $ P.wrap "I couldn't find matches in this namespace, searching in 'lib'..."
  NoExactTypeMatches ->
    pure . P.callout "☝️" $ P.wrap "I couldn't find exact type matches, resorting to fuzzy matching..."
  TypeParseError src e ->
    pure . P.fatalCallout $
      P.lines
        [ P.wrap "I couldn't parse the type you supplied:",
          "",
          prettyParseError src e
        ]
  ParseResolutionFailures src es ->
    pure $
      prettyResolutionFailures src es
  TypeHasFreeVars typ ->
    pure . P.warnCallout $
      P.lines
        [ P.wrap "The type uses these names, but I'm not sure what they are:",
          P.sep ", " (map (P.text . Var.name) . toList $ ABT.freeVars typ)
        ]
  ParseErrors src es ->
    pure . P.sep "\n\n" $ prettyParseError (Text.unpack src) <$> es
  TypeErrors curPath src ppenv notes -> do
    let showNote =
          intercalateMap "\n\n" (printNoteWithSource ppenv (Text.unpack src) curPath)
            . map Result.TypeError
    pure . showNote $ notes
  CompilerBugs src env bugs -> pure $ intercalateMap "\n\n" bug bugs
    where
      bug = renderCompilerBug env (Text.unpack src)
  Evaluated fileContents ppe bindings watches ->
    if null watches
      then pure "\n"
      else -- todo: hashqualify binding names if necessary to distinguish them from
      --       defs in the codebase.  In some cases it's fine for bindings to
      --       shadow codebase names, but you don't want it to capture them in
      --       the decompiled output.

        let prettyBindings =
              P.bracket . P.lines $
                P.wrap "The watch expression(s) reference these definitions:"
                  : ""
                  : [ P.syntaxToColor $ TermPrinter.prettyBinding ppe (HQ.unsafeFromVar v) b
                      | (v, b) <- bindings
                    ]
            prettyWatches =
              P.sep
                "\n\n"
                [ watchPrinter fileContents ppe ann kind evald isCacheHit
                  | (ann, kind, evald, isCacheHit) <-
                      sortOn (\(a, _, _, _) -> a) . toList $ watches
                ]
         in -- todo: use P.nonempty
            pure $
              if null bindings
                then prettyWatches
                else prettyBindings <> "\n" <> prettyWatches
  RunResult ppe term -> pure (TermPrinter.pretty ppe term)
  DisplayConflicts termNamespace typeNamespace ->
    pure $
      P.sepNonEmpty
        "\n\n"
        [ showConflicts "terms" terms,
          showConflicts "types" types
        ]
    where
      terms = R.dom termNamespace
      types = R.dom typeNamespace
      showConflicts :: (Foldable f) => Pretty -> f Name -> Pretty
      showConflicts thingsName things =
        if (null things)
          then mempty
          else
            P.lines
              [ "These " <> thingsName <> " have conflicts: ",
                "",
                P.lines [("  " <> prettyName x) | x <- toList things]
              ]
  -- TODO: Present conflicting TermEdits and TypeEdits
  -- if we ever allow users to edit hashes directly.
  Typechecked sourceName ppe slurpResult uf -> do
    let fileStatusMsg = SlurpResult.pretty False ppe slurpResult
    let containsWatchExpressions = notNull $ UF.watchComponents uf
    if UF.nonEmpty uf
      then do
        fileName <- renderFileName $ Text.unpack sourceName
        pure $
          P.linesNonEmpty
            ( [ if fileStatusMsg == mempty
                  then P.okCallout $ fileName <> " changed."
                  else
                    if SlurpResult.isAllDuplicates slurpResult
                      then
                        P.wrap $
                          "I found and"
                            <> P.bold "typechecked"
                            <> "the definitions in "
                            <> P.group (fileName <> ".")
                            <> "This file "
                            <> P.bold "has been previously added"
                            <> "to the codebase."
                      else
                        P.linesSpaced $
                          [ P.wrap $
                              "I found and"
                                <> P.bold "typechecked"
                                <> "these definitions in "
                                <> P.group (fileName <> ".")
                                <> "If you do an "
                                <> IP.makeExample' IP.add
                                <> " or "
                                <> P.group (IP.makeExample' IP.update <> ",")
                                <> "here's how your codebase would"
                                <> "change:",
                            P.indentN 2 $ SlurpResult.pretty False ppe slurpResult
                          ]
              ]
                ++ if containsWatchExpressions
                  then
                    [ "",
                      P.wrap $
                        "Now evaluating any watch expressions"
                          <> "(lines starting with `>`)... "
                          <> P.group (P.hiBlack "Ctrl+C cancels.")
                    ]
                  else []
            )
      else
        if (null $ UF.watchComponents uf)
          then
            pure . P.wrap $
              "I loaded " <> P.text sourceName <> " and didn't find anything."
          else pure mempty
  GitError e -> pure $ case e of
    GitSqliteCodebaseError e -> case e of
      CodebaseFileLockFailed ->
        P.wrap $
          "It looks to me like another ucm process is using this codebase. Only one ucm process can use a codebase at a time."
      NoDatabaseFile repo localPath ->
        P.wrap $
          "I didn't find a codebase in the repository at"
            <> prettyReadGitRepo repo
            <> "in the cache directory at"
            <> P.backticked' (P.string localPath) "."
      CodebaseRequiresMigration (SchemaVersion fromSv) (SchemaVersion toSv) -> do
        P.wrap $
          "The specified codebase codebase is on version "
            <> P.shown fromSv
            <> " but needs to be on version "
            <> P.shown toSv
      UnrecognizedSchemaVersion repo localPath (SchemaVersion v) ->
        P.wrap $
          "I don't know how to interpret schema version "
            <> P.shown v
            <> "in the repository at"
            <> prettyReadGitRepo repo
            <> "in the cache directory at"
            <> P.backticked' (P.string localPath) "."
      GitCouldntParseRootBranchHash repo s ->
        P.wrap $
          "I couldn't parse the string"
            <> P.red (P.string s)
            <> "into a namespace hash, when opening the repository at"
            <> P.group (prettyReadGitRepo repo <> ".")
    GitProtocolError e -> case e of
      NoGit ->
        P.wrap $
          "I couldn't find git. Make sure it's installed and on your path."
      CleanupError e ->
        P.wrap $
          "I encountered an exception while trying to clean up a git cache directory:"
            <> P.group (P.shown e)
      CloneException repo msg ->
        P.wrap $
          "I couldn't clone the repository at"
            <> prettyReadGitRepo repo
            <> ";"
            <> "the error was:"
            <> (P.indentNAfterNewline 2 . P.group . P.string) msg
      CopyException srcRepoPath destPath msg ->
        P.wrap $
          "I couldn't copy the repository at"
            <> P.string srcRepoPath
            <> "into"
            <> P.string destPath
            <> ";"
            <> "the error was:"
            <> (P.indentNAfterNewline 2 . P.group . P.string) msg
      PushNoOp repo ->
        P.wrap $
          "The repository at" <> prettyWriteGitRepo repo <> "is already up-to-date."
      PushException repo msg ->
        P.wrap $
          "I couldn't push to the repository at"
            <> prettyWriteGitRepo repo
            <> ";"
            <> "the error was:"
            <> (P.indentNAfterNewline 2 . P.group . P.string) msg
      RemoteRefNotFound repo ref ->
        P.wrap $
          "I couldn't find the ref " <> P.green (P.text ref) <> " in the repository at " <> P.blue (P.text repo) <> ";"
      UnrecognizableCacheDir uri localPath ->
        P.wrap $
          "A cache directory for"
            <> P.backticked (P.text $ RemoteRepo.printReadGitRepo uri)
            <> "already exists at"
            <> P.backticked' (P.string localPath) ","
            <> "but it doesn't seem to"
            <> "be a git repository, so I'm not sure what to do next.  Delete it?"
      UnrecognizableCheckoutDir uri localPath ->
        P.wrap $
          "I tried to clone"
            <> P.backticked (P.text $ RemoteRepo.printReadGitRepo uri)
            <> "into a cache directory at"
            <> P.backticked' (P.string localPath) ","
            <> "but I can't recognize the"
            <> "result as a git repository, so I'm not sure what to do next."
      PushDestinationHasNewStuff repo ->
        P.callout "⏸" . P.lines $
          [ P.wrap $
              "The repository at"
                <> prettyWriteGitRepo repo
                <> "has some changes I don't know about.",
            "",
            P.wrap $ "Try" <> pull <> "to merge these changes locally, then" <> push <> "again."
          ]
        where
          push = P.group . P.backticked . IP.patternName $ IP.push
          pull = P.group . P.backticked . IP.patternName $ IP.pull
    GitCodebaseError e -> case e of
      CouldntParseRemoteBranch repo s ->
        P.wrap $
          "I couldn't decode the root branch "
            <> P.string s
            <> "from the repository at"
            <> prettyReadGitRepo repo
      CouldntLoadRootBranch repo hash ->
        P.wrap $
          "I couldn't load the designated root hash"
            <> P.group ("(" <> P.text (Hash.toBase32HexText $ unCausalHash hash) <> ")")
            <> "from the repository at"
            <> prettyReadGitRepo repo
      CouldntLoadSyncedBranch ns h ->
        P.wrap $
          "I just finished importing the branch"
            <> P.red (P.shown h)
            <> "from"
            <> P.red (prettyReadRemoteNamespaceWith absurd (RemoteRepo.ReadRemoteNamespaceGit ns))
            <> "but now I can't find it."
      CouldntFindRemoteBranch repo path ->
        P.wrap $
          "I couldn't find the remote branch at"
            <> P.shown path
            <> "in the repository at"
            <> prettyReadGitRepo repo
      NoRemoteNamespaceWithHash repo sch ->
        P.wrap $
          "The repository at"
            <> prettyReadGitRepo repo
            <> "doesn't contain a namespace with the hash prefix"
            <> (P.blue . P.text . SCH.toText) sch
      RemoteNamespaceHashAmbiguous repo sch hashes ->
        P.lines
          [ P.wrap $
              "The namespace hash"
                <> prettySCH sch
                <> "at"
                <> prettyReadGitRepo repo
                <> "is ambiguous."
                <> "Did you mean one of these hashes?",
            "",
            P.indentN 2 $
              P.lines
                ( prettySCH . SCH.fromHash ((Text.length . SCH.toText) sch * 2)
                    <$> Set.toList hashes
                ),
            "",
            P.wrap "Try again with a few more hash characters to disambiguate."
          ]
  BustedBuiltins (Set.toList -> new) (Set.toList -> old) ->
    -- todo: this could be prettier!  Have a nice list like `find` gives, but
    -- that requires querying the codebase to determine term types.  Probably
    -- the only built-in types will be primitive types like `Int`, so no need
    -- to look up decl types.
    -- When we add builtin terms, they may depend on new derived types, so
    -- these derived types should be added to the branch too; but not
    -- necessarily ever be automatically deprecated.  (A library curator might
    -- deprecate them; more work needs to go into the idea of sharing deprecations and stuff.
    pure . P.warnCallout . P.lines $
      case (new, old) of
        ([], []) -> error "BustedBuiltins busted, as there were no busted builtins."
        ([], old) ->
          P.wrap ("This codebase includes some builtins that are considered deprecated. Use the " <> makeExample' IP.updateBuiltins <> " command when you're ready to work on eliminating them from your codebase:")
            : ""
            : fmap (P.text . Reference.toText) old
        (new, []) ->
          P.wrap ("This version of Unison provides builtins that are not part of your codebase. Use " <> makeExample' IP.updateBuiltins <> " to add them:")
            : ""
            : fmap (P.text . Reference.toText) new
        (new@(_ : _), old@(_ : _)) ->
          [ P.wrap
              ( "Sorry and/or good news!  This version of Unison supports a different set of builtins than this codebase uses.  You can use "
                  <> makeExample' IP.updateBuiltins
                  <> " to add the ones you're missing and deprecate the ones I'm missing. 😉"
              ),
            "You're missing:" `P.hang` P.lines (fmap (P.text . Reference.toText) new),
            "I'm missing:" `P.hang` P.lines (fmap (P.text . Reference.toText) old)
          ]
  ListOfPatches patches ->
    pure $
      if null patches
        then P.lit "nothing to show"
        else numberedPatches patches
    where
      numberedPatches :: Set Name -> Pretty
      numberedPatches patches =
        (P.column2 . fmap format) ([(1 :: Integer) ..] `zip` (toList patches))
        where
          format (i, p) = (P.hiBlack . fromString $ show i <> ".", prettyName p)
  ConfiguredMetadataParseError p md err ->
    pure . P.fatalCallout . P.lines $
      [ P.wrap $
          "I couldn't understand the default metadata that's set for "
            <> prettyPath' p
            <> " in .unisonConfig.",
        P.wrap $
          "The value I found was"
            <> (P.backticked . P.blue . P.string) md
            <> "but I encountered the following error when trying to parse it:",
        "",
        err
      ]
  NoConfiguredRemoteMapping pp p -> do
    let (localPathExample, sharePathExample) =
          if Path.isRoot p
            then ("myproject", "myuser.public.myproject")
            else (Path.toText (Path.unabsolute p), "myuser.public." <> Path.toText (Path.unabsolute p))
    pure . P.fatalCallout $
      P.lines
        [ "I don't know where to " <> PushPull.fold "push to." "pull from." pp,
          "Add a `RemoteMapping` configuration to your .unisonConfig file. E.g.",
          "",
          "```",
          "RemoteMapping {",
          P.text ("  " <> localPathExample <> " = \"" <> sharePathExample <> "\""),
          "}",
          "```",
          "",
          "Type `help " <> PushPull.fold "push" "pull" pp <> "` for more information."
        ]

  --  | ConfiguredGitUrlParseError PushPull Path' Text String
  ConfiguredRemoteMappingParseError pp p url err ->
    pure . P.fatalCallout . P.lines $
      [ P.wrap $
          "I couldn't understand the RemoteMapping that's set for"
            <> prettyAbsolute p
            <> "in .unisonConfig",
        P.wrap $
          "The value I found was"
            <> (P.backticked . P.blue . P.text) url
            <> "but I encountered the following error when trying to parse it:",
        "",
        P.string err,
        "",
        P.wrap $
          "Type"
            <> P.backticked ("help " <> PushPull.fold "push" "pull" pp)
            <> "for more information."
      ]
  NoBranchWithHash _h ->
    pure . P.callout "😶" $
      P.wrap $
        "I don't know of a namespace with that hash."
  NotImplemented -> pure $ P.wrap "That's not implemented yet. Sorry! 😬"
  BranchAlreadyExists p ->
    pure . P.wrap $
      "The namespace" <> prettyPath' p <> "already exists."
  LabeledReferenceNotFound hq ->
    pure . P.callout "\129300" . P.wrap . P.syntaxToColor $
      "Sorry, I couldn't find anything named" <> prettyHashQualified hq <> "."
  LabeledReferenceAmbiguous hashLen hq (LD.partition -> (tps, tms)) ->
    pure . P.callout "\129300" . P.lines $
      [ P.wrap "That name is ambiguous. It could refer to any of the following definitions:",
        "",
        P.indentN 2 (P.lines (map qualifyTerm tms ++ map qualifyType tps))
      ]
    where
      qualifyTerm :: Referent -> Pretty
      qualifyTerm =
        P.syntaxToColor . case hq of
          HQ.NameOnly n -> prettyNamedReferent hashLen n
          HQ.HashQualified n _ -> prettyNamedReferent hashLen n
          HQ.HashOnly _ -> prettyReferent hashLen
      qualifyType :: Reference -> Pretty
      qualifyType =
        P.syntaxToColor . case hq of
          HQ.NameOnly n -> prettyNamedReference hashLen n
          HQ.HashQualified n _ -> prettyNamedReference hashLen n
          HQ.HashOnly _ -> prettyReference hashLen
  DeleteNameAmbiguous hashLen p tms tys ->
    pure . P.callout "\129300" . P.lines $
      [ P.wrap "That name is ambiguous. It could refer to any of the following definitions:",
        "",
        P.indentN 2 (P.lines (map qualifyTerm (Set.toList tms) ++ map qualifyType (Set.toList tys))),
        "",
        P.wrap "You may:",
        "",
        P.indentN 2 . P.bulleted $
          [ P.wrap "Delete one by an unambiguous name, given above.",
            P.wrap "Delete them all by re-issuing the previous command."
          ]
      ]
    where
      name :: Name
      name = Path.unsafeToName' (HQ'.toName (Path.unsplitHQ' p))
      qualifyTerm :: Referent -> Pretty
      qualifyTerm = P.syntaxToColor . prettyNamedReferent hashLen name
      qualifyType :: Reference -> Pretty
      qualifyType = P.syntaxToColor . prettyNamedReference hashLen name
  TermAmbiguous _ _ -> pure "That term is ambiguous."
  HashAmbiguous h rs ->
    pure . P.callout "\129300" . P.lines $
      [ P.wrap $
          "The hash"
            <> prettyShortHash h
            <> "is ambiguous."
            <> "Did you mean one of these hashes?",
        "",
        P.indentN 2 $ P.lines (P.shown <$> Set.toList rs),
        "",
        P.wrap "Try again with a few more hash characters to disambiguate."
      ]
  BranchHashAmbiguous h rs ->
    pure . P.callout "\129300" . P.lines $
      [ P.wrap $
          "The namespace hash"
            <> prettySCH h
            <> "is ambiguous."
            <> "Did you mean one of these hashes?",
        "",
        P.indentN 2 $ P.lines (prettySCH <$> Set.toList rs),
        "",
        P.wrap "Try again with a few more hash characters to disambiguate."
      ]
  BadName n ->
    pure . P.wrap $ P.string n <> " is not a kind of name I understand."
  TermNotFound' sh ->
    pure $
      "I could't find a term with hash "
        <> (prettyShortHash sh)
  TypeNotFound' sh ->
    pure $
      "I could't find a type with hash "
        <> (prettyShortHash sh)
  NothingToPatch _patchPath dest ->
    pure $
      P.callout "😶" . P.wrap $
        "This had no effect. Perhaps the patch has already been applied"
          <> "or it doesn't intersect with the definitions in"
          <> P.group (prettyPath' dest <> ".")
  PatchNeedsToBeConflictFree ->
    pure . P.wrap $
      "I tried to auto-apply the patch, but couldn't because it contained"
        <> "contradictory entries."
  PatchInvolvesExternalDependents _ _ ->
    pure "That patch involves external dependents."
  ShowReflog [] -> pure . P.warnCallout $ "The reflog is empty"
  ShowReflog entries -> do
    now <- getCurrentTime
    pure $
      P.lines
        [ header,
          P.numberedColumnNHeader ["When", "Root Hash", "Action"] $ entries <&> renderEntry3Column now,
          "",
          tip $ "Use " <> IP.makeExample IP.diffNamespace ["1", "7"] <> " to compare namespaces between two points in history."
        ]
    where
      header =
        case entries of
          (_head : (_, prevSCH, _) : _) ->
            P.lines
              [ P.wrap $
                  "Here is a log of the root namespace hashes,"
                    <> "starting with the most recent,"
                    <> "along with the command that got us there."
                    <> "Try:",
                "",
                ( P.indentN 2 . P.wrapColumn2 $
                    [ ( IP.makeExample IP.forkLocal ["2", ".old"],
                        ""
                      ),
                      ( IP.makeExample IP.forkLocal [prettySCH prevSCH, ".old"],
                        "to make an old namespace accessible again,"
                      ),
                      (mempty, mempty),
                      ( IP.makeExample IP.resetRoot [prettySCH prevSCH],
                        "to reset the root namespace and its history to that of the specified"
                          <> "namespace."
                      )
                    ]
                ),
                ""
              ]
          _ -> mempty
      renderEntry3Column :: UTCTime -> (Maybe UTCTime, SCH.ShortCausalHash, Text) -> [Pretty]
      renderEntry3Column now (mayTime, sch, reason) =
        [maybe "" (prettyHumanReadableTime now) mayTime, P.blue (prettySCH sch), P.text $ truncateReason reason]
      truncateReason :: Text -> Text
      truncateReason txt = case Text.splitAt 60 txt of
        (short, "") -> short
        (short, _) -> short <> "..."
  StartOfCurrentPathHistory ->
    pure $
      P.wrap "You're already at the very beginning! 🙂"
  PullAlreadyUpToDate ns dest ->
    pure . P.callout "😶" $
      P.wrap $
        prettyPullTarget dest
          <> "was already up-to-date with"
          <> P.group (prettyReadRemoteNamespace ns <> ".")
  PullSuccessful ns dest ->
    pure . P.okCallout $
      P.wrap $
        "Successfully updated"
          <> prettyPullTarget dest
          <> "from"
          <> P.group (prettyReadRemoteNamespace ns <> ".")
  MergeOverEmpty dest ->
    pure . P.okCallout $
      P.wrap $
        "Successfully pulled into " <> P.group (prettyPullTarget dest <> ", which was empty.")
  MergeAlreadyUpToDate src dest ->
    pure . P.callout "😶" $
      P.wrap $
        prettyPathOrProjectAndBranchName dest
          <> "was already up-to-date with"
          <> P.group (prettyPathOrProjectAndBranchName src <> ".")
  PreviewMergeAlreadyUpToDate src dest ->
    pure . P.callout "😶" $
      P.wrap $
        prettyPathOrProjectAndBranchName dest
          <> "is already up-to-date with"
          <> P.group (prettyPathOrProjectAndBranchName src <> ".")
  DumpNumberedArgs args -> pure . P.numberedList $ fmap P.string args
  NoConflictsOrEdits ->
    pure (P.okCallout "No conflicts or edits in progress.")
  HelpMessage pat -> pure $ IP.showPatternHelp pat
  NoOp -> pure $ P.string "I didn't make any changes."
  DefaultMetadataNotification -> pure $ P.wrap "I added some default metadata."
  DumpBitBooster head map ->
    let go output [] = output
        go output (head : queue) = case Map.lookup head map of
          Nothing -> go (renderLine head [] : output) queue
          Just tails -> go (renderLine head tails : output) (queue ++ tails)
          where
            renderHash = take 10 . Text.unpack . Hash.toBase32HexText . unCausalHash
            renderLine head tail =
              (renderHash head)
                ++ "|"
                ++ intercalateMap " " renderHash tail
                ++ case Map.lookup (Hash.toBase32HexText . unCausalHash $ head) tags of
                  Just t -> "|tag: " ++ t
                  Nothing -> ""
            -- some specific hashes that we want to label in the output
            tags :: Map Text String
            tags =
              Map.fromList . fmap swap $
                [ ("unisonbase 2019/8/6", "54s9qjhaonotuo4sp6ujanq7brngk32f30qt5uj61jb461h9fcca6vv5levnoo498bavne4p65lut6k6a7rekaruruh9fsl19agu8j8"),
                  ("unisonbase 2019/8/5", "focmbmg7ca7ht7opvjaqen58fobu3lijfa9adqp7a1l1rlkactd7okoimpfmd0ftfmlch8gucleh54t3rd1e7f13fgei86hnsr6dt1g"),
                  ("unisonbase 2019/7/31", "jm2ltsg8hh2b3c3re7aru6e71oepkqlc3skr2v7bqm4h1qgl3srucnmjcl1nb8c9ltdv56dpsgpdur1jhpfs6n5h43kig5bs4vs50co"),
                  ("unisonbase 2019/7/25", "an1kuqsa9ca8tqll92m20tvrmdfk0eksplgjbda13evdlngbcn5q72h8u6nb86ojr7cvnemjp70h8cq1n95osgid1koraq3uk377g7g"),
                  ("ucm m1b", "o6qocrqcqht2djicb1gcmm5ct4nr45f8g10m86bidjt8meqablp0070qae2tvutnvk4m9l7o1bkakg49c74gduo9eati20ojf0bendo"),
                  ("ucm m1, m1a", "auheev8io1fns2pdcnpf85edsddj27crpo9ajdujum78dsncvfdcdu5o7qt186bob417dgmbd26m8idod86080bfivng1edminu3hug")
                ]
     in pure $
          P.lines
            [ P.lines (fmap fromString . reverse . nubOrd $ go [] [head]),
              "",
              "Paste that output into http://bit-booster.com/graph.html"
            ]
  ListDependents hqLength ld results ->
    pure $
      if null results
        then prettyLd <> " doesn't have any named dependents."
        else
          P.lines
            [ "Dependents of " <> prettyLd <> ":",
              "",
              P.indentN 2 (P.numberedColumn2Header num pairs),
              "",
              tip $ "Try " <> IP.makeExample IP.view ["1"] <> " to see the source of any numbered item in the above list."
            ]
    where
      prettyLd = P.syntaxToColor (prettyLabeledDependency hqLength ld)
      num n = P.hiBlack $ P.shown n <> "."
      header = (P.hiBlack "Name", P.hiBlack "Reference")
      pairs = header : map pair (List.sortOn (fmap (Name.convert :: Name -> HQ.HashQualified Name) . snd) results)
      pair :: (Reference, Maybe Name) -> (Pretty, Pretty)
      pair (reference, maybeName) =
        ( case maybeName of
            Nothing -> ""
            Just name -> prettyName name,
          prettyShortHash (SH.take hqLength (Reference.toShortHash reference))
        )

  -- this definition is identical to the previous one, apart from the word
  -- "Dependencies", but undecided about whether or how to refactor
  ListDependencies hqLength ld names missing ->
    pure $
      if names == mempty && missing == mempty
        then c (prettyLabeledDependency hqLength ld) <> " doesn't have any dependencies."
        else
          "Dependencies of "
            <> c (prettyLabeledDependency hqLength ld)
            <> ":\n\n"
            <> (P.indentN 2 (P.numberedColumn2Header num pairs))
    where
      num n = P.hiBlack $ P.shown n <> "."
      header = (P.hiBlack "Reference", P.hiBlack "Name")
      pairs =
        header
          : ( fmap (first c . second c) $
                [(p $ Reference.toShortHash r, prettyName n) | (n, r) <- names]
                  ++ [(p $ Reference.toShortHash r, "(no name available)") | r <- toList missing]
            )
      p = prettyShortHash . SH.take hqLength
      c = P.syntaxToColor
  ListNamespaceDependencies _ppe _path Empty -> pure $ "This namespace has no external dependencies."
  ListNamespaceDependencies ppe path' externalDependencies -> do
    let spacer = ("", "")
    pure . P.column2Header (P.hiBlack "External dependency") ("Dependents in " <> prettyAbsolute path') $
      List.intersperse spacer (externalDepsTable externalDependencies)
    where
      externalDepsTable :: Map LabeledDependency (Set Name) -> [(P.Pretty P.ColorText, P.Pretty P.ColorText)]
      externalDepsTable = ifoldMap $ \ld dependents ->
        [(prettyLD ld, prettyDependents dependents)]
      prettyLD :: LabeledDependency -> P.Pretty P.ColorText
      prettyLD =
        P.syntaxToColor
          . prettyHashQualified
          . LD.fold
            (PPE.typeName ppe)
            (PPE.termName ppe)
      prettyDependents :: Set Name -> P.Pretty P.ColorText
      prettyDependents refs =
        refs
          & Set.toList
          & fmap prettyName
          & P.lines
  DumpUnisonFileHashes hqLength datas effects terms ->
    pure . P.syntaxToColor . P.lines $
      ( effects <&> \(n, r) ->
          "ability "
            <> prettyHashQualified' (HQ'.take hqLength . HQ'.fromNamedReference n $ Reference.DerivedId r)
      )
        <> ( datas <&> \(n, r) ->
               "type "
                 <> prettyHashQualified' (HQ'.take hqLength . HQ'.fromNamedReference n $ Reference.DerivedId r)
           )
        <> ( terms <&> \(n, r) ->
               prettyHashQualified' (HQ'.take hqLength . HQ'.fromNamedReference n $ Reference.DerivedId r)
           )
  RefusedToPush pushBehavior path ->
    (pure . P.warnCallout) case pushBehavior of
      PushBehavior.ForcePush -> error "impossible: refused to push due to ForcePush?"
      PushBehavior.RequireEmpty -> expectedEmptyPushDest path
      PushBehavior.RequireNonEmpty -> expectedNonEmptyPushDest path
  GistCreated remoteNamespace ->
    pure $
      P.lines
        [ "Gist created. Pull via:",
          "",
          P.indentN 2 (IP.patternName IP.pull <> " " <> prettyReadRemoteNamespaceWith absurd remoteNamespace)
        ]
  InitiateAuthFlow authURI -> do
    pure $
      P.wrap $
        "Please navigate to " <> prettyURI authURI <> " to authorize UCM with the codebase server."
  UnknownCodeServer codeServerName -> do
    pure $
      P.lines
        [ P.wrap $ "No host configured for code server " <> P.red (P.text codeServerName) <> ".",
          "You can configure code server hosts in your .unisonConfig file."
        ]
  CredentialFailureMsg err -> pure $ case err of
    Auth.ReauthRequired host ->
      P.lines
        [ "Authentication for host " <> P.red (P.shown host) <> " is required.",
          "Run "
            <> IP.makeExample IP.help [IP.patternName IP.authLogin]
            <> " to learn how."
        ]
    Auth.CredentialParseFailure fp txt ->
      P.lines
        [ "Failed to parse the credentials file at " <> prettyFilePath fp <> ", with error: " <> P.text txt <> ".",
          "You can attempt to fix the issue, or may simply delete the credentials file and run " <> IP.makeExample IP.authLogin [] <> "."
        ]
    Auth.InvalidDiscoveryDocument uri txt ->
      P.lines
        [ "Failed to parse the discover document from " <> prettyURI uri <> ", with error: " <> P.text txt <> "."
        ]
    Auth.InvalidJWT txt ->
      P.lines
        [ "Failed to validate JWT from authentication server: " <> P.text txt
        ]
    Auth.RefreshFailure txt ->
      P.lines
        [ "Failed to refresh access token with authentication server: " <> P.text txt
        ]
    Auth.InvalidTokenResponse uri txt ->
      P.lines
        [ "Failed to parse token response from authentication server: " <> prettyURI uri,
          "The error was: " <> P.text txt
        ]
    Auth.InvalidHost host ->
      P.lines
        [ "Failed to parse a URI from the hostname: " <> P.shown host <> ".",
          "Host names should NOT include a schema or path."
        ]
    Auth.FailedToFetchUserInfo userInfoEndpoint err ->
      P.lines
        [ "Failed to parse the response from user info endpoint: " <> P.shown userInfoEndpoint,
          P.text err,
          "Please `auth.login` then try again, if this error persists please file a bug report and include the above error message."
        ]
  PrintVersion ucmVersion -> pure (P.text ucmVersion)
  ShareError shareError -> pure (prettyShareError shareError)
  ViewOnShare shareRef ->
    pure $
      "View it on Unison Share: " <> case shareRef of
        Left repoPath -> prettyShareLink repoPath
        Right branchInfo -> prettyRemoteBranchInfo branchInfo
  IntegrityCheck result -> pure $ case result of
    NoIntegrityErrors -> "🎉 No issues detected 🎉"
    IntegrityErrorDetected ns -> prettyPrintIntegrityErrors ns
  DisplayDebugNameDiff NameChanges {termNameAdds, termNameRemovals, typeNameAdds, typeNameRemovals} -> do
    let referentText =
          -- We don't use the constructor type in the actual output here, so there's no
          -- point in looking up the correct one.
          P.text . Referent.toText . runIdentity . Cv.referent2to1 (\_ref -> Identity CT.Data)
    let referenceText = P.text . Reference.toText . Cv.reference2to1
    pure $
      P.columnNHeader
        ["Kind", "Name", "Change", "Ref"]
        ( (termNameAdds <&> \(n, ref) -> ["Term", prettyName n, "Added", referentText ref])
            <> (termNameRemovals <&> \(n, ref) -> ["Term", prettyName n, "Removed", referentText ref])
            <> (typeNameAdds <&> \(n, ref) -> ["Type", prettyName n, "Added", referenceText ref])
            <> (typeNameRemovals <&> \(n, ref) -> ["Type", prettyName n, "Removed", referenceText ref])
        )
  DisplayDebugCompletions completions ->
    pure $
      P.column2
        ( completions <&> \comp ->
            let isCompleteTxt =
                  if Completion.isFinished comp
                    then "*"
                    else ""
             in (isCompleteTxt, P.string (Completion.replacement comp))
        )
  ClearScreen -> do
    ANSI.clearScreen
    ANSI.setCursorPosition 0 0
    pure mempty
  PulledEmptyBranch remote ->
    pure . P.warnCallout . P.wrap $
      P.group (prettyReadRemoteNamespace remote) <> "has some history, but is currently empty."
  CreatedProject projectName branchName ->
    pure $
      P.wrap
        ( "I just created project"
            <> prettyProjectName projectName
            <> "with branch"
            <> prettyProjectBranchName branchName
        )
        <> "."
  CreatedProjectBranch from projectAndBranch ->
    case from of
      CreatedProjectBranchFrom'LooseCode path ->
        pure $
          P.wrap
            ( "Done. I've created the"
                <> prettyProjectAndBranchName projectAndBranch
                <> "branch from the namespace"
                <> prettyAbsolute path
            )
            <> "."
      CreatedProjectBranchFrom'Nothingness ->
        pure $
          P.wrap ("Done. I've created an empty branch" <> prettyProjectAndBranchName projectAndBranch)
            <> "."
            <> P.newline
            <> P.newline
            <> tip
              ( "Use"
                  <> IP.makeExample IP.mergeLocal [prettySlashProjectBranchName (UnsafeProjectBranchName "somebranch")]
                  <> "or"
                  <> IP.makeExample
                    IP.mergeLocal
                    [prettyAbsolute (Path.Absolute (Path.fromList ["path", "to", "code"]))]
                  <> "to initialize this branch."
              )
      CreatedProjectBranchFrom'OtherBranch (ProjectAndBranch otherProject otherBranch) ->
        pure $
          P.wrap
            ( "Done. I've created the"
                <> prettyProjectAndBranchName projectAndBranch
                <> "branch based off"
                <> prettyProjectAndBranchName (ProjectAndBranch (otherProject ^. #name) (otherBranch ^. #name))
            )
            <> "."
      CreatedProjectBranchFrom'ParentBranch parentBranch ->
        pure $
          P.wrap
            ( "Done. I've created the"
                <> prettyProjectBranchName (projectAndBranch ^. #branch)
                <> "branch based off of"
                <> prettyProjectBranchName parentBranch
            )
            <> "."
            <> P.newline
            <> P.newline
            <> tip
              ( "Use"
                  <> IP.makeExample
                    IP.mergeLocal
                    [ prettySlashProjectBranchName (projectAndBranch ^. #branch),
                      prettySlashProjectBranchName parentBranch
                    ]
                  <> "to merge your work back into the"
                  <> prettyProjectBranchName parentBranch
                  <> "branch."
              )
  CreatedRemoteProject host (ProjectAndBranch projectName _) ->
    pure . P.wrap $
      "I just created"
        <> prettyProjectName projectName
        <> "on"
        <> prettyURI host
  CreatedRemoteProjectBranch host projectAndBranch ->
    pure . P.wrap $
      "I just created" <> prettyProjectAndBranchName projectAndBranch <> "on" <> prettyURI host
  RemoteProjectBranchIsUpToDate host projectAndBranch ->
    pure (P.wrap (prettyProjectAndBranchName projectAndBranch <> "on" <> prettyURI host <> "is already up-to-date."))
  InvalidProjectName name -> pure (P.wrap (P.text name <> "is not a valid project name."))
  InvalidProjectBranchName name -> pure (P.wrap (P.text name <> "is not a valid branch name."))
  ProjectNameAlreadyExists name ->
    pure . P.wrap $
      "Project" <> prettyProjectName name <> "already exists."
  ProjectNameRequiresUserSlug name ->
    pure . P.wrap $
      prettyProjectName name
        <> "requires a username, as in"
        <> prettyProjectName (unsafeFrom @Text "@unison/base")
  ProjectAndBranchNameAlreadyExists projectAndBranch ->
    pure . P.wrap $
      prettyProjectAndBranchName projectAndBranch <> "already exists."
  NotOnProjectBranch -> pure (P.wrap "You are not currently on a branch.")
  NoAssociatedRemoteProject host projectAndBranch ->
    pure . P.wrap $
      prettyProjectAndBranchName projectAndBranch <> "isn't associated with any project on" <> prettyURI host
  NoAssociatedRemoteProjectBranch host projectAndBranch ->
    pure . P.wrap $
      prettyProjectAndBranchName projectAndBranch <> "isn't associated with any branch on" <> prettyURI host
  LocalProjectBranchDoesntExist projectAndBranch ->
    pure . P.wrap $
      prettyProjectAndBranchName projectAndBranch <> "does not exist."
  LocalProjectNorProjectBranchExist project branch ->
    pure . P.wrap $
      "Neither project"
        <> prettyProjectName project
        <> "nor branch"
        <> prettySlashProjectBranchName branch
        <> "exists."
  RemoteProjectDoesntExist host project ->
    pure . P.wrap $
      prettyProjectName project <> "does not exist on" <> prettyURI host
  RemoteProjectBranchDoesntExist host projectAndBranch ->
    pure . P.wrap $
      prettyProjectAndBranchName projectAndBranch <> "does not exist on" <> prettyURI host
  RemoteProjectBranchHeadMismatch host projectAndBranch ->
    pure . P.wrap $
      prettyProjectAndBranchName projectAndBranch
        <> "on"
        <> prettyURI host
        <> "has some history that I don't know about."
  RemoteProjectPublishedReleaseCannotBeChanged host projectAndBranch ->
    pure . P.wrap $
      "The release"
        <> prettyProjectAndBranchName projectAndBranch
        <> "on"
        <> prettyURI host
        <> "has already been published and cannot be changed."
        <> "Consider making a new release instead."
  RemoteProjectReleaseIsDeprecated host projectAndBranch ->
    pure . P.wrap $
      "The release"
        <> prettyProjectAndBranchName projectAndBranch
        <> "on"
        <> prettyURI host
        <> "has been deprecated."
  Unauthorized message ->
    pure . P.wrap $
      P.text ("Unauthorized: " <> message)
  ServantClientError err ->
    pure case err of
      Servant.ConnectionError _exception -> P.wrap "Something went wrong with the connection. Try again?"
      Servant.DecodeFailure message response ->
        P.wrap "Huh, I failed to decode a response from the server."
          <> P.newline
          <> P.newline
          <> P.indentN 2 (P.text message)
          <> P.newline
          <> P.newline
          <> P.wrap "Here is the full response."
          <> P.newline
          <> P.newline
          <> P.indentN 2 (P.pshown response)
      Servant.FailureResponse request response ->
        P.wrap "Oops, I received an unexpected status code from the server."
          <> P.newline
          <> P.newline
          <> P.wrap "Here is the request."
          <> P.newline
          <> P.newline
          <> P.indentN 2 (P.pshown request)
          <> P.newline
          <> P.newline
          <> P.wrap "Here is the full response."
          <> P.newline
          <> P.newline
          <> P.indentN 2 (P.pshown response)
      Servant.InvalidContentTypeHeader response -> wrongContentType response
      Servant.UnsupportedContentType _mediaType response -> wrongContentType response
    where
      wrongContentType response =
        P.wrap "Huh, the server sent me the wrong content type."
          <> P.newline
          <> P.newline
          <> P.wrap "Here is the full response."
          <> P.newline
          <> P.newline
          <> P.indentN 2 (P.pshown response)
  MarkdownOut md -> pure $ P.text md
  DownloadedEntities n -> pure (P.wrap ("Downloaded" <> P.num n <> "entities."))
  UploadedEntities n -> pure (P.wrap ("Uploaded" <> P.num n <> "entities."))
  NotImplementedYet message -> pure (P.wrap ("Not implemented:" <> P.text message))
  DraftingRelease branch ver ->
    pure $
      P.wrap ("😎 Great! I've created a draft release for you at " <> prettySlashProjectBranchName branch)
        <> "."
        <> P.newline
        <> P.newline
        <> P.wrap
          ( "You can create a"
              <> P.group (P.backticked "ReleaseNotes : Doc")
              <> "in this branch to give an overview of the release."
              <> "It'll automatically show up on Unison Share when you publish."
          )
        <> P.newline
        <> P.newline
        <> P.wrap
          ( "When ready to release"
              <> prettySemver ver
              <> "to the world,"
              <> IP.makeExample' IP.push
              <> "the release to Unison Share, navigate to the release, and click \"Publish\"."
          )
        <> P.newline
        <> P.newline
        <> tip
          ( "if you get pulled away from drafting your release, you can always get back to it with "
              <> IP.makeExample IP.projectSwitch [prettySlashProjectBranchName branch]
          )
        <> "."
  CannotCreateReleaseBranchWithBranchCommand branch ver ->
    pure $
      P.wrap ("Branch names like" <> prettyProjectBranchName branch <> "are reserved for releases.")
        <> P.newline
        <> P.newline
        <> tip
          ( "to download an existing release, try "
              <> IP.makeExample IP.branchClone [prettySlashProjectBranchName branch]
          )
        <> "."
        <> P.newline
        <> P.newline
        <> tip ("to draft a new release, try " <> IP.makeExample IP.releaseDraft [prettySemver ver])
        <> "."
  where
    _nameChange _cmd _pastTenseCmd _oldName _newName _r = error "todo"

expectedEmptyPushDest :: WriteRemoteNamespace Void -> Pretty
expectedEmptyPushDest namespace =
  P.lines
    [ "The remote namespace " <> prettyWriteRemoteNamespace (absurd <$> namespace) <> " is not empty.",
      "",
      "Did you mean to use " <> IP.makeExample' IP.push <> " instead?"
    ]

expectedNonEmptyPushDest :: WriteRemoteNamespace Void -> Pretty
expectedNonEmptyPushDest namespace =
  P.lines
    [ P.wrap ("The remote namespace " <> prettyWriteRemoteNamespace (absurd <$> namespace) <> " is empty."),
      "",
      P.wrap ("Did you mean to use " <> IP.makeExample' IP.pushCreate <> " instead?")
    ]

prettyShareError :: ShareError -> Pretty
prettyShareError =
  P.fatalCallout . \case
    ShareErrorCheckAndSetPush err -> prettyCheckAndSetPushError err
    ShareErrorDownloadEntities err -> prettyDownloadEntitiesError err
    ShareErrorFastForwardPush err -> prettyFastForwardPushError err
    ShareErrorGetCausalHashByPath err -> prettyGetCausalHashByPathError err
    ShareErrorPull err -> prettyPullError err
    ShareErrorTransport err -> prettyTransportError err
    ShareErrorUploadEntities err -> prettyUploadEntitiesError err

prettyCheckAndSetPushError :: Share.CheckAndSetPushError -> Pretty
prettyCheckAndSetPushError = \case
  Share.CheckAndSetPushError'UpdatePath repoInfo err -> prettyUpdatePathError repoInfo err
  Share.CheckAndSetPushError'UploadEntities err -> prettyUploadEntitiesError err

prettyDownloadEntitiesError :: Share.DownloadEntitiesError -> Pretty
prettyDownloadEntitiesError = \case
  Share.DownloadEntitiesNoReadPermission repoInfo -> noReadPermissionForRepo repoInfo
  Share.DownloadEntitiesInvalidRepoInfo err repoInfo -> invalidRepoInfo err repoInfo
  Share.DownloadEntitiesUserNotFound userHandle -> shareUserNotFound (Share.RepoInfo userHandle)
  Share.DownloadEntitiesProjectNotFound project -> shareProjectNotFound project

prettyFastForwardPathError :: Share.Path -> Share.FastForwardPathError -> Pretty
prettyFastForwardPathError path = \case
  Share.FastForwardPathError'InvalidParentage Share.InvalidParentage {child, parent} ->
    P.lines
      [ "The server detected an error in the history being pushed, please report this as a bug in ucm.",
        "The history in question is the hash: " <> prettyHash32 child <> " with the ancestor: " <> prettyHash32 parent
      ]
  Share.FastForwardPathError'InvalidRepoInfo err repoInfo -> invalidRepoInfo err repoInfo
  Share.FastForwardPathError'MissingDependencies dependencies -> needDependencies dependencies
  Share.FastForwardPathError'NoHistory -> expectedNonEmptyPushDest (sharePathToWriteRemotePathShare path)
  Share.FastForwardPathError'NoWritePermission path -> noWritePermissionForPath path
  Share.FastForwardPathError'NotFastForward _hashJwt -> notFastForward path
  Share.FastForwardPathError'UserNotFound -> shareUserNotFound (Share.pathRepoInfo path)

prettyFastForwardPushError :: Share.FastForwardPushError -> Pretty
prettyFastForwardPushError = \case
  Share.FastForwardPushError'FastForwardPath path err -> prettyFastForwardPathError path err
  Share.FastForwardPushError'GetCausalHash err -> prettyGetCausalHashByPathError err
  Share.FastForwardPushError'NotFastForward path -> notFastForward path
  Share.FastForwardPushError'UploadEntities err -> prettyUploadEntitiesError err

prettyGetCausalHashByPathError :: Share.GetCausalHashByPathError -> Pretty
prettyGetCausalHashByPathError = \case
  Share.GetCausalHashByPathErrorNoReadPermission sharePath -> noReadPermissionForPath sharePath
  Share.GetCausalHashByPathErrorInvalidRepoInfo err repoInfo -> invalidRepoInfo err repoInfo
  Share.GetCausalHashByPathErrorUserNotFound repoInfo -> shareUserNotFound repoInfo

prettyPullError :: Share.PullError -> Pretty
prettyPullError = \case
  Share.PullError'DownloadEntities err -> prettyDownloadEntitiesError err
  Share.PullError'GetCausalHash err -> prettyGetCausalHashByPathError err
  Share.PullError'NoHistoryAtPath sharePath ->
    P.wrap $ P.text "The server didn't find anything at" <> prettySharePath sharePath

prettyUpdatePathError :: Share.RepoInfo -> Share.UpdatePathError -> Pretty
prettyUpdatePathError repoInfo = \case
  Share.UpdatePathError'HashMismatch Share.HashMismatch {path = sharePath, expectedHash, actualHash} ->
    case (expectedHash, actualHash) of
      (Nothing, Just _) -> expectedEmptyPushDest (sharePathToWriteRemotePathShare sharePath)
      _ ->
        P.wrap $
          P.text "It looks like someone modified"
            <> prettySharePath sharePath
            <> P.text "an instant before you. Pull and try again? 🤞"
  Share.UpdatePathError'InvalidRepoInfo err repoInfo -> invalidRepoInfo err repoInfo
  Share.UpdatePathError'MissingDependencies dependencies -> needDependencies dependencies
  Share.UpdatePathError'NoWritePermission path -> noWritePermissionForPath path
  Share.UpdatePathError'UserNotFound -> shareUserNotFound repoInfo

prettyUploadEntitiesError :: Share.UploadEntitiesError -> Pretty
prettyUploadEntitiesError = \case
  Share.UploadEntitiesError'HashMismatchForEntity _hashMismatch -> error "TODO: hash mismatch error message"
  Share.UploadEntitiesError'InvalidRepoInfo err repoInfo -> invalidRepoInfo err repoInfo
  Share.UploadEntitiesError'NeedDependencies dependencies -> needDependencies dependencies
  Share.UploadEntitiesError'NoWritePermission repoInfo -> noWritePermissionForRepo repoInfo
  Share.UploadEntitiesError'ProjectNotFound project -> shareProjectNotFound project
  Share.UploadEntitiesError'UserNotFound userHandle -> shareUserNotFound (Share.RepoInfo userHandle)

prettyTransportError :: CodeserverTransportError -> Pretty
prettyTransportError = \case
  DecodeFailure msg resp ->
    (P.lines . catMaybes)
      [ Just ("The server sent a response that we couldn't decode: " <> P.text msg),
        responseRequestId resp <&> \responseId -> P.newline <> "Request ID: " <> P.blue (P.text responseId)
      ]
  Unauthenticated codeServerURL ->
    P.wrap . P.lines $
      [ "Authentication with this code server (" <> P.string (Servant.showBaseUrl codeServerURL) <> ") is missing or expired.",
        "Please run " <> makeExample' IP.authLogin <> "."
      ]
  PermissionDenied msg -> P.hang "Permission denied:" (P.text msg)
  UnreachableCodeserver codeServerURL ->
    P.lines $
      [ P.wrap $ "Unable to reach the code server hosted at:" <> P.string (Servant.showBaseUrl codeServerURL),
        "",
        P.wrap "Please check your network, ensure you've provided the correct location, or try again later."
      ]
  RateLimitExceeded -> "Rate limit exceeded, please try again later."
  Timeout -> "The code server timed-out when responding to your request. Please try again later or report an issue if the problem persists."
  UnexpectedResponse resp ->
    (P.lines . catMaybes)
      [ Just
          ( "The server sent a "
              <> P.red (P.shown (Http.statusCode (Servant.responseStatusCode resp)))
              <> " that we didn't expect."
          ),
        let body = Text.decodeUtf8 (LazyByteString.toStrict (Servant.responseBody resp))
         in if Text.null body then Nothing else Just (P.newline <> "Response body: " <> P.text body),
        responseRequestId resp <&> \responseId -> P.newline <> "Request ID: " <> P.blue (P.text responseId)
      ]
  where
    -- Dig the request id out of a response header.
    responseRequestId :: Servant.Response -> Maybe Text
    responseRequestId =
      fmap Text.decodeUtf8 . List.lookup "X-RequestId" . Foldable.toList @Seq . Servant.responseHeaders

invalidRepoInfo :: Text -> Share.RepoInfo -> Pretty
invalidRepoInfo err repoInfo =
  P.lines
    [ P.wrap $
        "The server doesn't recognize the codebase path UCM provided. This is probably a bug in UCM.",
      P.text "",
      P.text "The invalid path is:\n"
        <> P.indentN 2 (P.text (Share.unRepoInfo repoInfo)),
      P.text err
    ]

pushPublicNote :: InputPattern -> Text -> [Text] -> Pretty
pushPublicNote cmd uname ys =
  let msg =
        mconcat
          [ "Unison Share currently only supports sharing public code. ",
            "This is done by hosting code in a public namespace under your handle.",
            "It looks like you were trying to push directly to the" <> P.backticked (P.text uname),
            "handle. Try nesting under `public` like so: "
          ]
      pushCommand = IP.makeExampleNoBackticks cmd [prettySharePath exPath]
      exPath = Share.Path (uname NEList.:| "public" : ys)
   in P.lines
        [ P.wrap msg,
          "",
          P.indentN 4 pushCommand
        ]

needDependencies :: Share.NeedDependencies Hash32 -> Pretty
needDependencies (Share.NeedDependencies hashes) =
  -- maybe todo: stuff in all the args to CheckAndSetPush
  P.lines
    [ P.wrap
        ( P.text "The server was expecting to have received some stuff from UCM during that last command, but claims to have not received it."
            <> P.text "(This is probably a bug in UCM.)"
        ),
      P.text "",
      P.text "The hashes it expected are:\n"
        <> P.indentN 2 (P.lines (map prettyHash32 (toList hashes)))
    ]

noReadPermissionForPath :: Share.Path -> Pretty
noReadPermissionForPath sharePath =
  P.wrap $ P.text "The server said you don't have permission to read" <> P.group (prettySharePath sharePath <> ".")

noReadPermissionForRepo :: Share.RepoInfo -> Pretty
noReadPermissionForRepo repoInfo =
  P.wrap $ P.text "The server said you don't have permission to read" <> P.group (prettyRepoInfo repoInfo <> ".")

noWritePermissionForPath :: Share.Path -> Pretty
noWritePermissionForPath sharePath =
  case Share.pathSegments sharePath of
    _ NEList.:| "public" : _ -> P.wrap $ P.text "The server said you don't have permission to write" <> P.group (prettySharePath sharePath <> ".")
    uname NEList.:| ys -> pushPublicNote IP.pushCreate uname ys

noWritePermissionForRepo :: Share.RepoInfo -> Pretty
noWritePermissionForRepo repoInfo =
  P.wrap $ P.text "The server said you don't have permission to write" <> P.group (prettyRepoInfo repoInfo <> ".")

notFastForward :: Share.Path -> Pretty
notFastForward path =
  P.lines $
    [ P.wrap $
        "There are some changes at" <> prettySharePath path <> "that aren't in the history you pushed.",
      "",
      P.wrap $
        "If you're sure you got the right paths, try"
          <> pull
          <> "to merge these changes locally, then"
          <> push
          <> "again."
    ]
  where
    push = P.group . P.backticked . IP.patternName $ IP.push
    pull = P.group . P.backticked . IP.patternName $ IP.pull

shareProjectNotFound :: Text -> Pretty
shareProjectNotFound projectShortHand =
  P.lines
    [ P.wrap $
        "This project does not exist: " <> P.text projectShortHand
    ]

shareUserNotFound :: Share.RepoInfo -> Pretty
shareUserNotFound repoInfo =
  P.wrap ("User" <> prettyRepoInfo repoInfo <> "does not exist.")

sharePathToWriteRemotePathShare :: Share.Path -> WriteRemoteNamespace void
sharePathToWriteRemotePathShare sharePath =
  -- Recover the original WriteRemotePath from the information in the error, which is thrown from generic share
  -- client code that doesn't know about WriteRemotePath
  WriteRemoteNamespaceShare
    WriteShareRemoteNamespace
      { server = RemoteRepo.DefaultCodeserver,
        repo = ShareUserHandle $ Share.unRepoInfo (Share.pathRepoInfo sharePath),
        path = Path.fromList (coerce @[Text] @[NameSegment] (Share.pathCodebasePath sharePath))
      }

shareOrigin :: Text
shareOrigin = "https://share.unison-lang.org"

prettyRepoInfo :: Share.RepoInfo -> Pretty
prettyRepoInfo (Share.RepoInfo repoInfo) =
  P.blue (P.text repoInfo)

prettyShareLink :: WriteShareRemoteNamespace -> Pretty
prettyShareLink WriteShareRemoteNamespace {repo, path} =
  let encodedPath =
        Path.toList path
          & fmap (URI.encodeText . NameSegment.toText)
          & Text.intercalate "/"
   in P.green . P.text $ shareOrigin <> "/@" <> shareUserHandleToText repo <> "/p/code/latest/namespaces/" <> encodedPath

prettySharePath :: Share.Path -> Pretty
prettySharePath =
  prettyRelative
    . Path.Relative
    . Path.fromList
    . coerce @[Text] @[NameSegment]
    . toList
    . Share.pathSegments

prettyFilePath :: FilePath -> Pretty
prettyFilePath fp =
  P.blue (P.string fp)

prettyPath' :: Path.Path' -> Pretty
prettyPath' p' =
  if Path.isCurrentPath p'
    then "the current namespace"
    else P.blue (P.shown p')

prettyPullTarget :: Input.PullTarget (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch) -> Pretty
prettyPullTarget = \case
  Input.PullTargetLooseCode path -> prettyPath' path
  Input.PullTargetProject (ProjectAndBranch project branch) ->
    prettyProjectAndBranchName (ProjectAndBranch (project ^. #name) (branch ^. #name))

prettyBranchId :: Input.AbsBranchId -> Pretty
prettyBranchId = \case
  Left sch -> prettySCH sch
  Right absPath -> prettyAbsolute $ absPath

prettyRelative :: Path.Relative -> Pretty
prettyRelative = P.blue . P.shown

prettyAbsolute :: Path.Absolute -> Pretty
prettyAbsolute = P.blue . P.shown

prettySCH :: (IsString s) => ShortCausalHash -> P.Pretty s
prettySCH hash = P.group $ "#" <> P.text (SCH.toText hash)

prettyCausalHash :: (IsString s) => CausalHash -> P.Pretty s
prettyCausalHash hash = P.group $ "#" <> P.text (Hash.toBase32HexText . unCausalHash $ hash)

prettyBase32Hex :: (IsString s) => Base32Hex -> P.Pretty s
prettyBase32Hex = P.text . Base32Hex.toText

prettyBase32Hex# :: (IsString s) => Base32Hex -> P.Pretty s
prettyBase32Hex# b = P.group $ "#" <> prettyBase32Hex b

prettyHash :: (IsString s) => Hash.Hash -> P.Pretty s
prettyHash = prettyBase32Hex# . Hash.toBase32Hex

prettyHash32 :: (IsString s) => Hash32 -> P.Pretty s
prettyHash32 = prettyBase32Hex# . Hash32.toBase32Hex

prettyProjectName :: ProjectName -> Pretty
prettyProjectName =
  P.green . P.text . into @Text

-- | 'prettyProjectName' with a trailing slash.
prettyProjectNameSlash :: ProjectName -> Pretty
prettyProjectNameSlash =
  P.blue . P.text . (`Text.snoc` '/') . into @Text

prettyProjectBranchName :: ProjectBranchName -> Pretty
prettyProjectBranchName =
  P.blue . P.text . into @Text

prettySemver :: Semver -> Pretty
prettySemver (Semver x y z) =
  P.group (P.num x <> "." <> P.num y <> "." <> P.num z)

-- | Like 'prettyProjectBranchName', but with a leading forward slash. This is used in some outputs to
-- encourage/advertise an unambiguous syntax for project branches, as there's an ambiguity with single-segment relative
-- paths.
--
-- Not all project branches are printed such: for example, when listing all branches of a project, we probably don't
-- need or want to prefix every one with a forward slash.
prettySlashProjectBranchName :: ProjectBranchName -> Pretty
prettySlashProjectBranchName branch =
  P.group (P.hiBlack "/" <> prettyProjectBranchName branch)

prettyProjectAndBranchName :: ProjectAndBranch ProjectName ProjectBranchName -> Pretty
prettyProjectAndBranchName (ProjectAndBranch project branch) =
  P.group (prettyProjectName project <> P.hiBlack "/" <> prettyProjectBranchName branch)

prettyPathOrProjectAndBranchName :: Either Path.Path' (ProjectAndBranch ProjectName ProjectBranchName) -> Pretty
prettyPathOrProjectAndBranchName = \case
  Left x -> prettyPath' x
  Right x -> prettyProjectAndBranchName x

formatMissingStuff ::
  (Show tm, Show typ) =>
  [(HQ.HashQualified Name, tm)] ->
  [(HQ.HashQualified Name, typ)] ->
  Pretty
formatMissingStuff terms types =
  ( Monoid.unlessM (null terms) . P.fatalCallout $
      P.wrap "The following terms have a missing or corrupted type signature:"
        <> "\n\n"
        <> P.column2 [(P.syntaxToColor $ prettyHashQualified name, fromString (show ref)) | (name, ref) <- terms]
  )
    <> ( Monoid.unlessM (null types) . P.fatalCallout $
           P.wrap "The following types weren't found in the codebase:"
             <> "\n\n"
             <> P.column2 [(P.syntaxToColor $ prettyHashQualified name, fromString (show ref)) | (name, ref) <- types]
       )

displayDefinitions' ::
  (Var v) =>
  (Ord a1) =>
  PPED.PrettyPrintEnvDecl ->
  Map Reference.Reference (DisplayObject () (DD.Decl v a1)) ->
  Map Reference.Reference (DisplayObject (Type v a1) (Term v a1)) ->
  Pretty
displayDefinitions' ppe0 types terms = P.syntaxToColor $ P.sep "\n\n" (prettyTypes <> prettyTerms)
  where
    ppeBody r = PPE.declarationPPE ppe0 r
    ppeDecl = PPED.unsuffixifiedPPE ppe0
    prettyTerms =
      map go . Map.toList
      -- sort by name
      $
        Map.mapKeys (first (PPE.termName ppeDecl . Referent.Ref) . dupe) terms
    prettyTypes =
      map go2 . Map.toList $
        Map.mapKeys (first (PPE.typeName ppeDecl) . dupe) types
    go ((n, r), dt) =
      case dt of
        MissingObject r -> missing n r
        BuiltinObject typ ->
          P.hang
            ("builtin " <> prettyHashQualified n <> " :")
            (TypePrinter.prettySyntax (ppeBody r) typ)
        UserObject tm -> TermPrinter.prettyBinding (ppeBody r) n tm
    go2 ((n, r), dt) =
      case dt of
        MissingObject r -> missing n r
        BuiltinObject _ -> builtin n
        UserObject decl -> DeclPrinter.prettyDecl (PPE.declarationPPEDecl ppe0 r) r n decl
    builtin n = P.wrap $ "--" <> prettyHashQualified n <> " is built-in."
    missing n r =
      P.wrap
        ( "-- The name "
            <> prettyHashQualified n
            <> " is assigned to the "
            <> "reference "
            <> fromString (show r ++ ",")
            <> "which is missing from the codebase."
        )
        <> P.newline
        <> tip "You might need to repair the codebase manually."

displayRendered :: Maybe FilePath -> Pretty -> IO Pretty
displayRendered outputLoc pp =
  maybe (pure pp) scratchAndDisplay outputLoc
  where
    scratchAndDisplay path = do
      path' <- canonicalizePath path
      prependToFile pp path'
      pure (message pp path')
      where
        prependToFile pp path = do
          existingContents <- do
            exists <- doesFileExist path
            if exists
              then readUtf8 path
              else pure ""
          writeUtf8 path . Text.pack . P.toPlain 80 $
            P.lines [pp, "", P.text existingContents]
        message pp path =
          P.callout "☝️" $
            P.lines
              [ P.wrap $ "I added this to the top of " <> fromString path,
                "",
                P.indentN 2 pp
              ]

displayDefinitions :: DisplayDefinitionsOutput -> IO Pretty
displayDefinitions DisplayDefinitionsOutput {isTest, outputFile, prettyPrintEnv = ppe, terms, types} =
  if Map.null types && Map.null terms
    then pure $ P.callout "😶" "No results to display."
    else maybe displayOnly scratchAndDisplay outputFile
  where
    ppeDecl = PPED.unsuffixifiedPPE ppe
    displayOnly = pure (code (const False))
    scratchAndDisplay path = do
      path' <- canonicalizePath path
      prependToFile (code isTest) path'
      pure (message (code (const False)) path')
      where
        prependToFile code path = do
          existingContents <- do
            exists <- doesFileExist path
            if exists
              then readUtf8 path
              else pure ""
          writeUtf8 path . Text.pack . P.toPlain 80 $
            P.lines
              [ code,
                "",
                "---- " <> "Anything below this line is ignored by Unison.",
                "",
                P.text existingContents
              ]
        message code path =
          P.callout "☝️" $
            P.lines
              [ P.wrap $ "I added these definitions to the top of " <> fromString path,
                "",
                P.indentN 2 code,
                "",
                P.wrap $
                  "You can edit them there, then do"
                    <> makeExample' IP.update
                    <> "to replace the definitions currently in this namespace."
              ]

    code :: (TermReference -> Bool) -> Pretty
    code isTest =
      P.syntaxToColor $ P.sep "\n\n" (prettyTypes <> prettyTerms isTest)

    prettyTypes :: [P.Pretty SyntaxText]
    prettyTypes =
      types
        & Map.toList
        & map (\(ref, dt) -> (PPE.typeName ppeDecl ref, ref, dt))
        & List.sortBy (\(n0, _, _) (n1, _, _) -> Name.compareAlphabetical n0 n1)
        & map prettyType

    prettyTerms :: (TermReference -> Bool) -> [P.Pretty SyntaxText]
    prettyTerms isTest =
      terms
        & Map.toList
        & map (\(ref, dt) -> (PPE.termName ppeDecl (Referent.Ref ref), ref, dt))
        & List.sortBy (\(n0, _, _) (n1, _, _) -> Name.compareAlphabetical n0 n1)
        & map (\t -> prettyTerm (isTest (t ^. _2)) t)

    prettyTerm ::
      Bool ->
      (HQ.HashQualified Name, Reference, DisplayObject (Type Symbol Ann) (Term Symbol Ann)) ->
      P.Pretty SyntaxText
    prettyTerm isTest (n, r, dt) =
      case dt of
        MissingObject r -> missing n r
        BuiltinObject typ ->
          (if isJust outputFile then P.indent "-- " else id) $
            P.hang
              ("builtin " <> prettyHashQualified n <> " :")
              (TypePrinter.prettySyntax (ppeBody n r) typ)
        UserObject tm ->
          if isTest
            then WK.TestWatch <> "> " <> TermPrinter.prettyBindingWithoutTypeSignature (ppeBody n r) n tm
            else TermPrinter.prettyBinding (ppeBody n r) n tm
      where
        ppeBody n r = PPE.biasTo (maybeToList $ HQ.toName n) $ PPE.declarationPPE ppe r

    prettyType :: (HQ.HashQualified Name, Reference, DisplayObject () (DD.Decl Symbol Ann)) -> P.Pretty SyntaxText
    prettyType (n, r, dt) =
      case dt of
        MissingObject r -> missing n r
        BuiltinObject _ -> builtin n
        UserObject decl -> DeclPrinter.prettyDecl (PPED.biasTo (maybeToList $ HQ.toName n) $ PPE.declarationPPEDecl ppe r) r n decl

    builtin n = P.wrap $ "--" <> prettyHashQualified n <> " is built-in."
    missing n r =
      P.wrap
        ( "-- The name "
            <> prettyHashQualified n
            <> " is assigned to the "
            <> "reference "
            <> fromString (show r ++ ",")
            <> "which is missing from the codebase."
        )
        <> P.newline
        <> tip "You might need to repair the codebase manually."

displayTestResults ::
  Bool -> -- whether to show the tip
  PPE.PrettyPrintEnv ->
  [(Reference, Text)] ->
  [(Reference, Text)] ->
  Pretty
displayTestResults showTip ppe oksUnsorted failsUnsorted =
  let oks = Name.sortByText fst [(name r, msg) | (r, msg) <- oksUnsorted]
      fails = Name.sortByText fst [(name r, msg) | (r, msg) <- failsUnsorted]
      name r = HQ.toText $ PPE.termName ppe (Referent.Ref r)
      okMsg =
        if null oks
          then mempty
          else P.column2 [(P.green "◉ " <> P.text r, "  " <> P.green (P.text msg)) | (r, msg) <- oks]
      okSummary =
        if null oks
          then mempty
          else "✅ " <> P.bold (P.num (length oks)) <> P.green " test(s) passing"
      failMsg =
        if null fails
          then mempty
          else P.column2 [(P.red "✗ " <> P.text r, "  " <> P.red (P.text msg)) | (r, msg) <- fails]
      failSummary =
        if null fails
          then mempty
          else "🚫 " <> P.bold (P.num (length fails)) <> P.red " test(s) failing"
      tipMsg =
        if not showTip || (null oks && null fails)
          then mempty
          else
            tip $
              "Use "
                <> P.blue ("view " <> P.text (fst $ head (fails ++ oks)))
                <> "to view the source of a test."
   in if null oks && null fails
        then "😶 No tests available."
        else
          P.sep "\n\n" . P.nonEmpty $
            [ okMsg,
              failMsg,
              P.sep ", " . P.nonEmpty $ [failSummary, okSummary],
              tipMsg
            ]

unsafePrettyTermResultSig' ::
  (Var v) =>
  PPE.PrettyPrintEnv ->
  SR'.TermResult' v a ->
  Pretty
unsafePrettyTermResultSig' ppe = \case
  SR'.TermResult' name (Just typ) r _aliases ->
    head (TypePrinter.prettySignaturesCT ppe [(r, name, typ)])
  _ -> error "Don't pass Nothing"

-- produces:
-- -- #5v5UtREE1fTiyTsTK2zJ1YNqfiF25SkfUnnji86Lms#0
-- Optional.None, Maybe.Nothing : Maybe a
unsafePrettyTermResultSigFull' ::
  (Var v) =>
  PPE.PrettyPrintEnv ->
  SR'.TermResult' v a ->
  Pretty
unsafePrettyTermResultSigFull' ppe = \case
  SR'.TermResult' hq (Just typ) r aliases ->
    P.lines
      [ P.hiBlack "-- " <> greyHash (HQ.fromReferent r),
        P.group $
          P.commas (fmap greyHash $ hq : map HQ'.toHQ (toList aliases))
            <> " : "
            <> P.syntaxToColor (TypePrinter.prettySyntax ppe typ),
        mempty
      ]
  _ -> error "Don't pass Nothing"
  where
    greyHash = styleHashQualified' id P.hiBlack

prettyTypeResultHeader' :: (Var v) => SR'.TypeResult' v a -> Pretty
prettyTypeResultHeader' (SR'.TypeResult' name dt r _aliases) =
  prettyDeclTriple (name, r, dt)

-- produces:
-- -- #5v5UtREE1fTiyTsTK2zJ1YNqfiF25SkfUnnji86Lms
-- type Optional
-- type Maybe
prettyTypeResultHeaderFull' :: (Var v) => SR'.TypeResult' v a -> Pretty
prettyTypeResultHeaderFull' (SR'.TypeResult' name dt r aliases) =
  P.lines stuff <> P.newline
  where
    stuff =
      (P.hiBlack "-- " <> greyHash (HQ.fromReference r))
        : fmap
          (\name -> prettyDeclTriple (name, r, dt))
          (name : map HQ'.toHQ (toList aliases))
      where
        greyHash = styleHashQualified' id P.hiBlack

prettyDeclTriple ::
  (Var v) =>
  (HQ.HashQualified Name, Reference.Reference, DisplayObject () (DD.Decl v a)) ->
  Pretty
prettyDeclTriple (name, _, displayDecl) = case displayDecl of
  BuiltinObject _ -> P.hiBlack "builtin " <> P.hiBlue "type " <> P.blue (P.syntaxToColor $ prettyHashQualified name)
  MissingObject _ -> mempty -- these need to be handled elsewhere
  UserObject decl -> P.syntaxToColor $ DeclPrinter.prettyDeclHeader name decl

prettyDeclPair ::
  (Var v) =>
  PPE.PrettyPrintEnv ->
  (Reference, DisplayObject () (DD.Decl v a)) ->
  Pretty
prettyDeclPair ppe (r, dt) = prettyDeclTriple (PPE.typeName ppe r, r, dt)

renderNameConflicts :: PPE.PrettyPrintEnv -> Names -> Numbered Pretty
renderNameConflicts ppe conflictedNames = do
  let conflictedTypeNames :: Map Name [HQ.HashQualified Name]
      conflictedTypeNames =
        conflictedNames
          & Names.types
          & R.domain
          & fmap (foldMap (pure @[] . PPE.typeName ppe))
  let conflictedTermNames :: Map Name [HQ.HashQualified Name]
      conflictedTermNames =
        conflictedNames
          & Names.terms
          & R.domain
          & fmap (foldMap (pure @[] . PPE.termName ppe))
  let allConflictedNames :: [Name]
      allConflictedNames = Set.toList (Map.keysSet conflictedTermNames <> Map.keysSet conflictedTypeNames)
  prettyConflictedTypes <- showConflictedNames "type" conflictedTypeNames
  prettyConflictedTerms <- showConflictedNames "term" conflictedTermNames
  pure $
    Monoid.unlessM (null allConflictedNames) $
      P.callout "❓" . P.sep "\n\n" . P.nonEmpty $
        [ prettyConflictedTypes,
          prettyConflictedTerms,
          tip $
            "This occurs when merging branches that both independently introduce the same name."
              <> "Use "
              <> makeExample'
                ( if (not . null) conflictedTypeNames
                    then IP.renameType
                    else IP.renameTerm
                )
              <> " or "
              <> makeExample'
                ( if (not . null) conflictedTypeNames
                    then IP.deleteType
                    else IP.deleteTerm
                )
              <> "to resolve the conflicts."
        ]
  where
    showConflictedNames :: Pretty -> Map Name [HQ.HashQualified Name] -> Numbered Pretty
    showConflictedNames thingKind conflictedNames =
      P.lines <$> do
        for (Map.toList conflictedNames) $ \(name, hashes) -> do
          prettyConflicts <- for hashes \hash -> do
            n <- addNumberedArg (HQ.toString hash)
            pure $ formatNum n <> (P.blue . P.syntaxToColor . prettyHashQualified $ hash)
          pure . P.wrap $
            ( "The "
                <> thingKind
                <> " "
                <> P.green (prettyName name)
                <> " has conflicting definitions:"
            )
              `P.hang` P.lines prettyConflicts

renderEditConflicts ::
  PPE.PrettyPrintEnv -> Patch -> Numbered Pretty
renderEditConflicts ppe Patch {..} = do
  formattedConflicts <- for editConflicts formatConflict
  pure . Monoid.unlessM (null editConflicts) . P.callout "❓" . P.sep "\n\n" $
    [ P.wrap $
        "These"
          <> P.bold "definitions were edited differently"
          <> "in namespaces that have been merged into this one."
          <> "You'll have to tell me what to use as the new definition:",
      P.indentN 2 (P.lines formattedConflicts)
      --    , tip $ "Use " <> makeExample IP.resolve [name (head editConflicts), " <replacement>"] <> " to pick a replacement." -- todo: eventually something with `edit`
    ]
  where
    -- todo: could possibly simplify all of this, but today is a copy/paste day.
    editConflicts :: [Either (Reference, Set TypeEdit.TypeEdit) (Reference, Set TermEdit.TermEdit)]
    editConflicts =
      (fmap Left . Map.toList . R.toMultimap . R.filterManyDom $ _typeEdits)
        <> (fmap Right . Map.toList . R.toMultimap . R.filterManyDom $ _termEdits)
    numberedHQName :: HQ.HashQualified Name -> Numbered Pretty
    numberedHQName hqName = do
      n <- addNumberedArg (HQ.toString hqName)
      pure $ formatNum n <> styleHashQualified P.bold hqName
    formatTypeEdits ::
      (Reference, Set TypeEdit.TypeEdit) ->
      Numbered Pretty
    formatTypeEdits (r, toList -> es) = do
      replacedType <- numberedHQName (PPE.typeName ppe r)
      replacements <- for [PPE.typeName ppe r | TypeEdit.Replace r <- es] numberedHQName
      pure . P.wrap $
        "The type"
          <> replacedType
          <> "was"
          <> ( if TypeEdit.Deprecate `elem` es
                 then "deprecated and also replaced with"
                 else "replaced with"
             )
            `P.hang` P.lines replacements
    formatTermEdits ::
      (Reference.TermReference, Set TermEdit.TermEdit) ->
      Numbered Pretty
    formatTermEdits (r, toList -> es) = do
      replacedTerm <- numberedHQName (PPE.termName ppe (Referent.Ref r))
      replacements <- for [PPE.termName ppe (Referent.Ref r) | TermEdit.Replace r _ <- es] numberedHQName
      pure . P.wrap $
        "The term"
          <> replacedTerm
          <> "was"
          <> ( if TermEdit.Deprecate `elem` es
                 then "deprecated and also replaced with"
                 else "replaced with"
             )
            `P.hang` P.lines replacements
    formatConflict ::
      Either
        (Reference, Set TypeEdit.TypeEdit)
        (Reference.TermReference, Set TermEdit.TermEdit) ->
      Numbered Pretty
    formatConflict = either formatTypeEdits formatTermEdits

type Numbered = State.State (Int, Seq.Seq String)

addNumberedArg :: String -> Numbered Int
addNumberedArg s = do
  (n, args) <- State.get
  State.put (n + 1, args Seq.|> s)
  pure $ (n + 1)

formatNum :: Int -> Pretty
formatNum n = P.string (show n <> ". ")

runNumbered :: Numbered a -> (a, NumberedArgs)
runNumbered m =
  let (a, (_, args)) = State.runState m (0, mempty)
   in (a, Foldable.toList args)

todoOutput :: (Var v) => PPED.PrettyPrintEnvDecl -> TO.TodoOutput v a -> (Pretty, NumberedArgs)
todoOutput ppe todo = runNumbered do
  conflicts <- todoConflicts
  edits <- todoEdits
  pure (conflicts <> edits)
  where
    ppeu = PPED.unsuffixifiedPPE ppe
    ppes = PPED.suffixifiedPPE ppe
    (frontierTerms, frontierTypes) = TO.todoFrontier todo
    (dirtyTerms, dirtyTypes) = TO.todoFrontierDependents todo
    corruptTerms =
      [(PPE.termName ppeu (Referent.Ref r), r) | (r, Nothing) <- frontierTerms]
    corruptTypes =
      [(PPE.typeName ppeu r, r) | (r, MissingObject _) <- frontierTypes]
    goodTerms ts =
      [(Referent.Ref r, PPE.termName ppeu (Referent.Ref r), typ) | (r, Just typ) <- ts]
    todoConflicts :: Numbered Pretty
    todoConflicts = do
      if TO.noConflicts todo
        then pure mempty
        else do
          editConflicts <- renderEditConflicts ppeu (TO.editConflicts todo)
          nameConflicts <- renderNameConflicts ppeu conflictedNames
          pure $ P.lines . P.nonEmpty $ [editConflicts, nameConflicts]
      where
        -- If a conflict is both an edit and a name conflict, we show it in the edit
        -- conflicts section
        conflictedNames :: Names
        conflictedNames = removeEditConflicts (TO.editConflicts todo) (TO.nameConflicts todo)
        -- e.g. `foo#a` has been independently updated to `foo#b` and `foo#c`.
        -- This means there will be a name conflict:
        --    foo -> #b
        --    foo -> #c
        -- as well as an edit conflict:
        --    #a -> #b
        --    #a -> #c
        -- We want to hide/ignore the name conflicts that are also targets of an
        -- edit conflict, so that the edit conflict will be dealt with first.
        -- For example, if hash `h` has multiple edit targets { #x, #y, #z, ...},
        -- we'll temporarily remove name conflicts pointing to { #x, #y, #z, ...}.
        removeEditConflicts :: Patch -> Names -> Names
        removeEditConflicts Patch {..} Names {..} = Names terms' types'
          where
            terms' = R.filterRan (`Set.notMember` conflictedTermEditTargets) terms
            types' = R.filterRan (`Set.notMember` conflictedTypeEditTargets) types
            conflictedTypeEditTargets :: Set Reference
            conflictedTypeEditTargets =
              Set.fromList $ toList (R.ran typeEditConflicts) >>= TypeEdit.references
            conflictedTermEditTargets :: Set Referent.Referent
            conflictedTermEditTargets =
              Set.fromList . fmap Referent.Ref $
                toList (R.ran termEditConflicts) >>= TermEdit.references
            typeEditConflicts = R.filterDom (`R.manyDom` _typeEdits) _typeEdits
            termEditConflicts = R.filterDom (`R.manyDom` _termEdits) _termEdits

    todoEdits :: Numbered Pretty
    todoEdits = do
      numberedTypes <- for (unscore <$> dirtyTypes) \(ref, displayObj) -> do
        n <- addNumberedArg (HQ.toString $ PPE.typeName ppeu ref)
        pure $ formatNum n <> prettyDeclPair ppeu (ref, displayObj)
      let filteredTerms = goodTerms (unscore <$> dirtyTerms)
      termNumbers <- for filteredTerms \(ref, _, _) -> do
        n <- addNumberedArg (HQ.toString $ PPE.termName ppeu ref)
        pure $ formatNum n
      let formattedTerms = TypePrinter.prettySignaturesCT ppes filteredTerms
          numberedTerms = zipWith (<>) termNumbers formattedTerms
      pure $
        Monoid.unlessM (TO.noEdits todo) . P.callout "🚧" . P.sep "\n\n" . P.nonEmpty $
          [ P.wrap
              ( "The namespace has"
                  <> fromString (show (TO.todoScore todo))
                  <> "transitive dependent(s) left to upgrade."
                  <> "Your edit frontier is the dependents of these definitions:"
              ),
            P.indentN 2 . P.lines $
              ( (prettyDeclPair ppeu <$> toList frontierTypes)
                  ++ TypePrinter.prettySignaturesCT ppes (goodTerms frontierTerms)
              ),
            P.wrap "I recommend working on them in the following order:",
            P.lines $ numberedTypes ++ numberedTerms,
            formatMissingStuff corruptTerms corruptTypes
          ]
    unscore :: (a, b, c) -> (b, c)
    unscore (_score, b, c) = (b, c)

listOfDefinitions ::
  (Var v) => Input.FindScope -> PPE.PrettyPrintEnv -> E.ListDetailed -> [SR'.SearchResult' v a] -> IO Pretty
listOfDefinitions fscope ppe detailed results =
  pure $ listOfDefinitions' fscope ppe detailed results

listOfLinks ::
  (Var v) => PPE.PrettyPrintEnv -> [(HQ.HashQualified Name, Maybe (Type v a))] -> IO Pretty
listOfLinks _ [] =
  pure . P.callout "😶" . P.wrap $
    "No results. Try using the "
      <> IP.makeExample IP.link []
      <> "command to add metadata to a definition."
listOfLinks ppe results =
  pure $
    P.lines
      [ P.numberedColumn2
          num
          [ (P.syntaxToColor $ prettyHashQualified hq, ": " <> prettyType typ) | (hq, typ) <- results
          ],
        "",
        tip $
          "Try using"
            <> IP.makeExample IP.display ["1"]
            <> "to display the first result or"
            <> IP.makeExample IP.view ["1"]
            <> "to view its source."
      ]
  where
    num i = P.hiBlack $ P.shown i <> "."
    prettyType Nothing = "❓ (missing a type for this definition)"
    prettyType (Just t) = TypePrinter.pretty ppe t

data ShowNumbers = ShowNumbers | HideNumbers

-- | `ppe` is just for rendering type signatures
--   `oldPath, newPath :: Path.Absolute` are just for producing fully-qualified
--                                       numbered args
showDiffNamespace ::
  forall v.
  (Var v) =>
  ShowNumbers ->
  PPE.PrettyPrintEnv ->
  Input.AbsBranchId ->
  Input.AbsBranchId ->
  OBD.BranchDiffOutput v Ann ->
  (Pretty, NumberedArgs)
showDiffNamespace _ _ _ _ diffOutput
  | OBD.isEmpty diffOutput =
      ("The namespaces are identical.", mempty)
showDiffNamespace sn ppe oldPath newPath OBD.BranchDiffOutput {..} =
  (P.sepNonEmpty "\n\n" p, toList args)
  where
    (p, (menuSize, args)) =
      (`State.runState` (0 :: Int, Seq.empty)) $
        sequence
          [ if (not . null) newTypeConflicts
              || (not . null) newTermConflicts
              then do
                prettyUpdatedTypes :: [Pretty] <- traverse prettyUpdateType newTypeConflicts
                prettyUpdatedTerms :: [Pretty] <- traverse prettyUpdateTerm newTermConflicts
                pure $
                  P.sepNonEmpty
                    "\n\n"
                    [ P.red "New name conflicts:",
                      P.indentN 2 . P.sepNonEmpty "\n\n" $ prettyUpdatedTypes <> prettyUpdatedTerms
                    ]
              else pure mempty,
            if (not . null) resolvedTypeConflicts
              || (not . null) resolvedTermConflicts
              then do
                prettyUpdatedTypes :: [Pretty] <- traverse prettyUpdateType resolvedTypeConflicts
                prettyUpdatedTerms :: [Pretty] <- traverse prettyUpdateTerm resolvedTermConflicts
                pure $
                  P.sepNonEmpty
                    "\n\n"
                    [ P.bold "Resolved name conflicts:",
                      P.indentN 2 . P.sepNonEmpty "\n\n" $ prettyUpdatedTypes <> prettyUpdatedTerms
                    ]
              else pure mempty,
            if (not . null) updatedTypes
              || (not . null) updatedTerms
              || propagatedUpdates > 0
              || (not . null) updatedPatches
              then do
                prettyUpdatedTypes :: [Pretty] <- traverse prettyUpdateType updatedTypes
                prettyUpdatedTerms :: [Pretty] <- traverse prettyUpdateTerm updatedTerms
                prettyUpdatedPatches :: [Pretty] <- traverse (prettySummarizePatch newPath) updatedPatches
                pure $
                  P.sepNonEmpty
                    "\n\n"
                    [ P.bold "Updates:",
                      P.indentNonEmptyN 2 . P.sepNonEmpty "\n\n" $ prettyUpdatedTypes <> prettyUpdatedTerms,
                      if propagatedUpdates > 0
                        then
                          P.indentN 2 $
                            P.wrap
                              ( P.hiBlack $
                                  "There were "
                                    <> P.shown propagatedUpdates
                                    <> "auto-propagated updates."
                              )
                        else mempty,
                      P.indentNonEmptyN 2 . P.linesNonEmpty $ prettyUpdatedPatches
                    ]
              else pure mempty,
            if (not . null) addedTypes
              || (not . null) addedTerms
              || (not . null) addedPatches
              then do
                prettyAddedTypes :: Pretty <- prettyAddTypes addedTypes
                prettyAddedTerms :: Pretty <- prettyAddTerms addedTerms
                prettyAddedPatches :: [Pretty] <- traverse (prettySummarizePatch newPath) addedPatches
                pure $
                  P.sepNonEmpty
                    "\n\n"
                    [ P.bold "Added definitions:",
                      P.indentNonEmptyN 2 $ P.linesNonEmpty [prettyAddedTypes, prettyAddedTerms],
                      P.indentNonEmptyN 2 $ P.lines prettyAddedPatches
                    ]
              else pure mempty,
            if (not . null) removedTypes
              || (not . null) removedTerms
              || (not . null) removedPatches
              then do
                prettyRemovedTypes :: Pretty <- prettyRemoveTypes removedTypes
                prettyRemovedTerms :: Pretty <- prettyRemoveTerms removedTerms
                prettyRemovedPatches :: [Pretty] <- traverse (prettyNamePatch oldPath) removedPatches
                pure $
                  P.sepNonEmpty
                    "\n\n"
                    [ P.bold "Removed definitions:",
                      P.indentN 2 $
                        P.linesNonEmpty
                          [ prettyRemovedTypes,
                            prettyRemovedTerms,
                            P.linesNonEmpty prettyRemovedPatches
                          ]
                    ]
              else pure mempty,
            if (not . null) renamedTypes
              || (not . null) renamedTerms
              then do
                results <- prettyRenameGroups renamedTypes renamedTerms
                pure $
                  P.sepNonEmpty
                    "\n\n"
                    [ P.bold "Name changes:",
                      P.indentN 2 . P.sepNonEmpty "\n\n" $ results
                    ]
              else -- todo: change separator to just '\n' here if all the results are 1 to 1
                pure mempty
          ]

    {- new implementation
      23. X  ┐  =>  (added)   24. X'
      25. X2 ┘      (removed) 26. X2
    -}
    prettyRenameGroups ::
      [OBD.RenameTypeDisplay v a] ->
      [OBD.RenameTermDisplay v a] ->
      Numbered [Pretty]
    prettyRenameGroups types terms =
      (<>)
        <$> traverse
          (prettyGroup . (over (_1 . _1) Referent.Ref))
          (types `zip` [0 ..])
        <*> traverse prettyGroup (terms `zip` [length types ..])
      where
        leftNamePad :: P.Width =
          foldl1' max $
            map
              (foldl1' max . map (P.Width . HQ'.nameLength Name.toText) . toList . view _3)
              terms
              <> map
                (foldl1' max . map (P.Width . HQ'.nameLength Name.toText) . toList . view _3)
                types
        prettyGroup ::
          ( (Referent, b, Set (HQ'.HashQualified Name), Set (HQ'.HashQualified Name)),
            Int
          ) ->
          Numbered Pretty
        prettyGroup ((r, _, olds, news), i) =
          let -- [ "peach  ┐"
              -- , "peach' ┘"]
              olds' :: [Numbered Pretty] =
                let olds0 = List.sortBy Name.compareAlphabetical (Set.toList olds)
                 in map (\(oldhq, oldp) -> numHQ' oldPath oldhq r <&> (\n -> n <> " " <> oldp))
                      . zip olds0
                      . P.boxRight
                      . map (P.rightPad leftNamePad . phq')
                      $ olds0

              added' = toList $ Set.difference news olds
              removed' = toList $ Set.difference olds news
              -- [ "(added)   24. X'"
              -- , "(removed) 26. X2"
              -- ]

              news' :: [Numbered Pretty] =
                map (number addedLabel) added' ++ map (number removedLabel) removed'
                where
                  addedLabel = "(added)"
                  removedLabel = "(removed)"
                  number label name =
                    numHQ' newPath name r
                      <&> (\num -> num <> " " <> phq' name <> " " <> label)

              buildTable :: [Numbered Pretty] -> [Numbered Pretty] -> Numbered Pretty
              buildTable lefts rights =
                let hlefts =
                      if i == 0
                        then pure (P.bold "Original") : lefts
                        else lefts
                    hrights = if i == 0 then pure (P.bold "Changes") : rights else rights
                 in P.column2UnzippedM @Numbered mempty hlefts hrights
           in buildTable olds' news'

    prettyUpdateType :: OBD.UpdateTypeDisplay v a -> Numbered Pretty
    {-
       1. ability Foo#pqr x y
          2. - AllRightsReserved : License
          3. + MIT               : License
       4. ability Foo#abc
          5. - apiDocs : License
          6. + MIT     : License
    -}
    prettyUpdateType (OBD.UpdateTypeDisplay Nothing mdUps) =
      P.column2 <$> traverse (mdTypeLine newPath) mdUps
    {-
        1. ┌ ability Foo#pqr x y
        2. └ ability Foo#xyz a b
           ⧩
        4. ┌ ability Foo#abc
           │  5. - apiDocs : Doc
           │  6. + MIT     : License
        7. └ ability Foo#def
              8. - apiDocs : Doc
              9. + MIT     : License

        1. ┌ foo#abc : Nat -> Nat -> Poop
        2. └ foo#xyz : Nat
           ↓
        4. foo	 : Poop
             5. + foo.docs : Doc
    -}
    prettyUpdateType (OBD.UpdateTypeDisplay (Just olds) news) =
      do
        olds <- traverse (mdTypeLine oldPath) [OBD.TypeDisplay name r decl mempty | (name, r, decl) <- olds]
        news <- traverse (mdTypeLine newPath) news
        let (oldnums, olddatas) = unzip olds
        let (newnums, newdatas) = unzip news
        pure . P.column2 $
          zip
            (oldnums <> [""] <> newnums)
            (P.boxLeft olddatas <> [downArrow] <> P.boxLeft newdatas)

    {-
    13. ┌ability Yyz         (+1 metadata)
    14. └ability copies.Yyz  (+2 metadata)
    -}
    prettyAddTypes :: forall a. [OBD.AddedTypeDisplay v a] -> Numbered Pretty
    prettyAddTypes = fmap P.lines . traverse prettyGroup
      where
        prettyGroup :: OBD.AddedTypeDisplay v a -> Numbered Pretty
        prettyGroup (hqmds, r, odecl) = do
          pairs <- traverse (prettyLine r odecl) hqmds
          let (nums, decls) = unzip pairs
          let boxLeft = case hqmds of _ : _ : _ -> P.boxLeft; _ -> id
          pure . P.column2 $ zip nums (boxLeft decls)
        prettyLine :: Reference -> Maybe (DD.DeclOrBuiltin v a) -> (HQ'.HashQualified Name, [OBD.MetadataDisplay v a]) -> Numbered (Pretty, Pretty)
        prettyLine r odecl (hq, mds) = do
          n <- numHQ' newPath hq (Referent.Ref r)
          pure . (n,) $
            prettyDecl hq odecl <> case length mds of
              0 -> mempty
              c -> " (+" <> P.shown c <> " metadata)"

    prettyAddTerms :: forall a. [OBD.AddedTermDisplay v a] -> Numbered Pretty
    prettyAddTerms = fmap (P.column3 . mconcat) . traverse prettyGroup . reorderTerms
      where
        reorderTerms = sortOn (not . Referent.isConstructor . view _2)
        prettyGroup :: OBD.AddedTermDisplay v a -> Numbered [(Pretty, Pretty, Pretty)]
        prettyGroup (hqmds, r, otype) = do
          pairs <- traverse (prettyLine r otype) hqmds
          let (nums, names, decls) = unzip3 pairs
              boxLeft = case hqmds of _ : _ : _ -> P.boxLeft; _ -> id
          pure $ zip3 nums (boxLeft names) decls
        prettyLine ::
          Referent ->
          Maybe (Type v a) ->
          (HQ'.HashQualified Name, [OBD.MetadataDisplay v a]) ->
          Numbered (Pretty, Pretty, Pretty)
        prettyLine r otype (hq, mds) = do
          n <- numHQ' newPath hq r
          pure . (n,phq' hq,) $
            ": " <> prettyType otype <> case length mds of
              0 -> mempty
              c -> " (+" <> P.shown c <> " metadata)"

    prettySummarizePatch, prettyNamePatch :: Input.AbsBranchId -> OBD.PatchDisplay -> Numbered Pretty
    --  12. patch p (added 3 updates, deleted 1)
    prettySummarizePatch prefix (name, patchDiff) = do
      n <- numPatch prefix name
      let addCount =
            (R.size . view Patch.addedTermEdits) patchDiff
              + (R.size . view Patch.addedTypeEdits) patchDiff
          delCount =
            (R.size . view Patch.removedTermEdits) patchDiff
              + (R.size . view Patch.removedTypeEdits) patchDiff
          messages =
            (if addCount > 0 then ["added " <> P.shown addCount] else [])
              ++ (if delCount > 0 then ["deleted " <> P.shown addCount] else [])
          message = case messages of
            [] -> mempty
            x : ys -> " (" <> P.commas (x <> " updates" : ys) <> ")"
      pure $ n <> P.bold " patch " <> prettyName name <> message
    -- 	18. patch q
    prettyNamePatch prefix (name, _patchDiff) = do
      n <- numPatch prefix name
      pure $ n <> P.bold " patch " <> prettyName name

    {-
     Removes:

       10. ┌ oldn'busted : Nat -> Nat -> Poop
       11. └ oldn'busted'
       12.  ability BadType
       13.  patch defunctThingy
    -}
    prettyRemoveTypes :: forall a. [OBD.RemovedTypeDisplay v a] -> Numbered Pretty
    prettyRemoveTypes = fmap P.lines . traverse prettyGroup
      where
        prettyGroup :: OBD.RemovedTypeDisplay v a -> Numbered Pretty
        prettyGroup (hqs, r, odecl) = do
          lines <- traverse (prettyLine r odecl) hqs
          let (nums, decls) = unzip lines
              boxLeft = case hqs of _ : _ : _ -> P.boxLeft; _ -> id
          pure . P.column2 $ zip nums (boxLeft decls)
        prettyLine :: Reference -> Maybe (DD.DeclOrBuiltin v a) -> HQ'.HashQualified Name -> Numbered (Pretty, Pretty)
        prettyLine r odecl hq = do
          n <- numHQ' newPath hq (Referent.Ref r)
          pure (n, prettyDecl hq odecl)

    prettyRemoveTerms :: forall a. [OBD.RemovedTermDisplay v a] -> Numbered Pretty
    prettyRemoveTerms = fmap (P.column3 . mconcat) . traverse prettyGroup . reorderTerms
      where
        reorderTerms = sortOn (not . Referent.isConstructor . view _2)
        prettyGroup :: OBD.RemovedTermDisplay v a -> Numbered [(Pretty, Pretty, Pretty)]
        prettyGroup ([], r, _) =
          error $ "trying to remove " <> show r <> " without any names."
        prettyGroup (hq1 : hqs, r, otype) = do
          line1 <- prettyLine1 r otype hq1
          lines <- traverse (prettyLine r) hqs
          let (nums, names, decls) = unzip3 (line1 : lines)
              boxLeft = case hqs of _ : _ -> P.boxLeft; _ -> id
          pure $ zip3 nums (boxLeft names) decls
        prettyLine1 r otype hq = do
          n <- numHQ' newPath hq r
          pure (n, phq' hq, ": " <> prettyType otype)
        prettyLine r hq = do
          n <- numHQ' newPath hq r
          pure (n, phq' hq, mempty)

    downArrow = P.bold "↓"
    mdTypeLine :: Input.AbsBranchId -> OBD.TypeDisplay v a -> Numbered (Pretty, Pretty)
    mdTypeLine p (OBD.TypeDisplay hq r odecl mddiff) = do
      n <- numHQ' p hq (Referent.Ref r)
      fmap ((n,) . P.linesNonEmpty) . sequence $
        [ pure $ prettyDecl hq odecl,
          P.indentN leftNumsWidth <$> prettyMetadataDiff mddiff
        ]

    -- + 2. MIT               : License
    -- - 3. AllRightsReserved : License
    mdTermLine ::
      Input.AbsBranchId ->
      P.Width ->
      OBD.TermDisplay v a ->
      Numbered (Pretty, Pretty)
    mdTermLine p namesWidth (OBD.TermDisplay hq r otype mddiff) = do
      n <- numHQ' p hq r
      fmap ((n,) . P.linesNonEmpty)
        . sequence
        $ [ pure $ P.rightPad namesWidth (phq' hq) <> " : " <> prettyType otype,
            prettyMetadataDiff mddiff
          ]

    prettyUpdateTerm :: OBD.UpdateTermDisplay v a -> Numbered Pretty
    prettyUpdateTerm (OBD.UpdateTermDisplay Nothing newTerms) =
      if null newTerms
        then error "Super invalid UpdateTermDisplay"
        else fmap P.column2 $ traverse (mdTermLine newPath namesWidth) newTerms
      where
        namesWidth = foldl1' max $ fmap (P.Width . HQ'.nameLength Name.toText . view #name) newTerms
    prettyUpdateTerm (OBD.UpdateTermDisplay (Just olds) news) = fmap P.column2 $ do
      olds <-
        traverse
          (mdTermLine oldPath namesWidth)
          [OBD.TermDisplay name r typ mempty | (name, r, typ) <- olds]
      news <- traverse (mdTermLine newPath namesWidth) news
      let (oldnums, olddatas) = unzip olds
      let (newnums, newdatas) = unzip news
      pure $
        zip
          (oldnums <> [""] <> newnums)
          (P.boxLeft olddatas <> [downArrow] <> P.boxLeft newdatas)
      where
        namesWidth =
          foldl1' max $
            fmap (P.Width . HQ'.nameLength Name.toText . view #name) news
              <> fmap (P.Width . HQ'.nameLength Name.toText . view _1) olds

    prettyMetadataDiff :: OBD.MetadataDiff (OBD.MetadataDisplay v a) -> Numbered Pretty
    prettyMetadataDiff OBD.MetadataDiff {..} =
      P.column2M $
        map (elem oldPath "- ") removedMetadata
          <> map (elem newPath "+ ") addedMetadata
      where
        elem p x (hq, r, otype) = do
          num <- numHQ p hq r
          pure (x <> num <> " " <> phq hq, ": " <> prettyType otype)

    prettyType :: Maybe (Type v a) -> Pretty
    prettyType = maybe (P.red "type not found") (TypePrinter.pretty ppe)
    prettyDecl hq =
      maybe
        (P.red "type not found")
        (P.syntaxToColor . DeclPrinter.prettyDeclOrBuiltinHeader (HQ'.toHQ hq))
    phq' :: _ -> Pretty = P.syntaxToColor . prettyHashQualified'
    phq :: _ -> Pretty = P.syntaxToColor . prettyHashQualified

    -- DeclPrinter.prettyDeclHeader : HQ -> Either
    numPatch :: Input.AbsBranchId -> Name -> Numbered Pretty
    numPatch prefix name =
      addNumberedArg' $ prefixBranchId prefix name

    numHQ :: Input.AbsBranchId -> HQ.HashQualified Name -> Referent -> Numbered Pretty
    numHQ prefix hq r =
      addNumberedArg' . HQ.toStringWith (prefixBranchId prefix) . HQ.requalify hq $ r

    numHQ' :: Input.AbsBranchId -> HQ'.HashQualified Name -> Referent -> Numbered Pretty
    numHQ' prefix hq r =
      addNumberedArg' . HQ'.toStringWith (prefixBranchId prefix) . HQ'.requalify hq $ r

    -- E.g.
    -- prefixBranchId "#abcdef" "base.List.map" -> "#abcdef.base.List.map"
    -- prefixBranchId ".base" "List.map" -> ".base.List.map"
    prefixBranchId :: Input.AbsBranchId -> Name -> String
    prefixBranchId branchId name = case branchId of
      Left sch -> "#" <> SCH.toString sch <> ":" <> Name.toString (Name.makeAbsolute name)
      Right pathPrefix -> Name.toString (Name.makeAbsolute . Path.prefixName pathPrefix $ name)

    addNumberedArg' :: String -> Numbered Pretty
    addNumberedArg' s = case sn of
      ShowNumbers -> do
        n <- addNumberedArg s
        pure $ padNumber n
      HideNumbers -> pure mempty

    padNumber :: Int -> Pretty
    padNumber n = P.hiBlack . P.rightPad leftNumsWidth $ P.shown n <> "."

    leftNumsWidth = P.Width $ length (show menuSize) + length ("." :: String)

noResults :: Input.FindScope -> Pretty
noResults fscope =
  P.callout "😶" $
    P.lines $
      [ P.wrap $
          "No results. Check your spelling, or try using tab completion "
            <> "to supply command arguments.",
        ""
      ]
        ++ case fscope of
          Input.FindGlobal -> []
          _ -> [suggestFindGlobal]
  where
    suggestFindGlobal =
      P.wrap $
        IP.makeExample IP.findGlobal []
          <> "can be used to search outside the current namespace."

listOfDefinitions' ::
  (Var v) =>
  Input.FindScope ->
  PPE.PrettyPrintEnv -> -- for printing types of terms :-\
  E.ListDetailed ->
  [SR'.SearchResult' v a] ->
  Pretty
listOfDefinitions' fscope ppe detailed results =
  if null results
    then noResults fscope
    else
      P.lines
        . P.nonEmpty
        $ prettyNumberedResults
          : [ formatMissingStuff termsWithMissingTypes missingTypes,
              Monoid.unlessM (null missingBuiltins)
                . bigproblem
                $ P.wrap
                  "I encountered an inconsistency in the codebase; these definitions refer to built-ins that this version of unison doesn't know about:"
                  `P.hang` P.column2
                    ( (P.bold "Name", P.bold "Built-in")
                        -- : ("-", "-")
                        : fmap
                          ( bimap
                              (P.syntaxToColor . prettyHashQualified)
                              (P.text . Referent.toText)
                          )
                          missingBuiltins
                    )
            ]
  where
    prettyNumberedResults = P.numberedList prettyResults
    -- todo: group this by namespace
    prettyResults =
      map
        (SR'.foldResult' renderTerm renderType)
        (filter (not . missingType) results)
      where
        (renderTerm, renderType) =
          if detailed
            then (unsafePrettyTermResultSigFull' ppe, prettyTypeResultHeaderFull')
            else (unsafePrettyTermResultSig' ppe, prettyTypeResultHeader')
    missingType (SR'.Tm _ Nothing _ _) = True
    missingType (SR'.Tp _ (MissingObject _) _ _) = True
    missingType _ = False
    -- termsWithTypes = [(name,t) | (name, Just t) <- sigs0 ]
    --   where sigs0 = (\(name, _, typ) -> (name, typ)) <$> terms
    termsWithMissingTypes =
      [ (name, Reference.idToShortHash r)
        | SR'.Tm name Nothing (Referent.Ref (Reference.DerivedId r)) _ <- results
      ]
    missingTypes =
      nubOrdOn snd $
        [(name, r) | SR'.Tp name (MissingObject r) _ _ <- results]
          <> [ (name, Reference.toShortHash r)
               | SR'.Tm name Nothing (Referent.toTypeReference -> Just r) _ <- results
             ]
    missingBuiltins =
      results >>= \case
        SR'.Tm name Nothing r@(Referent.Ref (Reference.Builtin _)) _ ->
          [(name, r)]
        _ -> []

watchPrinter ::
  (Var v) =>
  Text ->
  PPE.PrettyPrintEnv ->
  Ann ->
  WK.WatchKind ->
  Term v () ->
  Runtime.IsCacheHit ->
  Pretty
watchPrinter src ppe ann kind term isHit =
  P.bracket $
    let lines = Text.lines src
        lineNum = fromMaybe 1 $ startingLine ann
        lineNumWidth = length (show lineNum)
        extra = "     " <> replicate (length kind) ' ' -- for the ` | > ` after the line number
        line = lines !! (lineNum - 1)
        addCache p = if isHit then p <> " (cached)" else p
        renderTest (Term.App' (Term.Constructor' (ConstructorReference _ id)) (Term.Text' msg)) =
          "\n"
            <> if id == DD.okConstructorId
              then
                addCache
                  (P.green "✅ " <> P.bold "Passed" <> P.green (P.text msg'))
              else
                if id == DD.failConstructorId
                  then
                    addCache
                      (P.red "🚫 " <> P.bold "FAILED" <> P.red (P.text msg'))
                  else P.red "❓ " <> TermPrinter.pretty ppe term
          where
            msg' =
              if Text.take 1 msg == " "
                then msg
                else " " <> msg
        renderTest x =
          fromString $ "\n Unison bug: " <> show x <> " is not a test."
     in P.lines
          [ fromString (show lineNum) <> " | " <> P.text line,
            case (kind, term) of
              (WK.TestWatch, Term.List' tests) -> foldMap renderTest tests
              _ ->
                P.lines
                  [ fromString (replicate lineNumWidth ' ')
                      <> fromString extra
                      <> (if isHit then id else P.purple) "⧩",
                    P.indentN (P.Width (lineNumWidth + length extra))
                      . (if isHit then id else P.bold)
                      $ TermPrinter.pretty ppe term
                  ]
          ]

filestatusTip :: Pretty
filestatusTip = tip "Use `help filestatus` to learn more."

prettyDiff :: Names.Diff -> Pretty
prettyDiff diff =
  let orig = Names.originalNames diff
      adds = Names.addedNames diff
      removes = Names.removedNames diff

      addedTerms =
        [ (n, r) | (n, r) <- R.toList (Names.terms adds), not $ R.memberRan r (Names.terms removes)
        ]
      addedTypes =
        [ (n, r) | (n, r) <- R.toList (Names.types adds), not $ R.memberRan r (Names.types removes)
        ]
      added = List.sortBy Name.compareAlphabetical (hqTerms ++ hqTypes)
        where
          hqTerms = [Names.hqName adds n (Right r) | (n, r) <- addedTerms]
          hqTypes = [Names.hqName adds n (Left r) | (n, r) <- addedTypes]

      removedTerms =
        [ (n, r) | (n, r) <- R.toList (Names.terms removes), not $ R.memberRan r (Names.terms adds), Set.notMember n addedTermsSet
        ]
        where
          addedTermsSet = Set.fromList (map fst addedTerms)
      removedTypes =
        [ (n, r) | (n, r) <- R.toList (Names.types removes), not $ R.memberRan r (Names.types adds), Set.notMember n addedTypesSet
        ]
        where
          addedTypesSet = Set.fromList (map fst addedTypes)
      removed = List.sortBy Name.compareAlphabetical (hqTerms ++ hqTypes)
        where
          hqTerms = [Names.hqName removes n (Right r) | (n, r) <- removedTerms]
          hqTypes = [Names.hqName removes n (Left r) | (n, r) <- removedTypes]

      movedTerms =
        [ (n, n2) | (n, r) <- R.toList (Names.terms removes), n2 <- toList (R.lookupRan r (Names.terms adds))
        ]
      movedTypes =
        [ (n, n2) | (n, r) <- R.toList (Names.types removes), n2 <- toList (R.lookupRan r (Names.types adds))
        ]
      moved = Name.sortNamed Name.toText fst . nubOrd $ (movedTerms <> movedTypes)

      copiedTerms =
        List.multimap
          [ (n, n2) | (n2, r) <- R.toList (Names.terms adds), not (R.memberRan r (Names.terms removes)), n <- toList (R.lookupRan r (Names.terms orig))
          ]
      copiedTypes =
        List.multimap
          [ (n, n2) | (n2, r) <- R.toList (Names.types adds), not (R.memberRan r (Names.types removes)), n <- toList (R.lookupRan r (Names.types orig))
          ]
      copied =
        Name.sortNamed Name.toText fst $
          Map.toList (Map.unionWith (<>) copiedTerms copiedTypes)
   in P.sepNonEmpty
        "\n\n"
        [ if not $ null added
            then
              P.lines
                [ -- todo: split out updates
                  P.green "+ Adds / updates:",
                  "",
                  P.indentN 2 . P.wrap $
                    P.sep " " (P.syntaxToColor . prettyHashQualified' <$> added)
                ]
            else mempty,
          if not $ null removed
            then
              P.lines
                [ P.hiBlack "- Deletes:",
                  "",
                  P.indentN 2 . P.wrap $
                    P.sep " " (P.syntaxToColor . prettyHashQualified' <$> removed)
                ]
            else mempty,
          if not $ null moved
            then
              P.lines
                [ P.purple "> Moves:",
                  "",
                  P.indentN 2 $
                    P.column2 $
                      (P.hiBlack "Original name", P.hiBlack "New name")
                        : [(prettyName n, prettyName n2) | (n, n2) <- moved]
                ]
            else mempty,
          if not $ null copied
            then
              P.lines
                [ P.yellow "= Copies:",
                  "",
                  P.indentN 2 $
                    P.column2 $
                      (P.hiBlack "Original name", P.hiBlack "New name(s)")
                        : [ (prettyName n, P.sep " " (prettyName <$> ns))
                            | (n, ns) <- copied
                          ]
                ]
            else mempty
        ]

prettyTermName :: PPE.PrettyPrintEnv -> Referent -> Pretty
prettyTermName ppe r =
  P.syntaxToColor $
    prettyHashQualified (PPE.termName ppe r)

prettyTypeName :: PPE.PrettyPrintEnv -> Reference -> Pretty
prettyTypeName ppe r =
  P.syntaxToColor $
    prettyHashQualified (PPE.typeName ppe r)

prettyReadGitRepo :: ReadGitRepo -> Pretty
prettyReadGitRepo = \case
  RemoteRepo.ReadGitRepo {url} -> P.blue (P.text url)

prettyWriteGitRepo :: WriteGitRepo -> Pretty
prettyWriteGitRepo RemoteRepo.WriteGitRepo {url} = P.blue (P.text url)

-- prettyWriteRepo :: WriteRepo -> Pretty
-- prettyWriteRepo = \case
--   RemoteRepo.WriteRepoGit RemoteRepo.WriteGitRepo {url} -> P.blue (P.text url)
--   RemoteRepo.WriteRepoShare s -> P.blue (P.text (RemoteRepo.printShareRepo s))

-- | Pretty-print a 'WhichBranchEmpty'.
prettyWhichBranchEmpty :: WhichBranchEmpty -> Pretty
prettyWhichBranchEmpty = \case
  WhichBranchEmptyHash hash -> P.shown hash
  WhichBranchEmptyPath path -> prettyPath' path

isTestOk :: Term v Ann -> Bool
isTestOk tm = case tm of
  Term.List' ts -> all isSuccess ts
    where
      isSuccess (Term.App' (Term.Constructor' (ConstructorReference ref cid)) _) =
        cid == DD.okConstructorId
          && ref == DD.testResultRef
      isSuccess _ = False
  _ -> False

-- | Get the list of numbered args corresponding to an endangerment map, which is used by a
-- few outputs. See 'endangeredDependentsTable'.
numberedArgsForEndangerments ::
  PPED.PrettyPrintEnvDecl ->
  Map LabeledDependency (NESet LabeledDependency) ->
  NumberedArgs
numberedArgsForEndangerments (PPED.unsuffixifiedPPE -> ppe) m =
  m
    & Map.elems
    & concatMap toList
    & fmap (HQ.toString . PPE.labeledRefName ppe)

-- | Format and render all dependents which are endangered by references going extinct.
endangeredDependentsTable ::
  PPED.PrettyPrintEnvDecl ->
  Map LabeledDependency (NESet LabeledDependency) ->
  P.Pretty P.ColorText
endangeredDependentsTable ppeDecl m =
  m
    & Map.toList
    & fmap (second toList)
    & numberDependents
    & map
      ( \(dependency, dependents) ->
          (prettyLabeled suffixifiedEnv dependency, prettyDependents dependents)
      )
    & List.intersperse spacer
    & P.column2Header "Dependency" "Referenced In"
  where
    numberDependents :: [(x, [LabeledDependency])] -> [(x, [(Int, LabeledDependency)])]
    numberDependents xs =
      let (_acc, numbered) =
            mapAccumLOf
              (traversed . _2 . traversed)
              (\n ld -> (n + 1, (n, ld)))
              1
              xs
       in numbered
    spacer = ("", "")
    suffixifiedEnv = (PPED.suffixifiedPPE ppeDecl)
    fqnEnv = (PPED.unsuffixifiedPPE ppeDecl)
    prettyLabeled ppe = \case
      LD.TermReferent ref -> prettyTermName ppe ref
      LD.TypeReference ref -> prettyTypeName ppe ref
    numArg = (\n -> P.hiBlack . fromString $ show n <> ". ")
    prettyDependents refs =
      refs
        & fmap (\(n, dep) -> numArg n <> prettyLabeled fqnEnv dep)
        & P.lines

-- | Displays a full, non-truncated Branch.CausalHash to a string, e.g. #abcdef
displayBranchHash :: CausalHash -> String
displayBranchHash = ("#" <>) . Text.unpack . Hash.toBase32HexText . unCausalHash

prettyHumanReadableTime :: UTCTime -> UTCTime -> Pretty
prettyHumanReadableTime now time =
  P.green . P.string $ humanReadableTimeI18N' terseTimeLocale now time
  where
    terseTimeLocale =
      defaultHumanTimeLocale
        { justNow = "now",
          secondsAgo = \f -> (++ " secs" ++ dir f),
          oneMinuteAgo = \f -> "a min" ++ dir f,
          minutesAgo = \f -> (++ " mins" ++ dir f),
          oneHourAgo = \f -> "an hour" ++ dir f,
          aboutHoursAgo = \f x -> "about " ++ x ++ " hours" ++ dir f,
          at = \_ t -> t,
          daysAgo = \f -> (++ " days" ++ dir f),
          weekAgo = \f -> (++ " week" ++ dir f),
          weeksAgo = \f -> (++ " weeks" ++ dir f),
          onYear = \dt -> dt,
          dayOfWeekFmt = "%A, %-l:%M%p",
          thisYearFmt = "%b %e",
          prevYearFmt = "%b %e, %Y"
        }

    dir True = " from now"
    dir False = " ago"

prettyRemoteBranchInfo :: (URI, ProjectName, ProjectBranchName) -> Pretty
prettyRemoteBranchInfo (host, remoteProject, remoteBranch) =
  -- Special-case Unison Share since we know its project branch URLs
  if URI.uriToString id host "" == "https://api.unison-lang.org"
    then
      P.hiBlack . P.text $
        "https://share.unison-lang.org/"
          <> into @Text remoteProject
          <> "/code/"
          <> into @Text remoteBranch
    else
      prettyProjectAndBranchName (ProjectAndBranch remoteProject remoteBranch)
        <> " on "
        <> P.hiBlack (P.shown host)
