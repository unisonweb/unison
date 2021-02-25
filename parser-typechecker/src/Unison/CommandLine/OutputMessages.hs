{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ViewPatterns        #-}


module Unison.CommandLine.OutputMessages where

import Unison.Prelude hiding (unlessM)

import           Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Output           as E
import qualified Unison.Codebase.Editor.Output           as Output
import qualified Unison.Codebase.Editor.TodoOutput       as TO
import qualified Unison.Codebase.Editor.SearchResult'    as SR'
import qualified Unison.Codebase.Editor.Output.BranchDiff as OBD


import           Control.Lens
import qualified Control.Monad.State.Strict    as State
import           Data.Bifunctor                (first, second)
import           Data.List                     (sort, stripPrefix)
import           Data.List.Extra               (nubOrdOn, nubOrd, notNull)
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as Text
import           Data.Text.IO                  (readFile, writeFile)
import           Data.Tuple.Extra              (dupe, uncurry3)
import           Prelude                       hiding (readFile, writeFile)
import           System.Directory               ( canonicalizePath
                                                , doesFileExist
                                                , getHomeDirectory
                                                )
import qualified Unison.ABT                    as ABT
import qualified Unison.UnisonFile             as UF
import           Unison.Codebase.GitError
import qualified Unison.Codebase.Path          as Path
import qualified Unison.Codebase.Patch         as Patch
import           Unison.Codebase.Patch         (Patch(..))
import qualified Unison.Codebase.ShortBranchHash as SBH
import qualified Unison.Codebase.TermEdit      as TermEdit
import qualified Unison.Codebase.TypeEdit      as TypeEdit
import           Unison.CommandLine             ( bigproblem
                                                , tip
                                                , note
                                                )
import           Unison.PrettyTerminal          ( clearCurrentLine
                                                , putPretty'
                                                )
import qualified Unison.CommandLine.InputPattern as IP1
import           Unison.CommandLine.InputPatterns (makeExample, makeExample')
import qualified Unison.CommandLine.InputPatterns as IP
import qualified Unison.Builtin.Decls          as DD
import qualified Unison.DataDeclaration        as DD
import qualified Unison.DeclPrinter            as DeclPrinter
import qualified Unison.HashQualified          as HQ
import qualified Unison.HashQualified'         as HQ'
import           Unison.Name                   (Name)
import qualified Unison.Name                   as Name
import           Unison.NamePrinter            (prettyHashQualified,
                                                prettyReference, prettyReferent,
                                                prettyLabeledDependency,
                                                prettyNamedReference,
                                                prettyNamedReferent,
                                                prettyName, prettyShortHash,
                                                styleHashQualified,
                                                styleHashQualified', prettyHashQualified')
import           Unison.Names2                 (Names'(..), Names0)
import qualified Unison.Names2                 as Names
import qualified Unison.Names3                 as Names
import           Unison.Parser                 (Ann, startingLine)
import qualified Unison.PrettyPrintEnv         as PPE
import qualified Unison.Codebase.Runtime       as Runtime
import           Unison.PrintError              ( prettyParseError
                                                , printNoteWithSource
                                                , prettyResolutionFailures
                                                , renderCompilerBug
                                                )
import qualified Unison.Reference              as Reference
import           Unison.Reference              ( Reference )
import qualified Unison.Referent               as Referent
import           Unison.Referent               ( Referent )
import qualified Unison.Result                 as Result
import qualified Unison.Term                   as Term
import           Unison.Term                   (Term)
import           Unison.Type                   (Type)
import qualified Unison.TermPrinter            as TermPrinter
import qualified Unison.TypePrinter            as TypePrinter
import qualified Unison.Util.ColorText         as CT
import           Unison.Util.Monoid             ( intercalateMap
                                                , unlessM
                                                )
import qualified Unison.Util.Pretty            as P
import qualified Unison.Util.Relation          as R
import           Unison.Var                    (Var)
import qualified Unison.Var                    as Var
import qualified Unison.Codebase.Editor.SlurpResult as SlurpResult
import Unison.Codebase.Editor.DisplayThing (DisplayThing(MissingThing, BuiltinThing, RegularThing))
import qualified Unison.Codebase.Editor.Input as Input
import qualified Unison.Hash as Hash
import qualified Unison.Codebase.Causal as Causal
import qualified Unison.Codebase.Editor.RemoteRepo as RemoteRepo
import qualified Unison.Util.List              as List
import qualified Unison.Util.Monoid            as Monoid
import Data.Tuple (swap)
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.ShortHash as SH
import Unison.LabeledDependency as LD
import Unison.Codebase.Editor.RemoteRepo (RemoteRepo)

type Pretty = P.Pretty P.ColorText

shortenDirectory :: FilePath -> IO FilePath
shortenDirectory dir = do
  home <- getHomeDirectory
  pure $ case stripPrefix home dir of
    Just d  -> "~" <> d
    Nothing -> dir

renderFileName :: FilePath -> IO Pretty
renderFileName dir = P.group . P.blue . fromString <$> shortenDirectory dir

notifyNumbered :: Var v => NumberedOutput v -> (Pretty, NumberedArgs)
notifyNumbered o = case o of
  ShowDiffNamespace oldPrefix newPrefix ppe diffOutput ->
    showDiffNamespace ShowNumbers ppe oldPrefix newPrefix diffOutput

  ShowDiffAfterDeleteDefinitions ppe diff ->
    first (\p -> P.lines
      [ p
      , ""
      , undoTip
      ]) (showDiffNamespace ShowNumbers ppe e e diff)

  ShowDiffAfterDeleteBranch bAbs ppe diff ->
    first (\p -> P.lines
      [ p
      , ""
      , undoTip
      ]) (showDiffNamespace ShowNumbers ppe bAbs bAbs diff)

  ShowDiffAfterModifyBranch b' _ _ (OBD.isEmpty -> True) ->
    (P.wrap $ "Nothing changed in" <> prettyPath' b' <> ".", mempty)
  ShowDiffAfterModifyBranch b' bAbs ppe diff ->
    first (\p -> P.lines
      [ P.wrap $ "Here's what changed in" <> prettyPath' b' <> ":"
      , ""
      , p
      , ""
      , undoTip
      ]) (showDiffNamespace ShowNumbers ppe bAbs bAbs diff)

  ShowDiffAfterMerge _ _ _ (OBD.isEmpty -> True) ->
    (P.wrap $ "Nothing changed as a result of the merge.", mempty)
  ShowDiffAfterMerge dest' destAbs ppe diffOutput ->
    first (\p -> P.lines [
      P.wrap $ "Here's what's changed in " <> prettyPath' dest' <> "after the merge:"
      , ""
      , p
      , ""
      , tip $ "You can use " <> IP.makeExample' IP.todo
           <> "to see if this generated any work to do in this namespace"
           <> "and " <> IP.makeExample' IP.test <> "to run the tests."
           <> "Or you can use" <> IP.makeExample' IP.undo <> " or"
           <> IP.makeExample' IP.viewReflog <> " to undo the results of this merge."
      ]) (showDiffNamespace ShowNumbers ppe destAbs destAbs diffOutput)

  ShowDiffAfterMergePropagate dest' destAbs patchPath' ppe diffOutput ->
    first (\p -> P.lines [
      P.wrap $ "Here's what's changed in " <> prettyPath' dest'
        <> "after applying the patch at " <> P.group (prettyPath' patchPath' <> ":")
      , ""
      , p
      , ""
      , tip $ "You can use "
           <> IP.makeExample IP.todo [prettyPath' patchPath', prettyPath' dest']
           <> "to see if this generated any work to do in this namespace"
           <> "and " <> IP.makeExample' IP.test <> "to run the tests."
           <> "Or you can use" <> IP.makeExample' IP.undo <> " or"
           <> IP.makeExample' IP.viewReflog <> " to undo the results of this merge."
      ]) (showDiffNamespace ShowNumbers ppe destAbs destAbs diffOutput)

  ShowDiffAfterMergePreview dest' destAbs ppe diffOutput ->
    first (\p -> P.lines [
      P.wrap $ "Here's what would change in " <> prettyPath' dest' <> "after the merge:"
      , ""
      , p
      ]) (showDiffNamespace ShowNumbers ppe destAbs destAbs diffOutput)

  ShowDiffAfterUndo ppe diffOutput ->
    first (\p -> P.lines ["Here are the changes I undid", "", p ])
      (showDiffNamespace ShowNumbers ppe e e diffOutput)

  ShowDiffAfterPull dest' destAbs ppe diff ->
    if OBD.isEmpty diff then
      ("✅  Looks like " <> prettyPath' dest' <> " is up to date.", mempty)
    else
      first (\p -> P.lines [
          P.wrap $ "Here's what's changed in " <> prettyPath' dest' <> "after the pull:", "",
          p, "",
          undoTip
        ])
        (showDiffNamespace ShowNumbers ppe destAbs destAbs diff)
  ShowDiffAfterCreatePR baseRepo headRepo ppe diff ->
    if OBD.isEmpty diff then
      (P.wrap $ "Looks like there's no difference between "
            <> prettyRemoteNamespace baseRepo
            <> "and"
            <> prettyRemoteNamespace headRepo <> "."
      ,mempty)
    else first (\p ->
      (P.lines
        [P.wrap $ "The changes summarized below are available for you to review,"
                 <> "using the following command:"
        ,""
        ,P.indentN 2 $
          IP.makeExampleNoBackticks
            IP.loadPullRequest [(prettyRemoteNamespace baseRepo)
                               ,(prettyRemoteNamespace headRepo)]
        ,""
        ,p])) (showDiffNamespace HideNumbers ppe e e diff)
        -- todo: these numbers aren't going to work,
        --  since the content isn't necessarily here.
        -- Should we have a mode with no numbers? :P

  ShowDiffAfterCreateAuthor authorNS authorPath' bAbs ppe diff ->
    first (\p -> P.lines
      [ p
      , ""
      , tip $ "Add" <> prettyName "License" <> "values for"
           <> prettyName (Name.fromSegment authorNS)
           <> "under" <> P.group (prettyPath' authorPath' <> ".")
      ]) (showDiffNamespace ShowNumbers ppe bAbs bAbs diff)
  where
    e = Path.absoluteEmpty
    undoTip = tip $ "You can use" <> IP.makeExample' IP.undo
                 <> "or" <> IP.makeExample' IP.viewReflog
                 <> "to undo this change."

prettyRemoteNamespace :: (RemoteRepo.RemoteRepo,
                          Maybe ShortBranchHash, Path.Path)
                         -> P.Pretty P.ColorText
prettyRemoteNamespace =
          P.group . P.text . uncurry3 RemoteRepo.printNamespace

notifyUser :: forall v . Var v => FilePath -> Output v -> IO Pretty
notifyUser dir o = case o of
  Success     -> pure $ P.bold "Done."
  WarnIncomingRootBranch current hashes -> pure $
    if null hashes then P.wrap $
      "Please let someone know I generated an empty IncomingRootBranch"
                 <> " event, which shouldn't be possible!"
    else P.lines
      [ P.wrap $ (if length hashes == 1 then "A" else "Some")
         <> "codebase" <> P.plural hashes "root" <> "appeared unexpectedly"
         <> "with" <> P.group (P.plural hashes "hash" <> ":")
      , ""
      , (P.indentN 2 . P.oxfordCommas)
                (map prettySBH $ toList hashes)
      , ""
      , P.wrap $ "and I'm not sure what to do about it."
          <> "The last root namespace hash that I knew about was:"
      , ""
      , P.indentN 2 $ prettySBH current
      , ""
      , P.wrap $ "Now might be a good time to make a backup of your codebase. 😬"
      , ""
      , P.wrap $ "After that, you might try using the" <> makeExample' IP.forkLocal
          <> "command to inspect the namespaces listed above, and decide which"
          <> "one you want as your root."
          <> "You can also use" <> makeExample' IP.viewReflog <> "to see the"
          <> "last few root namespace hashes on record."
      , ""
      , P.wrap $ "Once you find one you like, you can use the"
          <> makeExample' IP.resetRoot <> "command to set it."
      ]
  LoadPullRequest baseNS headNS basePath headPath mergedPath squashedPath -> pure $ P.lines
    [ P.wrap $ "I checked out" <> prettyRemoteNamespace baseNS <> "to" <> P.group (prettyPath' basePath <> ".")
    , P.wrap $ "I checked out" <> prettyRemoteNamespace headNS <> "to" <> P.group (prettyPath' headPath <> ".")
    , ""
    , P.wrap $ "The merged result is in" <> P.group (prettyPath' mergedPath <> ".")
    , P.wrap $ "The (squashed) merged result is in" <> P.group (prettyPath' squashedPath <> ".")
    , P.wrap $ "Use" <>
        IP.makeExample IP.diffNamespace
          [prettyPath' basePath, prettyPath' mergedPath]
      <> "or" <>
        IP.makeExample IP.diffNamespace
          [prettyPath' basePath, prettyPath' squashedPath]
      <> "to see what's been updated."
    , P.wrap $ "Use" <>
        IP.makeExample IP.todo
          [ prettyPath' (snoc mergedPath "patch")
          , prettyPath' mergedPath ]
        <> "to see what work is remaining for the merge."
    , P.wrap $ "Use" <>
        IP.makeExample IP.push
          [prettyRemoteNamespace baseNS, prettyPath' mergedPath] <>
        "or" <>
        IP.makeExample IP.push
          [prettyRemoteNamespace baseNS, prettyPath' squashedPath]
        <> "to push the changes."
    ]

  DisplayDefinitions outputLoc ppe types terms ->
    displayDefinitions outputLoc ppe types terms
  DisplayRendered outputLoc pp ->
    displayRendered outputLoc pp
  DisplayLinks ppe md types terms ->
    if Map.null md then pure $ P.wrap "Nothing to show here. Use the "
      <> IP.makeExample' IP.link <> " command to add links from this definition."
    else
      pure $ intercalateMap "\n\n" go (Map.toList md)
      where
      go (_key, rs) =
        displayDefinitions' ppe (Map.restrictKeys types rs)
                                (Map.restrictKeys terms rs)
  TestResults stats ppe _showSuccess _showFailures oks fails -> case stats of
    CachedTests 0 _ -> pure . P.callout "😶" $ "No tests to run."
    CachedTests n n' | n == n' -> pure $
      P.lines [ cache, "", displayTestResults True ppe oks fails ]
    CachedTests _n m -> pure $
      if m == 0 then "✅  "
      else P.indentN 2 $
           P.lines [ "", cache, "", displayTestResults False ppe oks fails, "", "✅  " ]
      where
    NewlyComputed -> do
      clearCurrentLine
      pure $ P.lines [
        "  " <> P.bold "New test results:",
        "",
        displayTestResults True ppe oks fails ]
    where
      cache = P.bold "Cached test results " <> "(`help testcache` to learn more)"

  TestIncrementalOutputStart ppe (n,total) r _src -> do
    putPretty' $ P.shown (total - n) <> " tests left to run, current test: "
              <> (P.syntaxToColor $ prettyHashQualified (PPE.termName ppe $ Referent.Ref r))
    pure mempty

  TestIncrementalOutputEnd _ppe (_n, _total) _r result -> do
    clearCurrentLine
    if isTestOk result then putPretty' "  ✅  "
    else putPretty' "  🚫  "
    pure mempty

  MetadataMissingType ppe ref -> pure . P.fatalCallout . P.lines $ [
    P.wrap $ "The metadata value " <> P.red (prettyTermName ppe ref)
          <> "is missing a type signature in the codebase.",
    "",
    P.wrap $ "This might be due to pulling an incomplete"
          <> "or invalid codebase, or because files inside the codebase"
          <> "are being deleted external to UCM."
    ]
  MetadataAmbiguous hq _ppe [] -> pure . P.warnCallout .
    P.wrap $ "I couldn't find any metadata matching "
           <> P.syntaxToColor (prettyHashQualified hq)
  MetadataAmbiguous _ ppe refs -> pure . P.warnCallout . P.lines $ [
    P.wrap $ "I'm not sure which metadata value you're referring to"
          <> "since there are multiple matches:",
    "",
    P.indentN 2 $ P.spaced (P.blue . prettyTermName ppe <$> refs),
    "",
    tip "Try again and supply one of the above definitions explicitly."
    ]

  EvaluationFailure err -> pure err
  SearchTermsNotFound hqs | null hqs -> pure mempty
  SearchTermsNotFound hqs ->
    pure
      $  P.warnCallout "The following names were not found in the codebase. Check your spelling."
      <> P.newline
      <> (P.syntaxToColor $ P.indent "  " (P.lines (prettyHashQualified <$> hqs)))
  PatchNotFound _ ->
    pure . P.warnCallout $ "I don't know about that patch."
  NameNotFound _ ->
    pure . P.warnCallout $ "I don't know about that name."
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
  BranchEmpty b -> pure . P.warnCallout . P.wrap $
    P.group (either P.shown prettyPath' b) <> "is an empty namespace."
  BranchNotEmpty path ->
    pure . P.warnCallout $ "I was expecting the namespace " <> prettyPath' path
      <> " to be empty for this operation, but it isn't."
  CantDelete ppe failed failedDependents -> pure . P.warnCallout $
    P.lines [
      P.wrap "I couldn't delete ",
      "", P.indentN 2 $ listOfDefinitions' ppe False failed,
      "",
      "because it's still being used by these definitions:",
      "", P.indentN 2 $ listOfDefinitions' ppe False failedDependents
    ]
  CantUndo reason -> case reason of
    CantUndoPastStart -> pure . P.warnCallout $ "Nothing more to undo."
    CantUndoPastMerge -> pure . P.warnCallout $ "Sorry, I can't undo a merge (not implemented yet)."
  NoMainFunction main ppe ts -> pure . P.callout "😶" $ P.lines [
    P.wrap $ "I looked for a function" <> P.backticked (P.string main)
          <> "in the most recently typechecked file and codebase but couldn't find one. It has to have the type:",
    "",
    P.indentN 2 $ P.lines [ P.string main <> " : " <> TypePrinter.pretty ppe t | t <- ts ]
    ]
  BadMainFunction main ty ppe ts -> pure . P.callout "😶" $ P.lines [
    P.string "I found this function:",
    "",
    P.indentN 2 $ P.string main <> " : " <> TypePrinter.pretty ppe ty,
    "",
    P.wrap $ P.string "but in order for me to" <> P.backticked (P.string "run") <> "it it needs to have the type:",
    "",
    P.indentN 2 $ P.lines [ P.string main <> " : " <> TypePrinter.pretty ppe t | t <- ts ]
    ]
  NoUnisonFile -> do
    dir' <- canonicalizePath dir
    fileName <- renderFileName dir'
    pure . P.callout "😶" $ P.lines
      [ P.wrap "There's nothing for me to add right now."
      , ""
      , P.column2 [(P.bold "Hint:", msg fileName)] ]
    where
    msg dir = P.wrap
      $  "I'm currently watching for definitions in .u files under the"
      <> dir
      <> "directory. Make sure you've updated something there before using the"
      <> makeExample' IP.add <> "or" <> makeExample' IP.update
      <> "commands, or use" <> makeExample' IP.load <> "to load a file explicitly."
  InvalidSourceName name ->
    pure . P.callout "😶" $ P.wrap $  "The file "
                                   <> P.blue (P.shown name)
                                   <> " does not exist or is not a valid source file."
  SourceLoadFailed name ->
    pure . P.callout "😶" $ P.wrap $  "The file "
                                   <> P.blue (P.shown name)
                                   <> " could not be loaded."
  BranchNotFound b ->
    pure . P.warnCallout $ "The namespace " <> P.blue (P.shown b) <> " doesn't exist."
  CreatedNewBranch path -> pure $
    "☝️  The namespace " <> P.blue (P.shown path) <> " is empty."
 -- RenameOutput rootPath oldName newName r -> do
  --   nameChange "rename" "renamed" oldName newName r
  -- AliasOutput rootPath existingName newName r -> do
  --   nameChange "alias" "aliased" existingName newName r
  DeletedEverything ->
    pure . P.wrap . P.lines $
      ["Okay, I deleted everything except the history."
      ,"Use " <> IP.makeExample' IP.undo <> " to undo, or "
        <> IP.makeExample' IP.mergeBuiltins
        <> " to restore the absolute "
        <> "basics to the current path."]
  DeleteEverythingConfirmation ->
    pure . P.warnCallout . P.lines $
      ["Are you sure you want to clear away everything?"
      ,"You could use " <> IP.makeExample' IP.cd
        <> " to switch to a new namespace instead."]
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
  ListOfDefinitions ppe detailed results ->
     listOfDefinitions ppe detailed results
  ListOfLinks ppe results ->
     listOfLinks ppe [ (name,tm) | (name,_ref,tm) <- results ]
  ListNames _len [] [] -> pure . P.callout "😶" $
    P.wrap "I couldn't find anything by that name."
  ListNames len types terms -> pure . P.sepNonEmpty "\n\n" $ [
    formatTypes types, formatTerms terms ]
    where
    formatTerms tms =
      P.lines . P.nonEmpty $ P.plural tms (P.blue "Term") : (go <$> tms) where
      go (ref, hqs) = P.column2
        [ ("Hash:", P.syntaxToColor (prettyReferent len ref))
        , ("Names: ", P.group (P.spaced (P.bold . P.syntaxToColor . prettyHashQualified' <$> toList hqs)))
        ]
    formatTypes types =
      P.lines . P.nonEmpty $ P.plural types (P.blue "Type") : (go <$> types) where
      go (ref, hqs) = P.column2
        [ ("Hash:", P.syntaxToColor (prettyReference len ref))
        , ("Names:", P.group (P.spaced (P.bold . P.syntaxToColor . prettyHashQualified' <$> toList hqs)))
        ]
  -- > names foo
  --   Terms:
  --     Hash: #asdflkjasdflkjasdf
  --     Names: .util.frobnicate foo blarg.mcgee
  --
  --   Term (with hash #asldfkjsdlfkjsdf): .util.frobnicate, foo, blarg.mcgee
  --   Types (with hash #hsdflkjsdfsldkfj): Optional, Maybe, foo
  ListShallow ppe entries -> pure $
    -- todo: make a version of prettyNumberedResult to support 3-columns
    if null entries then P.lit "nothing to show"
    else numberedEntries entries
    where
    numberedEntries :: [ShallowListEntry v a] -> P.Pretty P.ColorText
    numberedEntries entries =
      (P.column3 . fmap f) ([(1::Integer)..] `zip` fmap formatEntry entries)
      where
      f (i, (p1, p2)) = (P.hiBlack . fromString $ show i <> ".", p1, p2)
    formatEntry :: ShallowListEntry v a -> (P.Pretty P.ColorText, P.Pretty P.ColorText)
    formatEntry = \case
      ShallowTermEntry _r hq ot ->
        (P.syntaxToColor . prettyHashQualified' . fmap Name.fromSegment $ hq
        , P.lit "(" <> maybe "type missing" (TypePrinter.pretty ppe) ot <> P.lit ")" )
      ShallowTypeEntry r hq ->
        (P.syntaxToColor . prettyHashQualified' . fmap Name.fromSegment $ hq
        ,isBuiltin r)
      ShallowBranchEntry ns count ->
        ((P.syntaxToColor . prettyName . Name.fromSegment) ns <> "/"
        ,case count of
          1 -> P.lit ("(1 definition)")
          _n -> P.lit "(" <> P.shown count <> P.lit " definitions)")
      ShallowPatchEntry ns ->
        ((P.syntaxToColor . prettyName . Name.fromSegment) ns
        ,P.lit "(patch)")
    isBuiltin = \case
      Reference.Builtin{} -> P.lit "(builtin type)"
      Reference.DerivedId{} -> P.lit "(type)"

  SlurpOutput input ppe s -> let
    isPast = case input of Input.AddI{} -> True
                           Input.UpdateI{} -> True
                           _ -> False
    in pure $ SlurpResult.pretty isPast ppe s

  NoExactTypeMatches ->
    pure . P.callout "☝️" $ P.wrap "I couldn't find exact type matches, resorting to fuzzy matching..."
  TypeParseError src e ->
    pure . P.fatalCallout $ P.lines [
      P.wrap "I couldn't parse the type you supplied:",
      "",
      prettyParseError src e
    ]
  ParseResolutionFailures src es -> pure $
    prettyResolutionFailures src es
  TypeHasFreeVars typ ->
    pure . P.warnCallout $ P.lines [
      P.wrap "The type uses these names, but I'm not sure what they are:",
      P.sep ", " (map (P.text . Var.name) . toList $ ABT.freeVars typ)
    ]
  ParseErrors src es ->
    pure . P.sep "\n\n" $ prettyParseError (Text.unpack src) <$> es
  TypeErrors src ppenv notes -> do
    let showNote =
          intercalateMap "\n\n" (printNoteWithSource ppenv (Text.unpack src))
            . map Result.TypeError
    pure . showNote $ notes
  CompilerBugs src env bugs -> pure $ intercalateMap "\n\n" bug bugs
    where bug = renderCompilerBug env (Text.unpack src)
  Evaluated fileContents ppe bindings watches ->
    if null watches then pure "\n"
    else
      -- todo: hashqualify binding names if necessary to distinguish them from
      --       defs in the codebase.  In some cases it's fine for bindings to
      --       shadow codebase names, but you don't want it to capture them in
      --       the decompiled output.
      let prettyBindings = P.bracket . P.lines $
            P.wrap "The watch expression(s) reference these definitions:" : "" :
            [(P.syntaxToColor $ TermPrinter.prettyBinding ppe (HQ.unsafeFromVar v) b)
            | (v, b) <- bindings]
          prettyWatches = P.sep "\n\n" [
            watchPrinter fileContents ppe ann kind evald isCacheHit |
            (ann,kind,evald,isCacheHit) <-
              sortOn (\(a,_,_,_)->a) . toList $ watches ]
      -- todo: use P.nonempty
      in pure $ if null bindings then prettyWatches
                else prettyBindings <> "\n" <> prettyWatches

  DisplayConflicts termNamespace typeNamespace ->
    pure $ P.sepNonEmpty "\n\n" [
      showConflicts "terms" terms,
      showConflicts "types" types
      ]
    where
    terms    = R.dom termNamespace
    types    = R.dom typeNamespace
    showConflicts :: Foldable f => Pretty -> f Name -> Pretty
    showConflicts thingsName things =
      if (null things) then mempty
      else P.lines [
        "These " <> thingsName <> " have conflicts: ", "",
        P.lines [ ("  " <> prettyName x) | x <- toList things ]
        ]
    -- TODO: Present conflicting TermEdits and TypeEdits
    -- if we ever allow users to edit hashes directly.
  Typechecked sourceName ppe slurpResult uf -> do
    let fileStatusMsg = SlurpResult.pretty False ppe slurpResult
    let containsWatchExpressions = notNull $ UF.watchComponents uf
    if UF.nonEmpty uf then do
      fileName <- renderFileName $ Text.unpack sourceName
      pure $ P.linesNonEmpty ([
        if fileStatusMsg == mempty then
          P.okCallout $ fileName <> " changed."
        else if  SlurpResult.isAllDuplicates slurpResult then
          P.wrap $ "I found and"
             <> P.bold "typechecked" <> "the definitions in "
             <> P.group (fileName <> ".")
             <> "This file " <> P.bold "has been previously added" <> "to the codebase."
        else
          P.linesSpaced $ [
            P.wrap $ "I found and"
             <> P.bold "typechecked" <> "these definitions in "
             <> P.group (fileName <> ".")
             <> "If you do an "
             <> IP.makeExample' IP.add
             <> " or "
             <> P.group (IP.makeExample' IP.update <> ",")
             <> "here's how your codebase would"
             <> "change:"
            , P.indentN 2 $ SlurpResult.pretty False ppe slurpResult
            ]
        ] ++ if containsWatchExpressions then [
          "",
          P.wrap $ "Now evaluating any watch expressions"
                 <> "(lines starting with `>`)... "
                 <> P.group (P.hiBlack "Ctrl+C cancels.")
          ] else [])
    else if (null $ UF.watchComponents uf) then pure . P.wrap $
      "I loaded " <> P.text sourceName <> " and didn't find anything."
    else pure mempty

  TodoOutput names todo -> pure (todoOutput names todo)
  GitError input e -> pure $ case e of
    CouldntParseRootBranch repo s -> P.wrap $ "I couldn't parse the string"
      <> P.red (P.string s) <> "into a namespace hash, when opening the repository at"
      <> P.group (prettyRepoBranch repo <> ".")
    NoGit -> P.wrap $
      "I couldn't find git. Make sure it's installed and on your path."
    CloneException repo msg -> P.wrap $
      "I couldn't clone the repository at" <> prettyRepoBranch repo <> ";"
      <> "the error was:" <> (P.indentNAfterNewline 2 . P.group . P.string) msg
    PushNoOp repo -> P.wrap $
      "The repository at" <> prettyRepoBranch repo <> "is already up-to-date."
    PushException repo msg -> P.wrap $
      "I couldn't push to the repository at" <> prettyRepoRevision repo <> ";"
      <> "the error was:" <> (P.indentNAfterNewline 2 . P.group . P.string) msg
    UnrecognizableCacheDir uri localPath -> P.wrap $ "A cache directory for"
      <> P.backticked (P.text uri) <> "already exists at"
      <> P.backticked' (P.string localPath) "," <> "but it doesn't seem to"
      <> "be a git repository, so I'm not sure what to do next.  Delete it?"
    UnrecognizableCheckoutDir uri localPath -> P.wrap $ "I tried to clone"
      <> P.backticked (P.text uri) <> "into a cache directory at"
      <> P.backticked' (P.string localPath) "," <> "but I can't recognize the"
      <> "result as a git repository, so I'm not sure what to do next."
    PushDestinationHasNewStuff repo ->
      P.callout "⏸" . P.lines $ [
      P.wrap $ "The repository at" <> prettyRepoRevision repo
            <> "has some changes I don't know about.",
      "",
      P.wrap $ "If you want to " <> push <> "you can do:", "",
       P.indentN 2 pull, "",
       P.wrap $
         "to merge these changes locally," <>
         "then try your" <> push <> "again."
      ]
      where
      push = P.group . P.backticked . P.string . IP1.patternName $ IP.patternFromInput input
      pull = P.group . P.backticked $ IP.inputStringFromInput input
    CouldntLoadRootBranch repo hash -> P.wrap
      $ "I couldn't load the designated root hash"
      <> P.group ("(" <> fromString (Hash.showBase32Hex hash) <> ")")
      <> "from the repository at" <> prettyRepoRevision repo
    NoRemoteNamespaceWithHash repo sbh -> P.wrap
      $ "The repository at" <> prettyRepoRevision repo
      <> "doesn't contain a namespace with the hash prefix"
      <> (P.blue . P.text . SBH.toText) sbh
    RemoteNamespaceHashAmbiguous repo sbh hashes -> P.lines [
      P.wrap $ "The namespace hash" <> prettySBH sbh
            <> "at" <> prettyRepoRevision repo
            <> "is ambiguous."
            <> "Did you mean one of these hashes?",
      "",
      P.indentN 2 $ P.lines
        (prettySBH . SBH.fromHash ((Text.length . SBH.toText) sbh * 2)
          <$> Set.toList hashes),
      "",
      P.wrap "Try again with a few more hash characters to disambiguate."
      ]
    SomeOtherError msg -> P.callout "‼" . P.lines $ [
      P.wrap "I ran into an error:", "",
      P.indentN 2 (P.string msg), "",
      P.wrap $ "Check the logging messages above for more info."
      ]
  ListEdits patch ppe -> do
    let
      types = Patch._typeEdits patch
      terms = Patch._termEdits patch

      prettyTermEdit (r, TermEdit.Deprecate) =
        (P.syntaxToColor . prettyHashQualified . PPE.termName ppe . Referent.Ref $ r
        , "-> (deprecated)")
      prettyTermEdit (r, TermEdit.Replace r' _typing) =
        (P.syntaxToColor . prettyHashQualified . PPE.termName ppe . Referent.Ref $ r
        , "-> " <> (P.syntaxToColor . prettyHashQualified . PPE.termName ppe . Referent.Ref $ r'))
      prettyTypeEdit (r, TypeEdit.Deprecate) =
        (P.syntaxToColor . prettyHashQualified $ PPE.typeName ppe r
        , "-> (deprecated)")
      prettyTypeEdit (r, TypeEdit.Replace r') =
        (P.syntaxToColor . prettyHashQualified $ PPE.typeName ppe r
        , "-> " <> (P.syntaxToColor . prettyHashQualified . PPE.typeName ppe $ r'))
    pure $ P.sepNonEmpty "\n\n" [
      if R.null types then mempty
      else "Edited Types:" `P.hang`
              P.column2 (prettyTypeEdit <$> R.toList types),
      if R.null terms then mempty
      else "Edited Terms:" `P.hang`
              P.column2 (prettyTermEdit <$> R.toList terms),
      if R.null types && R.null terms then "This patch is empty."
      else tip . P.string $ "To remove entries from a patch, use "
           <> IP.deleteTermReplacementCommand <> " or "
           <> IP.deleteTypeReplacementCommand <> ", as appropriate."
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
        ([],[]) -> error "BustedBuiltins busted, as there were no busted builtins."
        ([], old) ->
          P.wrap ("This codebase includes some builtins that are considered deprecated. Use the " <> makeExample' IP.updateBuiltins <> " command when you're ready to work on eliminating them from your codebase:")
            : ""
            : fmap (P.text . Reference.toText) old
        (new, []) -> P.wrap ("This version of Unison provides builtins that are not part of your codebase. Use " <> makeExample' IP.updateBuiltins <> " to add them:")
          : "" : fmap (P.text . Reference.toText) new
        (new@(_:_), old@(_:_)) ->
          [ P.wrap
            ("Sorry and/or good news!  This version of Unison supports a different set of builtins than this codebase uses.  You can use "
            <> makeExample' IP.updateBuiltins
            <> " to add the ones you're missing and deprecate the ones I'm missing. 😉"
            )
          , "You're missing:" `P.hang` P.lines (fmap (P.text . Reference.toText) new)
          , "I'm missing:" `P.hang` P.lines (fmap (P.text . Reference.toText) old)
          ]
  ListOfPatches patches -> pure $
    if null patches then P.lit "nothing to show"
    else numberedPatches patches
    where
    numberedPatches :: Set Name -> P.Pretty P.ColorText
    numberedPatches patches =
      (P.column2 . fmap format) ([(1::Integer)..] `zip` (toList patches))
      where
      format (i, p) = (P.hiBlack . fromString $ show i <> ".", prettyName p)
  ConfiguredMetadataParseError p md err ->
    pure . P.fatalCallout . P.lines $
      [ P.wrap $ "I couldn't understand the default metadata that's set for "
        <> prettyPath' p <> " in .unisonConfig."
      , P.wrap $ "The value I found was"
        <> (P.backticked . P.blue . P.string) md
        <> "but I encountered the following error when trying to parse it:"
      , ""
      , err
      ]
  NoConfiguredGitUrl pp p ->
    pure . P.fatalCallout . P.wrap $
      "I don't know where to " <>
        pushPull "push to!" "pull from!" pp <>
          (if Path.isRoot' p then ""
           else "Add a line like `GitUrl." <> P.shown p
                <> " = <some-git-url>' to .unisonConfig. "
          )
          <> "Type `help " <> pushPull "push" "pull" pp <>
          "` for more information."

--  | ConfiguredGitUrlParseError PushPull Path' Text String
  ConfiguredGitUrlParseError pp p url err ->
    pure . P.fatalCallout . P.lines $
      [ P.wrap $ "I couldn't understand the GitUrl that's set for"
          <> prettyPath' p <> "in .unisonConfig"
      , P.wrap $ "The value I found was" <> (P.backticked . P.blue . P.text) url
        <> "but I encountered the following error when trying to parse it:"
      , ""
      , P.string err
      , ""
      , P.wrap $ "Type" <> P.backticked ("help " <> pushPull "push" "pull" pp)
        <> "for more information."
      ]
--  | ConfiguredGitUrlIncludesShortBranchHash ShortBranchHash
  ConfiguredGitUrlIncludesShortBranchHash pp repo sbh remotePath ->
    pure . P.lines $
    [ P.wrap
    $ "The `GitUrl.` entry in .unisonConfig for the current path has the value"
    <> (P.group . (<>",") . P.blue . P.text)
        (RemoteRepo.printNamespace repo (Just sbh) remotePath)
    <> "which specifies a namespace hash"
    <> P.group (P.blue (prettySBH sbh) <> ".")
    , ""
    , P.wrap $
      pushPull "I can't push to a specific hash, because it's immutable."
      ("It's no use for repeated pulls,"
      <> "because you would just get the same immutable namespace each time.")
      pp
    , ""
    , P.wrap $ "You can use"
    <> P.backticked (
        pushPull "push" "pull" pp
        <> " "
        <> P.text (RemoteRepo.printNamespace repo Nothing remotePath))
    <> "if you want to" <> pushPull "push onto" "pull from" pp
    <> "the latest."
    ]
  NoBranchWithHash _h -> pure . P.callout "😶" $
    P.wrap $ "I don't know of a namespace with that hash."
  NotImplemented -> pure $ P.wrap "That's not implemented yet. Sorry! 😬"
  BranchAlreadyExists p -> pure . P.wrap $
    "The namespace" <> prettyPath' p <> "already exists."
  LabeledReferenceNotFound hq ->
    pure . P.callout "\129300" . P.wrap . P.syntaxToColor $
      "Sorry, I couldn't find anything named" <> prettyHashQualified hq <> "."
  LabeledReferenceAmbiguous hashLen hq (LD.partition -> (tps, tms)) ->
    pure . P.callout "\129300" . P.lines $ [
      P.wrap "That name is ambiguous. It could refer to any of the following definitions:"
    , ""
    , P.indentN 2 (P.lines (map qualifyTerm tms ++ map qualifyType tps))
    ]
    where
      qualifyTerm :: Referent -> P.Pretty P.ColorText
      qualifyTerm = P.syntaxToColor . case hq of
        HQ.NameOnly n -> prettyNamedReferent hashLen n
        HQ.HashQualified n _ -> prettyNamedReferent hashLen n
        HQ.HashOnly _ -> prettyReferent hashLen
      qualifyType :: Reference -> P.Pretty P.ColorText
      qualifyType = P.syntaxToColor . case hq of
        HQ.NameOnly n -> prettyNamedReference hashLen n
        HQ.HashQualified n _ -> prettyNamedReference hashLen n
        HQ.HashOnly _ -> prettyReference hashLen
  DeleteNameAmbiguous hashLen p tms tys ->
    pure . P.callout "\129300" . P.lines $ [
      P.wrap "That name is ambiguous. It could refer to any of the following definitions:"
    , ""
    , P.indentN 2 (P.lines (map qualifyTerm (Set.toList tms) ++ map qualifyType (Set.toList tys)))
    , ""
    , P.wrap "You may:"
    , ""
    , P.indentN 2 . P.bulleted $
        [ P.wrap "Delete one by an unambiguous name, given above."
        , P.wrap "Delete them all by re-issuing the previous command."
        ]
    ]
    where
      name :: Name
      name = Path.toName' (HQ'.toName (Path.unsplitHQ' p))
      qualifyTerm :: Referent -> P.Pretty P.ColorText
      qualifyTerm = P.syntaxToColor . prettyNamedReferent hashLen name
      qualifyType :: Reference -> P.Pretty P.ColorText
      qualifyType = P.syntaxToColor . prettyNamedReference hashLen name
  TermAmbiguous _ _ -> pure "That term is ambiguous."
  HashAmbiguous h rs -> pure . P.callout "\129300" . P.lines $ [
    P.wrap $ "The hash" <> prettyShortHash h <> "is ambiguous."
           <> "Did you mean one of these hashes?",
    "",
    P.indentN 2 $ P.lines (P.shown <$> Set.toList rs),
    "",
    P.wrap "Try again with a few more hash characters to disambiguate."
    ]
  BranchHashAmbiguous h rs -> pure . P.callout "\129300" . P.lines $ [
    P.wrap $ "The namespace hash" <> prettySBH h <> "is ambiguous."
           <> "Did you mean one of these hashes?",
    "",
    P.indentN 2 $ P.lines (prettySBH <$> Set.toList rs),
    "",
    P.wrap "Try again with a few more hash characters to disambiguate."
    ]
  BadName n ->
    pure . P.wrap $ P.string n <> " is not a kind of name I understand."
  TermNotFound' sh ->
    pure $ "I could't find a term with hash "
         <> (prettyShortHash sh)
  TypeNotFound' sh ->
    pure $ "I could't find a type with hash "
         <> (prettyShortHash sh)
  NothingToPatch _patchPath dest -> pure $
    P.callout "😶" . P.wrap
       $ "This had no effect. Perhaps the patch has already been applied"
      <> "or it doesn't intersect with the definitions in"
      <> P.group (prettyPath' dest <> ".")
  PatchNeedsToBeConflictFree ->
    pure . P.wrap $
      "I tried to auto-apply the patch, but couldn't because it contained"
      <> "contradictory entries."
  PatchInvolvesExternalDependents _ _ ->
    pure "That patch involves external dependents."
  ShowReflog [] ->  pure . P.warnCallout $ "The reflog appears to be empty!"
  ShowReflog entries -> pure $
    P.lines [
    P.wrap $ "Here is a log of the root namespace hashes,"
          <> "starting with the most recent,"
          <> "along with the command that got us there."
          <> "Try:",
    "",
    -- `head . tail` is safe: entries never has 1 entry, and [] is handled above
    let e2 = head . tail $ entries in
    P.indentN 2 . P.wrapColumn2 $ [
      (IP.makeExample IP.forkLocal ["2", ".old"],
        ""),
      (IP.makeExample IP.forkLocal [prettySBH . Output.hash $ e2, ".old"],
       "to make an old namespace accessible again,"),
      (mempty,mempty),
      (IP.makeExample IP.resetRoot [prettySBH . Output.hash $ e2],
        "to reset the root namespace and its history to that of the specified"
         <> "namespace.")
    ],
    "",
    P.numberedList . fmap renderEntry $ entries
    ]
    where
    renderEntry :: Output.ReflogEntry -> P.Pretty CT.ColorText
    renderEntry (Output.ReflogEntry hash reason) = P.wrap $
      P.blue (prettySBH hash) <> " : " <> P.text reason
  History _cap history tail -> pure $
    P.lines [
      note $ "The most recent namespace hash is immediately below this message.", "",
      P.sep "\n\n" [ go h diff | (h,diff) <- reverse history ], "",
      tailMsg
      ]
    where
    tailMsg = case tail of
      E.EndOfLog h -> P.lines [
        "□ " <> prettySBH h <> " (start of history)"
        ]
      E.MergeTail h hs -> P.lines [
        P.wrap $ "This segment of history starts with a merge." <> ex,
        "",
        "⊙ " <> prettySBH h,
        "⑃",
        P.lines (prettySBH <$> hs)
        ]
      E.PageEnd h _n -> P.lines [
        P.wrap $ "There's more history before the versions shown here." <> ex, "",
        dots, "",
        "⊙ " <> prettySBH h,
        ""
        ]
    dots = "⠇"
    go hash diff = P.lines [
      "⊙ " <> prettySBH hash,
      "",
      P.indentN 2 $ prettyDiff diff
      ]
    ex = "Use" <> IP.makeExample IP.history ["#som3n4m3space"]
               <> "to view history starting from a given namespace hash."
  StartOfCurrentPathHistory -> pure $
    P.wrap "You're already at the very beginning! 🙂"
  PullAlreadyUpToDate ns dest -> pure . P.callout "😶" $
    P.wrap $ prettyPath' dest <> "was already up-to-date with"
          <> P.group (prettyRemoteNamespace ns <> ".")

  MergeAlreadyUpToDate src dest -> pure . P.callout "😶" $
    P.wrap $ prettyPath' dest <> "was already up-to-date with"
          <> P.group (prettyPath' src <> ".")
  PreviewMergeAlreadyUpToDate src dest -> pure . P.callout "😶" $
    P.wrap $ prettyPath' dest <> "is already up-to-date with"
          <> P.group (prettyPath' src <> ".")
  DumpNumberedArgs args -> pure . P.numberedList $ fmap P.string args
  NoConflictsOrEdits ->
    pure (P.okCallout "No conflicts or edits in progress.")
  NoOp -> pure $ P.string "I didn't make any changes."
  DefaultMetadataNotification -> pure $ P.wrap "I added some default metadata."
  DumpBitBooster head map -> let
    go output []          = output
    go output (head : queue) = case Map.lookup head map of
      Nothing -> go (renderLine head [] : output) queue
      Just tails -> go (renderLine head tails : output) (queue ++ tails)
      where
      renderHash = take 10 . Text.unpack . Hash.base32Hex . Causal.unRawHash
      renderLine head tail =
        (renderHash head) ++ "|" ++ intercalateMap " " renderHash tail ++
          case Map.lookup (Hash.base32Hex . Causal.unRawHash $ head) tags of
            Just t -> "|tag: " ++ t
            Nothing -> ""
      -- some specific hashes that we want to label in the output
      tags :: Map Text String
      tags = Map.fromList . fmap swap $
        [ ("unisonbase 2019/8/6",  "54s9qjhaonotuo4sp6ujanq7brngk32f30qt5uj61jb461h9fcca6vv5levnoo498bavne4p65lut6k6a7rekaruruh9fsl19agu8j8")
        , ("unisonbase 2019/8/5",  "focmbmg7ca7ht7opvjaqen58fobu3lijfa9adqp7a1l1rlkactd7okoimpfmd0ftfmlch8gucleh54t3rd1e7f13fgei86hnsr6dt1g")
        , ("unisonbase 2019/7/31", "jm2ltsg8hh2b3c3re7aru6e71oepkqlc3skr2v7bqm4h1qgl3srucnmjcl1nb8c9ltdv56dpsgpdur1jhpfs6n5h43kig5bs4vs50co")
        , ("unisonbase 2019/7/25", "an1kuqsa9ca8tqll92m20tvrmdfk0eksplgjbda13evdlngbcn5q72h8u6nb86ojr7cvnemjp70h8cq1n95osgid1koraq3uk377g7g")
        , ("ucm m1b", "o6qocrqcqht2djicb1gcmm5ct4nr45f8g10m86bidjt8meqablp0070qae2tvutnvk4m9l7o1bkakg49c74gduo9eati20ojf0bendo")
        , ("ucm m1, m1a", "auheev8io1fns2pdcnpf85edsddj27crpo9ajdujum78dsncvfdcdu5o7qt186bob417dgmbd26m8idod86080bfivng1edminu3hug")
        ]

    in pure $ P.lines [
      P.lines (fmap fromString . reverse . nubOrd $ go [] [head]),
      "",
      "Paste that output into http://bit-booster.com/graph.html"
      ]
  ListDependents hqLength ld names missing -> pure $
    if names == mempty && missing == mempty
    then c (prettyLabeledDependency hqLength ld) <> " doesn't have any dependents."
    else
      "Dependents of " <> c (prettyLabeledDependency hqLength ld) <> ":\n\n" <>
      (P.indentN 2 (P.numberedColumn2Header num pairs))
    where
    num n = P.hiBlack $ P.shown n <> "."
    header = (P.hiBlack "Reference", P.hiBlack "Name")
    pairs = header : (fmap (first c . second c) $
        [ (p $ Reference.toShortHash r, prettyName n) | (n, r) <- names ] ++
        [ (p $ Reference.toShortHash r, "(no name available)") | r <- toList missing ])
    p = prettyShortHash . SH.take hqLength
    c = P.syntaxToColor
  -- this definition is identical to the previous one, apart from the word
  -- "Dependencies", but undecided about whether or how to refactor
  ListDependencies hqLength ld names missing -> pure $
    if names == mempty && missing == mempty
    then c (prettyLabeledDependency hqLength ld) <> " doesn't have any dependencies."
    else
      "Dependencies of " <> c (prettyLabeledDependency hqLength ld) <> ":\n\n" <>
      (P.indentN 2 (P.numberedColumn2Header num pairs))
    where
    num n = P.hiBlack $ P.shown n <> "."
    header = (P.hiBlack "Reference", P.hiBlack "Name")
    pairs = header : (fmap (first c . second c) $
        [ (p $ Reference.toShortHash r, prettyName n) | (n, r) <- names ] ++
        [ (p $ Reference.toShortHash r, "(no name available)") | r <- toList missing ])
    p = prettyShortHash . SH.take hqLength
    c = P.syntaxToColor
  DumpUnisonFileHashes hqLength datas effects terms ->
    pure . P.syntaxToColor . P.lines $
      (effects <&> \(n,r) -> "ability " <>
        prettyHashQualified' (HQ'.take hqLength . HQ'.fromNamedReference n $ Reference.DerivedId r)) <>
      (datas <&> \(n,r) -> "type " <>
        prettyHashQualified' (HQ'.take hqLength . HQ'.fromNamedReference n $ Reference.DerivedId r)) <>
      (terms <&> \(n,r) ->
        prettyHashQualified' (HQ'.take hqLength . HQ'.fromNamedReference n $ Reference.DerivedId r))

  where
  _nameChange _cmd _pastTenseCmd _oldName _newName _r = error "todo"
  -- do
  --   when (not . Set.null $ E.changedSuccessfully r) . putPrettyLn . P.okCallout $
  --     P.wrap $ "I" <> pastTenseCmd <> "the"
  --       <> ns (E.changedSuccessfully r)
  --       <> P.blue (prettyName oldName)
  --       <> "to" <> P.group (P.green (prettyName newName) <> ".")
  --   when (not . Set.null $ E.oldNameConflicted r) . putPrettyLn . P.warnCallout $
  --     (P.wrap $ "I couldn't" <> cmd <> "the"
  --          <> ns (E.oldNameConflicted r)
  --          <> P.blue (prettyName oldName)
  --          <> "to" <> P.green (prettyName newName)
  --          <> "because of conflicts.")
  --     <> "\n\n"
  --     <> tip ("Use " <> makeExample' IP.todo <> " to view more information on conflicts and remaining work.")
  --   when (not . Set.null $ E.newNameAlreadyExists r) . putPrettyLn . P.warnCallout $
  --     (P.wrap $ "I couldn't" <> cmd <> P.blue (prettyName oldName)
  --          <> "to" <> P.green (prettyName newName)
  --          <> "because the "
  --          <> ns (E.newNameAlreadyExists r)
  --          <> "already exist(s).")
  --     <> "\n\n"
  --     <> tip
  --        ("Use" <> makeExample IP.rename [prettyName newName, "<newname>"] <> "to make" <> prettyName newName <> "available.")
--    where
--      ns targets = P.oxfordCommas $
--        map (fromString . Names.renderNameTarget) (toList targets)

prettyPath' :: Path.Path' -> Pretty
prettyPath' p' =
  if Path.isCurrentPath p'
  then "the current namespace"
  else P.blue (P.shown p')

prettyRelative :: Path.Relative -> Pretty
prettyRelative = P.blue . P.shown

prettySBH :: IsString s => ShortBranchHash -> P.Pretty s
prettySBH hash = P.group $ "#" <> P.text (SBH.toText hash)

formatMissingStuff :: (Show tm, Show typ) =>
  [(HQ.HashQualified, tm)] -> [(HQ.HashQualified, typ)] -> Pretty
formatMissingStuff terms types =
  (unlessM (null terms) . P.fatalCallout $
    P.wrap "The following terms have a missing or corrupted type signature:"
    <> "\n\n"
    <> P.column2 [ (P.syntaxToColor $ prettyHashQualified name, fromString (show ref)) | (name, ref) <- terms ]) <>
  (unlessM (null types) . P.fatalCallout $
    P.wrap "The following types weren't found in the codebase:"
    <> "\n\n"
    <> P.column2 [ (P.syntaxToColor $ prettyHashQualified name, fromString (show ref)) | (name, ref) <- types ])

displayDefinitions' :: Var v => Ord a1
  => PPE.PrettyPrintEnvDecl
  -> Map Reference.Reference (DisplayThing (DD.Decl v a1))
  -> Map Reference.Reference (DisplayThing (Term v a1))
  -> Pretty
displayDefinitions' ppe0 types terms = P.syntaxToColor $ P.sep "\n\n" (prettyTypes <> prettyTerms)
  where
  ppeBody r = PPE.declarationPPE ppe0 r
  ppeDecl = PPE.unsuffixifiedPPE ppe0
  prettyTerms = map go . Map.toList
             -- sort by name
             $ Map.mapKeys (first (PPE.termName ppeDecl . Referent.Ref) . dupe) terms
  prettyTypes = map go2 . Map.toList
              $ Map.mapKeys (first (PPE.typeName ppeDecl) . dupe) types
  go ((n, r), dt) =
    case dt of
      MissingThing r -> missing n r
      BuiltinThing -> builtin n
      RegularThing tm -> TermPrinter.prettyBinding (ppeBody r) n tm
  go2 ((n, r), dt) =
    case dt of
      MissingThing r -> missing n r
      BuiltinThing -> builtin n
      RegularThing decl -> case decl of
        Left d  -> DeclPrinter.prettyEffectDecl (ppeBody r) r n d
        Right d -> DeclPrinter.prettyDataDecl (ppeBody r) r n d
  builtin n = P.wrap $ "--" <> prettyHashQualified n <> " is built-in."
  missing n r = P.wrap (
    "-- The name " <> prettyHashQualified n <> " is assigned to the "
    <> "reference " <> fromString (show r ++ ",")
    <> "which is missing from the codebase.")
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
        if exists then readFile path
        else pure ""
      writeFile path . Text.pack . P.toPlain 80 $
        P.lines [ pp, "", P.text existingContents ]
    message pp path =
      P.callout "☝️" $ P.lines [
        P.wrap $ "I added this to the top of " <> fromString path,
        "",
        P.indentN 2 pp
      ]

displayDefinitions :: Var v => Ord a1 =>
  Maybe FilePath
  -> PPE.PrettyPrintEnvDecl
  -> Map Reference.Reference (DisplayThing (DD.Decl v a1))
  -> Map Reference.Reference (DisplayThing (Term v a1))
  -> IO Pretty
displayDefinitions _outputLoc _ppe types terms | Map.null types && Map.null terms =
  pure $ P.callout "😶" "No results to display."
displayDefinitions outputLoc ppe types terms =
  maybe displayOnly scratchAndDisplay outputLoc
  where
  displayOnly = pure code
  scratchAndDisplay path = do
    path' <- canonicalizePath path
    prependToFile code path'
    pure (message code path')
    where
    prependToFile code path = do
      existingContents <- do
        exists <- doesFileExist path
        if exists then readFile path
        else pure ""
      writeFile path . Text.pack . P.toPlain 80 $
        P.lines [ code, ""
                , "---- " <> "Anything below this line is ignored by Unison."
                , "", P.text existingContents ]
    message code path =
      P.callout "☝️" $ P.lines [
        P.wrap $ "I added these definitions to the top of " <> fromString path,
        "",
        P.indentN 2 code,
        "",
        P.wrap $
          "You can edit them there, then do" <> makeExample' IP.update <>
          "to replace the definitions currently in this namespace."
      ]
  code = displayDefinitions' ppe types terms

displayTestResults :: Bool -- whether to show the tip
                   -> PPE.PrettyPrintEnv
                   -> [(Reference, Text)]
                   -> [(Reference, Text)]
                   -> Pretty
displayTestResults showTip ppe oksUnsorted failsUnsorted = let
  oks   = Name.sortByText fst [ (name r, msg) | (r, msg) <- oksUnsorted ]
  fails = Name.sortByText fst [ (name r, msg) | (r, msg) <- failsUnsorted ]
  name r = HQ.toText $ PPE.termName ppe (Referent.Ref r)
  okMsg =
    if null oks then mempty
    else P.column2 [ (P.green "◉ " <> P.text r, "  " <> P.green (P.text msg)) | (r, msg) <- oks ]
  okSummary =
    if null oks then mempty
    else "✅ " <> P.bold (P.num (length oks)) <> P.green " test(s) passing"
  failMsg =
    if null fails then mempty
    else P.column2 [ (P.red "✗ " <> P.text r, "  " <> P.red (P.text msg)) | (r, msg) <- fails ]
  failSummary =
    if null fails then mempty
    else "🚫 " <> P.bold (P.num (length fails)) <> P.red " test(s) failing"
  tipMsg =
    if not showTip || (null oks && null fails) then mempty
    else tip $ "Use " <> P.blue ("view " <> P.text (fst $ head (fails ++ oks)))
            <> "to view the source of a test."
  in if null oks && null fails then "😶 No tests available."
     else P.sep "\n\n" . P.nonEmpty $ [
          okMsg, failMsg,
          P.sep ", " . P.nonEmpty $ [failSummary, okSummary], tipMsg]

unsafePrettyTermResultSig' :: Var v =>
  PPE.PrettyPrintEnv -> SR'.TermResult' v a -> Pretty
unsafePrettyTermResultSig' ppe = \case
  SR'.TermResult' (HQ'.toHQ -> name) (Just typ) _r _aliases ->
    head (TypePrinter.prettySignatures' ppe [(name,typ)])
  _ -> error "Don't pass Nothing"

-- produces:
-- -- #5v5UtREE1fTiyTsTK2zJ1YNqfiF25SkfUnnji86Lms#0
-- Optional.None, Maybe.Nothing : Maybe a
unsafePrettyTermResultSigFull' :: Var v =>
  PPE.PrettyPrintEnv -> SR'.TermResult' v a -> Pretty
unsafePrettyTermResultSigFull' ppe = \case
  SR'.TermResult' (HQ'.toHQ -> hq) (Just typ) r (Set.map HQ'.toHQ -> aliases) ->
   P.lines
    [ P.hiBlack "-- " <> greyHash (HQ.fromReferent r)
    , P.group $
      P.commas (fmap greyHash $ hq : toList aliases) <> " : "
      <> (P.syntaxToColor $ TypePrinter.pretty0 ppe mempty (-1) typ)
    , mempty
    ]
  _ -> error "Don't pass Nothing"
  where greyHash = styleHashQualified' id P.hiBlack

prettyTypeResultHeader' :: Var v => SR'.TypeResult' v a -> Pretty
prettyTypeResultHeader' (SR'.TypeResult' (HQ'.toHQ -> name) dt r _aliases) =
  prettyDeclTriple (name, r, dt)

-- produces:
-- -- #5v5UtREE1fTiyTsTK2zJ1YNqfiF25SkfUnnji86Lms
-- type Optional
-- type Maybe
prettyTypeResultHeaderFull' :: Var v => SR'.TypeResult' v a -> Pretty
prettyTypeResultHeaderFull' (SR'.TypeResult' (HQ'.toHQ -> name) dt r (Set.map HQ'.toHQ -> aliases)) =
  P.lines stuff <> P.newline
  where
  stuff =
    (P.hiBlack "-- " <> greyHash (HQ.fromReference r)) :
      fmap (\name -> prettyDeclTriple (name, r, dt))
           (name : toList aliases)
    where greyHash = styleHashQualified' id P.hiBlack

prettyDeclTriple :: Var v =>
  (HQ.HashQualified, Reference.Reference, DisplayThing (DD.Decl v a))
  -> Pretty
prettyDeclTriple (name, _, displayDecl) = case displayDecl of
   BuiltinThing -> P.hiBlack "builtin " <> P.hiBlue "type " <> P.blue (P.syntaxToColor $ prettyHashQualified name)
   MissingThing _ -> mempty -- these need to be handled elsewhere
   RegularThing decl -> case decl of
     Left ed -> P.syntaxToColor $ DeclPrinter.prettyEffectHeader name ed
     Right dd   -> P.syntaxToColor $ DeclPrinter.prettyDataHeader name dd

prettyDeclPair :: Var v =>
  PPE.PrettyPrintEnv -> (Reference, DisplayThing (DD.Decl v a))
  -> Pretty
prettyDeclPair ppe (r, dt) = prettyDeclTriple (PPE.typeName ppe r, r, dt)

renderNameConflicts :: Set.Set Name -> Set.Set Name -> Pretty
renderNameConflicts conflictedTypeNames conflictedTermNames =
  unlessM (null allNames) $ P.callout "❓" . P.sep "\n\n" . P.nonEmpty $ [
    showConflictedNames "types" conflictedTypeNames,
    showConflictedNames "terms" conflictedTermNames,
    tip $ "This occurs when merging branches that both independently introduce the same name. Use "
        <> makeExample IP.view (prettyName <$> take 3 allNames)
        <> "to see the conflicting defintions, then use "
        <> makeExample' (if (not . null) conflictedTypeNames
                         then IP.renameType else IP.renameTerm)
        <> "to resolve the conflicts."
  ]
  where
    allNames = toList (conflictedTermNames <> conflictedTypeNames)
    showConflictedNames things conflictedNames =
      unlessM (Set.null conflictedNames) $
        P.wrap ("These" <> P.bold (things <> "have conflicting definitions:"))
        `P.hang` P.commas (P.blue . prettyName <$> toList conflictedNames)

renderEditConflicts ::
  PPE.PrettyPrintEnv -> Patch -> Pretty
renderEditConflicts ppe Patch{..} =
  unlessM (null editConflicts) . P.callout "❓" . P.sep "\n\n" $ [
    P.wrap $ "These" <> P.bold "definitions were edited differently"
          <> "in namespaces that have been merged into this one."
          <> "You'll have to tell me what to use as the new definition:",
    P.indentN 2 (P.lines (formatConflict <$> editConflicts))
--    , tip $ "Use " <> makeExample IP.resolve [name (head editConflicts), " <replacement>"] <> " to pick a replacement." -- todo: eventually something with `edit`
    ]
  where
    -- todo: could possibly simplify all of this, but today is a copy/paste day.
    editConflicts :: [Either (Reference, Set TypeEdit.TypeEdit) (Reference, Set TermEdit.TermEdit)]
    editConflicts =
      (fmap Left . Map.toList . R.toMultimap . R.filterManyDom $ _typeEdits) <>
      (fmap Right . Map.toList . R.toMultimap . R.filterManyDom $ _termEdits)
    typeName r = styleHashQualified P.bold (PPE.typeName ppe r)
    termName r = styleHashQualified P.bold (PPE.termName ppe (Referent.Ref r))
    formatTypeEdits (r, toList -> es) = P.wrap $
      "The type" <> typeName r <> "was" <>
      (if TypeEdit.Deprecate `elem` es
      then "deprecated and also replaced with"
      else "replaced with") <>
      P.oxfordCommas [ typeName r | TypeEdit.Replace r <- es ]
    formatTermEdits (r, toList -> es) = P.wrap $
      "The term" <> termName r <> "was" <>
      (if TermEdit.Deprecate `elem` es
      then "deprecated and also replaced with"
      else "replaced with") <>
      P.oxfordCommas [ termName r | TermEdit.Replace r _ <- es ]
    formatConflict = either formatTypeEdits formatTermEdits

type Numbered = State.State (Int, Seq.Seq String)

todoOutput :: Var v => PPE.PrettyPrintEnvDecl -> TO.TodoOutput v a -> Pretty
todoOutput ppe todo =
  todoConflicts <> todoEdits
  where
  ppeu = PPE.unsuffixifiedPPE ppe
  ppes = PPE.suffixifiedPPE ppe
  (frontierTerms, frontierTypes) = TO.todoFrontier todo
  (dirtyTerms, dirtyTypes) = TO.todoFrontierDependents todo
  corruptTerms =
    [ (PPE.termName ppeu (Referent.Ref r), r) | (r, Nothing) <- frontierTerms ]
  corruptTypes =
    [ (PPE.typeName ppeu r, r) | (r, MissingThing _) <- frontierTypes ]
  goodTerms ts =
    [ (PPE.termName ppeu (Referent.Ref r), typ) | (r, Just typ) <- ts ]
  todoConflicts = if TO.noConflicts todo then mempty else P.lines . P.nonEmpty $
    [ renderEditConflicts ppeu (TO.editConflicts todo)
    , renderNameConflicts conflictedTypeNames conflictedTermNames ]
    where
    -- If a conflict is both an edit and a name conflict, we show it in the edit
    -- conflicts section
    c :: Names0
    c = removeEditConflicts (TO.editConflicts todo) (TO.nameConflicts todo)
    conflictedTypeNames = (R.dom . Names.types) c
    conflictedTermNames = (R.dom . Names.terms) c
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
    removeEditConflicts :: Ord n => Patch -> Names' n -> Names' n
    removeEditConflicts Patch{..} Names{..} = Names terms' types' where
      terms' = R.filterRan (`Set.notMember` conflictedTermEditTargets) terms
      types' = R.filterRan (`Set.notMember` conflictedTypeEditTargets) types
      conflictedTypeEditTargets :: Set Reference
      conflictedTypeEditTargets =
        Set.fromList $ toList (R.ran typeEditConflicts) >>= TypeEdit.references
      conflictedTermEditTargets :: Set Referent.Referent
      conflictedTermEditTargets =
        Set.fromList . fmap Referent.Ref
          $ toList (R.ran termEditConflicts) >>= TermEdit.references
      typeEditConflicts = R.filterDom (`R.manyDom` _typeEdits) _typeEdits
      termEditConflicts = R.filterDom (`R.manyDom` _termEdits) _termEdits


  todoEdits = unlessM (TO.noEdits todo) . P.callout "🚧" . P.sep "\n\n" . P.nonEmpty $
      [ P.wrap ("The namespace has" <> fromString (show (TO.todoScore todo))
              <> "transitive dependent(s) left to upgrade."
              <> "Your edit frontier is the dependents of these definitions:")
      , P.indentN 2 . P.lines $ (
          (prettyDeclPair ppeu <$> toList frontierTypes) ++
          TypePrinter.prettySignatures' ppes (goodTerms frontierTerms)
          )
      , P.wrap "I recommend working on them in the following order:"
      , P.numberedList $
          let unscore (_score,a,b) = (a,b)
          in (prettyDeclPair ppeu . unscore <$> toList dirtyTypes) ++
             TypePrinter.prettySignatures'
                ppes
                (goodTerms $ unscore <$> dirtyTerms)
      , formatMissingStuff corruptTerms corruptTypes
      ]

listOfDefinitions ::
  Var v => PPE.PrettyPrintEnv -> E.ListDetailed -> [SR'.SearchResult' v a] -> IO Pretty
listOfDefinitions ppe detailed results =
  pure $ listOfDefinitions' ppe detailed results

listOfLinks ::
  Var v => PPE.PrettyPrintEnv -> [(HQ.HashQualified, Maybe (Type v a))] -> IO Pretty
listOfLinks _ [] = pure . P.callout "😶" . P.wrap $
  "No results. Try using the " <>
  IP.makeExample IP.link [] <>
  "command to add metadata to a definition."
listOfLinks ppe results = pure $ P.lines [
    P.numberedColumn2 num [
    (P.syntaxToColor $ prettyHashQualified hq, ": " <> prettyType typ) | (hq,typ) <- results
    ], "",
    tip $ "Try using" <> IP.makeExample IP.display ["1"]
       <> "to display the first result or"
       <> IP.makeExample IP.view ["1"] <> "to view its source."
    ]
  where
  num i = P.hiBlack $ P.shown i <> "."
  prettyType Nothing = "❓ (missing a type for this definition)"
  prettyType (Just t) = TypePrinter.pretty ppe t

data ShowNumbers = ShowNumbers | HideNumbers
-- | `ppe` is just for rendering type signatures
--   `oldPath, newPath :: Path.Absolute` are just for producing fully-qualified
--                                       numbered args
showDiffNamespace :: forall v . Var v
                  => ShowNumbers
                  -> PPE.PrettyPrintEnv
                  -> Path.Absolute
                  -> Path.Absolute
                  -> OBD.BranchDiffOutput v Ann
                  -> (Pretty, NumberedArgs)
showDiffNamespace _ _ _ _ diffOutput | OBD.isEmpty diffOutput =
  ("The namespaces are identical.", mempty)
showDiffNamespace sn ppe oldPath newPath OBD.BranchDiffOutput{..} =
  (P.sepNonEmpty "\n\n" p, toList args)
  where
  (p, (menuSize, args)) = (`State.runState` (0::Int, Seq.empty)) $ sequence [
    if (not . null) newTypeConflicts
    || (not . null) newTermConflicts
    then do
      prettyUpdatedTypes :: [Pretty] <- traverse prettyUpdateType newTypeConflicts
      prettyUpdatedTerms :: [Pretty] <- traverse prettyUpdateTerm newTermConflicts
      pure $ P.sepNonEmpty "\n\n"
        [ P.red "New name conflicts:"
        , P.indentN 2 . P.sepNonEmpty "\n\n" $ prettyUpdatedTypes <> prettyUpdatedTerms
        ]
    else pure mempty
   ,if (not . null) resolvedTypeConflicts
    || (not . null) resolvedTermConflicts
    then do
      prettyUpdatedTypes :: [Pretty] <- traverse prettyUpdateType resolvedTypeConflicts
      prettyUpdatedTerms :: [Pretty] <- traverse prettyUpdateTerm resolvedTermConflicts
      pure $ P.sepNonEmpty "\n\n"
        [ P.bold "Resolved name conflicts:"
        , P.indentN 2 . P.sepNonEmpty "\n\n" $ prettyUpdatedTypes <> prettyUpdatedTerms
        ]
    else pure mempty
   ,if (not . null) updatedTypes
    || (not . null) updatedTerms
    || propagatedUpdates > 0
    || (not . null) updatedPatches
    then do
      prettyUpdatedTypes :: [Pretty] <- traverse prettyUpdateType updatedTypes
      prettyUpdatedTerms :: [Pretty] <- traverse prettyUpdateTerm updatedTerms
      prettyUpdatedPatches :: [Pretty] <- traverse (prettySummarizePatch newPath) updatedPatches
      pure $ P.sepNonEmpty "\n\n"
        [ P.bold "Updates:"
        , P.indentNonEmptyN 2 . P.sepNonEmpty "\n\n" $ prettyUpdatedTypes <> prettyUpdatedTerms
        , if propagatedUpdates > 0
          then P.indentN 2
                $ P.wrap (P.hiBlack $ "There were "
                                   <> P.shown propagatedUpdates
                                   <> "auto-propagated updates.")
          else mempty
        , P.indentNonEmptyN 2 . P.linesNonEmpty $ prettyUpdatedPatches
        ]
    else pure mempty
   ,if (not . null) addedTypes
    || (not . null) addedTerms
    || (not . null) addedPatches
    then do
      prettyAddedTypes :: Pretty <- prettyAddTypes addedTypes
      prettyAddedTerms :: Pretty <- prettyAddTerms addedTerms
      prettyAddedPatches :: [Pretty] <- traverse (prettySummarizePatch newPath) addedPatches
      pure $ P.sepNonEmpty "\n\n"
        [ P.bold "Added definitions:"
        , P.indentNonEmptyN 2 $ P.linesNonEmpty [prettyAddedTypes, prettyAddedTerms]
        , P.indentNonEmptyN 2 $ P.lines prettyAddedPatches
        ]
    else pure mempty
   ,if (not . null) removedTypes
    || (not . null) removedTerms
    || (not . null) removedPatches
    then do
      prettyRemovedTypes :: Pretty <- prettyRemoveTypes removedTypes
      prettyRemovedTerms :: Pretty <- prettyRemoveTerms removedTerms
      prettyRemovedPatches :: [Pretty] <- traverse (prettyNamePatch oldPath) removedPatches
      pure $ P.sepNonEmpty "\n\n"
       [ P.bold "Removed definitions:"
       , P.indentN 2 $ P.linesNonEmpty [ prettyRemovedTypes
                                       , prettyRemovedTerms
                                       , P.linesNonEmpty prettyRemovedPatches ]
       ]
    else pure mempty
   ,if (not . null) renamedTypes
    || (not . null) renamedTerms
    then do
      results <- prettyRenameGroups renamedTypes renamedTerms
      pure $ P.sepNonEmpty "\n\n"
        [ P.bold "Name changes:"
        , P.indentN 2 . P.sepNonEmpty "\n\n" $ results
        ]
        -- todo: change separator to just '\n' here if all the results are 1 to 1
    else pure mempty
   ]

  {- new implementation
    23. X  ┐  =>  (added)   24. X'
    25. X2 ┘      (removed) 26. X2
  -}
  prettyRenameGroups :: [OBD.RenameTypeDisplay v a]
                     -> [OBD.RenameTermDisplay v a]
                     -> Numbered [Pretty]
  prettyRenameGroups types terms =
    (<>) <$> traverse (prettyGroup . (over (_1._1) Referent.Ref))
                      (types `zip` [0..])
         <*> traverse prettyGroup (terms `zip` [length types ..])
    where
        leftNamePad :: Int = foldl1' max $
          map (foldl1' max . map HQ'.nameLength . toList . view _3) terms <>
          map (foldl1' max . map HQ'.nameLength . toList . view _3) types
        prettyGroup :: ((Referent, b, Set HQ'.HashQualified, Set HQ'.HashQualified), Int)
                    -> Numbered Pretty
        prettyGroup ((r, _, olds, news),i) = let
          -- [ "peach  ┐"
          -- , "peach' ┘"]
          olds' :: [Numbered Pretty] =
            map (\(oldhq, oldp) -> numHQ' oldPath oldhq r <&> (\n -> n <> " " <> oldp))
              . (zip (toList olds))
              . P.boxRight
              . map (P.rightPad leftNamePad . phq')
              $ toList olds

          added' = toList $ Set.difference news olds
          removed' = toList $ Set.difference olds news
          -- [ "(added)   24. X'"
          -- , "(removed) 26. X2"
          -- ]

          news' :: [Numbered Pretty] =
            map (number addedLabel) added' ++ map (number removedLabel) removed'
            where
            addedLabel   = "(added)"
            removedLabel = "(removed)"
            number label name =
              numHQ' newPath name r <&>
                (\num -> num <> " " <> phq' name <> " " <> label)

          buildTable :: [Numbered Pretty] -> [Numbered Pretty] -> Numbered Pretty
          buildTable lefts rights = let
            hlefts = if i == 0 then pure (P.bold "Original") : lefts
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
  prettyUpdateType (Nothing, mdUps) =
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
  prettyUpdateType (Just olds, news) =
    do
      olds <- traverse (mdTypeLine oldPath) [ (name,r,decl,mempty) | (name,r,decl) <- olds ]
      news <- traverse (mdTypeLine newPath) news
      let (oldnums, olddatas) = unzip olds
      let (newnums, newdatas) = unzip news
      pure . P.column2 $
        zip (oldnums <> [""] <> newnums)
            (P.boxLeft olddatas <> [downArrow] <> P.boxLeft newdatas)

  {-
  13. ┌ability Yyz         (+1 metadata)
  14. └ability copies.Yyz  (+2 metadata)
  -}
  prettyAddTypes :: [OBD.AddedTypeDisplay v a] -> Numbered Pretty
  prettyAddTypes = fmap P.lines . traverse prettyGroup where
    prettyGroup :: OBD.AddedTypeDisplay v a -> Numbered Pretty
    prettyGroup (hqmds, r, odecl) = do
      pairs <- traverse (prettyLine r odecl) hqmds
      let (nums, decls) = unzip pairs
      let boxLeft = case hqmds of _:_:_ -> P.boxLeft; _ -> id
      pure . P.column2 $ zip nums (boxLeft decls)
    prettyLine :: Reference -> Maybe (DD.DeclOrBuiltin v a) -> (HQ'.HashQualified, [OBD.MetadataDisplay v a]) -> Numbered (Pretty, Pretty)
    prettyLine r odecl (hq, mds) = do
      n <- numHQ' newPath hq (Referent.Ref r)
      pure . (n,) $ prettyDecl hq odecl <> case length mds of
        0 -> mempty
        c -> " (+" <> P.shown c <> " metadata)"

  prettyAddTerms :: [OBD.AddedTermDisplay v a] -> Numbered Pretty
  prettyAddTerms = fmap (P.column3 . mconcat) . traverse prettyGroup . reorderTerms where
    reorderTerms = sortOn (not . Referent.isConstructor . view _2)
    prettyGroup :: OBD.AddedTermDisplay v a -> Numbered [(Pretty, Pretty, Pretty)]
    prettyGroup (hqmds, r, otype) = do
      pairs <- traverse (prettyLine r otype) hqmds
      let (nums, names, decls) = unzip3 pairs
          boxLeft = case hqmds of _:_:_ -> P.boxLeft; _ -> id
      pure $ zip3 nums (boxLeft names) decls
    prettyLine r otype (hq, mds) = do
      n <- numHQ' newPath hq r
      pure . (n, phq' hq, ) $ ": " <> prettyType otype <> case length mds of
        0 -> mempty
        c -> " (+" <> P.shown c <> " metadata)"

  prettySummarizePatch, prettyNamePatch :: Path.Absolute -> OBD.PatchDisplay -> Numbered Pretty
  --  12. patch p (added 3 updates, deleted 1)
  prettySummarizePatch prefix (name, patchDiff) = do
    n <- numPatch prefix name
    let addCount = (R.size . view Patch.addedTermEdits) patchDiff +
                   (R.size . view Patch.addedTypeEdits) patchDiff
        delCount = (R.size . view Patch.removedTermEdits) patchDiff +
                   (R.size . view Patch.removedTypeEdits) patchDiff
        messages =
          (if addCount > 0 then ["added " <> P.shown addCount] else []) ++
          (if delCount > 0 then ["deleted " <> P.shown addCount] else [])
        message = case messages of
          [] -> mempty
          x : ys -> " (" <> P.commas (x <> " updates" : ys) <> ")"
    pure $ n <> P.bold " patch " <> prettyName name <> message
  --	18. patch q
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
  prettyRemoveTypes :: [OBD.RemovedTypeDisplay v a] -> Numbered Pretty
  prettyRemoveTypes = fmap P.lines . traverse prettyGroup where
    prettyGroup :: OBD.RemovedTypeDisplay v a -> Numbered Pretty
    prettyGroup (hqs, r, odecl) = do
      lines <- traverse (prettyLine r odecl) hqs
      let (nums, decls) = unzip lines
          boxLeft = case hqs of _:_:_ -> P.boxLeft; _ -> id
      pure . P.column2 $ zip nums (boxLeft decls)
    prettyLine r odecl hq = do
      n <- numHQ' newPath hq (Referent.Ref r)
      pure (n, prettyDecl hq odecl)

  prettyRemoveTerms :: [OBD.RemovedTermDisplay v a] -> Numbered Pretty
  prettyRemoveTerms = fmap (P.column3 . mconcat) . traverse prettyGroup . reorderTerms where
    reorderTerms = sortOn (not . Referent.isConstructor . view _2)
    prettyGroup :: OBD.RemovedTermDisplay v a -> Numbered [(Pretty, Pretty, Pretty)]
    prettyGroup ([], r, _) =
      error $ "trying to remove " <> show r <> " without any names."
    prettyGroup (hq1:hqs, r, otype) = do
      line1 <- prettyLine1 r otype hq1
      lines <- traverse (prettyLine r) hqs
      let (nums, names, decls) = unzip3 (line1:lines)
          boxLeft = case hqs of _:_ -> P.boxLeft; _ -> id
      pure $ zip3 nums (boxLeft names) decls
    prettyLine1 r otype hq = do
      n <- numHQ' newPath hq r
      pure (n, phq' hq, ": " <> prettyType otype)
    prettyLine r hq = do
      n <- numHQ' newPath hq r
      pure (n, phq' hq, mempty)

  downArrow = P.bold "↓"
  mdTypeLine :: Path.Absolute -> OBD.TypeDisplay v a -> Numbered (Pretty, Pretty)
  mdTypeLine p (hq, r, odecl, mddiff) = do
    n <- numHQ' p hq (Referent.Ref r)
    fmap ((n,) . P.linesNonEmpty) . sequence $
      [ pure $ prettyDecl hq odecl
      , P.indentN leftNumsWidth <$> prettyMetadataDiff mddiff ]

  -- + 2. MIT               : License
  -- - 3. AllRightsReserved : License
  mdTermLine :: Path.Absolute -> Int -> OBD.TermDisplay v a -> Numbered (Pretty, Pretty)
  mdTermLine p namesWidth (hq, r, otype, mddiff) = do
    n <- numHQ' p hq r
    fmap ((n,) . P.linesNonEmpty) . sequence $
      [ pure $ P.rightPad namesWidth (phq' hq) <> " : " <> prettyType otype
      , prettyMetadataDiff mddiff ]
      -- , P.indentN 2 <$> prettyMetadataDiff mddiff ]

  prettyUpdateTerm :: OBD.UpdateTermDisplay v a -> Numbered Pretty
  prettyUpdateTerm (Nothing, newTerms) =
    if null newTerms then error "Super invalid UpdateTermDisplay" else
    fmap P.column2 $ traverse (mdTermLine newPath namesWidth) newTerms
    where namesWidth = foldl1' max $ fmap (HQ'.nameLength . view _1) newTerms
  prettyUpdateTerm (Just olds, news) =
    fmap P.column2 $ do
      olds <- traverse (mdTermLine oldPath namesWidth) [ (name,r,typ,mempty) | (name,r,typ) <- olds ]
      news <- traverse (mdTermLine newPath namesWidth) news
      let (oldnums, olddatas) = unzip olds
      let (newnums, newdatas) = unzip news
      pure $ zip (oldnums <> [""] <> newnums)
                 (P.boxLeft olddatas <> [downArrow] <> P.boxLeft newdatas)
    where namesWidth = foldl1' max $ fmap (HQ'.nameLength . view _1) news
                                   <> fmap (HQ'.nameLength . view _1) olds

  prettyMetadataDiff :: OBD.MetadataDiff (OBD.MetadataDisplay v a) -> Numbered Pretty
  prettyMetadataDiff OBD.MetadataDiff{..} = P.column2M $
    map (elem oldPath "- ") removedMetadata <>
    map (elem newPath "+ ") addedMetadata
    where
    elem p x (hq, r, otype) = do
      num <- numHQ p hq r
      pure (x <> num <> " " <> phq hq, ": " <> prettyType otype)

  prettyType = maybe (P.red "type not found") (TypePrinter.pretty ppe)
  prettyDecl hq =
    maybe (P.red "type not found")
          (P.syntaxToColor . DeclPrinter.prettyDeclOrBuiltinHeader (HQ'.toHQ hq))
  phq' :: _ -> Pretty = P.syntaxToColor . prettyHashQualified'
  phq  :: _ -> Pretty = P.syntaxToColor . prettyHashQualified
  --
  -- DeclPrinter.prettyDeclHeader : HQ -> Either
  numPatch :: Path.Absolute -> Name -> Numbered Pretty
  numPatch prefix name =
    addNumberedArg . Name.toString . Name.makeAbsolute $ Path.prefixName prefix name

  numHQ :: Path.Absolute -> HQ.HashQualified -> Referent -> Numbered Pretty
  numHQ prefix hq r = addNumberedArg (HQ.toString hq')
    where
    hq' = HQ.requalify (fmap (Name.makeAbsolute . Path.prefixName prefix) hq) r

  numHQ' :: Path.Absolute -> HQ'.HashQualified -> Referent -> Numbered Pretty
  numHQ' prefix hq r = addNumberedArg (HQ'.toString hq')
    where
    hq' = HQ'.requalify (fmap (Name.makeAbsolute . Path.prefixName prefix) hq) r

  addNumberedArg :: String -> Numbered Pretty
  addNumberedArg s = case sn of
   ShowNumbers -> do
    (n, args) <- State.get
    State.put (n+1, args Seq.|> s)
    pure $ padNumber (n+1)
   HideNumbers -> pure mempty

  padNumber :: Int -> Pretty
  padNumber n = P.hiBlack . P.rightPad leftNumsWidth $ P.shown n <> "."

  leftNumsWidth = length (show menuSize) + length ("."  :: String)

noResults :: Pretty
noResults = P.callout "😶" $
    P.wrap $ "No results. Check your spelling, or try using tab completion "
          <> "to supply command arguments."

listOfDefinitions' :: Var v
                   => PPE.PrettyPrintEnv -- for printing types of terms :-\
                   -> E.ListDetailed
                   -> [SR'.SearchResult' v a]
                   -> Pretty
listOfDefinitions' ppe detailed results =
  if null results then noResults
  else P.lines . P.nonEmpty $ prettyNumberedResults :
    [formatMissingStuff termsWithMissingTypes missingTypes
    ,unlessM (null missingBuiltins) . bigproblem $ P.wrap
      "I encountered an inconsistency in the codebase; these definitions refer to built-ins that this version of unison doesn't know about:" `P.hang`
        P.column2 ( (P.bold "Name", P.bold "Built-in")
                  -- : ("-", "-")
                  : fmap (bimap (P.syntaxToColor . prettyHashQualified)
                                (P.text . Referent.toText)) missingBuiltins)
    ]
  where
  prettyNumberedResults = P.numberedList prettyResults
  -- todo: group this by namespace
  prettyResults =
    map (SR'.foldResult' renderTerm renderType)
        (filter (not.missingType) results)
    where
      (renderTerm, renderType) =
        if detailed then
          (unsafePrettyTermResultSigFull' ppe, prettyTypeResultHeaderFull')
        else
          (unsafePrettyTermResultSig' ppe, prettyTypeResultHeader')
  missingType (SR'.Tm _ Nothing _ _)          = True
  missingType (SR'.Tp _ (MissingThing _) _ _) = True
  missingType _                             = False
  -- termsWithTypes = [(name,t) | (name, Just t) <- sigs0 ]
  --   where sigs0 = (\(name, _, typ) -> (name, typ)) <$> terms
  termsWithMissingTypes =
    [ (HQ'.toHQ name, r)
    | SR'.Tm name Nothing (Referent.Ref (Reference.DerivedId r)) _ <- results ]
  missingTypes = nubOrdOn snd $
    [ (HQ'.toHQ name, Reference.DerivedId r)
    | SR'.Tp name (MissingThing r) _ _ <- results ] <>
    [ (HQ'.toHQ name, r)
    | SR'.Tm name Nothing (Referent.toTypeReference -> Just r) _ <- results]
  missingBuiltins = results >>= \case
    SR'.Tm name Nothing r@(Referent.Ref (Reference.Builtin _)) _ -> [(HQ'.toHQ name,r)]
    _ -> []

watchPrinter
  :: Var v
  => Text
  -> PPE.PrettyPrintEnv
  -> Ann
  -> UF.WatchKind
  -> Term v ()
  -> Runtime.IsCacheHit
  -> Pretty
watchPrinter src ppe ann kind term isHit =
  P.bracket
    $ let
        lines        = Text.lines src
        lineNum      = fromMaybe 1 $ startingLine ann
        lineNumWidth = length (show lineNum)
        extra        = "     " <> replicate (length kind) ' ' -- for the ` | > ` after the line number
        line         = lines !! (lineNum - 1)
        addCache p = if isHit then p <> " (cached)" else p
        renderTest (Term.App' (Term.Constructor' _ id) (Term.Text' msg)) =
          "\n" <> if id == DD.okConstructorId
            then addCache
              (P.green "✅ " <> P.bold "Passed" <> P.green (P.text msg'))
            else if id == DD.failConstructorId
              then addCache
                (P.red "🚫 " <> P.bold "FAILED" <> P.red (P.text msg'))
              else P.red "❓ " <> TermPrinter.pretty ppe term
            where
              msg' = if Text.take 1 msg == " " then msg
                     else " " <> msg

        renderTest x =
          fromString $ "\n Unison bug: " <> show x <> " is not a test."
      in
        P.lines
          [ fromString (show lineNum) <> " | " <> P.text line
          , case (kind, term) of
            (UF.TestWatch, Term.Sequence' tests) -> foldMap renderTest tests
            _ -> P.lines
              [ fromString (replicate lineNumWidth ' ')
              <> fromString extra
              <> (if isHit then id else P.purple) "⧩"
              , P.indentN (lineNumWidth + length extra)
              . (if isHit then id else P.bold)
              $ TermPrinter.pretty ppe term
              ]
          ]

filestatusTip :: Pretty
filestatusTip = tip "Use `help filestatus` to learn more."

prettyDiff :: Names.Diff -> Pretty
prettyDiff diff = let
  orig = Names.originalNames diff
  adds = Names.addedNames diff
  removes = Names.removedNames diff

  addedTerms = [ (n,r) | (n,r) <- R.toList (Names.terms0 adds)
                       , not $ R.memberRan r (Names.terms0 removes) ]
  addedTypes = [ (n,r) | (n,r) <- R.toList (Names.types0 adds)
                       , not $ R.memberRan r (Names.types0 removes) ]
  added = sort (hqTerms ++ hqTypes)
    where
      hqTerms = [ Names.hqName adds n (Right r) | (n, r) <- addedTerms ]
      hqTypes = [ Names.hqName adds n (Left r)  | (n, r) <- addedTypes ]

  removedTerms = [ (n,r) | (n,r) <- R.toList (Names.terms0 removes)
                         , not $ R.memberRan r (Names.terms0 adds)
                         , Set.notMember n addedTermsSet ] where
    addedTermsSet = Set.fromList (map fst addedTerms)
  removedTypes = [ (n,r) | (n,r) <- R.toList (Names.types0 removes)
                         , not $ R.memberRan r (Names.types0 adds)
                         , Set.notMember n addedTypesSet ] where
    addedTypesSet = Set.fromList (map fst addedTypes)
  removed = sort (hqTerms ++ hqTypes)
    where
      hqTerms = [ Names.hqName removes n (Right r) | (n, r) <- removedTerms ]
      hqTypes = [ Names.hqName removes n (Left r)  | (n, r) <- removedTypes ]

  movedTerms = [ (n,n2) | (n,r) <- R.toList (Names.terms0 removes)
                        , n2 <- toList (R.lookupRan r (Names.terms adds)) ]
  movedTypes = [ (n,n2) | (n,r) <- R.toList (Names.types removes)
                        , n2 <- toList (R.lookupRan r (Names.types adds)) ]
  moved = Name.sortNamed fst . nubOrd $ (movedTerms <> movedTypes)

  copiedTerms = List.multimap [
    (n,n2) | (n2,r) <- R.toList (Names.terms0 adds)
           , not (R.memberRan r (Names.terms0 removes))
           , n <- toList (R.lookupRan r (Names.terms0 orig)) ]
  copiedTypes = List.multimap [
    (n,n2) | (n2,r) <- R.toList (Names.types0 adds)
           , not (R.memberRan r (Names.types0 removes))
           , n <- toList (R.lookupRan r (Names.types0 orig)) ]
  copied = Name.sortNamed fst $
    Map.toList (Map.unionWith (<>) copiedTerms copiedTypes)
  in
  P.sepNonEmpty "\n\n" [
     if not $ null added then
       P.lines [
         -- todo: split out updates
         P.green "+ Adds / updates:", "",
         P.indentN 2 . P.wrap $
           P.sep " " (P.syntaxToColor . prettyHashQualified' <$> added)
       ]
     else mempty,
     if not $ null removed then
       P.lines [
         P.hiBlack "- Deletes:", "",
         P.indentN 2 . P.wrap $
           P.sep " " (P.syntaxToColor . prettyHashQualified' <$> removed)
       ]
     else mempty,
     if not $ null moved then
       P.lines [
         P.purple "> Moves:", "",
         P.indentN 2 $
           P.column2 $
             (P.hiBlack "Original name", P.hiBlack "New name") :
             [ (prettyName n,prettyName n2) | (n, n2) <- moved ]
       ]
     else mempty,
     if not $ null copied then
       P.lines [
         P.yellow "= Copies:", "",
         P.indentN 2 $
           P.column2 $
             (P.hiBlack "Original name", P.hiBlack "New name(s)") :
             [ (prettyName n, P.sep " " (prettyName <$> ns))
             | (n, ns) <- copied ]
       ]
     else mempty
   ]

prettyTermName :: PPE.PrettyPrintEnv -> Referent -> Pretty
prettyTermName ppe r = P.syntaxToColor $
  prettyHashQualified (PPE.termName ppe r)

prettyRepoRevision :: RemoteRepo -> Pretty
prettyRepoRevision (RemoteRepo.GitRepo url treeish) =
  P.blue (P.text url) <> prettyRevision treeish
  where
  prettyRevision treeish =
    Monoid.fromMaybe $
      treeish <&> \treeish -> "at revision" <> P.blue (P.text treeish)

prettyRepoBranch :: RemoteRepo -> Pretty
prettyRepoBranch (RemoteRepo.GitRepo url treeish) =
  P.blue (P.text url) <> prettyRevision treeish
  where
  prettyRevision treeish =
    Monoid.fromMaybe $
      treeish <&> \treeish -> "at branch" <> P.blue (P.text treeish)

isTestOk :: Term v Ann -> Bool
isTestOk tm = case tm of
  Term.Sequence' ts -> all isSuccess ts where
    isSuccess (Term.App' (Term.Constructor' ref cid) _) =
      cid == DD.okConstructorId &&
      ref == DD.testResultRef
    isSuccess _ = False
  _ -> False
