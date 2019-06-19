{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- {-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}


module Unison.CommandLine.OutputMessages2 (notifyUser) where

import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Output       as E
import Unison.Codebase.Editor.SlurpResult (SlurpResult(..))


--import Debug.Trace
import           Control.Monad                 (when, unless, join)
import           Data.Bifunctor                (bimap, first)
import           Data.Foldable                 (toList, traverse_)
import           Data.List                     (sortOn)
import           Data.List.Extra               (nubOrdOn)
import           Data.ListLike                 (ListLike)
import           Data.Maybe                    (fromMaybe)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Set                      (Set)
import           Data.String                   (IsString, fromString)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Data.Text.IO                  (readFile, writeFile)
import           Data.Tuple.Extra              (dupe)
import           Prelude                       hiding (readFile, writeFile)
import qualified System.Console.ANSI           as Console
import           System.Directory              (canonicalizePath, doesFileExist)
import qualified Unison.UnisonFile             as UF
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase.GitError
import           Unison.Codebase.Branch        (Branch0)
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.Codebase.Patch         as Patch
import           Unison.Codebase.Patch         (Patch(..))
import qualified Unison.Codebase.TermEdit      as TermEdit
import qualified Unison.Codebase.TypeEdit      as TypeEdit
import           Unison.CommandLine            (
                                                -- backtick, backtickEOS,
                                                bigproblem,
                                                putPrettyLn,
                                                putPrettyLn',
                                                tip,
                                                -- warn,
                                                -- watchPrinter,
                                                -- plural
                                                )
import           Unison.CommandLine.InputPatterns (makeExample, makeExample')
import qualified Unison.CommandLine.InputPatterns as IP
import qualified Unison.DataDeclaration        as DD
import qualified Unison.DeclPrinter            as DeclPrinter
import qualified Unison.HashQualified          as HQ
import           Unison.Name                   (Name)
import qualified Unison.Name                   as Name
import           Unison.NamePrinter            (prettyHashQualified,
                                                -- prettyHashQualified',
                                                prettyName,
                                                styleHashQualified,
                                                styleHashQualified')
import           Unison.Names2                 (Names'(..), Names, Names0)
import qualified Unison.Names2                 as Names
import           Unison.Parser                 (Ann, startingLine)
import qualified Unison.PrettyPrintEnv         as PPE
import qualified Unison.Codebase.Runtime       as Runtime
import           Unison.PrintError              ( prettyParseError
                                                , renderNoteAsANSI
                                                )
import qualified Unison.Reference              as Reference
import           Unison.Reference              ( Reference )
import qualified Unison.Referent               as Referent
import qualified Unison.Result                 as Result
import qualified Unison.Term                   as Term
import           Unison.Term                   (AnnotatedTerm)
import qualified Unison.TermPrinter            as TermPrinter
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.Typechecker            as Typechecker
import qualified Unison.TypePrinter            as TypePrinter
import qualified Unison.Util.ColorText         as CT
import           Unison.Util.Monoid             ( intercalateMap
                                                , unlessM
                                                )
import qualified Unison.Util.Pretty            as P
import qualified Unison.Util.Relation          as R
import           Unison.Var                    (Var)
import qualified Unison.Codebase.Editor.SlurpResult as SlurpResult

notifyUser :: forall v . Var v => FilePath -> Output v -> IO ()
notifyUser dir o = case o of
  -- Success (MergeBranchI _ _) ->
  --   putPrettyLn $ P.bold "Merged. " <> "Here's what's " <> makeExample' IP.todo <> " after the merge:"
  Success _    -> putPrettyLn $ P.bold "Done."
  DisplayDefinitions outputLoc ppe types terms ->
    displayDefinitions outputLoc ppe types terms
  DisplayLinks ppe md types terms ->
    if Map.null md then putPrettyLn $ P.wrap "Nothing to show here. Use the `link` command to add links from this definition."
    else
      putPrettyLn $ intercalateMap "\n\n" go (Map.toList md)
      where
      go (key, rs) = P.lines [
        P.bold (prettyHashQualified $ PPE.termName ppe key),
        "",
        P.indentN 2 $ displayDefinitions' ppe (Map.restrictKeys types rs)
                                              (Map.restrictKeys terms rs)
        ]
  LinkFailure input -> putPrettyLn . P.warnCallout . P.shown $ input
  TermNotFound input _ ->
    putPrettyLn . P.warnCallout $ "I don't know about that term."
  TypeNotFound input _ ->
    putPrettyLn . P.warnCallout $ "I don't know about that type."
  TermAlreadyExists input _ _ ->
    putPrettyLn . P.warnCallout $ "A term by that name already exists."
  TypeAlreadyExists input _ _ ->
    putPrettyLn . P.warnCallout $ "A type by that name already exists."
  CantDelete input names failed failedDependents -> putPrettyLn . P.warnCallout $
    P.lines [
      P.wrap "I couldn't delete ",
      "", P.indentN 2 $ listOfDefinitions' names False failed,
      "",
      "because it's still being used by these definitions:",
      "", P.indentN 2 $ listOfDefinitions' names False failedDependents
    ]
  CantUndo reason -> case reason of
    CantUndoPastStart -> putPrettyLn . P.warnCallout $ "Nothing more to undo."
    CantUndoPastMerge -> putPrettyLn . P.warnCallout $ "Sorry, I can't undo a merge (not implemented yet)."
  NoUnisonFile -> do
    dir' <- canonicalizePath dir
    putPrettyLn . P.callout "😶" $ P.lines
      [ P.wrap "There's nothing for me to add right now."
      , ""
      , P.column2 [(P.bold "Hint:", msg dir')] ]
    where
    msg dir = P.wrap
      $  "I'm currently watching for definitions in .u files under the"
      <> renderFileName dir
      <> "directory. Make sure you've updated something there before using the"
      <> makeExample' IP.add <> "or" <> makeExample' IP.update
      <> "commands."
  BranchNotFound _ b ->
    putPrettyLn . P.warnCallout $ "The branch " <> P.blue (P.shown b) <> " doesn't exist."
  CreatedNewBranch path -> putPrettyLn . P.okCallout $
    "The branch " <> P.blue (P.shown path) <> "is empty."
 -- RenameOutput rootPath oldName newName r -> do
  --   nameChange "rename" "renamed" oldName newName r
  -- AliasOutput rootPath existingName newName r -> do
  --   nameChange "alias" "aliased" existingName newName r
  DeleteBranchConfirmation _uniqueDeletions -> error "todo"
    -- let
    --   pretty (branchName, (ppe, results)) =
    --     header $ listOfDefinitions' ppe False results
    --     where
    --     header = plural uniqueDeletions id ((P.text branchName <> ":") `P.hang`)
    --
    -- in putPrettyLn . P.warnCallout
    --   $ P.wrap ("The"
    --   <> plural uniqueDeletions "branch contains" "branches contain"
    --   <> "definitions that don't exist in any other branches:")
    --   <> P.border 2 (mconcat (fmap pretty uniqueDeletions))
    --   <> P.newline
    --   <> P.wrap "Please repeat the same command to confirm the deletion."
  ListOfDefinitions names detailed results ->
     listOfDefinitions names detailed results
  SlurpOutput _input ppe s ->
    putPrettyLn $
      SlurpResult.pretty ppe s <> "\n\n" <>
      filestatusTip

  ParseErrors src es -> do
    Console.setTitle "Unison ☹︎"
    traverse_ (putStrLn . CT.toANSI . prettyParseError (Text.unpack src)) es
  TypeErrors src ppenv notes -> do
    Console.setTitle "Unison ☹︎"
    let showNote =
          intercalateMap "\n\n" (renderNoteAsANSI ppenv (Text.unpack src))
            . map Result.TypeError
    putStrLn . showNote $ notes
  Evaluated fileContents ppe bindings watches ->
    if null watches then putStrLn ""
    else
      -- todo: hashqualify binding names if necessary to distinguish them from
      --       defs in the codebase.  In some cases it's fine for bindings to
      --       shadow codebase names, but you don't want it to capture them in
      --       the decompiled output.
      let prettyBindings = P.bracket . P.lines $
            P.wrap "The watch expression(s) reference these definitions:" : "" :
            [TermPrinter.prettyBinding ppe (HQ.fromVar v) b
            | (v, b) <- bindings]
          prettyWatches = P.sep "\n\n" [
            watchPrinter fileContents ppe ann kind evald isCacheHit |
            (ann,kind,evald,isCacheHit) <-
              sortOn (\(a,_,_,_)->a) . toList $ watches ]
      -- todo: use P.nonempty
      in putPrettyLn $ if null bindings then prettyWatches
                       else prettyBindings <> "\n" <> prettyWatches

  DisplayConflicts termNamespace typeNamespace -> do
    showConflicts "terms" terms
    showConflicts "types" types
    where
    terms    = R.dom termNamespace
    types    = R.dom typeNamespace
    showConflicts :: Foldable f => String -> f Name -> IO ()
    showConflicts thingsName things =
      unless (null things) $ do
        putStrLn $ "🙅 These " <> thingsName <> " have conflicts: "
        traverse_ (\x -> putStrLn ("  " ++ Name.toString x)) things
    -- TODO: Present conflicting TermEdits and TypeEdits
    -- if we ever allow users to edit hashes directly.
  FileChangeEvent _sourceName _src -> pure ()
    -- do
    -- Console.clearScreen
    -- Console.setCursorPosition 0 0
  Typechecked sourceName ppe slurpResult uf -> do
    Console.setTitle "Unison ✅"
    if UF.nonEmpty uf then putPrettyLn' . ("\n" <>) . P.okCallout . P.sep "\n\n" $ [
      P.wrap $ "I found and" <> P.bold "typechecked" <> "these definitions in "
            <> P.group (P.text sourceName <> ":"),
      P.indentN 2 $ SlurpResult.pretty ppe slurpResult,
      filestatusTip,
      P.wrap "Now evaluating any watch expressions (lines starting with `>`)..." ]
    else when (null $ UF.watchComponents uf) $ putPrettyLn' . P.wrap $
      "I loaded " <> P.text sourceName <> " and didn't find anything."
  TodoOutput names todo -> todoOutput names todo
  GitError e -> case e of
                  NoGit -> putPrettyLn' . P.wrap $ "I couldn't find git. "
                           <> "Make sure it's installed and on your path."
                  NoGithubAt p ->
                    putPrettyLn' . P.wrap $ "I couldn't access a git "
                      <> "repository at " <> P.text p
                      <> ". Make sure the repo exists "
                      <> "and that you have access to it."
                  NotAGitRepo p ->
                    putPrettyLn' . P.wrap $ "The directory at " <> P.string p
                    <> "doesn't seem to contain a git repository."
                  CheckoutFailed t ->
                    putPrettyLn' . P.wrap $ "I couldn't do a git checkout of "
                    <> P.text t <> ". Make sure there's a branch or commit "
                    <> "with that name."
  ListEdits patch names0 -> do
    let
      ppe = PPE.fromNames0 names0
      types = Patch._typeEdits patch
      terms = Patch._termEdits patch

      prettyTermEdit (r, TermEdit.Deprecate) =
        (prettyHashQualified . PPE.termName ppe . Referent.Ref $ r
        , "-> (deprecated)")
      prettyTermEdit (r, TermEdit.Replace r' _typing) =
        (prettyHashQualified . PPE.termName ppe . Referent.Ref $ r
        , "-> " <> (prettyHashQualified . PPE.termName ppe . Referent.Ref $ r'))
      prettyTypeEdit (r, TypeEdit.Deprecate) =
        (prettyHashQualified $ PPE.typeName ppe r
        , "-> (deprecated)")
      prettyTypeEdit (r, TypeEdit.Replace r') =
        (prettyHashQualified $ PPE.typeName ppe r
        , "-> " <> (prettyHashQualified . PPE.typeName ppe $ r'))
    when (not $ R.null types) $
       putPrettyLn $ "Edited Types:" `P.hang`
        P.column2 (prettyTypeEdit <$> R.toList types)
    when (not $ R.null terms) $
       putPrettyLn $ "Edited Terms:" `P.hang`
        P.column2 (prettyTermEdit <$> R.toList terms)
    when (R.null types && R.null terms)
         (putPrettyLn "This patch is empty.")
  BustedBuiltins (Set.toList -> new) (Set.toList -> old) ->
    -- todo: this could be prettier!  Have a nice list like `find` gives, but
    -- that requires querying the codebase to determine term types.  Probably
    -- the only built-in types will be primitive types like `Int`, so no need
    -- to look up decl types.
    -- When we add builtin terms, they may depend on new derived types, so
    -- these derived types should be added to the branch too; but not
    -- necessarily ever be automatically deprecated.  (A library curator might
    -- deprecate them; more work needs to go into the idea of sharing deprecations and stuff.
    putPrettyLn . P.warnCallout . P.lines $
      case (new, old) of
        ([],[]) -> error "BustedBuiltins busted, as there were no busted builtins."
        ([], old) ->
          P.wrap ("This branch includes some builtins that are considered deprecated. Use the " <> makeExample' IP.updateBuiltins <> " command when you're ready to work on eliminating them from your branch:")
            : ""
            : fmap (P.text . Reference.toText) old
        (new, []) -> P.wrap ("This version of Unison provides builtins that are not part of your branch. Use " <> makeExample' IP.updateBuiltins <> " to add them:")
          : "" : fmap (P.text . Reference.toText) new
        (new@(_:_), old@(_:_)) ->
          [ P.wrap
            ("Sorry and/or good news!  This version of Unison supports a different set of builtins than this branch uses.  You can use "
            <> makeExample' IP.updateBuiltins
            <> " to add the ones you're missing and deprecate the ones I'm missing. 😉"
            )
          , "You're missing:" `P.hang` P.lines (fmap (P.text . Reference.toText) new)
          , "I'm missing:" `P.hang` P.lines (fmap (P.text . Reference.toText) old)
          ]
  ListOfPatches patches ->
    -- todo: make this prettier
    putPrettyLn . P.lines . fmap prettyName $ toList patches
  x -> error $ "todo: output message for\n\n" ++ show x
  where
  renderFileName = P.group . P.blue . fromString
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

formatMissingStuff :: (Show tm, Show typ) =>
  [(HQ.HashQualified, tm)] -> [(HQ.HashQualified, typ)] -> P.Pretty P.ColorText
formatMissingStuff terms types =
  (unlessM (null terms) . P.fatalCallout $
    P.wrap "The following terms have a missing or corrupted type signature:"
    <> "\n\n"
    <> P.column2 [ (prettyHashQualified name, fromString (show ref)) | (name, ref) <- terms ]) <>
  (unlessM (null types) . P.fatalCallout $
    P.wrap "The following types weren't found in the codebase:"
    <> "\n\n"
    <> P.column2 [ (prettyHashQualified name, fromString (show ref)) | (name, ref) <- types ])

displayDefinitions' :: Var v => Ord a1
  => PPE.PrettyPrintEnv
  -> Map Reference.Reference (DisplayThing (DD.Decl v a1))
  -> Map Reference.Reference (DisplayThing (Unison.Term.AnnotatedTerm v a1))
  -> P.Pretty P.ColorText
displayDefinitions' ppe types terms = P.sep "\n\n" (prettyTypes <> prettyTerms)
  where
  prettyTerms = map go . Map.toList
             -- sort by name
             $ Map.mapKeys (first (PPE.termName ppe . Referent.Ref) . dupe) terms
  prettyTypes = map go2 . Map.toList
              $ Map.mapKeys (first (PPE.typeName ppe) . dupe) types
  go ((n, r), dt) =
    case dt of
      MissingThing r -> missing n r
      BuiltinThing -> builtin n
      RegularThing tm -> TermPrinter.prettyBinding ppe n tm
  go2 ((n, r), dt) =
    case dt of
      MissingThing r -> missing n r
      BuiltinThing -> builtin n
      RegularThing decl -> case decl of
        Left d  -> DeclPrinter.prettyEffectDecl ppe r n d
        Right d -> DeclPrinter.prettyDataDecl ppe r n d
  builtin n = P.wrap $ "--" <> prettyHashQualified n <> " is built-in."
  missing n r = P.wrap (
    "-- The name " <> prettyHashQualified n <> " is assigned to the "
    <> "reference " <> fromString (show r ++ ",")
    <> "which is missing from the codebase.")
    <> P.newline
    <> tip "You might need to repair the codebase manually."

displayDefinitions :: Var v => Ord a1 =>
  Maybe FilePath
  -> PPE.PrettyPrintEnv
  -> Map Reference.Reference (DisplayThing (DD.Decl v a1))
  -> Map Reference.Reference (DisplayThing (Unison.Term.AnnotatedTerm v a1))
  -> IO ()
displayDefinitions outputLoc ppe types terms =
  maybe displayOnly scratchAndDisplay outputLoc
  where
  displayOnly = putPrettyLn code
  scratchAndDisplay path = do
    path' <- canonicalizePath path
    prependToFile code path'
    putPrettyLn (message code path')
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
          "to replace the definitions currently in this branch."
      ]
  code = displayDefinitions' ppe types terms

unsafePrettyTermResultSig' :: Var v =>
  PPE.PrettyPrintEnv -> E.TermResult' v a -> P.Pretty P.ColorText
unsafePrettyTermResultSig' ppe = \case
  E.TermResult' name (Just typ) _r _aliases ->
    head (TypePrinter.prettySignatures' ppe [(name,typ)])
  _ -> error "Don't pass Nothing"

-- produces:
-- -- #5v5UtREE1fTiyTsTK2zJ1YNqfiF25SkfUnnji86Lms#0
-- Optional.None, Maybe.Nothing : Maybe a
unsafePrettyTermResultSigFull' :: Var v =>
  PPE.PrettyPrintEnv -> E.TermResult' v a -> P.Pretty P.ColorText
unsafePrettyTermResultSigFull' ppe = \case
  E.TermResult' hq (Just typ) r aliases -> P.lines
    [ P.hiBlack "-- " <> greyHash (HQ.fromReferent r)
    , P.commas (fmap greyHash $ hq : toList aliases) <> " : "
      <> TypePrinter.pretty ppe mempty (-1) typ
    , mempty
    ]
  _ -> error "Don't pass Nothing"
  where greyHash = styleHashQualified' id P.hiBlack

prettyTypeResultHeader' :: Var v => E.TypeResult' v a -> P.Pretty P.ColorText
prettyTypeResultHeader' (E.TypeResult' name dt r _aliases) =
  prettyDeclTriple (name, r, dt)

-- produces:
-- -- #5v5UtREE1fTiyTsTK2zJ1YNqfiF25SkfUnnji86Lms
-- type Optional
-- type Maybe
prettyTypeResultHeaderFull' :: Var v => E.TypeResult' v a -> P.Pretty P.ColorText
prettyTypeResultHeaderFull' (E.TypeResult' name dt r aliases) =
  P.lines stuff <> P.newline
  where
  stuff =
    (P.hiBlack "-- " <> greyHash (HQ.fromReference r)) :
      fmap (\name -> prettyDeclTriple (name, r, dt))
           (name : toList aliases)
    where greyHash = styleHashQualified' id P.hiBlack


-- todo: maybe delete this
prettyAliases ::
  (Foldable t, ListLike s Char, IsString s) => t HQ.HashQualified -> P.Pretty s
prettyAliases aliases = if length aliases < 2 then mempty else error "todo"
  -- (P.commented . (:[]) . P.wrap . P.commas . fmap prettyHashQualified' . toList) aliases <> P.newline

prettyDeclTriple :: Var v =>
  (HQ.HashQualified, Reference.Reference, DisplayThing (DD.Decl v a))
  -> P.Pretty P.ColorText
prettyDeclTriple (name, _, displayDecl) = case displayDecl of
   BuiltinThing -> P.hiBlack "builtin " <> P.hiBlue "type " <> P.blue (prettyHashQualified name)
   MissingThing _ -> mempty -- these need to be handled elsewhere
   RegularThing decl -> case decl of
     Left ed -> DeclPrinter.prettyEffectHeader name ed
     Right dd   -> DeclPrinter.prettyDataHeader name dd

renderNameConflicts :: Set.Set Name -> Set.Set Name -> P.Pretty CT.ColorText
renderNameConflicts conflictedTypeNames conflictedTermNames =
  unlessM (null allNames) $ P.callout "❓" . P.sep "\n\n" . P.nonEmpty $ [
    showConflictedNames "types" conflictedTypeNames,
    showConflictedNames "terms" conflictedTermNames,
    tip $ "This occurs when merging branches that both independently introduce the same name. Use "
        <> makeExample IP.view (prettyName <$> take 3 allNames)
        <> "to see the conflicting defintions, then use "
        <> makeExample' IP.rename <> "and/or " <> makeExample' IP.replace
        <> "to resolve the conflicts."
  ]
  where
    allNames = toList (conflictedTermNames <> conflictedTypeNames)
    showConflictedNames things conflictedNames =
      unlessM (Set.null conflictedNames) $
        P.wrap ("These" <> P.bold (things <> "have conflicting definitions:"))
        `P.hang` P.commas (P.blue . prettyName <$> toList conflictedNames)

renderEditConflicts ::
  PPE.PrettyPrintEnv -> Patch -> P.Pretty CT.ColorText
renderEditConflicts ppe Patch{..} =
  unlessM (null editConflicts) . P.callout "❓" . P.sep "\n\n" $ [
    P.wrap $ "These" <> P.bold "definitions were edited differently"
          <> "in branches that have been merged into this branch."
          <> "You'll have to tell me what to use as the new definition:",
    P.indentN 2 (P.lines (formatConflict <$> editConflicts)),
    tip $ "Use " <> makeExample IP.resolve [name (head editConflicts), " <replacement>"] <> " to pick a replacement." -- todo: eventually something with `edit`
    ]
  where
    -- todo: could possibly simplify all of this, but today is a copy/paste day.
    editConflicts :: [Either (Reference, Set TypeEdit.TypeEdit) (Reference, Set TermEdit.TermEdit)]
    editConflicts =
      (fmap Left . Map.toList . R.toMultimap . R.filterManyDom $ _typeEdits) <>
      (fmap Right . Map.toList . R.toMultimap . R.filterManyDom $ _termEdits)
    name = either (typeName . fst) (termName . fst)
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

todoOutput :: Var v => Names0 -> E.TodoOutput v a -> IO ()
todoOutput (PPE.fromNames0 -> ppe) todo =
  if noConflicts && noEdits
  then putPrettyLn $ P.okCallout "No conflicts or edits in progress."
  else putPrettyLn (todoConflicts <> todoEdits)
  where
  noConflicts = E.nameConflicts todo == mempty
             && E.editConflicts todo == Patch.empty
  noEdits = E.todoScore todo == 0
  (frontierTerms, frontierTypes) = E.todoFrontier todo
  (dirtyTerms, dirtyTypes) = E.todoFrontierDependents todo
  corruptTerms = [ (name, r) | (name, r, Nothing) <- frontierTerms ]
  corruptTypes = [ (name, r) | (name, r, MissingThing _) <- frontierTypes ]
  goodTerms ts = [ (name, typ) | (name, _, Just typ) <- ts ]
  todoConflicts = if noConflicts then mempty else P.lines . P.nonEmpty $
    [ renderEditConflicts ppe (E.editConflicts todo)
    , renderNameConflicts conflictedTypeNames conflictedTermNames ]
    where
    -- If a conflict is both an edit and a name conflict, we show it in the edit
    -- conflicts section
    c :: Names0
    c = removeEditConflicts (E.editConflicts todo) (E.nameConflicts todo)
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


  todoEdits = unlessM noEdits . P.callout "🚧" . P.sep "\n\n" . P.nonEmpty $
      [ P.wrap ("The branch has" <> fromString (show (E.todoScore todo))
              <> "transitive dependent(s) left to upgrade."
              <> "Your edit frontier is the dependents of these definitions:")
      , P.indentN 2 . P.lines $ (
          (prettyDeclTriple <$> toList frontierTypes) ++
          TypePrinter.prettySignatures' ppe (goodTerms frontierTerms)
          )
      , P.wrap "I recommend working on them in the following order:"
      , P.indentN 2 . P.lines $
          let unscore (_score,a,b,c) = (a,b,c)
          in (prettyDeclTriple . unscore <$> toList dirtyTypes) ++
             TypePrinter.prettySignatures'
                ppe
                (goodTerms $ unscore <$> dirtyTerms)
      , formatMissingStuff corruptTerms corruptTypes
      ]

listOfDefinitions ::
  Var v => Names0 -> E.ListDetailed -> [E.SearchResult' v a] -> IO ()
listOfDefinitions names detailed results =
  putPrettyLn $ listOfDefinitions' names detailed results

listOfDefinitions' :: Var v
                   => Names0 -- for printing types of terms :-\
                   -> E.ListDetailed
                   -> [E.SearchResult' v a]
                   -> P.Pretty P.ColorText
listOfDefinitions' (PPE.fromNames0 -> ppe) detailed results =
  P.lines . P.nonEmpty $ prettyNumberedResults :
    [formatMissingStuff termsWithMissingTypes missingTypes
    ,unlessM (null missingBuiltins) . bigproblem $ P.wrap
      "I encountered an inconsistency in the codebase; these definitions refer to built-ins that this version of unison doesn't know about:" `P.hang`
        P.column2 ( (P.bold "Name", P.bold "Built-in")
                  -- : ("-", "-")
                  : fmap (bimap prettyHashQualified
                                (P.text . Referent.toText)) missingBuiltins)
    ]
  where
  prettyNumberedResults =
    P.numbered (\i -> P.hiBlack . fromString $ show i <> ".") prettyResults
  -- todo: group this by namespace
  prettyResults =
    map (E.foldResult' renderTerm renderType)
        (filter (not.missingType) results)
    where
      (renderTerm, renderType) =
        if detailed then
          (unsafePrettyTermResultSigFull' ppe, prettyTypeResultHeaderFull')
        else
          (unsafePrettyTermResultSig' ppe, prettyTypeResultHeader')
  missingType (E.Tm _ Nothing _ _)          = True
  missingType (E.Tp _ (MissingThing _) _ _) = True
  missingType _                             = False
  -- termsWithTypes = [(name,t) | (name, Just t) <- sigs0 ]
  --   where sigs0 = (\(name, _, typ) -> (name, typ)) <$> terms
  termsWithMissingTypes =
    [ (name, r)
    | E.Tm name Nothing (Referent.Ref (Reference.DerivedId r)) _ <- results ]
  missingTypes = nubOrdOn snd $
    [ (name, Reference.DerivedId r)
    | E.Tp name (MissingThing r) _ _ <- results ] <>
    [ (name, r)
    | E.Tm name Nothing (Referent.toTypeReference -> Just r) _ <- results]
  missingBuiltins = results >>= \case
    E.Tm name Nothing r@(Referent.Ref (Reference.Builtin _)) _ -> [(name,r)]
    _ -> []

watchPrinter
  :: Var v
  => Text
  -> PPE.PrettyPrintEnv
  -> Ann
  -> UF.WatchKind
  -> Codebase.Term v ()
  -> Runtime.IsCacheHit
  -> P.Pretty P.ColorText
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
              (P.green "✅ " <> P.bold "Passed - " <> P.green (P.text msg))
            else if id == DD.failConstructorId
              then addCache
                (P.red "🚫 " <> P.bold "FAILED - " <> P.red (P.text msg))
              else P.red "❓ " <> TermPrinter.prettyTop ppe term
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
              $ TermPrinter.prettyTop ppe term
              ]
          ]

filestatusTip :: P.Pretty CT.ColorText
filestatusTip = tip "Use `help filestatus` to learn more."
