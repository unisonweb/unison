{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}


module Unison.CommandLine.OutputMessages where

import           Control.Applicative             ((<|>))
import           Control.Monad                   (join, when, unless)
import           Data.Foldable                   (toList, traverse_)
import           Data.List                       (sort)
import           Data.List.Extra                 (nubOrdOn)
import qualified Data.Map                        as Map
import           Data.Maybe                      (listToMaybe)
import qualified Data.Set                        as Set
import           Data.String                     (fromString)
import qualified Data.Text                       as Text
import           Data.Text.IO                    (readFile, writeFile)
import           Prelude                         hiding (readFile, writeFile)
import qualified System.Console.ANSI             as Console
import           System.Directory                (canonicalizePath,
                                                  doesFileExist)
import           Unison.Codebase.Branch          (Branch, Branch0)
import qualified Unison.Codebase.Branch          as Branch
import           Unison.Codebase.Editor          (DisplayThing (..), Input (..),
                                                  Output (..))
import qualified Unison.Codebase.Editor          as E
import qualified Unison.Codebase.TermEdit        as TermEdit
import qualified Unison.Codebase.TypeEdit        as TypeEdit
import           Unison.CommandLine              (backtick, backtickEOS,
                                                  putPrettyLn, putPrettyLn',
                                                  tip, warn, watchPrinter)
import qualified Unison.HashQualified            as HQ
import           Unison.Name                     (Name)
import qualified Unison.Name                     as Name
import           Unison.NamePrinter              (prettyHashQualified,
                                                  prettyName,
                                                  styleHashQualified)
import qualified Unison.Names                    as Names
import qualified Unison.PrettyPrintEnv           as PPE
import           Unison.PrintError               (prettyParseError,
                                                  prettyTypecheckedFile,
                                                  renderNoteAsANSI)
import qualified Unison.Reference                as Reference
import qualified Unison.Referent                 as Referent
import qualified Unison.Result                   as Result
import           Unison.Term                     (AnnotatedTerm)
import qualified Unison.TermPrinter              as TermPrinter
import           Unison.Type                     (AnnotatedType)
import qualified Unison.TypePrinter              as TypePrinter
import qualified Unison.Typechecker.TypeLookup   as TL
import qualified Unison.UnisonFile               as UF
import qualified Unison.Util.ColorText           as CT
import           Unison.Util.Monoid              (intercalateMap, unlessM)
import qualified Unison.Util.Pretty              as P
import qualified Unison.Util.Relation            as R
import           Unison.Var                      (Var)
import qualified Unison.Var                      as Var

notifyUser :: forall v . Var v => FilePath -> Output v -> IO ()
notifyUser dir o = case o of
  Success (MergeBranchI _) ->
    putPrettyLn $ P.bold "Merged. " <> "Here's what's `todo` after the merge:"
  Success _    -> putPrettyLn $ P.bold "Done."
  DisplayDefinitions outputLoc ppe terms types ->
    displayDefinitions outputLoc ppe terms types
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
      <> P.bold "`add`" <> "or" <> P.bold "`update`"
      <> "commands."
  UnknownBranch branchName ->
    putPrettyLn
      .  warn
      .  P.wrap
      $  "I don't know of a branch named "
      <> P.red (P.text branchName)
      <> "."
  RenameOutput oldName newName r -> do
    nameChange "rename" "renamed" oldName newName r
  AliasOutput existingName newName r -> do
    nameChange "alias" "aliased" existingName newName r
  UnknownName branchName nameTarget name ->
    putPrettyLn . warn . P.wrap $
     "I don't know of any " <> targets <> " named " <> n <> " in the branch " <> b <> "."
    where
    targets = fromString (Names.renderNameTarget nameTarget)
    n = P.red (prettyName name)
    b = P.blue (P.text branchName)
  NameAlreadyExists branchName nameTarget name ->
    putPrettyLn
      .  warn
      .  P.wrap
      $  "There's already a "
      <> fromString (Names.renderNameTarget nameTarget)
      <> " named "
      <> P.red (prettyName name)
      <> " in the branch "
      <> P.blue (P.text branchName)
      <> "."
  ConflictedName branchName nameTarget name ->
    putPrettyLn
      .  warn
      .  P.wrap
      $  "The name "
      <> P.red (prettyName name)
      <> " refers to more than one "
      <> fromString (Names.renderNameTarget nameTarget)
      <> " in the branch "
      <> P.blue (P.text branchName)
      <> "."
  BranchAlreadyExists b ->
    putPrettyLn . P.warnCallout
      $  P.wrap ("There's already a branch called " <> P.group (P.text b <> "."))
      <> "\n\n"
      <> (  tip
         $  "You can switch to that branch via"
         <> backtick ("branch " <> P.text b)
         <> "or delete it via"
         <> backtickEOS ("branch.delete " <> P.text b)
         )
  ListOfBranches current branches ->
    putPrettyLn
      $ let
          go n = if n == current
            then P.bold ("* " <> P.text n)
            else "  " <> P.text n
        in  intercalateMap "\n" go (sort branches)
  ListOfDefinitions branch terms types ->
    listOfDefinitions (Branch.head branch) terms types
  SlurpOutput s -> slurpOutput s
  ParseErrors src es -> do
    Console.setTitle "Unison ☹︎"
    traverse_ (putStrLn . CT.toANSI . prettyParseError (Text.unpack src)) es
  TypeErrors src ppenv notes -> do
    Console.setTitle "Unison ☹︎"
    let showNote =
          intercalateMap "\n\n" (renderNoteAsANSI ppenv (Text.unpack src))
            . map Result.TypeError
    putStrLn . showNote $ notes
  Evaluated fileContents ppe watches ->
    if null watches then putStrLn ""
    else putPrettyLn $ P.lines
      [ watchPrinter fileContents ppe ann evald isCacheHit
      | (_v, (ann,evald,isCacheHit)) <- Map.toList watches ]
  DisplayConflicts branch -> do
    showConflicts "terms" terms
    showConflicts "types" types
    where
    terms    = R.dom $ Branch.termNamespace branch
    types    = R.dom $ Branch.typeNamespace branch
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
  Typechecked sourceName errorEnv uf -> do
    Console.setTitle "Unison ☺︎"
    -- todo: we should just print this the same way as everything else
    let defs       = prettyTypecheckedFile uf errorEnv
    when (not $ null defs) . putPrettyLn' . ("\n" <>) . P.okCallout . P.lines$
     [ P.wrap $ "I found and" <> P.bold "typechecked" <> "these definitions in " <> P.group (P.text sourceName <> ":")
     , ""
     , P.lit defs
     , P.wrap "Now evaluating any watch expressions (lines starting with `>`)..."
     ]
  TodoOutput branch todo -> todoOutput branch todo

 where
  renderFileName = P.group . P.blue . fromString
  nameChange cmd pastTenseCmd oldName newName r = do
    when (not . Set.null $ E.changedSuccessfully r) . putPrettyLn . P.okCallout $
      P.wrap $ "I" <> pastTenseCmd <> "the"
        <> ns (E.changedSuccessfully r)
        <> P.blue (prettyName oldName)
        <> "to" <> P.group (P.green (prettyName newName) <> ".")
    when (not . Set.null $ E.oldNameConflicted r) . putPrettyLn . P.warnCallout $
      (P.wrap $ "I couldn't" <> cmd <> "the"
           <> ns (E.oldNameConflicted r)
           <> P.blue (prettyName oldName)
           <> "to" <> P.green (prettyName newName)
           <> "because of conflicts.")
      <> "\n\n"
      <> tip "Use `todo` to view more information on conflicts and remaining work."
    when (not . Set.null $ E.newNameAlreadyExists r) . putPrettyLn . P.warnCallout $
      (P.wrap $ "I couldn't" <> cmd <> P.blue (prettyName oldName)
           <> "to" <> P.green (prettyName newName)
           <> "because the "
           <> ns (E.newNameAlreadyExists r)
           <> "already exist(s).")
      <> "\n\n"
      <> tip
         ("Use" <> P.group ("`rename " <> prettyName newName <> " <newname>`") <>
           "to make" <> prettyName newName <> "available.")
    where
      ns targets = P.oxfordCommas $
        map (fromString . Names.renderNameTarget) (toList targets)

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



displayDefinitions :: Var v =>
  Maybe FilePath
  -> PPE.PrettyPrintEnv
  -> [(Reference.Reference, DisplayThing (Unison.Term.AnnotatedTerm v a1))]
  -> [(Reference.Reference, DisplayThing (Either a2 b))]
  -> IO ()
displayDefinitions outputLoc ppe terms types =
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
          "You can edit them there, then do `update` to replace the" <>
          "definitions currently in this branch."
       ]
  code = P.sep "\n\n" (prettyTypes <> prettyTerms)
  prettyTerms = map go terms
  prettyTypes = map go2 types
  go (r, dt) =
    let n = PPE.termName ppe (Referent.Ref r) in
    case dt of
      MissingThing r -> missing n r
      BuiltinThing -> builtin n
      RegularThing tm -> P.map fromString $
        TermPrinter.prettyBinding ppe n tm
  go2 (r, dt) =
    let n = PPE.typeName ppe r in
    case dt of
      MissingThing r -> missing n r
      BuiltinThing -> builtin n
      RegularThing decl -> case decl of
        Left _ability -> TypePrinter.prettyEffectHeader n <> " -- todo"
        Right _d      -> TypePrinter.prettyDataHeader n <> " -- todo"
  builtin n = P.wrap $ "--" <> prettyHashQualified n <> " is built-in."
  missing n r = P.wrap (
    "-- The name " <> prettyHashQualified n <> " is assigned to the "
    <> "reference " <> fromString (show r ++ ",")
    <> "which is missing from the codebase.")
    <> P.newline
    <> tip "You might need to repair the codebase manually."

prettyDeclTriple ::
  (HQ.HashQualified, Reference.Reference, DisplayThing (TL.Decl v a))
  -> P.Pretty P.ColorText
prettyDeclTriple (name, _, displayDecl) = case displayDecl of
  BuiltinThing -> P.wrap $ TypePrinter.prettyDataHeader name <> "(built-in)"
  MissingThing _ -> mempty -- these need to be handled elsewhere
  RegularThing decl -> case decl of
    Left _ability -> TypePrinter.prettyEffectHeader name
    Right _d      -> TypePrinter.prettyDataHeader name

renderNameConflicts :: Set.Set Name -> Set.Set Name -> P.Pretty CT.ColorText
renderNameConflicts conflictedTypeNames conflictedTermNames =
  unlessM (null allNames) $ P.callout "❓" . P.sep "\n\n" . P.nonEmpty $ [
    showConflictedNames "types" conflictedTypeNames,
    showConflictedNames "terms" conflictedTermNames,
    tip $ "This occurs when merging branches that both indepenently introduce the same name. Use "
        <> P.group ("`view " <> P.sep " " (prettyName <$> take 3 allNames) <> "`")
        <> "to see the conflicting defintions, then use `rename`"
        <> "and/or `replace` to resolve the conflicts."
  ]
  where
    allNames = toList (conflictedTermNames <> conflictedTypeNames)
    showConflictedNames things conflictedNames =
      unlessM (Set.null conflictedNames) $
        P.wrap ("These" <> P.bold (things <> "have conflicting definitions:"))
        `P.hang` P.commas (P.blue . prettyName <$> toList conflictedNames)

renderEditConflicts ::
  PPE.PrettyPrintEnv -> Branch.Branch0 -> P.Pretty CT.ColorText
renderEditConflicts ppe (Branch.editConflicts -> editConflicts) =
  unlessM (null editConflicts) . P.callout "❓" . P.sep "\n\n" $ [
    P.wrap $ "These" <> P.bold "definitions were edited differently"
          <> "in branches that have been merged into this branch."
          <> "You'll have to tell me what to use as the new definition:",
    P.indentN 2 (P.lines (formatConflict <$> editConflicts)),
    tip $ "Use " <> backtick ("resolve-edit " <> name (head editConflicts) <> " <replacement>") <> " to pick a replacement." -- todo: eventually something with `edit`
    ]
  where
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

todoOutput :: Var v => Branch -> E.TodoOutput v a -> IO ()
todoOutput (Branch.head -> branch) todo =
  if noConflicts && noEdits
  then putPrettyLn $ P.okCallout "No conflicts or edits in progress."
  else putPrettyLn (todoConflicts <> todoEdits)
  where
  noConflicts = E.todoConflicts todo == mempty
  noEdits = E.todoScore todo == 0
  ppe = Branch.prettyPrintEnv branch
  (frontierTerms, frontierTypes) = E.todoFrontier todo
  (dirtyTerms, dirtyTypes) = E.todoFrontierDependents todo
  corruptTerms = [ (name, r) | (name, r, Nothing) <- frontierTerms ]
  corruptTypes = [ (name, r) | (name, r, MissingThing _) <- frontierTypes ]
  goodTerms ts = [ (name, typ) | (name, _, Just typ) <- ts ]
  todoConflicts = if noConflicts then mempty else P.lines . P.nonEmpty $
    [ renderEditConflicts ppe branch
    , renderNameConflicts conflictedTypeNames conflictedTermNames ]
    where
    -- If a conflict is both an edit and a name conflict, we show it in the edit
    -- conflicts section
    c = Branch.nameOnlyConflicts (E.todoConflicts todo)
    conflictedTypeNames = Branch.allTypeNames c
    conflictedTermNames = Branch.allTermNames c
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
             (TypePrinter.prettySignatures'
                ppe
                (goodTerms $ unscore <$> dirtyTerms))
      , formatMissingStuff corruptTerms corruptTypes
      ]

listOfDefinitions :: Var v =>
  Branch0
  -> [(HQ.HashQualified, Referent.Referent, Maybe (AnnotatedType v a1))]
  -> [(HQ.HashQualified, Reference.Reference, DisplayThing (TL.Decl v2 a2))]
  -> IO ()
listOfDefinitions branch terms types = do
  putPrettyLn . P.lines $
    typeResults ++
    TypePrinter.prettySignatures' ppe termsWithTypes ++
    [formatMissingStuff termsWithMissingTypes missingTypes]
  unless (null impossible) . error $ "Compiler bug, these referents are missing types: " <> show impossible
  where
  ppe  = Branch.prettyPrintEnv branch
  typeResults = map prettyDeclTriple types
  termsWithTypes = [(name,t) | (name, Just t) <- sigs0 ]
    where sigs0 = (\(name, _, typ) -> (name, typ)) <$> terms
  termsWithMissingTypes =
    [ (name, r) | (name, Referent.Ref (Reference.DerivedId r), Nothing) <- terms ]
  missingTypes = nubOrdOn snd $
    [ (name, Reference.DerivedId r) | (name, _, MissingThing r) <- types ] <>
    [ (name, r) | (name, Referent.toTypeReference -> Just r, Nothing) <- terms]
  impossible = terms >>= \case
    (name, r@(Referent.Ref (Reference.Builtin _)), Nothing) -> [(name,r)]
    _ -> []

-- todo: could probably use more cleanup
slurpOutput :: Var v => E.SlurpResult v -> IO ()
slurpOutput s =
  putPrettyLn . P.sep "\n" . P.nonEmpty $ [
      addedMsg, updatedMsg, alreadyAddedMsg, namesExistMsg,
      namesConflictedMsg, aliasingMsg, termExistingCtorMsg,
      ctorExistingTermMsg, blockedDependenciesMsg ]
  where
  -- todo: move this to a separate function
  branch = E.updatedBranch s
  file = E.originalFile s
  E.SlurpComponent addedTypes addedTerms = E.adds s
  E.SlurpComponent dupeTypes dupeTerms = E.duplicates s
  E.SlurpComponent collidedTypes collidedTerms = E.collisions s
  E.SlurpComponent conflictedTypes conflictedTerms = E.conflicts s
  E.SlurpComponent updatedTypes updatedTerms = E.updates s
  termTypesFromFile =
    Map.fromList [ (v,t) | (v,_,t) <- join (UF.topLevelComponents file) ]
  ppe = Branch.prettyPrintEnv (Branch.head branch)
    <> Branch.prettyPrintEnv (Branch.fromTypecheckedFile file)
  filterTermTypes vs =
    [ (HQ.fromVar v,t)
    | v <- toList vs
    , t <- maybe (error $ "There wasn't a type for " ++ show v ++ " in termTypesFromFile!") pure (Map.lookup v termTypesFromFile)]
  prettyDeclHeader v = case UF.getDecl' file v of
    Just (Left _)  -> TypePrinter.prettyEffectHeader (HQ.fromVar v)
    Just (Right _) -> TypePrinter.prettyDataHeader (HQ.fromVar v)
    Nothing        -> error "Wat."
  addedMsg =
    unlessM (null addedTypes && null addedTerms) . P.okCallout $
    P.wrap ("I" <> P.bold "added" <> "these definitions:")
    <> "\n\n"
    <> (P.indentN 2 . P.lines $
        (prettyDeclHeader <$> toList addedTypes) ++
        TypePrinter.prettySignatures' ppe (filterTermTypes addedTerms))
  updatedMsg =
    unlessM (null updatedTypes && null updatedTerms) . P.okCallout $
    P.wrap ("I" <> P.bold "updated" <> "these definitions:")
    -- todo: show the partial hash too?
    <> "\n\n"
    <> (P.indentN 2 . P.lines $
        (prettyDeclHeader <$> toList updatedTypes) ++
        TypePrinter.prettySignatures' ppe (filterTermTypes updatedTerms))
    -- todo "You probably have a bunch more work to do."
  alreadyAddedMsg =
    unlessM (null dupeTypes && null dupeTerms) . P.callout "☑️" $
    P.wrap ("I skipped these definitions because they have"
             <> P.bold "already been added:")
    <> "\n\n"
    <> (P.indentN 2 . P.lines $
          (prettyDeclHeader <$> toList dupeTypes) ++
          TypePrinter.prettySignatures' ppe (filterTermTypes dupeTerms))
  namesExistMsg =
    unlessM (null collidedTypes && null collidedTerms) . P.warnCallout $
    P.wrap ("I skipped these definitions because the" <> P.bold "names already exist," <> "but with different definitions:")
    <> "\n\n"
    <> (P.indentN 2 . P.lines $
        (prettyDeclHeader <$> toList collidedTypes) ++
        TypePrinter.prettySignatures' ppe (filterTermTypes collidedTerms))
    <> "\n\n"
    <> tip ("You can use `update` if you're trying to replace the existing definitions and all their usages, or `rename` the existing definition to free up the name for the definitions in your .u file.")
  namesConflictedMsg =
    unlessM (null conflictedTypes && null conflictedTerms) . P.warnCallout $
    P.wrap ("I didn't try to update these definitions because the names are" <> P.bold "conflicted" <> "(already associated with multiple definitions):")
    <> "\n\n"
    <> (P.indentN 2 . P.lines $
        (prettyDeclHeader <$> toList conflictedTypes) ++
        TypePrinter.prettySignatures' ppe (filterTermTypes conflictedTerms))
    <> "\n\n"
    <> tip ("Use " <> backtick ("view " <> sampleName) <> " to view the conflicting definitions and " <> backtick ("rename " <> sampleNameHash <> " " <> sampleNewName) <> " to give each definition a distinct name. Alternatively, use " <> backtick ("resolve " <> sampleNameHash) <> "to make" <> backtick sampleNameHash <> " the canonical " <> backtick sampleName <> "and remove the name from the other definitions.")
    where
    sampleName =
      P.text . head . fmap Var.name . toList $ (conflictedTypes <> conflictedTerms)
    sampleHash = "#abc" -- todo: get real hash prefix for sampleName
    sampleNameHash = P.group (sampleName <> sampleHash)
    -- todo: get real unused name from branch
    sampleNewName = P.group (sampleName <> "2")
  aliasingMsg =
    unlessM (R.null (Branch.termCollisions (E.needsAlias s))
        && R.null (Branch.typeCollisions (E.needsAlias s))) . P.warnCallout $
    P.wrap ("I skipped these definitions because they already" <> P.bold "exist with other names:")
    <> "\n\n"
    <> P.indentN 2 (P.lines . join $ [
        P.align
      -- ("type Optional", "aka " ++ commas existingNames)
      -- todo: something is wrong here: only one oldName is being shown, instead of all
        [(prettyDeclHeader $ Name.toVar newName,
          "aka " <> P.commas (prettyName <$> toList oldNames)) |
          (newName, oldNames) <-
            Map.toList . R.domain . Branch.typeCollisions $ (E.needsAlias s) ],
      TypePrinter.prettySignaturesAlt' ppe
          -- foo, foo2, fasdf : a -> b -> c
          -- note: this shit vvvv is not a Name.
          [ (name : fmap HQ.fromName (toList oldNames), typ)
          | (newName, oldNames) <-
              Map.toList . R.domain . Branch.termCollisions $ (E.needsAlias s)
          , (name, typ) <- filterTermTypes [Name.toVar newName]
          ]
      ])
      <> "\n\n"
      <> tip ("Use " <> backtick ("alias " <> sampleOldName <> " " <> sampleNewName) <> "to create an additional name for this definition.")
    where
      f = listToMaybe . Map.toList . R.domain
      Just (prettyName -> sampleNewName,
            prettyName . head . toList -> sampleOldName) =
            (f . Branch.typeCollisions) (E.needsAlias s) <|>
            (f . Branch.termCollisions) (E.needsAlias s)
  termExistingCtorMsg =
    unlessM (null ctorCollisions) . P.warnCallout $
    P.wrap ("I can't update these terms because the" <> P.bold "names are currently assigned to constructors:")
    <> "\n\n"
    <> (P.indentN 2 $
        (P.column2 [ (P.text $ Var.name v, "is a constructor for " <> go r)
                   | (v, r) <- Map.toList ctorCollisions ])
        <> "\n\n"
        <> tip "You can `rename` these constructors to free up the names for your new definitions.")
    where
      ctorCollisions = E.termExistingConstructorCollisions s
      go r = prettyHashQualified (PPE.typeName ppe (Referent.toReference r))
  ctorExistingTermMsg =
    unlessM (null ctorExistingTermCollisions) . P.warnCallout $
    P.wrap ("I can't update these types because one or more of the" <> P.bold "constructor names matches an existing term:") <> "\n\n" <>
      P.indentN 2 (
        P.column2 [
          (P.text $ Var.name v, "has name collisions for: " <> commaRefs rs)
          | (v, rs) <- Map.toList ctorExistingTermCollisions ]
        )
        <> "\n\n"
        <> tip "You can `rename` existing definitions to free up the names for your new definitions."
    where
    ctorExistingTermCollisions = E.constructorExistingTermCollisions s
    commaRefs rs = P.wrap $ P.commas (map go rs)
    go r = prettyHashQualified (PPE.termName ppe r)
  blockedDependenciesMsg =
    unlessM (null blockedTerms && null blockedTypes) . P.warnCallout $
    P.wrap ("I also skipped these definitions with a" <> P.bold "transitive dependency on a skipped definition" <> "mentioned above:")
    <> "\n\n"
    <> (P.indentN 2 . P.lines $
        (prettyDeclHeader <$> toList blockedTypes) ++
        TypePrinter.prettySignatures' ppe (filterTermTypes blockedTerms))
    where
      blockedTerms = Map.keys (E.termsWithBlockedDependencies s)
      blockedTypes = Map.keys (E.typesWithBlockedDependencies s)
