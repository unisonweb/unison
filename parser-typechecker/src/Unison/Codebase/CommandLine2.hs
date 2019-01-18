{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Unison.Codebase.CommandLine2 where

-- import Debug.Trace
import Data.Maybe (catMaybes)
import           Control.Applicative            ((<|>))
import           Control.Concurrent             (forkIO, killThread)
import qualified Control.Concurrent.Async       as Async
import           Control.Concurrent.STM         (atomically)
import           Control.Exception              (finally)
import           Control.Monad                  (forever, join, when)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Trans            (lift)
import           Data.Foldable                  (toList, traverse_)
import           Data.IORef
import           Data.List                      (intercalate, isSuffixOf, sort)
import           Data.ListLike                  (ListLike)
import           Data.List.Extra                (nubOrdOn)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe, listToMaybe)
import           Data.String                    (IsString, fromString)
import qualified Data.Set                       as Set
import qualified Data.Text                      as Text
import qualified System.Console.ANSI            as Console
import qualified System.Console.Haskeline       as Line
import qualified System.Console.Terminal.Size   as Terminal
import           System.Directory               (canonicalizePath)
import           System.Random                  (randomRIO)
import           Unison.Codebase                (Codebase)
import qualified Unison.Codebase                as Codebase
import           Unison.Codebase.Branch         (Branch)
import qualified Unison.Codebase.Branch         as Branch
import           Unison.Codebase.Editor         (BranchName, DisplayThing (..),
                                                 Event (..), Input (..),
                                                 Output (..))
import qualified Unison.Codebase.Editor         as E
import qualified Unison.Codebase.Editor.Actions as Actions
import           Unison.Codebase.Runtime        (Runtime)
import qualified Unison.Codebase.Runtime        as Runtime
import qualified Unison.Codebase.Watch          as Watch
import qualified Unison.Names                   as Names
import           Unison.Parser                  (Ann)
import qualified Unison.PrettyPrintEnv          as PPE
import           Unison.PrintError              (prettyParseError,
                                                 prettyTypecheckedFile,
                                                 renderNoteAsANSI)
import qualified Unison.Result                  as Result
import qualified Unison.Referent                as Referent
import qualified Unison.Reference               as Reference
import qualified Unison.TypePrinter             as TypePrinter
import qualified Unison.TermPrinter             as TermPrinter
import qualified Unison.UnisonFile              as UF
import qualified Unison.Util.ColorText          as CT
import           Unison.Util.Monoid             (intercalateMap)
import qualified Unison.Util.Pretty             as P
import qualified Unison.Util.Relation           as R
import           Unison.Util.TQueue             (TQueue)
import qualified Unison.Util.TQueue             as Q
import           Unison.Var                     (Var)
import qualified Unison.Var                     as Var

notifyUser :: forall v . Var v => FilePath -> Output v -> IO ()
notifyUser dir o = do
  -- note - even if user's terminal is huge, we restrict available width since
  -- it's hard to read code or text that's super wide.
  width <- fromMaybe 80 . fmap (min 100 . Terminal.width) <$> Terminal.size
  let putPrettyLn = putStrLn . P.toANSI width . P.border 2
  case o of
    Success _    -> putStrLn "Done."
    DisplayDefinitions ppe terms types -> let
      prettyTerms = map go terms
      go (r, dt) =
        let n = PPE.termName ppe (Referent.Ref r) in
        case dt of
          MissingThing r -> missing n r
          BuiltinThing -> builtin n
          RegularThing tm -> P.map fromString $
            TermPrinter.prettyBinding ppe (Var.named n) tm
      prettyTypes = map go2 types
      builtin n = P.wrap $ "--" <> P.text n <> " is built-in."
      missing n r = P.wrap (
        "-- The name " <> P.text n <> " is assigned to the "
        <> "reference " <> fromString (show r ++ ",")
        <> "which is missing from the codebase.")
        <> P.newline
        <> tip "You might need to repair the codebase manually."
      go2 (r, dt) =
        let n = PPE.typeName ppe r in
        case dt of
          MissingThing r -> missing n r
          BuiltinThing -> builtin n
          RegularThing decl -> case decl of
            Left _ability -> TypePrinter.prettyEffectHeader n <> " -- todo"
            Right _d -> TypePrinter.prettyDataHeader n <> " -- todo"
      in putPrettyLn $ P.sep "\n\n" (prettyTerms <> prettyTypes)
    NoUnisonFile -> do
      dir' <- canonicalizePath dir
      putPrettyLn . P.callout "😶" $ P.lines
        [ P.wrap "There's nothing for me to add right now."
        , ""
        , P.column2 [(P.bold "Hint:", msg dir')] ]
     where
      msg dir =
        P.wrap
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
      putPrettyLn
        .  warn
        .  P.wrap
        $  "I don't know of any "
        <> fromString (Names.renderNameTarget nameTarget)
        <> " named "
        <> P.red (P.text name)
        <> " in the branch "
        <> P.blue (P.text branchName)
        <> "."
    NameAlreadyExists branchName nameTarget name ->
      putPrettyLn
        .  warn
        .  P.wrap
        $  "There's already a "
        <> fromString (Names.renderNameTarget nameTarget)
        <> " named "
        <> P.red (P.text name)
        <> " in the branch "
        <> P.blue (P.text branchName)
        <> "."
    ConflictedName branchName nameTarget name ->
      putPrettyLn
        .  warn
        .  P.wrap
        $  "The name "
        <> P.red (P.text name)
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
      let ppe  = Branch.prettyPrintEnv1 (Branch.head branch)
          sigs0 = (\(name, _, typ) -> (name, typ)) <$> terms
          sigs = [(name,t) | (name, Just t) <- sigs0 ]
          impossible = terms >>= \(name, r, _) -> case r of
            Referent.Ref (Reference.Builtin _) -> [(name,r)]
            _ -> []
          termsWithMissingTypes =
            [ (name, r) | (name, Referent.Ref (Reference.DerivedId r), Nothing) <- terms ]
          missingTypes = nubOrdOn snd $
            [ (name, Reference.DerivedPrivate_ r) | (name, _, MissingThing r) <- types ] <>
            [ (name, r) | (name, Referent.toTypeReference -> Just r, Nothing) <- terms]
          typeResults = map prettyDeclTriple types
      in do
        putPrettyLn . P.lines $
          typeResults ++ TypePrinter.prettySignatures' ppe sigs ++
          formatMissingStuff termsWithMissingTypes missingTypes
        when (not $ null impossible) . error $ "Compiler bug, these referents are missing types: " <> show impossible

    SlurpOutput s -> let
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
      ppe =
        Branch.prettyPrintEnv1 (Branch.head branch) `PPE.unionLeft`
        Branch.prettyPrintEnv1 (Branch.fromTypecheckedFile file)
      filterTermTypes vs =
        [ (Var.name v,t) | v <- toList vs
                , t <- maybe (error $ "There wasn't a type for " ++ show v ++ " in termTypesFromFile!") pure (Map.lookup v termTypesFromFile)]
      prettyDeclHeader v = case UF.getDecl' file v of
        Just (Left _) -> TypePrinter.prettyEffectHeader (Var.name v)
        Just (Right _) -> TypePrinter.prettyDataHeader (Var.name v)
        Nothing -> error "Wat."
      addMsg = if not (null addedTypes && null addedTerms)
        then Just . P.okCallout $
          P.wrap ("I" <> P.bold "added" <> "these definitions:")
          <> "\n\n" <> P.indentN 2
            (P.lines (
              (prettyDeclHeader <$> toList addedTypes) ++
              TypePrinter.prettySignatures' ppe (filterTermTypes addedTerms))
            )
        else Nothing
      updateMsg = if not (null updatedTypes && null updatedTerms)
        then Just . P.okCallout $
          P.wrap ("I" <> P.bold "updated" <> "these definitions:")
          -- todo: show the partial hash too?
          <> "\n\n"
          <> P.indentN 2 (
              P.lines (
                (prettyDeclHeader <$> toList updatedTypes) ++
                TypePrinter.prettySignatures' ppe (filterTermTypes updatedTerms))
             )
          -- todo "You probably have a bunch more work to do."
        else Nothing
      dupeMsg = if not (null dupeTypes && null dupeTerms)
        then Just . P.callout "☑️" $
          P.wrap ("I skipped these definitions because they have" <> P.bold "already been added:")
          <> "\n\n"
          <> P.indentN 2 (
              P.lines (
                (prettyDeclHeader <$> toList dupeTypes) ++
                TypePrinter.prettySignatures' ppe (filterTermTypes dupeTerms))
            )
        else Nothing
      collMsg =
        if not (null collidedTypes && null collidedTerms)
        then Just . P.warnCallout $
          P.wrap ("I skipped these definitions because the" <> P.bold "names already exist," <> "but with different definitions:") <> "\n\n" <>
          P.indentN 2 (
            P.lines (
              (prettyDeclHeader <$> toList collidedTypes) ++
              TypePrinter.prettySignatures' ppe (filterTermTypes collidedTerms))
            )
            <> "\n\n"
            <> tip ("You can use `update` if you're trying to replace the existing definitions and all their usages, or `rename` the existing definition to free up the name for the definitions in your .u file.")
        else Nothing
      conflictMsg =
        if not (null conflictedTypes && null conflictedTerms)
        then Just . P.warnCallout $
          let sampleName =
                P.text . head . fmap Var.name . toList $
                  (conflictedTypes <> conflictedTerms)
              sampleHash = "#abc" -- todo: get real hash prefix for sampleName
              sampleNameHash = P.group (sampleName <> sampleHash)
              sampleNameHash' = P.group (sampleNameHash <> "`")
              sampleNameHash'' = P.group ("`" <> sampleNameHash <> "`")
              -- todo: get real unused name from branch
              sampleNewName = P.group (sampleName <> "2")
              sampleNewName' = P.group (sampleNewName <> "`")
              sampleName' = P.group (sampleName <>"`")
              sampleName'' = P.group ("`" <> sampleName <>"`") in
          P.wrap ("I didn't try to update these definitions because the names are" <>
                  P.bold "conflicted" <>
                  "(already associated with multiple definitions):")
          <> P.newline
          <> P.newline
          <> P.indentN 2 (
            P.lines (
              (prettyDeclHeader <$> toList conflictedTypes) ++
              TypePrinter.prettySignatures' ppe (filterTermTypes conflictedTerms))
          ) <> "\n\n"
            <> tip ("Use `view " <> sampleName' <> " to view the conflicting definitions and `rename " <> sampleNameHash <> " " <> sampleNewName' <> " to give each definition a distinct name. Alternatively, use `resolve " <> sampleNameHash' <> "to make" <> sampleNameHash'' <> " the canonical " <> sampleName'' <> "and remove the name from the other definitions.")
        else Nothing

      aliasingMsg =
        if not (R.null (Branch.termCollisions (E.needsAlias s))
                && R.null (Branch.typeCollisions (E.needsAlias s)))
        then Just . P.warnCallout $
          let f = listToMaybe . Map.toList . R.domain
              Just (sampleName0, sampleExistingName0) =
                (f . Branch.typeCollisions) (E.needsAlias s) <|>
                (f . Branch.termCollisions) (E.needsAlias s)
              sampleNewName' = P.group (P.text sampleName0 <> "`")
              sampleOldName = P.text . head . toList $ sampleExistingName0 in

          P.wrap ("I skipped these definitions because they already" <> P.bold "exist with other names:") <> "\n\n" <>
          P.indentN 2 (
            P.lines . join $ [
              P.align
            -- ("type Optional", "aka " ++ commas existingNames)
            -- todo: something is wrong here: only one oldName is being shown, instead of all
              [(prettyDeclHeader $ Var.named newName,
                "aka " <> P.commas (P.text <$> toList oldNames)) |
                (newName, oldNames) <-
                  Map.toList . R.domain . Branch.typeCollisions $ (E.needsAlias s) ],
            TypePrinter.prettySignatures' ppe
                -- foo, foo2, fasdf : a -> b -> c
                [ (intercalateMap ", " id (name : toList oldNames), typ)
                | (newName, oldNames) <-
                    Map.toList . R.domain . Branch.termCollisions $ (E.needsAlias s)
                , (name, typ) <- filterTermTypes [Var.named newName]
                ]
            ])
            <> "\n\n"
            <> tip ("Use `alias" <> sampleOldName <> " " <> sampleNewName' <> "to create an additional name for this definition.")
        else Nothing
      termExistingCtorCollisions = E.termExistingConstructorCollisions s
      termExistingCtorMsg =
        if not (null termExistingCtorCollisions)
        then Just . P.warnCallout $
          P.wrap ("I can't update these terms because the" <> P.bold "names are currently assigned to constructors:") <> "\n\n" <>
            P.indentN 2
              (P.column2
                [ (P.text $ Var.name v
                  , "is a constructor for " <>
                      P.text (PPE.typeName ppe (Referent.toReference r)))
                | (v, r) <- Map.toList termExistingCtorCollisions ]
              )
              <> "\n\n"
              <> tip "You can `rename` these constructors to free up the names for your new definitions."
        else Nothing
      ctorExistingTermCollisions = E.constructorExistingTermCollisions s
      commaRefs rs = P.wrap $ P.commas (map go rs) where
        go r = P.text (PPE.termName ppe r)
      ctorExistingTermMsg =
        if not (null ctorExistingTermCollisions)
        then Just . P.warnCallout $
          P.wrap ("I can't update these types because one or more of the" <> P.bold "constructor names matches an existing term:") <> "\n\n" <>
            P.indentN 2 (
              P.column2 [
                (P.text $ Var.name v, "has name collisions for: " <> commaRefs rs)
                | (v, rs) <- Map.toList ctorExistingTermCollisions ]
              )
              <> "\n\n"
              <> tip "You can `rename` existing definitions to free up the names for your new definitions."
        else Nothing
      blockedTerms = Map.keys (E.termsWithBlockedDependencies s)
      blockedTypes = Map.keys (E.typesWithBlockedDependencies s)
      blockedDependenciesMsg =
        if null blockedTerms && null blockedTypes then Nothing
        else Just . P.warnCallout $
          P.wrap ("I also skipped these definitions with a" <> P.bold "transitive dependency on a skipped definition" <> "mentioned above:") <> "\n\n"
          <> P.indentN 2 (
              P.lines (
                (prettyDeclHeader <$> toList blockedTypes) ++
                TypePrinter.prettySignatures' ppe (filterTermTypes blockedTerms)
              )
             )
      in putPrettyLn . P.sep "\n" . catMaybes $ [
          addMsg, updateMsg, dupeMsg, collMsg,
          conflictMsg, aliasingMsg, termExistingCtorMsg,
          ctorExistingTermMsg, blockedDependenciesMsg ]
    ParseErrors src es -> do
      Console.setTitle "Unison ☹︎"
      traverse_ (putStrLn . CT.toANSI . prettyParseError (Text.unpack src)) es
    TypeErrors src ppenv notes -> do
      Console.setTitle "Unison ☹︎"
      let showNote =
            intercalateMap "\n\n" (renderNoteAsANSI ppenv (Text.unpack src))
              . map Result.TypeError
      putStrLn . showNote $ notes
    Evaluated names (watches, _term) -> do
      traverse_ (uncurry $ Watch.watchPrinter names) watches
    DisplayConflicts branch -> do
      let terms    = R.dom $ Branch.termNamespace branch
          types    = R.dom $ Branch.typeNamespace branch
      when (not $ null terms) $ do
        putStrLn "🙅 These terms have conflicts: "
        traverse_ (\x -> putStrLn ("  " ++ Text.unpack x)) terms
      when (not $ null types) $ do
        putStrLn "🙅 These types have conflicts: "
        traverse_ (\x -> putStrLn ("  " ++ Text.unpack x)) types
      -- TODO: Present conflicting TermEdits and TypeEdits
      -- if we ever allow users to edit hashes directly.
    FileChangeEvent _sourceName _src -> do
      Console.clearScreen
      Console.setCursorPosition 0 0
    Typechecked sourceName errorEnv unisonFile -> do
      Console.setTitle "Unison ☺︎"
      let emoticons = "🌸🌺🌹🌻🌼🌷🌵🌴🍄🌲"
      n <- randomRIO (0, length emoticons - 1)
      let uf         = UF.discardTerm unisonFile
          defs       = prettyTypecheckedFile uf errorEnv
          prettyDefs = CT.toANSI defs
      when (not $ null defs)
        .  putStrLn
        $  "✅ "
        ++ [emoticons !! n]
        ++ "  Found and typechecked the following definitions in "
        ++ (Text.unpack sourceName)
        ++ ":\n"
      putStrLn prettyDefs
      putStrLn $
          "👀  Now evaluating any watch expressions (lines starting with `>`)"
        <> " ...\n"
    TodoOutput branch todo ->
      let ppe = Branch.prettyPrintEnv1 (Branch.head branch) in
      if E.todoScore todo == 0
      then putPrettyLn . emojiNote "✅" $ "No conflicts or edits in progress."
      else do
        let (frontierTerms, frontierTypes) = E.todoFrontier todo
            (dirtyTerms, dirtyTypes) = E.todoFrontierDependents todo
            corruptTerms = [ (name, r) | (name, r, Nothing) <- frontierTerms ]
            corruptTypes = [ (name, r) | (name, r, MissingThing _) <- frontierTypes ]
            goodTerms ts = [ (name, typ) | (name, _, Just typ) <- ts ]

        putPrettyLn . P.callout "🚧" . P.lines . join $ [
          [P.wrap ("The branch has" <> fromString (show (E.todoScore todo))
                  <> "transitive dependents left to upgrade."
                  <> "Your edit frontier is the dependents of these definitions:")],
          [""],
          [P.indentN 2 . P.lines $ (
              (prettyDeclTriple <$> toList frontierTypes) ++
              TypePrinter.prettySignatures' ppe (goodTerms frontierTerms)
           )],
          [P.wrap "I recommend working on them in the following order:"],
          [""],
          [P.indentN 2 . P.lines $
            let unscore (_score,a,b,c) = (a,b,c)
            in (prettyDeclTriple . unscore <$> toList dirtyTypes) ++
               (TypePrinter.prettySignatures' ppe (goodTerms $ unscore <$> dirtyTerms))
          ],
          formatMissingStuff corruptTerms corruptTypes
         ]

 where
  renderFileName = P.group . P.blue . fromString
  prettyDeclTriple (name, _, displayDecl) = case displayDecl of
    BuiltinThing -> P.wrap $
      TypePrinter.prettyDataHeader name <> "(built-in)"
    MissingThing _ -> mempty -- these need to be handled elsewhere
    RegularThing decl -> case decl of
      Left _ability -> TypePrinter.prettyEffectHeader name
      Right _d -> TypePrinter.prettyDataHeader name

  nameChange cmd pastTenseCmd oldName newName r = do
    when (not . Set.null $ E.changedSuccessfully r) . putPrettyLn . P.okCallout $
      P.wrap $ "I" <> pastTenseCmd <> "the"
        <> ns (E.changedSuccessfully r)
        <> P.blue (P.text oldName)
        <> "to" <> P.green (P.text (newName <> "."))
    when (not . Set.null $ E.oldNameConflicted r) . putPrettyLn . P.warnCallout $
      (P.wrap $ "I couldn't" <> cmd <> "the"
           <> ns (E.oldNameConflicted r)
           <> P.blue (P.text oldName)
           <> "to" <> P.green (P.text newName)
           <> "because of conflicts.")
      <> "\n\n"
      <> tip "Use `todo` to view more information on conflicts and remaining work."
    when (not . Set.null $ E.newNameAlreadyExists r) . putPrettyLn . P.warnCallout $
      (P.wrap $ "I couldn't" <> cmd <> P.blue (P.text oldName)
           <> "to" <> P.green (P.text newName)
           <> "because the "
           <> ns (E.newNameAlreadyExists r)
           <> "already exist(s).")
      <> "\n\n"
      <> tip
         ("Use" <> P.group ("`rename " <> P.text newName <> " <newname>`") <>
           "to make" <> P.text newName <> "available.")
    where
      ns targets = P.oxfordCommas $
        map (fromString . Names.renderNameTarget) (toList targets)

  formatMissingStuff :: (Show a, Show b)
    => [(Names.Name, a)] -> [(Names.Name, b)] -> [P.Pretty P.ColorText]
  formatMissingStuff terms types = catMaybes [
    (if null terms then Nothing else Just . P.fatalCallout $
      P.wrap "The following terms have a missing or corrupted type signature:"
      <> "\n\n"
      <> P.column2 [ (P.text name, fromString (show ref)) | (name, ref) <- terms ]),
    (if null types then Nothing else Just . P.fatalCallout $
      P.wrap "The following types weren't found in the codebase:"
      <> "\n\n"
      <> P.column2 [ (P.text name, fromString (show ref)) | (name, ref) <- types ])
    ]

allow :: FilePath -> Bool
allow = (||) <$> (".u" `isSuffixOf`) <*> (".uu" `isSuffixOf`)

-- TODO: Return all of these thread IDs so we can throw async exceptions at
-- them when we need to quit.

watchFileSystem :: TQueue Event -> FilePath -> IO (IO ())
watchFileSystem q dir = do
  (cancel, watcher) <- Watch.watchDirectory dir allow
  t <- forkIO . forever $ do
    (filePath, text) <- watcher
    atomically . Q.enqueue q $ UnisonFileChanged (Text.pack filePath) text
  pure (cancel >> killThread t)

watchBranchUpdates :: TQueue Event -> Codebase IO v a -> IO (IO ())
watchBranchUpdates q codebase = do
  (cancelExternalBranchUpdates, externalBranchUpdates) <-
    Codebase.branchUpdates codebase
  thread <- forkIO . forever $ do
    updatedBranches <- externalBranchUpdates
    atomically . Q.enqueue q . UnisonBranchChanged $ updatedBranches
  pure (cancelExternalBranchUpdates >> killThread thread)

warnNote :: String -> String
warnNote s = "⚠️  " <> s

backtick :: IsString s => P.Pretty s -> P.Pretty s
backtick s = P.group ("`" <> s <> "`")

backtickEOS :: IsString s => P.Pretty s -> P.Pretty s
backtickEOS s = P.group ("`" <> s <> "`.")

tip :: P.Pretty CT.ColorText -> P.Pretty CT.ColorText
tip s = P.column2 [(P.bold "Tip:", P.wrap s)]

warn :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
warn s = emojiNote "⚠️" s

problem :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
problem = emojiNote "❗️"

bigproblem :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
bigproblem = emojiNote "‼️"

emojiNote :: (ListLike s Char, IsString s) => String -> P.Pretty s -> P.Pretty s
emojiNote lead s = P.group (fromString lead) <> "\n" <> P.wrap s

nothingTodo :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
nothingTodo s = emojiNote "😶" s

type IsOptional = Bool

data InputPattern = InputPattern
  { patternName :: String
  , aliases     :: [String]
  , args        :: [(IsOptional, ArgumentType)]
  , help        :: P.Pretty CT.ColorText
  , parse       :: [String] -> Either (P.Pretty CT.ColorText) Input
  }

data ArgumentType = ArgumentType
  { typeName :: String
  , suggestions :: forall m v a . Monad m
                => String
                -> Codebase m v a
                -> Branch
                -> m [Line.Completion]
  }

showPatternHelp :: InputPattern -> P.Pretty CT.ColorText
showPatternHelp i = P.lines [
  P.bold (fromString $ patternName i) <> fromString
    (if not . null $ aliases i
     then " (or " <> intercalate ", " (aliases i) <> ")"
     else ""),
  help i ]

validInputs :: [InputPattern]
validInputs = validPatterns
 where
  commandNames = patternName <$> validPatterns
  commandMap   = Map.fromList (commandNames `zip` validPatterns)
  helpPattern  = InputPattern
    "help"
    ["?"]
    [(True, commandName)]
    "`help` shows general help and `help <cmd>` shows help for one command."
    (\case
      []    -> Left $ intercalateMap "\n\n" showPatternHelp validPatterns
      [cmd] -> case Map.lookup cmd commandMap of
        Nothing  -> Left . warn $ "I don't know of that command. Try `help`."
        Just pat -> Left $ help pat
      _ -> Left $ warn "Use `help <cmd>` or `help`."
    )
  commandName =
    ArgumentType "command" $ \q _ _ -> pure $ autoComplete q commandNames
  branchArg = ArgumentType "branch" $ \q codebase _ -> do
    branches <- Codebase.branches codebase
    let bs = Text.unpack <$> branches
    pure $ autoComplete q bs
  definitionQueryArg = ArgumentType "definition query" $ \q _ b -> do
    let names = Text.unpack <$> toList (Branch.allNames (Branch.head b))
    pure $ autoComplete q names
  noCompletions = ArgumentType "a word" $ \_ _ _ -> pure []
  quit          = InputPattern
    "quit"
    ["exit"]
    []
    "Exits the Unison command line interface."
    (\case
      [] -> pure QuitI
      _  -> Left "Use `quit`, `exit`, or <Ctrl-D> to quit."
    )
  validPatterns
    = [ helpPattern
      , InputPattern
        "add"
        []
        []
        (  P.wrap
        $  "`add` adds to the codebase all the definitions from "
        <> "the most recently typechecked file."
        )
        (\ws -> if not $ null ws
          then Left $ warn "`add` doesn't take any arguments."
          else pure $ SlurpFileI False
        )
      , InputPattern
        "branch"
        []
        [(True, branchArg)]
        (P.column2
          [ ("`branch`", P.wrap "lists all branches in the codebase.")
          , ( "`branch foo`"
            , P.wrap
            $  "switches to the branch named 'foo', "
            <> "creating it first if it doesn't exist."
            )
          ]
        )
        (\case
          []  -> pure ListBranchesI
          [b] -> pure . SwitchBranchI $ Text.pack b
          _ ->
            Left
              .  warn
              .  P.wrap
              $  "Use `branch` to list all branches "
              <> "or `branch foo` to switch to or create the branch 'foo'."
        )
      , InputPattern
        "fork"
        []
        [(False, branchArg)]
        (  P.wrap
        $  "`fork foo` creates the branch 'foo' "
        <> "as a fork of the current branch."
        )
        (\case
          [b] -> pure . ForkBranchI $ Text.pack b
          _ ->
            Left
              .  warn
              .  P.wrap
              $  "Use `fork foo` to create the branch 'foo'"
              <> "from the current branch."
        )
      , InputPattern
        "list"
        ["ls"]
        [(True, definitionQueryArg)]
        (P.column2
          [ ("`list`", P.wrap $ "lists all definitions in the current branch.")
          , ( "`list foo`"
            , P.wrap
            $  "lists all definitions with a name similar"
            <> "to 'foo' in the current branch."
            )
          , ( "`list foo bar`"
            , P.wrap
            $  "lists all definitions with a name similar"
            <> "to 'foo' or 'bar' in the current branch."
            )
          ]
        )
        (pure . SearchByNameI)
      , InputPattern
        "merge"
        []
        [(False, branchArg)]
        (P.wrap "`merge foo` merges the branch 'foo' into the current branch.")
        (\case
          [b] -> pure . MergeBranchI $ Text.pack b
          _ ->
            Left
              .  warn
              .  P.wrap
              $  "Use `merge foo` to merge the branch 'foo'"
              <> "into the current branch."
        )
      , InputPattern "view"
                     []
                     [(False, definitionQueryArg)]
                     (P.wrap "`view foo` prints the definition of `foo`.")
                     (pure . ShowDefinitionI)
      , InputPattern
        "rename"
        ["mv"]
        [(False, definitionQueryArg), (False, noCompletions)]
        (P.wrap "`rename foo bar` renames `foo` to `bar`.")
        (\case
          [oldName, newName] -> Right $ RenameUnconflictedI
            allTargets
            (Text.pack oldName)
            (Text.pack newName)
          _ -> Left . P.warnCallout $ P.wrap
            "`rename` takes two arguments, like `rename oldname newname`."
        )
      , InputPattern
        "alias"
        ["cp"]
        [(False, definitionQueryArg), (False, noCompletions)]
        (P.wrap
          "`alias foo bar` introduces `bar` with the same definition as `foo`."
        )
        (\case
          [oldName, newName] -> Right $ AliasUnconflictedI
            allTargets
            (Text.pack oldName)
            (Text.pack newName)
          _ -> Left . warn $ P.wrap
            "`alias` takes two arguments, like `alias oldname newname`."
        )
      , InputPattern
        "update"
        []
        []
        (  P.wrap
        $  "`update` works like `add`, except "
        <> "if a definition in the file "
        <> "has the same name as an existing definition, the name gets updated "
        <> "to point to the new definition. "
        <> "If the old definition has any dependents, `update` will add "
        <> "those dependents to a refactoring session."
        )
        (\ws -> if not $ null ws
          then Left $ warn "`update` doesn't take any arguments."
          else pure $ SlurpFileI True
        )
      , InputPattern
        "todo"
        []
        []
        (P.wrap
        $ "`todo` lists the work remaining in the current branch " <>
          "to complete an ongoing refactoring."
        )
        (\ws -> if not $ null ws
                   then Left $ warn "`todo` doesn't take any arguments."
                   else pure $ TodoI)
      , quit
      ]
  allTargets = Set.fromList [Names.TermName, Names.TypeName]

completion :: String -> Line.Completion
completion s = Line.Completion s s True

autoComplete :: String -> [String] -> [Line.Completion]
autoComplete q ss = fixup $
  completion <$> (id $ Codebase.sortedApproximateMatches q ss)
  where
  -- workaround for https://github.com/judah/haskeline/issues/100
  -- if the common prefix of all the completions is smaller than
  -- the query, we make all the replacements equal to the query,
  -- which will preserve what the user has typed
  fixup [] = []
  fixup [c] = [c]
  fixup cs@(h:t) = let
    commonPrefix (h1:t1) (h2:t2) | h1 == h2 = h1 : commonPrefix t1 t2
    commonPrefix _ _             = ""
    overallCommonPrefix =
      foldl commonPrefix (Line.replacement h) (Line.replacement <$> t)
    in if length overallCommonPrefix < length q
       then [ c { Line.replacement = q } | c <- cs ]
       else cs

parseInput
  :: Map String InputPattern -> [String] -> Either (P.Pretty CT.ColorText) Input
parseInput patterns ss = case ss of
  []             -> Left ""
  command : args -> case Map.lookup command patterns of
    Just pat -> parse pat args
    Nothing ->
      Left
        .  warn
        .  P.wrap
        $  "I don't know how to "
        <> P.group (fromString command <> ".")
        <> "Type `help` or `?` to get help."

prompt :: String
prompt = "> "

putPrettyLn :: P.Pretty CT.ColorText -> IO ()
putPrettyLn p = do
  width <- getAvailableWidth
  putStrLn . P.toANSI width $ P.border 2 p

getAvailableWidth :: IO Int
getAvailableWidth =
  fromMaybe 80 . fmap (\s -> 120 `min` Terminal.width s) <$> Terminal.size

getUserInput
  :: (MonadIO m, Line.MonadException m)
  => Map String InputPattern
  -> Codebase m v a
  -> Branch
  -> BranchName
  -> m Input
getUserInput patterns codebase branch branchName = Line.runInputT settings $ do
  line <- Line.getInputLine $
    P.toANSI 80 (P.green (P.text branchName <> fromString prompt))
  case line of
    Nothing -> pure QuitI
    Just l  -> case parseInput patterns $ words l of
      Left msg -> lift $ do
        liftIO $ putPrettyLn msg
        getUserInput patterns codebase branch branchName
      Right i -> pure i
 where
  settings    = Line.Settings tabComplete (Just ".unisonHistory") True
  tabComplete = Line.completeWordWithPrev Nothing " " $ \prev word ->
    -- User hasn't finished a command name, complete from command names
    if null prev
      then pure $ autoComplete word (Map.keys patterns)
    -- User has finished a command name; use completions for that command
      else case words $ reverse prev of
        h : t -> fromMaybe (pure []) $ do
          p            <- Map.lookup h patterns
          (_, argType) <- listToMaybe $ drop (length t) (args p)
          pure $ suggestions argType word codebase branch
        _ -> pure []

main
  :: forall v
   . Var v
  => FilePath
  -> BranchName
  -> Maybe FilePath
  -> IO (Runtime v)
  -> Codebase IO v Ann
  -> IO ()
main dir currentBranchName _initialFile startRuntime codebase = do
  currentBranch <- Codebase.getBranch codebase currentBranchName
  eventQueue    <- Q.newIO
  currentBranch <- case currentBranch of
    Nothing ->
      Codebase.mergeBranch codebase currentBranchName Codebase.builtinBranch
        <* (  putStrLn
           $  "☝️  I found no branch named '"
           <> Text.unpack currentBranchName
           <> "' so I've created it for you."
           )
    Just b -> pure b
  do
    runtime                  <- startRuntime
    branchRef                <- newIORef (currentBranch, currentBranchName)
    cancelFileSystemWatch    <- watchFileSystem eventQueue dir
    cancelWatchBranchUpdates <- watchBranchUpdates eventQueue codebase
    let patternMap =
          Map.fromList
            $   validInputs
            >>= (\p -> [(patternName p, p)] ++ ((, p) <$> aliases p))
        getInput = do
          (branch, branchName) <- readIORef branchRef
          getUserInput patternMap codebase branch branchName
    let awaitInput = do
          -- Race the user input and file watch.
          Async.race (atomically $ Q.peek eventQueue) getInput >>= \case
            Left _ -> Left <$> atomically (Q.dequeue eventQueue)
            x      -> pure x
        cleanup = do
          Runtime.terminate runtime
          cancelFileSystemWatch
          cancelWatchBranchUpdates
    (`finally` cleanup)
      $ E.commandLine awaitInput
                           runtime
                           (\b bn -> writeIORef branchRef (b, bn))
                           (notifyUser dir)
                           codebase
      $ Actions.startLoop currentBranch currentBranchName
