module Unison.Codebase.Editor.HandleInput.Delete
  ( handleDelete,
  )
where

import Control.Lens
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.Text qualified as Text
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.HandleInput.DeleteNamespace (getEndangeredDependents)
import Unison.Codebase.Editor.HandleInput.NamespaceDiffUtils (diffHelper)
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import Unison.Codebase.Path (Path' (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency (LabeledDependency)
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Names (Names (Names))
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Util.Relation qualified as R

handleDelete :: DeleteTarget -> Cli ()
handleDelete target = do
  pp <- Cli.getCurrentProjectPath
  let getTerms = Cli.getTermsAt . fmap (first $ flip (set PP.absPath_) pp)
  let getTypes = Cli.getTypesAt . fmap (first $ flip (set PP.absPath_) pp)
  case target of
    DeleteTarget'TermOrType doutput hqs ->
      let description = Text.unwords (foldOutput "delete" "delete.verbose" doutput : map targetToText hqs)
       in delete description doutput getTerms getTypes hqs
    DeleteTarget'Term doutput hqs ->
      let description = Text.unwords (foldOutput "delete.term" "delete.term.verbose" doutput : map targetToText hqs)
       in delete description doutput getTerms (const (pure Set.empty)) hqs
    DeleteTarget'Type doutput hqs ->
      let description = Text.unwords (foldOutput "delete.type" "delete.type.verbose" doutput : map targetToText hqs)
       in delete description doutput (const (pure Set.empty)) getTypes hqs
  where
    foldOutput :: a -> a -> DeleteOutput -> a
    foldOutput noDiff yesDiff = \case
      DeleteOutput'NoDiff -> noDiff
      DeleteOutput'Diff -> yesDiff

    targetToText :: HQ'.HashQualified (Path.Split Path') -> Text
    targetToText =
      HQ'.toTextWith (Path.toText . Path.unsplit)

delete ::
  Text ->
  DeleteOutput ->
  (HQ'.HashQualified (Path.Split Path.Absolute) -> Cli (Set Referent)) -> -- compute matching terms
  (HQ'.HashQualified (Path.Split Path.Absolute) -> Cli (Set Reference)) -> -- compute matching types
  [HQ'.HashQualified (Path.Split Path')] -> -- targets for deletion
  Cli ()
delete description doutput getTerms getTypes hqs' = do
  -- persists the original hash qualified entity for error reporting
  typesTermsTuples <-
    for hqs' \hq -> do
      absolute <- traverse Cli.resolveSplit' hq
      types <- getTypes (first PP.absPath <$> absolute)
      terms <- getTerms (first PP.absPath <$> absolute)
      return (hq, types, terms)

  -- if there are any entities which cannot be deleted because they don't exist, short circuit.
  do
    let notFounds = List.filter (\(_, types, terms) -> Set.null terms && Set.null types) typesTermsTuples
    when (not (null notFounds)) do
      let toName :: [(HQ'.HashQualified (Path.Split Path'), Set Reference, Set referent)] -> [Name]
          toName notFounds =
            map (\(split, _, _) -> Path.nameFromSplit $ HQ'.toName split) notFounds
      Cli.returnEarly $ NamesNotFound (toName notFounds)

  let toSplitName ::
        (HQ'.HashQualified (Path.Split Path'), Set Reference, Set Referent) ->
        Cli (Path.Split Path.Absolute, Name, Set Reference, Set Referent)
      toSplitName hq = do
        (pp, ns) <- Cli.resolveSplit' (HQ'.toName $ hq ^. _1)
        let resolvedSplit = (pp.absPath, ns)
        pure
          ( resolvedSplit,
            Name.makeRelative $ Path.nameFromSplit resolvedSplit,
            hq ^. _2,
            hq ^. _3
          )

  -- get the splits and names with terms and types
  splitsNames <- traverse toSplitName typesTermsTuples
  let toRel :: (Ord ref) => Set ref -> Name -> R.Relation Name ref
      toRel setRef name = R.fromList (fmap (name,) (toList setRef))
  let toDelete = fmap (\(_, names, types, terms) -> Names (toRel terms names) (toRel types names)) splitsNames
  -- make sure endangered is compeletely contained in paths
  currentBranch <- Cli.getCurrentProjectRoot0
  let projectNames = Branch.toNames currentBranch
      projectNamesSansLib = Branch.toNames (Branch.deleteLibdeps currentBranch)
  -- get only once for the entire deletion set
  let allTermsToDelete :: Set LabeledDependency
      allTermsToDelete = Set.unions (fmap Names.labeledReferences toDelete)
  -- get the endangered dependencies for each entity to delete
  endangered <-
    Cli.runTransaction $
      traverse
        (\targetToDelete -> getEndangeredDependents targetToDelete allTermsToDelete projectNames projectNamesSansLib)
        toDelete
  -- If the overall dependency map is not completely empty, abort deletion
  let endangeredDeletions = List.filter (\m -> not $ null m || Map.foldr (\s b -> null s || b) False m) endangered
  if null endangeredDeletions
    then do
      let deleteTypesTerms =
            splitsNames
              >>= ( \(split, _, types, terms) ->
                      (map (BranchUtil.makeDeleteTypeName split) . Set.toList $ types)
                        ++ (map (BranchUtil.makeDeleteTermName split) . Set.toList $ terms)
                  )
      before <- Cli.getCurrentBranch0
      pb <- Cli.getCurrentProjectBranch
      Cli.stepManyAt pb description deleteTypesTerms
      case doutput of
        DeleteOutput'Diff -> do
          after <- Cli.getCurrentBranch0
          (ppe, diff) <- diffHelper before after
          Cli.respondNumbered (ShowDiffAfterDeleteDefinitions ppe diff)
        DeleteOutput'NoDiff -> do
          Cli.respond Success
    else do
      let ppeDecl = PPED.makePPED (PPE.hqNamer 10 projectNames) (PPE.suffixifyByHash projectNames)
      let combineRefs = List.foldl (Map.unionWith NESet.union) Map.empty endangeredDeletions
      Cli.respondNumbered (CantDeleteDefinitions ppeDecl combineRefs)
