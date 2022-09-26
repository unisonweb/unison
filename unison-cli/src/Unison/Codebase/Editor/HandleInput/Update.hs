module Unison.Codebase.Editor.HandleInput.Update
  ( handleUpdate,
    doSlurpAdds,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import Unison.Cli.NamesUtils (displayNames)
import Unison.Cli.PrettyPrintUtils (prettyPrintEnvDecl)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch0 (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Names as Branch
import qualified Unison.Codebase.BranchUtil as BranchUtil
import Unison.Codebase.Editor.HandleInput.MetadataUtils (addDefaultMetadata)
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Propagate as Propagate
import qualified Unison.Codebase.Editor.Slurp as Slurp
import Unison.Codebase.Editor.SlurpComponent (SlurpComponent (..))
import qualified Unison.Codebase.Editor.SlurpComponent as SC
import Unison.Codebase.Editor.SlurpResult (SlurpResult (..))
import qualified Unison.Codebase.Editor.SlurpResult as Slurp
import qualified Unison.Codebase.Metadata as Metadata
import Unison.Codebase.Patch (Patch (..))
import qualified Unison.Codebase.Patch as Patch
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.TermEdit as TermEdit
import qualified Unison.Codebase.TypeEdit as TypeEdit
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Names (Names)
import qualified Unison.Names as Names
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPE hiding (biasTo)
import Unison.Reference (Reference (..), TermReference, TypeReference)
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Runtime.IOSource (isTest)
import Unison.Symbol (Symbol)
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Typechecker as Typechecker
import qualified Unison.UnisonFile as UF
import qualified Unison.UnisonFile.Names as UF
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Set as Set
import Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.WatchKind as WK

-- | Handle an @update@ command.
handleUpdate :: Input -> OptionalPatch -> Set Name -> Cli r ()
handleUpdate input optionalPatch requestedNames = do
  Cli.Env {codebase} <- ask
  currentPath' <- Cli.getCurrentPath
  uf <- Cli.expectLatestTypecheckedFile
  let patchPath =
        case optionalPatch of
          NoPatch -> Nothing
          DefaultPatch -> Just Cli.defaultPatchPath
          UsePatch p -> Just p
  slurpCheckNames <- Branch.toNames <$> Cli.getCurrentBranch0
  let requestedVars = Set.map Name.toVar requestedNames
  let sr = Slurp.slurpFile uf requestedVars Slurp.UpdateOp slurpCheckNames
      addsAndUpdates :: SlurpComponent Symbol
      addsAndUpdates = Slurp.updates sr <> Slurp.adds sr
      fileNames :: Names
      fileNames = UF.typecheckedToNames (Slurp.originalFile sr)
      -- todo: display some error if typeEdits or termEdits itself contains a loop
      typeEdits :: [(Name, Reference, Reference)]
      typeEdits = do
        v <- Set.toList (SC.types (updates sr))
        let n = Name.unsafeFromVar v
        let oldRefs0 = Names.typesNamed slurpCheckNames n
        let newRefs = Names.typesNamed fileNames n
        case (,) <$> NESet.nonEmptySet oldRefs0 <*> Set.asSingleton newRefs of
          Nothing -> error (reportBug "E722145" ("bad (old,new) names: " ++ show (oldRefs0, newRefs)))
          Just (oldRefs, newRef) -> do
            oldRef <- Foldable.toList oldRefs
            [(n, oldRef, newRef)]
      hashTerms :: Map Reference (Type Symbol Ann)
      hashTerms = Map.fromList (toList hashTerms0)
        where
          hashTerms0 = (\(_ann, r, _wk, _tm, typ) -> (r, typ)) <$> UF.hashTerms (Slurp.originalFile sr)
      termEdits :: [(Name, Reference, Reference)]
      termEdits = do
        v <- Set.toList (SC.terms (updates sr))
        let n = Name.unsafeFromVar v
        let oldRefs0 = Names.refTermsNamed slurpCheckNames n
        let newRefs = Names.refTermsNamed fileNames n
        case (,) <$> NESet.nonEmptySet oldRefs0 <*> Set.asSingleton newRefs of
          Nothing -> error (reportBug "E936103" ("bad (old,new) names: " ++ show (oldRefs0, newRefs)))
          Just (oldRefs, newRef) -> do
            oldRef <- Foldable.toList oldRefs
            [(n, oldRef, newRef)]
      termDeprecations :: [(Name, Referent)]
      termDeprecations =
        [ (n, r)
          | (_, oldTypeRef, _) <- typeEdits,
            (n, r) <- Names.constructorsForType oldTypeRef slurpCheckNames
        ]
  patchOps <- for patchPath \patchPath -> do
    ye'ol'Patch <- Cli.getPatchAt patchPath
    -- If `uf` updates a -> a', we want to replace all (a0 -> a) in patch
    -- with (a0 -> a') in patch'.
    -- So for all (a0 -> a) in patch, for all (a -> a') in `uf`,
    -- we must know the type of a0, a, a'.
    let -- we need:
        -- all of the `old` references from the `new` edits,
        -- plus all of the `old` references for edits from patch we're replacing
        collectOldForTyping :: [(Reference, Reference)] -> Patch -> Set Reference
        collectOldForTyping new old = foldl' f mempty (new ++ fromOld)
          where
            f acc (r, _r') = Set.insert r acc
            newLHS = Set.fromList . fmap fst $ new
            fromOld :: [(Reference, Reference)]
            fromOld =
              [ (r, r') | (r, TermEdit.Replace r' _) <- R.toList . Patch._termEdits $ old, Set.member r' newLHS
              ]
        neededTypes = collectOldForTyping (map (\(_, old, new) -> (old, new)) termEdits) ye'ol'Patch

    allTypes :: Map Reference (Type v Ann) <-
      fmap Map.fromList . for (toList neededTypes) $ \r ->
        (r,) . fromMaybe (Type.builtin External "unknown type")
          <$> (liftIO . Codebase.getTypeOfTerm codebase) r

    let typing r1 r2 = case (Map.lookup r1 allTypes, Map.lookup r2 hashTerms) of
          (Just t1, Just t2)
            | Typechecker.isEqual t1 t2 -> TermEdit.Same
            | Typechecker.isSubtype t1 t2 -> TermEdit.Subtype
            | otherwise -> TermEdit.Different
          e ->
            error $
              "compiler bug: typing map not constructed properly\n"
                <> "typing "
                <> show r1
                <> " "
                <> show r2
                <> " : "
                <> show e

        updatePatch :: Patch -> Patch
        updatePatch p = foldl' step2 p' termEdits
          where
            p' = foldl' step1 p typeEdits
            step1 p (_, r, r') = Patch.updateType r (TypeEdit.Replace r') p
            step2 p (_, r, r') = Patch.updateTerm typing r (TermEdit.Replace r' (typing r r')) p
        (p, seg) = Path.toAbsoluteSplit currentPath' patchPath
        updatePatches :: Monad m => Branch0 m -> m (Branch0 m)
        updatePatches = Branch.modifyPatches seg updatePatch
    pure (updatePatch ye'ol'Patch, updatePatches, p)

  when (Slurp.hasAddsOrUpdates sr) $ do
    -- take a look at the `updates` from the SlurpResult
    -- and make a patch diff to record a replacement from the old to new references
    Cli.stepManyAtMNoSync
      ( [ ( Path.unabsolute currentPath',
            pure . doSlurpUpdates typeEdits termEdits termDeprecations
          ),
          ( Path.unabsolute currentPath',
            pure . doSlurpAdds addsAndUpdates (Slurp.originalFile sr)
          )
        ]
          ++ case patchOps of
            Nothing -> []
            Just (_, update, p) -> [(Path.unabsolute p, update)]
      )
    liftIO . Codebase.addDefsToCodebase codebase . Slurp.filterUnisonFile sr $ Slurp.originalFile sr
  ppe <- prettyPrintEnvDecl =<< displayNames (Slurp.originalFile sr)
  Cli.respond $ SlurpOutput input (PPE.suffixifiedPPE ppe) sr
  whenJust patchOps \(updatedPatch, _, _) ->
    void $ propagatePatchNoSync updatedPatch currentPath'
  addDefaultMetadata addsAndUpdates
  Cli.syncRoot case patchPath of
    Nothing -> "update.nopatch"
    Just p ->
      p & Path.unsplit'
        & Path.resolve @_ @_ @Path.Absolute currentPath'
        & tShow

-- updates the namespace for adding `slurp`
doSlurpAdds ::
  forall m v.
  (Monad m, Var v) =>
  SlurpComponent v ->
  UF.TypecheckedUnisonFile v Ann ->
  (Branch0 m -> Branch0 m)
doSlurpAdds slurp uf = Branch.batchUpdates (typeActions <> termActions)
  where
    typeActions = map doType . toList $ SC.types slurp
    termActions =
      map doTerm . toList $
        SC.terms slurp <> UF.constructorsForDecls (SC.types slurp) uf
    names = UF.typecheckedToNames uf
    tests = Set.fromList $ view _2 <$> UF.watchesOfKind WK.TestWatch (UF.discardTypes uf)
    (isTestType, isTestValue) = isTest
    md v =
      if Set.member v tests
        then Metadata.singleton isTestType isTestValue
        else Metadata.empty
    doTerm :: v -> (Path, Branch0 m -> Branch0 m)
    doTerm v = case toList (Names.termsNamed names (Name.unsafeFromVar v)) of
      [] -> errorMissingVar v
      [r] ->
        let split = Path.splitFromName (Name.unsafeFromVar v)
         in BranchUtil.makeAddTermName split r (md v)
      wha ->
        error $
          "Unison bug, typechecked file w/ multiple terms named "
            <> Var.nameStr v
            <> ": "
            <> show wha
    doType :: v -> (Path, Branch0 m -> Branch0 m)
    doType v = case toList (Names.typesNamed names (Name.unsafeFromVar v)) of
      [] -> errorMissingVar v
      [r] ->
        let split = Path.splitFromName (Name.unsafeFromVar v)
         in BranchUtil.makeAddTypeName split r Metadata.empty
      wha ->
        error $
          "Unison bug, typechecked file w/ multiple types named "
            <> Var.nameStr v
            <> ": "
            <> show wha
    errorMissingVar v = error $ "expected to find " ++ show v ++ " in " ++ show uf

doSlurpUpdates ::
  Monad m =>
  [(Name, TypeReference, TypeReference)] ->
  [(Name, TermReference, TermReference)] ->
  [(Name, Referent)] ->
  (Branch0 m -> Branch0 m)
doSlurpUpdates typeEdits termEdits deprecated b0 =
  Branch.batchUpdates (typeActions <> termActions <> deprecateActions) b0
  where
    typeActions = join . map doType $ typeEdits
    termActions = join . map doTerm $ termEdits
    deprecateActions = join . map doDeprecate $ deprecated
      where
        doDeprecate (n, r) = [BranchUtil.makeDeleteTermName (Path.splitFromName n) r]

    -- we copy over the metadata on the old thing
    -- todo: if the thing being updated, m, is metadata for something x in b0
    -- update x's md to reference `m`
    doType :: (Name, TypeReference, TypeReference) -> [(Path, Branch0 m -> Branch0 m)]
    doType (n, old, new) =
      let split = Path.splitFromName n
          oldMd = BranchUtil.getTypeMetadataAt split old b0
       in [ BranchUtil.makeDeleteTypeName split old,
            BranchUtil.makeAddTypeName split new oldMd
          ]
    doTerm :: (Name, TermReference, TermReference) -> [(Path, Branch0 m -> Branch0 m)]
    doTerm (n, old, new) =
      [ BranchUtil.makeDeleteTermName split (Referent.Ref old),
        BranchUtil.makeAddTermName split (Referent.Ref new) oldMd
      ]
      where
        split = Path.splitFromName n
        -- oldMd is the metadata linked to the old definition
        -- we relink it to the new definition
        oldMd = BranchUtil.getTermMetadataAt split (Referent.Ref old) b0

-- Returns True if the operation changed the namespace, False otherwise.
propagatePatchNoSync ::
  Patch ->
  Path.Absolute ->
  Cli r Bool
propagatePatchNoSync patch scopePath =
  Cli.time "propagatePatchNoSync" do
    Cli.stepAtNoSync' (Path.unabsolute scopePath, Propagate.propagateAndApply patch)
