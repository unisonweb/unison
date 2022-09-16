module Unison.Codebase.Editor.HandleInput.Update
  ( handleUpdate,
    doSlurpAdds,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import Debug.Pretty.Simple
import qualified Unison.ABT as ABT
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import Unison.Cli.NamesUtils (displayNames)
import Unison.Cli.PrettyPrintUtils (prettyPrintEnvDecl)
import Unison.Cli.TypeCheck (typecheckFile)
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
import Unison.Hash (Hash)
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Names (Names)
import qualified Unison.Names as Names
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPE hiding (biasTo)
import Unison.Reference (Reference (..), TermReference, TermReferenceId, TypeReference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Result as Result
import Unison.Runtime.IOSource (isTest)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Typechecker as Typechecker
import Unison.UnisonFile (TypecheckedUnisonFile, UnisonFile)
import qualified Unison.UnisonFile as UF
import qualified Unison.UnisonFile.Names as UF
import Unison.UnisonFile.Type (UnisonFile (UnisonFileId))
import Unison.Util.Monoid (foldMapM)
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
  let patchPath =
        case optionalPatch of
          NoPatch -> Nothing
          DefaultPatch -> Just Cli.defaultPatchPath
          UsePatch p -> Just p
  slurpCheckNames <- Branch.toNames <$> Cli.getCurrentBranch0
  sr <- getSlurpResultForUpdate requestedNames
  let addsAndUpdates :: SlurpComponent Symbol
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
          hashTerms0 = (\(r, _wk, _tm, typ) -> (r, typ)) <$> UF.hashTerms (Slurp.originalFile sr)
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

getSlurpResultForUpdate :: Set Name -> Cli r (SlurpResult Symbol)
getSlurpResultForUpdate requestedNames = do
  Cli.Env {codebase} <- ask
  -- ("ping", #old.ping)
  -- ("pong", #old.pong)
  slurpCheckNames <- Branch.toNames <$> Cli.getCurrentBranch0

  let slurpItUp :: TypecheckedUnisonFile Symbol Ann -> SlurpResult Symbol
      slurpItUp file =
        Slurp.slurpFile file (Set.map Name.toVar requestedNames) Slurp.UpdateOp slurpCheckNames

  -- #old.ping => {"ping"}
  -- #old.pong => {"pong"}
  let termNames :: TermReferenceId -> Set Name
      termNames =
        Names.namesForReferent slurpCheckNames . Referent.fromTermReferenceId

  -- First, compute an initial slurp, which will identify the initial set of definitions we are updating.
  slurp0 <- slurpItUp <$> Cli.expectLatestTypecheckedFile

  -- slurp0 says "ping is an update"

  -- {"ping"}
  let termVarsBeingUpdated :: Set Symbol
      termVarsBeingUpdated =
        SC.terms (Slurp.updates slurp0)

  pTraceM "termVarsBeingUpdated"
  pTraceShowM termVarsBeingUpdated

  -- Map each term update to the *old* set of references that its name referred to.
  -- TODO rename
  -- TODO non-empty set?
  --
  -- "ping" => {#old.ping}
  let oinkmap :: Map Symbol (Set TermReference)
      oinkmap =
        Map.fromSet (Names.refTermsNamed slurpCheckNames . Name.unsafeFromVar) termVarsBeingUpdated

  -- TODO inline this?
  -- {#old}
  let termHashesBeingUpdated :: Set Hash
      termHashesBeingUpdated =
        foldMap (Set.mapMaybe Reference.toHash) oinkmap

  pTraceM "termHashesBeingUpdated"
  pTraceShowM termHashesBeingUpdated

  let notBeingUpdated :: (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) -> Bool
      notBeingUpdated (ref, _) =
        Set.disjoint termVarsBeingUpdated (Set.map Name.toVar (termNames ref))

  -- FIXME: this only looks at old components; doesn't search for new cycles
  --
  -- [(#old.pong, (oldpong, oldpongtype))]
  implicitTerms <-
    liftIO do
      foldMapM
        ( \hash -> do
            terms <- Codebase.unsafeGetTermComponent codebase hash
            pure (filter notBeingUpdated (Reference.componentFor hash terms))
        )
        (Set.toList termHashesBeingUpdated)

  pTraceM "implicitTerms"
  pTraceShowM implicitTerms

  if null implicitTerms
    then pure slurp0
    else do
      -- For the name of a term being updated, resolve its name to its reference.
      --
      -- "ping" => #newping
      let varToNewRef :: Symbol -> TermReferenceId
          varToNewRef var =
            case UF.hashTermsId (Slurp.originalFile slurp0) Map.! var of
              (r, _, _, _) -> r
      -- Construct a mapping from old-to-new references, for each term being updated.
      --
      -- #old.ping => #newping
      let termMapping :: Map TermReference TermReferenceId
          termMapping =
            Map.foldlWithKey'
              ( \acc var refs0 ->
                  let ref1 = varToNewRef var
                   in foldl' (\acc1 ref0 -> Map.insert ref0 ref1 acc1) acc refs0
              )
              Map.empty
              oinkmap
      let rewriteReferences :: Term Symbol Ann -> Term Symbol Ann
          rewriteReferences =
            ABT.rebuildUp \term ->
              case term of
                Term.Ref ref0 ->
                  case Map.lookup ref0 termMapping of
                    Nothing -> term
                    Just ref1 -> Term.Ref (Reference.fromId ref1)
                _ -> term

      -- For each implicit term, rewrite its references in-place, from old to new.
      --
      -- #old.pong => oldpong[#newping/#old.ping]
      let implicitTerms1 :: Map TermReferenceId (Term Symbol Ann)
          implicitTerms1 =
            List.foldl'
              (\acc (ref, (term, _typ)) -> Map.insert ref (rewriteReferences term) acc)
              Map.empty
              implicitTerms
      -- #newping  => ("v0", newping["v1"/#old.ping])
      -- #old.ping => ("v1", oldpong["v0"/#old.ping])
      let oogabooga :: Map TermReferenceId (Symbol, Term Symbol Ann)
          oogabooga =
            let fileTerms =
                  slurp0
                    & Slurp.originalFile
                    & UF.hashTermsId
                    & Map.elems
                    & map (\(ref, _wk, term, _typ) -> (ref, term))
                    & Map.fromList
             in Term.unhashComponent (Map.union fileTerms implicitTerms1)
      let unisonFile :: UnisonFile Symbol Ann
          unisonFile =
            UnisonFileId
              { dataDeclarationsId = UF.dataDeclarationsId' (Slurp.originalFile slurp0),
                effectDeclarationsId = UF.effectDeclarationsId' (Slurp.originalFile slurp0),
                terms = Map.elems oogabooga,
                -- In the context of this update, whatever watches were in the latest typechecked Unison file are
                -- irrelevant, so we don't need to copy them over.
                -- FIXME ugh entire component of watches
                watches = Map.empty
              }
      result <- typecheckFile [] unisonFile
      case runIdentity (Result.toMaybe result) of
        -- TODO recover better names
        Just (Right file) -> pure (slurpItUp file)
        _ -> pure slurp0

-- updates the namespace for adding `slurp`
doSlurpAdds ::
  forall m v.
  (Monad m, Var v) =>
  SlurpComponent v ->
  TypecheckedUnisonFile v Ann ->
  (Branch0 m -> Branch0 m)
doSlurpAdds slurp uf = Branch.batchUpdates (typeActions <> termActions)
  where
    typeActions = map doType . toList $ SC.types slurp
    termActions =
      map doTerm . toList $
        SC.terms slurp <> UF.constructorsForDecls (SC.types slurp) uf
    names = UF.typecheckedToNames uf
    tests = Set.fromList $ fst <$> UF.watchesOfKind WK.TestWatch (UF.discardTypes uf)
    (isTestType, isTestValue) = isTest
    md v =
      if Set.member v tests
        then Metadata.singleton isTestType isTestValue
        else Metadata.empty
    doTerm :: v -> (Path, Branch0 m -> Branch0 m)
    doTerm v = case toList (Names.termsNamed names (Name.unsafeFromVar v)) of
      [] -> errorMissingVar v
      [r] -> case Path.splitFromName (Name.unsafeFromVar v) of
        Nothing -> errorEmptyVar
        Just split -> BranchUtil.makeAddTermName split r (md v)
      wha ->
        error $
          "Unison bug, typechecked file w/ multiple terms named "
            <> Var.nameStr v
            <> ": "
            <> show wha
    doType :: v -> (Path, Branch0 m -> Branch0 m)
    doType v = case toList (Names.typesNamed names (Name.unsafeFromVar v)) of
      [] -> errorMissingVar v
      [r] -> case Path.splitFromName (Name.unsafeFromVar v) of
        Nothing -> errorEmptyVar
        Just split -> BranchUtil.makeAddTypeName split r Metadata.empty
      wha ->
        error $
          "Unison bug, typechecked file w/ multiple types named "
            <> Var.nameStr v
            <> ": "
            <> show wha
    errorEmptyVar = error "encountered an empty var name"
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
        doDeprecate (n, r) = case Path.splitFromName n of
          Nothing -> errorEmptyVar
          Just split -> [BranchUtil.makeDeleteTermName split r]

    -- we copy over the metadata on the old thing
    -- todo: if the thing being updated, m, is metadata for something x in b0
    -- update x's md to reference `m`
    doType :: (Name, TypeReference, TypeReference) -> [(Path, Branch0 m -> Branch0 m)]
    doType (n, old, new) = case Path.splitFromName n of
      Nothing -> errorEmptyVar
      Just split ->
        [ BranchUtil.makeDeleteTypeName split old,
          BranchUtil.makeAddTypeName split new oldMd
        ]
        where
          oldMd = BranchUtil.getTypeMetadataAt split old b0
    doTerm :: (Name, TermReference, TermReference) -> [(Path, Branch0 m -> Branch0 m)]
    doTerm (n, old, new) = case Path.splitFromName n of
      Nothing -> errorEmptyVar
      Just split ->
        [ BranchUtil.makeDeleteTermName split (Referent.Ref old),
          BranchUtil.makeAddTermName split (Referent.Ref new) oldMd
        ]
        where
          -- oldMd is the metadata linked to the old definition
          -- we relink it to the new definition
          oldMd = BranchUtil.getTermMetadataAt split (Referent.Ref old) b0
    errorEmptyVar = error "encountered an empty var name"

-- Returns True if the operation changed the namespace, False otherwise.
propagatePatchNoSync ::
  Patch ->
  Path.Absolute ->
  Cli r Bool
propagatePatchNoSync patch scopePath =
  Cli.time "propagatePatchNoSync" do
    Cli.stepAtNoSync' (Path.unabsolute scopePath, Propagate.propagateAndApply patch)
