{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Unison.Codebase.Editor.Slurp where

import Control.Lens
import Control.Monad.State
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.DataDeclaration as DD
import Unison.Hash (Hash)
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Names (Names)
import qualified Unison.Names as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Reference as Ref
import qualified Unison.Referent as Referent
import qualified Unison.UnisonFile as UF
import qualified Unison.UnisonFile.Names as UF
import qualified Unison.UnisonFile.Type as UF
import Unison.Var (Var)

data LabeledVar v = LabeledVar v LD.LabeledDependency

data SlurpStatus = New | Updated | Duplicate

data SlurpOp = Add | Update

data TypeOrTermVar v = TypeVar v | TermVar v

untypedVar :: TypeOrTermVar v -> v
untypedVar = \case
  TypeVar v -> v
  TermVar v -> v

data SlurpPrintout v = SlurpPrintout
  { notOk :: Map v SlurpErr,
    ok :: Map v SlurpStatus
  }

data SlurpErr
  = TermCtorCollision
  | CtorTermCollision

data SlurpComponent v = SlurpComponent {types :: Set v, terms :: Set v}
  deriving (Eq, Ord, Show)

data DefinitionNotes v
  = DefStatus SlurpStatus
  | DefErr SlurpErr

data ComponentNotes v = ComponentNotes
  { deps :: Set ComponentHash,
    definitions :: Map v (DefinitionNotes v)
  }

data SlurpResult v = SlurpResult
  { componentNotes :: Map (LabeledVar v) (DefinitionNotes v, Set (LabeledVar v))
  }

type ComponentHash = Hash

data Components v = Components
  { termComponents :: Map Hash (Set v),
    typeComponents :: Map Hash (Set v)
  }

toSlurpPrintout :: SlurpResult v -> SlurpPrintout v
toSlurpPrintout = undefined

collectComponents :: UF.TypecheckedUnisonFile v Ann -> Components v
collectComponents _uf = Components {termComponents, typeComponents}
  where
    termComponents = undefined
    typeComponents = undefined

computeComponentDependencies :: Components v -> Map Hash (Set Hash)
computeComponentDependencies = undefined

analyzeTypecheckedUnisonFile ::
  forall v.
  Var v =>
  UF.TypecheckedUnisonFile v Ann ->
  Names ->
  Maybe (Set v) ->
  SlurpResult v
analyzeTypecheckedUnisonFile uf unalteredCodebaseNames maybeDefsToConsider =
  let allInvolvedVars :: Set (LabeledVar v)
      allInvolvedVars =
        Foldable.foldl' transitiveVarDeps mempty defsToConsider
          & Set.map (\v -> labeledVars Map.! v)
      allDefStatuses :: Map (LabeledVar v) (DefinitionNotes v, Set (LabeledVar v))
      allDefStatuses =
        allInvolvedVars
          & Set.toList
          & fmap
            ( \v ->
                -- TODO, save time by memoizing transitiveVarDeps?
                (v, (definitionStatus v, transitiveVarDeps mempty v))
            )
          & Map.fromList
   in SlurpResult allDefStatuses
  where
    fileNames :: Names
    fileNames = UF.typecheckedToNames uf

    defsToConsider :: Set v
    defsToConsider = case maybeDefsToConsider of
      Nothing -> Set.map untypedVar allDefinitions
      Just vs -> vs

    labeledVars :: Map v (LabeledVar v)
    labeledVars = undefined

    componentMapping :: Map ComponentHash (Set (TypeOrTermVar v))
    componentMapping = undefined UF.componentMap uf
    -- codebaseNames with deprecated constructors removed.

    allDefinitions :: Set (TypeOrTermVar v)
    allDefinitions = fold componentMapping

    componentNotes' :: Map ComponentHash (Map v (DefinitionNotes v))
    componentNotes' = undefined

    varToLabeledDependency :: v -> LD.LabeledDependency
    varToLabeledDependency = undefined

    definitionStatus :: LabeledVar v -> DefinitionNotes v
    definitionStatus (undefined -> tv) =
      let v = untypedVar tv
          existingTypesAtName = Names.typesNamed codebaseNames (Name.unsafeFromVar v)
          existingTermsAtName = Names.termsNamed codebaseNames (Name.unsafeFromVar v)
          existingTermsMatchingReference = Names.termsNamed codebaseNames (Name.unsafeFromVar v)
          varRef = varReferences Map.! v
       in case tv of
            TermVar {} ->
              case Set.toList existingTermsAtName of
                [] -> DefStatus New
                [r] | LD.referent r == varRef -> DefStatus Duplicate
                [Referent.Con {}] | LD.ConReference {} <- varRef -> DefErr TermCtorCollision
                [Referent.Ref {}] | LD.ConReference {} <- varRef -> DefErr CtorTermCollision
                -- This allows us to resolve conflicts with an update.
                _ -> DefStatus Updated
            -- [r] -> DefStatus Updated
            -- _ -> DefStatus Conflicted
            TypeVar {} -> undefined
    varReferences :: Map v LD.LabeledDependency
    varReferences = UF.referencesMap uf

    -- Get the set of all DIRECT definitions in the file which a definition depends on.
    varDeps :: v -> Set v
    varDeps v = do
      undefined
    -- let varComponentHash = varToComponentHash Map.! v
    --     componentPeers = componentMapping Map.! varComponentHash
    --     directDeps = case UF.hashTermsId uf Map.!? v of
    --       Nothing -> mempty
    --       Just (_, _, term, _) -> ABT.freeVars term
    --  in Set.delete v (componentPeers <> directDeps)

    transitiveVarDeps :: Set v -> v -> Set v
    transitiveVarDeps resolved v =
      let directDeps = varDeps v
       in Foldable.foldl' transitiveVarDeps (Set.insert v resolved) directDeps
      where
        go resolved nextV =
          if Set.member nextV resolved
            then resolved
            else resolved <> transitiveVarDeps resolved nextV

    -- varToComponentHash :: Map v ComponentHash
    -- varToComponentHash = Map.fromList $ do
    --   -- List monad
    --   (hash, vars) <- Map.toList componentMapping
    --   v <- Set.toList vars
    --   pure (v, hash)

    codebaseNames :: Names
    codebaseNames =
      -- TODO: make faster
      -- TODO: how does defsToConsider affect deprecations?
      Names.filter (`Set.notMember` deprecatedConstructors) unalteredCodebaseNames
    constructorNamesInFile :: Set Name
    constructorNamesInFile =
      Map.elems (UF.dataDeclarationsId' uf)
        <> (fmap . fmap) DD.toDataDecl (Map.elems (UF.effectDeclarationsId' uf))
          & fmap snd
          & concatMap
            ( \decl ->
                DD.constructors' decl <&> \(_ann, v, _typ) ->
                  Name.unsafeFromVar v
            )
          & Set.fromList

    deprecatedConstructors :: Set Name
    deprecatedConstructors =
      let allRefIds =
            fmap fst (Map.elems (UF.dataDeclarationsId' uf))
              <> fmap fst (Map.elems (UF.effectDeclarationsId' uf))
          existingConstructorsFromEditedTypes = Set.fromList $ do
            -- List Monad
            refId <- allRefIds
            (name, _ref) <- Names.constructorsForType (Ref.DerivedId refId) unalteredCodebaseNames
            pure name
       in -- Compute any constructors which were deleted
          existingConstructorsFromEditedTypes `Set.difference` constructorNamesInFile

slurpErrs :: SlurpResult v -> Map (LabeledVar v) SlurpErr
slurpErrs (SlurpResult defs) =
  defs
    & Map.mapMaybe
      ( \case
          (DefErr err, _) -> Just err
          _ -> Nothing
      )

slurpOp ::
  forall v.
  SlurpResult v ->
  (SlurpComponent v, SlurpComponent v, Map (LabeledVar v) SlurpErr)
slurpOp (SlurpResult sr) = do
  let (adds, updates, errs) =
        flip execState mempty $
          for (Map.toList sr) $ \(v, (dn, _)) -> do
            case dn of
              DefStatus New -> _1 %= Set.insert v
              DefStatus Updated -> _2 %= Set.insert v
              DefStatus Duplicate -> pure ()
              DefErr err -> _3 . at v ?= err
      adds' = partitionTypesAndTerms adds
      updates' = partitionTypesAndTerms updates
   in (adds', updates', errs)

partitionTypesAndTerms :: Set (LabeledVar v) -> SlurpComponent v
partitionTypesAndTerms = undefined
