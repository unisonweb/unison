{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Unison.Codebase.Editor.Slurp where

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
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.UnisonFile as UF
import qualified Unison.UnisonFile.Names as UF
import qualified Unison.UnisonFile.Type as UF
import Unison.Var (Var)

data SlurpStatus = New | Updated | Duplicate

data SlurpOp = Add | Update

data TypeOrTermVar v = TypeVar v | TermVar v

untypedVar :: TypeOrTermVar v -> v
untypedVar = \case
  TypeVar v -> v
  TermVar v -> v

data SlurpPrintout v = SlurpPrintout
  { notOk :: Map v (SlurpErr v),
    ok :: Map v SlurpStatus
  }

data SlurpErr v
  = TermCtorCollision
  | CtorTermCollision
  | RequiresUpdateOf v

data SlurpComponent v = SlurpComponent {types :: Set v, terms :: Set v}
  deriving (Eq, Ord, Show)

data DefinitionNotes v
  = DefStatus SlurpStatus
  | DefErr (Set (SlurpErr v))

data ComponentNotes v = ComponentNotes
  { deps :: Set ComponentHash,
    definitions :: Map v (DefinitionNotes v)
  }

data SlurpResult v = SlurpResult
  { componentNotes :: Map ComponentHash (Map v (DefinitionNotes v)),
    varToComponent :: Map v ComponentHash
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
analyzeTypecheckedUnisonFile uf unalteredCodebaseNames _defsToConsider =
  SlurpResult _varToComponents _componentNotes'
  where
    fileNames :: Names
    fileNames = UF.typecheckedToNames uf
    componentMapping :: Map ComponentHash (Set v)
    componentMapping = UF.componentMap uf
    -- codebaseNames with deprecated constructors removed.

    allDefinitions :: Set v
    allDefinitions = fold componentMapping

    componentNotes' :: Map ComponentHash (Map v (DefinitionNotes v))
    componentNotes' = undefined

    definitionStatus :: TypeOrTermVar v -> DefinitionNotes v
    definitionStatus tv =
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
            TypeVar {} -> _
    varReferences :: Map v LD.LabeledDependency
    varReferences = UF.referencesMap uf

    -- Get the set of all DIRECT definitions in the file which a definition depends on.
    varDeps :: v -> Set v
    varDeps v = do
      let varComponentHash = varToComponentHash Map.! v
          componentPeers = componentMapping Map.! varComponentHash
          directDeps = case UF.hashTermsId uf Map.!? v of
            Nothing -> mempty
            Just (_, _, term, _) -> ABT.freeVars term
       in Set.delete v (componentPeers <> directDeps)

    transitiveVarDeps :: Set v -> v -> Set v
    transitiveVarDeps resolved v =
      let directDeps = varDeps v
       in Foldable.foldl' transitiveVarDeps (Set.insert v resolved) directDeps
      where
        go resolved nextV =
          if Set.member nextV resolved
            then resolved
            else resolved <> transitiveVarDeps resolved nextV

    varToComponentHash :: Map v ComponentHash
    varToComponentHash = Map.fromList $ do
      -- List monad
      (hash, vars) <- Map.toList componentMapping
      v <- Set.toList vars
      pure (v, hash)

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

-- [ (n, r)
--   | (oldTypeRef, _) <- Map.elems typeEdits,
--     (n, r) <- Names.constructorsForType oldTypeRef codebaseNames
-- ]

slurpOp ::
  SlurpOp ->
  SlurpResult v ->
  Either
    (Set (SlurpErr v))
    -- adds,           updates
    (SlurpComponent v, SlurpComponent v)
slurpOp = undefined
