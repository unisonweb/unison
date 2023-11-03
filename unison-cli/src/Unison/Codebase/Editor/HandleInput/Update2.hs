{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Unison.Codebase.Editor.HandleInput.Update2
  ( handleUpdate2,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Lens (Lens', over, view, (%=), (.=), (.~), (^.))
import Control.Monad.Except qualified as Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Except qualified as Except
import Data.Foldable (foldlM)
import Data.Function (on)
import Data.Functor.Compose (Compose (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Semialign (alignWith, unzip, zip)
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.These (These (..))
import Data.Tuple.Strict (T2 (..))
import GHC.Clock (getMonotonicTime)
import Text.ANSI qualified as Text
import Text.Printf (printf)
import U.Codebase.Branch (Branch (..), CausalBranch)
import U.Codebase.Branch qualified as Branch
import U.Codebase.BranchV3 (BranchV3 (..), CausalBranchV3)
import U.Codebase.BranchV3 qualified as BranchV3
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (BranchHash (..), CausalHash (..))
import U.Codebase.Reference
  ( Reference,
    Reference' (..),
    ReferenceType,
    TermReferenceId,
    TypeReference,
    TypeReferenceId,
  )
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.HashHandle qualified as HashHandle
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite (ProjectBranch)
import U.Codebase.Sqlite.Queries qualified as Queries
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import U.Codebase.Term (Term)
import Unison.Builtin qualified as Builtins
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as Cli
import Unison.Cli.TypeCheck (computeTypecheckingEnvironment, typecheckTerm)
import Unison.Cli.UniqueTypeGuidLookup qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as V1 (Branch (..), Branch0)
import Unison.Codebase.Branch qualified as V1.Branch
import Unison.Codebase.Causal qualified as V1 (Causal)
import Unison.Codebase.Causal qualified as V1.Causal
import Unison.Codebase.Causal.Type qualified as V1.Causal
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.HandleInput.Merge2 qualified as Merge2
import Unison.Codebase.Editor.Input (Input)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Conversions
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType (ConstructorType)
import Unison.DataDeclaration qualified as V1 (Decl)
import Unison.DataDeclaration qualified as V1.Decl
import Unison.FileParsers qualified as FileParsers
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Merge2 (MergeOutput)
import Unison.Merge2 qualified as Merge2
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Parser.Ann (Ann)
import Unison.Prelude hiding (catMaybes)
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPE
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.Reference (TermReference)
import Unison.Referent qualified as V1 (Referent)
import Unison.Referent qualified as V1.Referent
import Unison.Result qualified as Result
import Unison.Server.Backend qualified as Backend
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as ShortHash
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name (toText, unsafeFromVar)
import Unison.Syntax.Parser qualified as Parser
import Unison.Term qualified as V1 (Term)
import Unison.Term qualified as V1.Term
import Unison.Type qualified as V1 (Type)
import Unison.Type qualified as V1.Type
import Unison.UnisonFile.Type (TypecheckedUnisonFile (TypecheckedUnisonFileId, dataDeclarationsId', effectDeclarationsId', hashTermsId), UnisonFile)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Cache qualified as Cache
import Unison.Util.Map qualified as Map
import Unison.Util.Nametree
  ( Defns (..),
    Nametree (..),
    flattenNametree,
    traverseNametreeWithName,
    unflattenNametree,
  )
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.Util.Star3 (Star3)
import Unison.Util.Star3 qualified as Star3
import Witch (unsafeFrom)
import Prelude hiding (unzip, zip)

-- | Handle an @update@ command.
handleUpdate2 :: Text -> Input -> Set Name -> Cli ()
handleUpdate2 description input _requestedNames = do
  Cli.Env {codebase} <- ask
  unisonFile0 <- Cli.expectLatestTypecheckedFile
  currentPath' <- Cli.getCurrentPath
  typecheck <- Merge2.mkTypecheckFnCli
  db <- Merge2.makeMergeDatabase codebase
  () <-
    Cli.runTransactionWithRollback \abort0 -> do
      codebaseInfo <- do
        let abort :: Merge2.PreconditionViolation -> Transaction void
            abort =
              Merge2.mergePreconditionViolationToOutput db >=> abort0
        loadCurrentCodebase abort db (Path.unabsolute currentPath')

      -- TODO: complain about lib changes
      let updatedDefns = computeUpdatedCodebase codebaseInfo unisonFile0

      uf <- gatherTypecheckingCandidates db codebaseInfo updatedDefns

      typecheck uf >>= \case
        Nothing -> do
          -- typechecking failed
          wundefined
        Just tuf -> do
          -- typechecking succeeded
          persistTuf codebase tuf
          wundefined

      pure ()
  undefined

data CodebaseInfo = CodebaseInfo
  { defns :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name),
    constructorDeclRel :: BiMultimap Name Name
  }

loadCurrentCodebase ::
  (forall x. Merge2.PreconditionViolation -> Transaction x) ->
  Merge2.MergeDatabase ->
  Path ->
  Transaction CodebaseInfo
loadCurrentCodebase abort db codebasePath = do
  -- Load causals
  codebaseCausal <- Merge2.step "load codebase causal" $ Codebase.getShallowCausalFromRoot Nothing codebasePath

  -- Load shallow branches
  codebaseBranch <- Merge2.step "load codebase shallow branch" $ Causal.value codebaseCausal

  -- Load deep definitions

  (_codebaseCausalTree, codebaseConstructorDeclRel, codebaseDefns) <- do
    (definitions0, causalHashes) <-
      Merge2.step "load codebase definitions" $
        unzip <$> Merge2.loadNamespaceInfo abort (codebaseCausal ^. #causalHash) codebaseBranch
    (declNames, definitions1) <- Merge2.assertNamespaceSatisfiesPreconditions db abort wundefined codebaseBranch definitions0
    pure (causalHashes, declNames, definitions1)
  pure
    CodebaseInfo
      { defns = codebaseDefns,
        constructorDeclRel = codebaseConstructorDeclRel
      }

computeUpdatedCodebase ::
  CodebaseInfo ->
  TypecheckedUnisonFile Symbol Ann ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)
computeUpdatedCodebase CodebaseInfo {defns, constructorDeclRel} unisonFile0 =
  let updatedDefns =
        let T2 updatedTypes updatedTerms0 =
              Map.foldlWithKey'
                ( \(T2 typs terms) k v ->
                    T2
                      (BiMultimap.insert (Reference.ReferenceDerived v) (fromSymbol k) typs)
                      (BiMultimap.withoutRan (BiMultimap.lookupDom (fromSymbol k) constructorDeclRel) terms)
                )
                (T2 codebaseTypes codebaseTerms)
                ((fst <$> dataDeclarationsId' unisonFile0) <> (fst <$> effectDeclarationsId' unisonFile0))
            Defns codebaseTerms codebaseTypes = defns
            updatedTerms1 =
              Map.foldlWithKey'
                (\b k (_, trefid, _, _, _) -> BiMultimap.insert (Referent.Ref (Reference.ReferenceDerived trefid)) (fromSymbol k) b)
                updatedTerms0
                (hashTermsId unisonFile0)
         in Defns updatedTerms1 updatedTypes
   in updatedDefns
  where
    fromSymbol :: Symbol -> Name
    fromSymbol = Name.unsafeFromVar

gatherTypecheckingCandidates ::
  Merge2.MergeDatabase ->
  CodebaseInfo ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Transaction (UnisonFile Symbol Ann)
gatherTypecheckingCandidates db CodebaseInfo { defns } updatedDefns = do
  diff <-
    Merge2.nameBasedNamespaceDiff
      db
      Merge2.TwoOrThreeWay
        { lca = Just defns,
          alice = defns,
          bob = updatedDefns
        }

  computeUnisonFile diff
  
computeUnisonFile ::
  Merge2.TwoWay (Defns (Map Name (Merge2.DiffOp Hash)) (Map Name (Merge2.DiffOp Hash))) ->
  Transaction (UnisonFile Symbol Ann)
computeUnisonFile = wundefined

persistTuf ::
  Codebase.Codebase IO Symbol Ann ->
  TypecheckedUnisonFile Symbol Ann ->
  Transaction ()
persistTuf codebase tuf = do
  Codebase.addDefsToCodebase codebase tuf
  -- need to update branch and sync root
