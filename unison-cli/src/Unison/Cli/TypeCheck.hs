module Unison.Cli.TypeCheck
  ( computeTypecheckingEnvironment,
    ambientGivensFromBranch,
    typecheckTerm,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Givens qualified as Givens
import Unison.FileParsers qualified as FileParsers
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Result qualified as Result
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol (Symbol))
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.GivenElaborator qualified as GivenElaborator
import Unison.Typechecker.GivenResolver qualified as GivenResolver
import Unison.Typechecker.Variance qualified as Variance
import Unison.UnisonFile (UnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.Var qualified as Var

computeTypecheckingEnvironment ::
  FileParsers.ShouldUseTndr Sqlite.Transaction ->
  Codebase IO Symbol Ann ->
  [Type Symbol Ann] ->
  -- | Ambient givens harvested from the namespace (chunk L1).
  -- Build with 'ambientGivensFromBranch' or pass @[]@ if no namespace
  -- is in scope.
  [GivenElaborator.AmbientGiven Symbol Ann] ->
  UnisonFile Symbol Ann ->
  Sqlite.Transaction (Typechecker.Env Symbol Ann)
computeTypecheckingEnvironment shouldUseTndr codebase ambientAbilities ambientGivens unisonFile =
  FileParsers.computeTypecheckingEnvironment
    shouldUseTndr
    ambientAbilities
    (Codebase.typeLookupForDependencies codebase)
    ambientGivens
    unisonFile

-- | Phase-2 chunk L1: harvest the ambient given pool from a 'Branch0'.
--
-- Walks the branch's 'deepReferents' and keeps any term referent
-- carrying 'Unison.Codebase.Givens.givenSentinel' in its metadata.
-- For each such referent (assumed to be 'Referent.Ref'; constructors
-- cannot be marked as givens at the term level), the term's declared
-- type is fetched via 'Codebase.getTypeOfTerm' and packaged as an
-- 'AmbientGiven'. Callers pass the result to
-- 'computeTypecheckingEnvironment'.
ambientGivensFromBranch ::
  Codebase IO Symbol Ann ->
  Branch0 m ->
  Sqlite.Transaction [GivenElaborator.AmbientGiven Symbol Ann]
ambientGivensFromBranch codebase b0 = do
  let givenRefs :: [Reference.TermReference]
      givenRefs =
        [ ref
        | r <- Set.toList (Branch.deepReferents b0),
          Givens.isGiven r b0,
          Just ref <- [Referent.toTermReference r]
        ]
  fmap catMaybes . for givenRefs $ \r -> do
    Codebase.getTypeOfTerm codebase r >>= \case
      Nothing -> pure Nothing
      Just ty ->
        pure $
          Just
            GivenElaborator.AmbientGiven
              { GivenElaborator.ambientName = r,
                GivenElaborator.ambientType = ty
              }

typecheckTerm ::
  Codebase IO Symbol Ann ->
  Term Symbol Ann ->
  Sqlite.Transaction
    ( Result.Result
        (Seq (Result.Note Symbol Ann))
        (Type Symbol Ann)
    )
typecheckTerm codebase tm = do
  let v = Symbol 0 (Var.Inference Var.Other)
  let file = UF.UnisonFileId Nothing mempty mempty (Map.singleton v (External, tm)) mempty mempty
  typeLookup <- Codebase.typeLookupForDependencies codebase (UF.dependencies file)
  let typecheckingEnv =
        Typechecker.Env
          { ambientAbilities = [],
            typeLookup,
            termsByShortname = Map.empty,
            freeNameToFuzzyTermsByShortName = Map.empty,
            topLevelComponents = Map.empty,
            variances = Variance.fromTypeLookup typeLookup,
            ambientGivens = GivenResolver.poolFromList [],
            givenBindings = mempty
          }
  pure $ fmap extract $ FileParsers.synthesizeFile typecheckingEnv file
  where
    extract tuf
      | [[(_, _, _, ty)]] <- UF.topLevelComponents' tuf = ty
      | otherwise = error "internal error: typecheckTerm"
