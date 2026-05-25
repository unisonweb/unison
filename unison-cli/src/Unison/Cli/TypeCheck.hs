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
import Unison.Util.Relation qualified as Relation
import Unison.Util.Star2 qualified as Star2
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
  -- Walk the namespace recursively so we catch givens nested in
  -- sub-branches. The metadata that marks a referent as a given is
  -- stored in the *enclosing* sub-branch's @terms_@, not in the
  -- project root, so a flat scan of 'deepReferents b0' followed by
  -- 'Givens.isGiven r b0' never finds anything past the top level.
  -- 'Unison.Codebase.Editor.HandleInput.Givens.handleGivens' uses the
  -- same recursive pattern. Per ADR-005, ambient resolution includes
  -- the @lib@ subtree, so we do *not* prune libraries here.
  let givenRefs :: [Reference.TermReference]
      givenRefs = Set.toList (collectGivenRefs b0)
  fmap catMaybes . for givenRefs $ \r ->
    Codebase.getTypeOfTerm codebase r >>= \case
      Nothing -> pure Nothing
      Just ty ->
        pure $
          Just
            GivenElaborator.AmbientGiven
              { GivenElaborator.ambientName = r,
                GivenElaborator.ambientType = ty
              }
  where
    -- Mirror 'handleGivens': at each level, check the *direct* term
    -- referents (via 'Star2.d1' of 'terms_') against this level's
    -- metadata, then recurse into each child.
    collectGivenRefs :: Branch0 m -> Set.Set Reference.TermReference
    collectGivenRefs b =
      let here :: Set.Set Reference.TermReference
          here =
            Set.fromList
              [ ref
              | (r, _seg) <- Relation.toList (Star2.d1 (view Branch.terms_ b)),
                Givens.isGiven r b,
                Just ref <- [Referent.toTermReference r]
              ]
          there :: Set.Set Reference.TermReference
          there =
            foldMap
              (collectGivenRefs . Branch.head . snd)
              (Map.toList (view Branch.children_ b))
       in Set.union here there

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
  let file = UF.UnisonFileId Nothing mempty mempty (Map.singleton v (External, tm)) mempty mempty mempty
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
