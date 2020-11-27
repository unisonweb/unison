{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Unison.Codebase.Editor.HandleCommand where

import Control.Monad.Except (runExceptT)
import qualified Crypto.Random as Random
import qualified Data.Configurator as Config
import Data.Configurator.Types (Config)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.Builtin as B
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Editor.AuthorInfo as AuthorInfo
import Unison.Codebase.Editor.Command
import qualified Unison.Codebase.Editor.Git as Git
import Unison.Codebase.Editor.Output
import Unison.Codebase.Runtime (Runtime)
import qualified Unison.Codebase.Runtime as Runtime
import Unison.FileParsers
  ( parseAndSynthesizeFile,
    synthesizeFile',
  )
import Unison.Parser (Ann)
import qualified Unison.Parser as Parser
import qualified Unison.Parsers as Parsers
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Unison.Result as Result
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.UnisonFile as UF
import Unison.Util.Free (Free)
import qualified Unison.Util.Free as Free
import Unison.Var (Var)

typecheck ::
  (Monad m, Var v) =>
  [Type v Ann] ->
  Codebase m v Ann ->
  Parser.ParsingEnv ->
  SourceName ->
  LexedSource ->
  m (TypecheckingResult v)
typecheck ambient codebase parsingEnv sourceName src =
  Result.getResult $
    parseAndSynthesizeFile
      ambient
      (((<> B.typeLookup) <$>) . Codebase.typeLookupForDependencies codebase)
      parsingEnv
      (Text.unpack sourceName)
      (fst src)

typecheck' ::
  Monad m =>
  Var v =>
  [Type v Ann] ->
  Codebase m v Ann ->
  UF.UnisonFile v Ann ->
  m (TypecheckingResult v)
typecheck' ambient codebase file = do
  typeLookup <-
    (<> B.typeLookup)
      <$> Codebase.typeLookupForDependencies codebase (UF.dependencies file)
  pure . fmap Right $ synthesizeFile' ambient typeLookup file

commandLine ::
  forall i v a gen.
  (Var v, Random.DRG gen) =>
  Config ->
  IO i ->
  (Branch IO -> IO ()) ->
  Runtime v ->
  (Output v -> IO ()) ->
  (NumberedOutput v -> IO NumberedArgs) ->
  (SourceName -> IO LoadSourceResult) ->
  Codebase IO v Ann ->
  (Int -> IO gen) ->
  Branch.Cache IO ->
  Free (Command IO i v) a ->
  IO a
commandLine config awaitInput setBranchRef rt notifyUser notifyNumbered loadSource codebase rngGen branchCache =
  Free.foldWithIndex go
  where
    go :: forall x. Int -> Command IO i v x -> IO x
    go i x = case x of
      -- Wait until we get either user input or a unison file update
      Eval m -> m
      Input -> awaitInput
      Notify output -> notifyUser output
      NotifyNumbered output -> notifyNumbered output
      ConfigLookup name ->
        Config.lookup config name
      LoadSource sourcePath -> loadSource sourcePath
      Typecheck ambient names sourceName source -> do
        -- todo: if guids are being shown to users,
        -- not ideal to generate new guid every time
        rng <- rngGen i
        let namegen = Parser.uniqueBase32Namegen rng
            env = Parser.ParsingEnv namegen names
        typecheck ambient codebase env sourceName source
      TypecheckFile file ambient -> typecheck' ambient codebase file
      Evaluate ppe unisonFile -> evalUnisonFile ppe unisonFile
      Evaluate1 ppe term -> eval1 ppe term
      LoadLocalRootBranch -> either (const Branch.empty) id <$> Codebase.getRootBranch codebase
      LoadLocalBranch h -> fromMaybe Branch.empty <$> Codebase.getBranchForHash codebase h
      SyncLocalRootBranch branch -> do
        setBranchRef branch
        Codebase.putRootBranch codebase branch
      ViewRemoteBranch ns ->
        runExceptT $ Git.viewRemoteBranch branchCache ns
      ImportRemoteBranch ns syncMode ->
        runExceptT $ Git.importRemoteBranch codebase branchCache ns syncMode
      SyncRemoteRootBranch repo branch syncMode ->
        runExceptT $ Git.pushGitRootBranch codebase branchCache branch repo syncMode
      LoadTerm r -> Codebase.getTerm codebase r
      LoadType r -> Codebase.getTypeDeclaration codebase r
      LoadTypeOfTerm r -> Codebase.getTypeOfTerm codebase r
      PutTerm r tm tp -> Codebase.putTerm codebase r tm tp
      PutDecl r decl -> Codebase.putTypeDeclaration codebase r decl
      PutWatch kind r e -> Codebase.putWatch codebase kind r e
      LoadWatches kind rs -> catMaybes <$> traverse go (toList rs)
        where
          go (Reference.Builtin _) = pure Nothing
          go r@(Reference.DerivedId rid) =
            fmap (r,) <$> Codebase.getWatch codebase kind rid
      IsTerm r -> Codebase.isTerm codebase r
      IsType r -> Codebase.isType codebase r
      GetDependents r -> Codebase.dependents codebase r
      AddDefsToCodebase unisonFile -> Codebase.addDefsToCodebase codebase unisonFile
      GetTermsOfType ty -> Codebase.termsOfType codebase ty
      GetTermsMentioningType ty -> Codebase.termsMentioningType codebase ty
      CodebaseHashLength -> Codebase.hashLength codebase
      -- all builtin and derived type references
      TypeReferencesByShortHash sh -> do
        fromCodebase <- Codebase.typeReferencesByPrefix codebase sh
        let fromBuiltins =
              Set.filter (\r -> sh == Reference.toShortHash r) $
                B.intrinsicTypeReferences
        pure (fromBuiltins <> Set.map Reference.DerivedId fromCodebase)
      -- all builtin and derived term references
      TermReferencesByShortHash sh -> do
        fromCodebase <- Codebase.termReferencesByPrefix codebase sh
        let fromBuiltins =
              Set.filter (\r -> sh == Reference.toShortHash r) $
                B.intrinsicTermReferences
        pure (fromBuiltins <> Set.map Reference.DerivedId fromCodebase)
      -- all builtin and derived term references & type constructors
      TermReferentsByShortHash sh -> do
        fromCodebase <- Codebase.termReferentsByPrefix codebase sh
        let fromBuiltins =
              Set.map Referent.Ref
                . Set.filter (\r -> sh == Reference.toShortHash r)
                $ B.intrinsicTermReferences
        pure (fromBuiltins <> Set.map (fmap Reference.DerivedId) fromCodebase)
      BranchHashLength -> Codebase.branchHashLength codebase
      BranchHashesByPrefix h -> Codebase.branchHashesByPrefix codebase h
      ParseType names (src, _) ->
        pure $
          Parsers.parseType (Text.unpack src) (Parser.ParsingEnv mempty names)
      RuntimeMain -> pure $ Runtime.mainType rt
      RuntimeTest -> pure $ Runtime.ioTestType rt
      --    Todo b -> doTodo codebase (Branch.head b)
      --    Propagate b -> do
      --      b0 <- Codebase.propagate codebase (Branch.head b)
      --      pure $ Branch.append b0 b

      Execute ppe uf ->
        evalUnisonFile ppe uf
      AppendToReflog reason old new -> Codebase.appendReflog codebase reason old new
      LoadReflog -> Codebase.getReflog codebase
      CreateAuthorInfo t -> AuthorInfo.createAuthorInfo Parser.External t

    eval1 :: PPE.PrettyPrintEnv -> Term v Ann -> _
    eval1 ppe tm = do
      let codeLookup = Codebase.toCodeLookup codebase
      r <- Runtime.evaluateTerm codeLookup ppe rt tm
      pure $ r <&> Term.amap (const Parser.External)

    evalUnisonFile :: PPE.PrettyPrintEnv -> UF.TypecheckedUnisonFile v Ann -> _
    evalUnisonFile ppe (UF.discardTypes -> unisonFile) = do
      let codeLookup = Codebase.toCodeLookup codebase
      evalFile <-
        if Runtime.needsContainment rt
          then Codebase.makeSelfContained' codeLookup unisonFile
          else pure unisonFile
      let watchCache (Reference.DerivedId h) = do
            m1 <- Codebase.getWatch codebase UF.RegularWatch h
            m2 <- maybe (Codebase.getWatch codebase UF.TestWatch h) (pure . Just) m1
            pure $ Term.amap (const ()) <$> m2
          watchCache Reference.Builtin {} = pure Nothing
      r <- Runtime.evaluateWatches codeLookup ppe watchCache rt evalFile
      case r of
        Left e -> pure (Left e)
        Right rs@(_, map) -> do
          forM_ (Map.elems map) $ \(_loc, kind, hash, _src, value, isHit) ->
            if isHit
              then pure ()
              else case hash of
                Reference.DerivedId h -> do
                  let value' = Term.amap (const Parser.External) value
                  Codebase.putWatch codebase kind h value'
                Reference.Builtin {} -> pure ()
          pure $ Right rs

-- doTodo :: Monad m => Codebase m v a -> Branch0 -> m (TodoOutput v a)
-- doTodo code b = do
--   -- traceM $ "edited terms: " ++ show (Branch.editedTerms b)
--   f <- Codebase.frontier code b
--   let dirty = R.dom f
--       frontier = R.ran f
--       ppe = Branch.prettyPrintEnv b
--   (frontierTerms, frontierTypes) <- loadDefinitions code frontier
--   (dirtyTerms, dirtyTypes) <- loadDefinitions code dirty
--   -- todo: something more intelligent here?
--   scoreFn <- pure $ const 1
--   remainingTransitive <- Codebase.frontierTransitiveDependents code b frontier
--   let
--     addTermNames terms = [(PPE.termName ppe (Referent.Ref r), r, t) | (r,t) <- terms ]
--     addTypeNames types = [(PPE.typeName ppe r, r, d) | (r,d) <- types ]
--     frontierTermsNamed = addTermNames frontierTerms
--     frontierTypesNamed = addTypeNames frontierTypes
--     dirtyTermsNamed = sortOn (\(s,_,_,_) -> s) $
--       [ (scoreFn r, n, r, t) | (n,r,t) <- addTermNames dirtyTerms ]
--     dirtyTypesNamed = sortOn (\(s,_,_,_) -> s) $
--       [ (scoreFn r, n, r, t) | (n,r,t) <- addTypeNames dirtyTypes ]
--   pure $
--     TodoOutput_
--       (Set.size remainingTransitive)
--       (frontierTermsNamed, frontierTypesNamed)
--       (dirtyTermsNamed, dirtyTypesNamed)
--       (Branch.conflicts' b)

-- loadDefinitions :: Monad m => Codebase m v a -> Set Reference
--                 -> m ( [(Reference, Maybe (Type v a))],
--                        [(Reference, DisplayThing (Decl v a))] )
-- loadDefinitions code refs = do
--   termRefs <- filterM (Codebase.isTerm code) (toList refs)
--   terms <- forM termRefs $ \r -> (r,) <$> Codebase.getTypeOfTerm code r
--   typeRefs <- filterM (Codebase.isType code) (toList refs)
--   types <- forM typeRefs $ \r -> do
--     case r of
--       Reference.Builtin _ -> pure (r, BuiltinThing)
--       Reference.DerivedId id -> do
--         decl <- Codebase.getTypeDeclaration code id
--         case decl of
--           Nothing -> pure (r, MissingThing id)
--           Just d -> pure (r, RegularThing d)
--   pure (terms, types)
--
-- -- | Write all of the builtins into the codebase
-- initializeCodebase :: forall m . Monad m => Codebase m Symbol Ann -> m ()
-- initializeCodebase c = do
--   traverse_ (go Right) B.builtinDataDecls
--   traverse_ (go Left)  B.builtinEffectDecls
--   void $ fileToBranch updateCollisionHandler c mempty IOSource.typecheckedFile
--  where
--   go :: (t -> Decl Symbol Ann) -> (a, (Reference.Reference, t)) -> m ()
--   go f (_, (ref, decl)) = case ref of
--     Reference.DerivedId id -> Codebase.putTypeDeclaration c id (f decl)
--     _                      -> pure ()
--
-- -- todo: probably don't use this anywhere
-- nameDistance :: Name -> Name -> Maybe Int
-- nameDistance (Name.toString -> q) (Name.toString -> n) =
--   if q == n                              then Just 0-- exact match is top choice
--   else if map toLower q == map toLower n then Just 1-- ignore case
--   else if q `isSuffixOf` n               then Just 2-- matching suffix is p.good
--   else if q `isPrefixOf` n               then Just 3-- matching prefix
--   else Nothing
