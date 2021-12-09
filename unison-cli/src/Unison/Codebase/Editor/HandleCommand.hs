{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}

module Unison.Codebase.Editor.HandleCommand where

import Unison.Prelude

import Control.Monad.Except (runExceptT)
import qualified Crypto.Random as Random
import qualified Data.Configurator as Config
import Data.Configurator.Types (Config)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Unison.Builtin as B
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Merge as Branch
import qualified Unison.Codebase.Editor.AuthorInfo as AuthorInfo
import Unison.Codebase.Editor.Command (Command (..), LexedSource, LoadSourceResult, SourceName, TypecheckingResult, UseCache)
import Unison.Codebase.Editor.Output (NumberedArgs, NumberedOutput, Output)
import Unison.Codebase.Runtime (Runtime)
import qualified Unison.Codebase.Runtime as Runtime
import Unison.FileParsers (parseAndSynthesizeFile, synthesizeFile')
import qualified Unison.Hashing.V2.Convert as Hashing
import qualified Unison.Parser as Parser
import Unison.Parser.Ann (Ann)
import qualified Unison.Parser.Ann as Ann
import qualified Unison.Parsers as Parsers
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Reference as Reference
import qualified Unison.Result as Result
import qualified Unison.Server.Backend as Backend
import qualified Unison.Server.CodebaseServer as Server
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.UnisonFile as UF
import Unison.Util.Free (Free)
import qualified Unison.Util.Free as Free
import qualified Unison.WatchKind as WK
import Web.Browser (openBrowser)
import System.Environment (withArgs)
import qualified Unison.CommandLine.FuzzySelect as Fuzzy
import qualified Unison.Codebase.Path as Path
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import qualified Control.Concurrent.STM as STM
import qualified UnliftIO
import Unison.Symbol (Symbol)

typecheck
  :: Monad m
  => [Type Symbol Ann]
  -> Codebase m Symbol Ann
  -> Parser.ParsingEnv
  -> SourceName
  -> LexedSource
  -> m (TypecheckingResult Symbol)
typecheck ambient codebase parsingEnv sourceName src =
  Result.getResult $ parseAndSynthesizeFile ambient
    (((<> B.typeLookup) <$>) . Codebase.typeLookupForDependencies codebase)
    parsingEnv
    (Text.unpack sourceName)
    (fst src)

typecheck'
  :: Monad m
  => [Type Symbol Ann]
  -> Codebase m Symbol Ann
  -> UF.UnisonFile Symbol Ann
  -> m (TypecheckingResult Symbol)
typecheck' ambient codebase file = do
  typeLookup <- (<> B.typeLookup)
    <$> Codebase.typeLookupForDependencies codebase (UF.dependencies file)
  pure . fmap Right $ synthesizeFile' ambient typeLookup file

commandLine
  :: forall i a gen
   . Random.DRG gen
  => Config
  -> IO i
  -> (Branch IO -> IO ())
  -> Runtime Symbol
  -> (Output Symbol -> IO ())
  -> (NumberedOutput Symbol -> IO NumberedArgs)
  -> (SourceName -> IO LoadSourceResult)
  -> Codebase IO Symbol Ann
  -> Maybe Server.BaseUrl
  -> (Int -> IO gen)
  -> Free (Command IO i Symbol) a
  -> IO a
commandLine config awaitInput setBranchRef rt notifyUser notifyNumbered loadSource codebase serverBaseUrl rngGen free = do
 rndSeed <- STM.newTVarIO 0
 flip runReaderT rndSeed . Free.fold go $ free
 where
  go :: forall x . Command IO i Symbol x -> ReaderT (STM.TVar Int) IO x
  go x = case x of
    -- Wait until we get either user input or a unison file update
    Eval m        -> lift m
    UI            ->
      case serverBaseUrl of
        Just url -> lift . void $ openBrowser (Server.urlFor Server.UI url)
        Nothing -> lift (return ())

    DocsToHtml root sourcePath destination ->
      liftIO $ Backend.docsInBranchToHtmlFiles rt codebase root sourcePath destination

    Input         -> lift awaitInput
    Notify output -> lift $ notifyUser output
    NotifyNumbered output -> lift $ notifyNumbered output
    ConfigLookup name ->
      lift $ Config.lookup config name
    LoadSource sourcePath -> lift $ loadSource sourcePath

    Typecheck ambient names sourceName source -> do
      -- todo: if guids are being shown to users,
      -- not ideal to generate new guid every time
      iVar <- ask
      i <- UnliftIO.atomically $ do
             i <- STM.readTVar iVar
             STM.writeTVar iVar (i + 1)
             pure i
      rng <- lift $ rngGen i
      let namegen = Parser.uniqueBase32Namegen rng
          env = Parser.ParsingEnv namegen names
      lift $ typecheck ambient codebase env sourceName source
    TypecheckFile file ambient     -> lift $ typecheck' ambient codebase file
    Evaluate ppe unisonFile        -> lift $ evalUnisonFile ppe unisonFile []
    Evaluate1 ppe useCache term    -> lift $ eval1 ppe useCache term
    LoadLocalRootBranch        -> lift $ either (const Branch.empty) id <$> Codebase.getRootBranch codebase
    LoadLocalBranch h          -> lift $ fromMaybe Branch.empty <$> Codebase.getBranchForHash codebase h
    Merge mode b1 b2 ->
      lift $ Branch.merge'' (Codebase.lca codebase) mode b1 b2
    SyncLocalRootBranch branch -> lift $ do
      setBranchRef branch
      Codebase.putRootBranch codebase branch
    ViewRemoteBranch ns action -> do
      -- TODO: We probably won'd need to unlift anything once we remove the Command
      -- abstraction.
      toIO <- UnliftIO.askRunInIO
      lift $ Codebase.viewRemoteBranch codebase ns (toIO . Free.fold go . action)
    ImportRemoteBranch ns syncMode ->
      lift $ Codebase.importRemoteBranch codebase ns syncMode
    SyncRemoteBranch branch repo opts ->
      lift $ Codebase.pushGitBranch codebase branch repo opts
    LoadTerm r -> lift $ Codebase.getTerm codebase r
    LoadType r -> lift $ Codebase.getTypeDeclaration codebase r
    LoadTypeOfTerm r -> lift $ Codebase.getTypeOfTerm codebase r
    PutTerm r tm tp -> lift $ Codebase.putTerm codebase r tm tp
    PutDecl r decl -> lift $ Codebase.putTypeDeclaration codebase r decl
    PutWatch kind r e -> lift $ Codebase.putWatch codebase kind r e
    LoadWatches kind rs -> lift $ catMaybes <$> traverse go (toList rs) where
      go (Reference.Builtin _) = pure Nothing
      go r@(Reference.DerivedId rid) =
        fmap (r,) <$> Codebase.getWatch codebase kind rid
    IsTerm r -> lift $ Codebase.isTerm codebase r
    IsType r -> lift $ Codebase.isType codebase r
    GetDependents r -> lift $ Codebase.dependents codebase r
    AddDefsToCodebase unisonFile -> lift $ Codebase.addDefsToCodebase codebase unisonFile
    GetTermsOfType ty -> lift $ Codebase.termsOfType codebase ty
    GetTermsMentioningType ty -> lift $ Codebase.termsMentioningType codebase ty
    CodebaseHashLength -> lift $ Codebase.hashLength codebase
    -- all builtin and derived type references
    TypeReferencesByShortHash sh ->
      lift $ Backend.typeReferencesByShortHash codebase sh
    -- all builtin and derived term references
    TermReferencesByShortHash sh ->
      lift $ Backend.termReferencesByShortHash codebase sh
    -- all builtin and derived term references & type constructors
    TermReferentsByShortHash sh ->
      lift $ Backend.termReferentsByShortHash codebase sh
    BranchHashLength ->
      lift $ Codebase.branchHashLength codebase
    BranchHashesByPrefix h ->
      lift $ Codebase.branchHashesByPrefix codebase h
    ParseType names (src, _) -> pure $
      Parsers.parseType (Text.unpack src) (Parser.ParsingEnv mempty names)
    RuntimeMain -> pure $ Runtime.mainType rt
    RuntimeTest -> pure $ Runtime.ioTestType rt

--    Todo b -> doTodo codebase (Branch.head b)
--    Propagate b -> do
--      b0 <- Codebase.propagate codebase (Branch.head b)
--      pure $ Branch.append b0 b

    Execute ppe uf args ->
      lift $ evalUnisonFile ppe uf args
    AppendToReflog reason old new -> lift $ Codebase.appendReflog codebase reason old new
    LoadReflog -> lift $ Codebase.getReflog codebase
    CreateAuthorInfo t -> AuthorInfo.createAuthorInfo Ann.External t
    HQNameQuery mayPath branch query -> do
      let namingScope = Backend.AllNames $ fromMaybe Path.empty mayPath
      lift $ Backend.hqNameQuery namingScope branch codebase query
    LoadSearchResults srs -> lift $ Backend.loadSearchResults codebase srs
    GetDefinitionsBySuffixes mayPath branch includeCycles query ->  do
      let namingScope = Backend.AllNames $ fromMaybe Path.empty mayPath
      lift (Backend.definitionsBySuffixes namingScope branch codebase includeCycles query)
    FindShallow path -> lift . runExceptT $ Backend.findShallow codebase path
    MakeStandalone ppe ref out -> lift $ do
      let cl = Codebase.toCodeLookup codebase
      Runtime.compileTo rt (() <$ cl) ppe ref (out <> ".uc")
    ClearWatchCache -> lift $ Codebase.clearWatches codebase
    FuzzySelect opts display choices -> liftIO $ Fuzzy.fuzzySelect opts display choices
    CmdUnliftIO -> do
      -- Get the unlifter for the ReaderT we're currently working in.
      unlifted <- UnliftIO.askUnliftIO
      -- Built an unliftIO for the Free monad
      let runF :: UnliftIO.UnliftIO (Free (Command IO i Symbol))
          runF = UnliftIO.UnliftIO $ case unlifted of
                   -- We need to case-match on the UnliftIO within this function
                   -- because `toIO` is existential and we need the right types
                   -- in-scope.
                   UnliftIO.UnliftIO toIO -> toIO . Free.fold go
      pure runF

  watchCache :: Reference.Id -> IO (Maybe (Term Symbol ()))
  watchCache h = do
    maybeTerm <- Codebase.lookupWatchCache codebase h
    pure (Term.amap (const ()) <$> maybeTerm)

  eval1 :: PPE.PrettyPrintEnv -> UseCache -> Term Symbol Ann -> _
  eval1 ppe useCache tm = do
    let codeLookup = Codebase.toCodeLookup codebase
        cache = if useCache then watchCache else Runtime.noCache
    r <- Runtime.evaluateTerm' codeLookup cache ppe rt tm
    when useCache $ case r of
      Right tmr -> Codebase.putWatch codebase WK.RegularWatch (Hashing.hashClosedTerm tm)
                                     (Term.amap (const Ann.External) tmr)
      Left _ -> pure ()
    pure $ r <&> Term.amap (const Ann.External)

  evalUnisonFile :: PPE.PrettyPrintEnv -> UF.TypecheckedUnisonFile Symbol Ann -> [String] -> _
  evalUnisonFile ppe unisonFile args = withArgs args do
    let codeLookup = Codebase.toCodeLookup codebase
    r <- Runtime.evaluateWatches codeLookup ppe watchCache rt unisonFile
    case r of
      Left e -> pure (Left e)
      Right rs@(_,map) -> do
        forM_ (Map.elems map) $ \(_loc, kind, hash, _src, value, isHit) ->
          if isHit then pure ()
          else do
            let value' = Term.amap (const Ann.External) value
            Codebase.putWatch codebase kind hash value'
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
--                        [(Reference, DisplayObject (Decl v a))] )
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

