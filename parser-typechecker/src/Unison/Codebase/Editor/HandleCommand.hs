{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Editor.HandleCommand where

import Unison.Prelude

import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.Command
import Unison.Codebase.Editor.RemoteRepo

import qualified Unison.Builtin                as B

import           Control.Monad.Except           ( runExceptT )
import qualified Data.Configurator             as Config
import           Data.Configurator.Types        ( Config )
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import           System.Directory               ( getXdgDirectory
                                                , XdgDirectory(..)
                                                )
import           System.FilePath                ( (</>) )

import           Unison.Codebase                ( Codebase )
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase.Branch         ( Branch )
import qualified Unison.Codebase.Editor.Git    as Git
import qualified Unison.Hash                   as Hash
import           Unison.Parser                  ( Ann )
import qualified Unison.Parser                 as Parser
import qualified Unison.Parsers                as Parsers
import qualified Unison.Reference              as Reference
import qualified Unison.Codebase.Runtime       as Runtime
import           Unison.Codebase.Runtime       (Runtime)
import qualified Unison.Term                   as Term
import qualified Unison.UnisonFile             as UF
import           Unison.Util.Free               ( Free )
import qualified Unison.Util.Free              as Free
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import qualified Unison.Result as Result
import           Unison.FileParsers             ( parseAndSynthesizeFile )
import qualified Unison.PrettyPrintEnv         as PPE
import Unison.Type (Type)

typecheck
  :: (Monad m, Var v)
  => [Type v Ann]
  -> Codebase m v Ann
  -> Parser.ParsingEnv
  -> SourceName
  -> LexedSource
  -> m (TypecheckingResult v)
typecheck ambient codebase parsingEnv sourceName src =
  Result.getResult $ parseAndSynthesizeFile ambient
    (((<> B.typeLookup) <$>) . Codebase.typeLookupForDependencies codebase)
    parsingEnv
    (Text.unpack sourceName)
    (fst src)

tempGitDir :: Text -> Text -> IO FilePath
tempGitDir url commit =
  getXdgDirectory XdgCache
    $   "unisonlanguage"
    </> "gitfiles"
    </> Hash.showBase32Hex url
    </> Text.unpack commit

commandLine
  :: forall i v a
   . Var v
  => Config
  -> IO i
  -> (Branch IO -> IO ())
  -> Runtime v
  -> (Output v -> IO ())
  -> Codebase IO v Ann
  -> Free (Command IO i v) a
  -> IO a
commandLine config awaitInput setBranchRef rt notifyUser codebase =
 Free.fold go
 where
  go :: forall x . Command IO i v x -> IO x
  go = \case
    -- Wait until we get either user input or a unison file update
    Eval m        -> m
    Input         -> awaitInput
    Notify output -> notifyUser output
    ConfigLookup name -> Config.lookup config name
    Typecheck ambient names sourceName source -> do
      -- todo: if guids are being shown to users,
      -- not ideal to generate new guid every time
      namegen <- Parser.uniqueBase58Namegen
      let env = Parser.ParsingEnv namegen names
      typecheck ambient codebase env sourceName source
    Evaluate ppe unisonFile        -> evalUnisonFile ppe unisonFile
    Evaluate1 ppe term             -> eval1 ppe term
    LoadLocalRootBranch        -> Codebase.getRootBranch codebase
    LoadLocalBranch h          -> Codebase.getBranchForHash codebase h 
    SyncLocalRootBranch branch -> do
      setBranchRef branch
      Codebase.putRootBranch codebase branch
    LoadRemoteRootBranch GitRepo {..} -> do
      tmp <- tempGitDir url commit
      runExceptT $ Git.pullGitRootBranch tmp codebase url commit
    SyncRemoteRootBranch GitRepo {..} branch -> do
      tmp <- tempGitDir url commit
      runExceptT
        $ Git.pushGitRootBranch tmp codebase branch url commit
    LoadTerm r -> Codebase.getTerm codebase r
    LoadType r -> Codebase.getTypeDeclaration codebase r
    LoadTypeOfTerm r -> Codebase.getTypeOfTerm codebase r
    PutTerm r tm tp -> Codebase.putTerm codebase r tm tp
    PutWatch kind r e -> Codebase.putWatch codebase kind r e
    LoadWatches kind rs -> catMaybes <$> traverse go (toList rs) where
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
    ParseType names (src, _) -> pure $
      Parsers.parseType (Text.unpack src) (Parser.ParsingEnv mempty names)

--    Todo b -> doTodo codebase (Branch.head b)
--    Propagate b -> do
--      b0 <- Codebase.propagate codebase (Branch.head b)
--      pure $ Branch.append b0 b
    Execute ppe uf -> void $ evalUnisonFile ppe uf

  eval1 :: PPE.PrettyPrintEnv -> Term.AnnotatedTerm v Ann -> _
  eval1 ppe tm = do
    let codeLookup = Codebase.toCodeLookup codebase
    let uf = UF.UnisonFile mempty mempty mempty
               (Map.singleton UF.RegularWatch [(Var.nameds "result", tm)])
    r <- Runtime.evaluateWatches codeLookup ppe Runtime.noCache rt uf
    pure $ r <&> \(_,map) ->
      let [(_loc, _kind, _hash, _src, value, _isHit)] = Map.elems map
      in Term.amap (const Parser.External) value

  evalUnisonFile :: PPE.PrettyPrintEnv -> UF.TypecheckedUnisonFile v Ann -> _
  evalUnisonFile ppe (UF.discardTypes -> unisonFile) = do
    let codeLookup = Codebase.toCodeLookup codebase
    let watchCache (Reference.DerivedId h) = do
          m1 <- Codebase.getWatch codebase UF.RegularWatch h
          m2 <- maybe (Codebase.getWatch codebase UF.TestWatch h) (pure . Just) m1
          pure $ Term.amap (const ()) <$> m2
        watchCache _ = pure Nothing
    r <- Runtime.evaluateWatches codeLookup ppe watchCache rt unisonFile
    case r of
      Left e -> pure (Left e)
      Right rs@(_,map) -> do
        forM_ (Map.elems map) $ \(_loc, kind, hash, _src, value, isHit) ->
          if isHit then pure ()
          else case hash of
            Reference.DerivedId h -> do
              let value' = Term.amap (const Parser.External) value
              Codebase.putWatch codebase kind h value'
            _ -> pure ()
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
