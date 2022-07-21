{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Unison.Codebase.Editor.HandleCommand where

import qualified Control.Concurrent.STM as STM
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (ReaderT))
import Control.Monad.Trans.Cont
import Unison.Codebase.Editor.Input (Event, Input)
import qualified Crypto.Random as Random
import qualified Data.Configurator as Config
import Data.Configurator.Types (Config)
import qualified Data.Map as Map
import qualified Data.Text as Text
import System.Environment (withArgs)
import qualified Unison.Builtin as B
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Editor.Command (Action (..), Command (..), Env, LexedSource, LoadSourceResult, LoopState, SourceName, TypecheckingResult, UseCache)
import Unison.Codebase.Editor.Output (NumberedArgs, NumberedOutput, Output (PrintMessage))
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Runtime (Runtime)
import qualified Unison.Codebase.Runtime as Runtime
import Unison.FileParsers (parseAndSynthesizeFile, synthesizeFile')
import qualified Unison.Hashing.V2.Convert as Hashing
import qualified Unison.NamesWithHistory as NamesWithHistory
import qualified Unison.Parser as Parser
import Unison.Parser.Ann (Ann)
import qualified Unison.Parser.Ann as Ann
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Reference as Reference
import qualified Unison.Result as Result
import qualified Unison.Server.Backend as Backend
import qualified Unison.Server.CodebaseServer as Server
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Free as Free
import qualified Unison.Util.Pretty as P
import qualified Unison.WatchKind as WK
import qualified UnliftIO
import Web.Browser (openBrowser)

typecheck ::
  Monad m =>
  [Type Symbol Ann] ->
  Codebase m Symbol Ann ->
  Parser.ParsingEnv ->
  SourceName ->
  LexedSource ->
  m (TypecheckingResult Symbol)
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
  [Type Symbol Ann] ->
  Codebase m Symbol Ann ->
  UF.UnisonFile Symbol Ann ->
  m (TypecheckingResult Symbol)
typecheck' ambient codebase file = do
  typeLookup <-
    (<> B.typeLookup)
      <$> Codebase.typeLookupForDependencies codebase (UF.dependencies file)
  pure . fmap Right $ synthesizeFile' ambient typeLookup file

data ReturnType a
  = Success a
  | HaltStep
  | HaltRepl

data Bailing
  = HaltingStep
  | HaltingRepl
  deriving stock (Show)
  deriving anyclass (Exception)

newtype Cli r a = Cli {unCli :: (a -> Env Symbol -> IO (ReturnType r)) -> Env Symbol -> IO (ReturnType r)}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (Env Symbol)
    )
    via ContT (ReturnType r) (ReaderT (Env Symbol) IO)

withCliToIO :: ((forall x. Cli x x -> IO x) -> IO a) -> Cli r a
withCliToIO run = Cli \k env -> do
  ea <- try $
    run $ \(Cli ma) ->
      ma (\a _ -> pure (Success a)) env >>= \case
        HaltStep -> UnliftIO.throwIO HaltingStep
        HaltRepl -> UnliftIO.throwIO HaltingRepl
        Success a -> pure a
  case ea of
    Left HaltingStep -> pure HaltStep
    Left HaltingRepl -> pure HaltRepl
    Right a -> k a env

short :: ReturnType r -> Cli r a
short r = Cli \_k _env -> pure r

commandLine ::
  forall gen.
  Random.DRG gen =>
  Env Symbol ->
  LoopState Symbol ->
  Config ->
  IO (Either Event Input) ->
  (Branch IO -> IO ()) ->
  Runtime Symbol ->
  Runtime Symbol ->
  (Output Symbol -> IO ()) ->
  (NumberedOutput Symbol -> IO NumberedArgs) ->
  (SourceName -> IO LoadSourceResult) ->
  Codebase IO Symbol Ann ->
  Maybe Server.BaseUrl ->
  (Int -> IO gen) ->
  (Either Event Input -> Action ()) ->
  IO (Maybe (), LoopState Symbol)
commandLine env0 loopState0 config awaitInput setBranchRef rt sdbxRt notifyUser notifyNumbered loadSource codebase serverBaseUrl rngGen action = do
  rndSeed :: STM.TVar Int <- STM.newTVarIO 0
  loopStateRef <- UnliftIO.newIORef loopState0
  let go :: forall r x. Command x -> Cli r x
      go x = case x of
        AskEnv -> ask
        LocalEnv f e -> local f (Free.fold go e)
        GetLoopState -> liftIO (UnliftIO.readIORef loopStateRef)
        PutLoopState st -> liftIO (UnliftIO.writeIORef loopStateRef st)
        Eval m -> liftIO m
        API -> liftIO $
          forM_ serverBaseUrl $ \baseUrl ->
            notifyUser $
              PrintMessage $
                P.lines
                  [ "The API information is as follows:",
                    P.newline,
                    P.indentN 2 (P.hiBlue ("UI: " <> fromString (Server.urlFor Server.UI baseUrl))),
                    P.newline,
                    P.indentN 2 (P.hiBlue ("API: " <> fromString (Server.urlFor Server.Api baseUrl)))
                  ]
        UI ->
          case serverBaseUrl of
            Just url -> liftIO . void $ openBrowser (Server.urlFor Server.UI url)
            Nothing -> liftIO (return ())
        Notify output -> liftIO $ notifyUser output
        NotifyNumbered output -> liftIO $ notifyNumbered output
        ConfigLookup name ->
          liftIO $ Config.lookup config name
        LoadSource sourcePath -> liftIO $ loadSource sourcePath
        Typecheck ambient names sourceName source -> do
          -- todo: if guids are being shown to users,
          -- not ideal to generate new guid every time
          i <- UnliftIO.atomically $ do
            i <- STM.readTVar rndSeed
            STM.writeTVar rndSeed (i + 1)
            pure i
          rng <- liftIO $ rngGen i
          let namegen = Parser.uniqueBase32Namegen rng
              env = Parser.ParsingEnv namegen names
          liftIO $ typecheck ambient codebase env sourceName source
        TypecheckFile file ambient -> liftIO $ typecheck' ambient codebase file
        Evaluate sdbx ppe unisonFile -> liftIO $ evalUnisonFile sdbx ppe unisonFile []
        Evaluate1 sdbx ppe useCache term -> liftIO $ eval1 sdbx ppe useCache term
        SyncLocalRootBranch branch -> liftIO $ do
          setBranchRef branch
          Codebase.putRootBranch codebase branch
        WithRunInIO doUnlifts -> Cli \k env -> do
          let phi :: forall x. Action x -> IO x
              phi (Action ma) =
                unCli (Free.fold go ma) (\a _env -> pure (Success a)) env >>= \case
                  HaltStep -> UnliftIO.throwIO HaltingStep
                  HaltRepl -> UnliftIO.throwIO HaltingRepl
                  Success x -> pure x
          UnliftIO.try (doUnlifts phi) >>= \case
            Left HaltingStep -> pure HaltStep
            Left HaltingRepl -> pure HaltRepl
            Right x -> k x env
        Abort -> short HaltStep
        Quit -> short HaltRepl
        Execute ppe uf args ->
          liftIO $ evalUnisonFile False ppe uf args
        HQNameQuery mayPath branch query -> do
          hqLength <- liftIO $ Codebase.hashLength codebase
          let namingScope = Backend.AllNames $ fromMaybe Path.empty mayPath
          let parseNames = Backend.parseNamesForBranch branch namingScope
          let nameSearch = Backend.makeNameSearch hqLength (NamesWithHistory.fromCurrentNames parseNames)
          liftIO $ Backend.hqNameQuery codebase nameSearch query

      watchCache :: Reference.Id -> IO (Maybe (Term Symbol ()))
      watchCache h = do
        maybeTerm <- Codebase.lookupWatchCache codebase h
        pure (Term.amap (const ()) <$> maybeTerm)

      eval1 :: Bool -> PPE.PrettyPrintEnv -> UseCache -> Term Symbol Ann -> _
      eval1 sandbox ppe useCache tm = do
        let codeLookup = Codebase.toCodeLookup codebase
            cache = if useCache then watchCache else Runtime.noCache
            rt' | sandbox = sdbxRt | otherwise = rt
        r <- Runtime.evaluateTerm' codeLookup cache ppe rt' tm
        when useCache $ case r of
          Right tmr ->
            Codebase.putWatch
              codebase
              WK.RegularWatch
              (Hashing.hashClosedTerm tm)
              (Term.amap (const Ann.External) tmr)
          Left _ -> pure ()
        pure $ r <&> Term.amap (const Ann.External)

      evalUnisonFile :: Bool -> PPE.PrettyPrintEnv -> UF.TypecheckedUnisonFile Symbol Ann -> [String] -> _
      evalUnisonFile sandbox ppe unisonFile args = withArgs args do
        let codeLookup = Codebase.toCodeLookup codebase
            rt' | sandbox = sdbxRt | otherwise = rt
        r <- Runtime.evaluateWatches codeLookup ppe watchCache rt' unisonFile
        case r of
          Left e -> pure (Left e)
          Right rs@(_, map) -> do
            forM_ (Map.elems map) $ \(_loc, kind, hash, _src, value, isHit) ->
              if isHit
                then pure ()
                else do
                  let value' = Term.amap (const Ann.External) value
                  Codebase.putWatch codebase kind hash value'
            pure $ Right rs
  input <- awaitInput
  res <- (\(Cli ma) -> ma (\a _env -> pure (Success a)) env0) . Free.fold go $ unAction (action input)
  finalState <- UnliftIO.readIORef loopStateRef
  pure case res of
    Success () -> (Just (), finalState)
    HaltStep -> (Just (), finalState)
    HaltRepl -> (Nothing, finalState)
