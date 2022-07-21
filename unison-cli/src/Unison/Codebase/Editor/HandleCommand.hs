{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Unison.Codebase.Editor.HandleCommand where

import Control.Monad.Reader (MonadReader (ask, local), ReaderT (ReaderT))
import Control.Monad.Trans.Cont
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Editor.Command (Action (..), Command (..), Env, LoopState)
import Unison.Codebase.Editor.Input (Event, Input)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Symbol (Symbol)
import qualified Unison.Util.Free as Free
import qualified UnliftIO

data ReturnType a
  = Success a
  | HaltStep
  | HaltRepl

data Bailing
  = HaltingStep
  | HaltingRepl
  deriving stock (Show)
  deriving anyclass (Exception)

newtype Cli r a = Cli {unCli :: (a -> Env -> IO (ReturnType r)) -> Env -> IO (ReturnType r)}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env
    )
    via ContT (ReturnType r) (ReaderT Env IO)

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
  Env ->
  LoopState ->
  IO (Either Event Input) ->
  (Branch IO -> IO ()) ->
  Codebase IO Symbol Ann ->
  (Either Event Input -> Action ()) ->
  IO (Maybe (), LoopState)
commandLine env0 loopState0 awaitInput setBranchRef codebase action = do
  loopStateRef <- UnliftIO.newIORef loopState0
  let go :: forall r x. Command x -> Cli r x
      go x = case x of
        AskEnv -> ask
        LocalEnv f e -> local f (Free.fold go e)
        GetLoopState -> liftIO (UnliftIO.readIORef loopStateRef)
        PutLoopState st -> liftIO (UnliftIO.writeIORef loopStateRef st)
        Eval m -> liftIO m
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

  input <- awaitInput
  res <- (\(Cli ma) -> ma (\a _env -> pure (Success a)) env0) . Free.fold go $ unAction (action input)
  finalState <- UnliftIO.readIORef loopStateRef
  pure case res of
    Success () -> (Just (), finalState)
    HaltStep -> (Just (), finalState)
    HaltRepl -> (Nothing, finalState)
