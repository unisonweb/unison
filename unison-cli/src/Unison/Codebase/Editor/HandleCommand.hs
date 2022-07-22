module Unison.Codebase.Editor.HandleCommand
  ( commandLine,
  )
where

import Control.Monad.Reader (MonadReader (ask, local))
import Unison.Codebase.Editor.Command (Action (..), Command (..), Env, LoopState)
import Unison.Codebase.Editor.Input (Event, Input)
import Unison.Monad.Cli (Cli (..), ReturnType (..), abortStep, haltRepl, scopeWith, with, withCliToIO')
import Unison.Prelude
import qualified Unison.Util.Free as Free
import qualified UnliftIO

commandLine ::
  Env ->
  LoopState ->
  IO (Either Event Input) ->
  (Either Event Input -> Action ()) ->
  IO (Maybe (), LoopState)
commandLine env0 loopState0 awaitInput action = do
  loopStateRef <- UnliftIO.newIORef loopState0
  let go :: forall r x. Command x -> Cli r x
      go x = case x of
        AskEnv -> ask
        LocalEnv f e -> local f (Free.fold go e)
        GetLoopState -> liftIO (UnliftIO.readIORef loopStateRef)
        PutLoopState st -> liftIO (UnliftIO.writeIORef loopStateRef st)
        Eval m -> liftIO m
        WithRunInIO doUnlifts -> withCliToIO' \runInIO ->
          doUnlifts (\(Action free) -> runInIO (Free.fold go) free)
        Abort -> abortStep
        Quit -> haltRepl
        WithResource k -> with k
        Reset (Action act) -> scopeWith (Free.fold go act)
        RunCli cli -> undefined

  input <- awaitInput
  res <- (\(Cli ma) -> ma (\a _env -> pure (Success a)) env0) . Free.fold go $ unAction (action input)
  finalState <- UnliftIO.readIORef loopStateRef
  pure case res of
    Success () -> (Just (), finalState)
    HaltStep -> (Just (), finalState)
    HaltRepl -> (Nothing, finalState)
