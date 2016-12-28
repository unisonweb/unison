{-# Language BangPatterns #-}
{-# Language FunctionalDependencies #-}
{-# Language GeneralizedNewtypeDeriving #-}

module EasyTest (Test, crash, currentScope, noteScoped, skip, ok, fork, fork', scope, note, expect, tests, random, randomBetween, run', runOnly, run, rerun, rerunOnly, parseMessages, module Control.Monad.IO.Class) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List
import GHC.Stack
import qualified System.Random as Random
import System.Random (Random)
import System.Exit
import qualified Control.Concurrent.Async as A
import qualified Data.Map as Map

data Status = Failed | Passed | Skipped

combineStatus :: Status -> Status -> Status
combineStatus Skipped s = s
combineStatus s Skipped = s
combineStatus Failed _ = Failed
combineStatus _ Failed = Failed
combineStatus Passed Passed = Passed

data Env =
  Env { rng :: TVar Random.StdGen
      , messages :: [String]
      , results :: TQueue (Maybe (TMVar ([String], Status)))
      , note_ :: String -> IO ()
      , allow :: [String] }

newtype Test a = Test (ReaderT Env IO (Maybe a))

atomicLogger :: IO (String -> IO ())
atomicLogger = do
  lock <- newMVar ()
  pure $ \msg -> bracket (takeMVar lock) (\_ -> putMVar lock ()) (\_ -> putStrLn msg)

expect :: HasCallStack => Bool -> Test ()
expect False = crash "unexpected"
expect True = ok

tests :: [Test ()] -> Test ()
tests = msum

runOnly :: String -> Test a -> IO ()
runOnly allow t = do
  logger <- atomicLogger
  seed <- abs <$> Random.randomIO :: IO Int
  run' seed logger (parseMessages allow) t

rerunOnly :: Int -> String -> Test a -> IO ()
rerunOnly seed allow t = do
  logger <- atomicLogger
  run' seed logger (parseMessages allow) t

run :: Test a -> IO ()
run = runOnly ""

rerun :: Int -> Test a -> IO ()
rerun seed = rerunOnly seed []

run' :: Int -> (String -> IO ()) -> [String] -> Test a -> IO ()
run' seed note allow (Test t) = do
  let !rng = Random.mkStdGen seed
  resultsQ <- atomically newTQueue
  rngVar <- newTVarIO rng
  note $ "Random number generation (RNG) state for this run is " ++ show seed ++ ""
  results <- atomically $ newTVar Map.empty
  rs <- A.async . forever $ do
    -- note, totally fine if this bombs once queue is empty
    Just result <- atomically $ readTQueue resultsQ
    (msgs, passed) <- atomically $ takeTMVar result
    atomically $ modifyTVar results (Map.insertWith combineStatus msgs passed)
    case passed of
      Skipped -> pure ()
      Passed -> note $ "OK " ++ showMessages msgs
      Failed -> note $ "FAILED " ++ showMessages msgs
  let line = "------------------------------------------------------------"
  note "Raw test output to follow ... "
  note line
  e <- try (runReaderT (void t) (Env rngVar [] resultsQ note allow)) :: IO (Either SomeException ())
  case e of
    Left e -> note $ "Exception while running tests: " ++ show e
    Right () -> note $ "Waiting for any asynchronously spawned tests to complete ..."
  atomically $ writeTQueue resultsQ Nothing
  _ <- A.waitCatch rs
  note line
  note "\n"
  resultsMap <- readTVarIO results
  let
    resultsList = Map.toList resultsMap
    succeeded = length [ a | a@(_, Passed) <- resultsList ]
    failures = [ a | (a, Failed) <- resultsList ]
    failed = length failures
  note $ "  " ++ show succeeded ++ (if failed == 0 then " PASSED" else " passed")
  note $ "  " ++ show (length failures) ++ (if failed == 0 then " failed" else " FAILED (failed scopes below)")
  case failures of
    [] -> do
      note "\n"
      note line
      note "✅  all tests passed! 👍 🎉"
    (hd:_) -> do
      note $ "    " ++ intercalate "\n    " (map showMessages failures)
      note ""
      note $ "  To rerun with same random seed:\n"
      note $ "    EasyTest.rerun " ++ show seed
      note $ "    EasyTest.rerunOnly " ++ show seed ++ " " ++ "\"" ++ showMessages hd ++ "\""
      note "\n"
      note line
      note "❌"
      exitWith (ExitFailure 1)

showMessages :: [String] -> String
showMessages = intercalate "." . reverse

parseMessages :: String -> [String]
parseMessages s = reverse (go s) where
  go "" = []
  go s = case span (/= '.') s of
    (hd, tl) -> hd : go (drop 1 tl)

scope :: String -> Test a -> Test a
scope msg (Test t) = Test $ do
  env <- ask
  let messages' = msg : messages env
      dropRight1 [] = []
      dropRight1 xs = init xs
  case (null (allow env) || [msg] `isSuffixOf` allow env) of
    False -> putResult Skipped >> pure Nothing
    True -> liftIO $ runReaderT t (env { messages = messages', allow = dropRight1 (allow env) })

note :: String -> Test ()
note msg = do
  note_ <- asks note_
  liftIO $ note_ msg
  pure ()

random :: Random a => Test a
random = do
  rng <- asks rng
  liftIO . atomically $ do
    rng0 <- readTVar rng
    let (a, rng1) = Random.random rng0
    writeTVar rng rng1
    pure a

randomBetween :: Random a => (a,a) -> Test a
randomBetween bounds = do
  rng <- asks rng
  liftIO . atomically $ do
    rng0 <- readTVar rng
    let (a, rng1) = Random.randomR bounds rng0
    writeTVar rng rng1
    pure a

wrap :: Test a -> Test a
wrap (Test t) = Test $ do
  env <- ask
  lift $ runWrap env t

runWrap :: Env -> ReaderT Env IO (Maybe a) -> IO (Maybe a)
runWrap env t = do
  e <- liftIO . try $ runReaderT t env
  case e of
    Left e -> do
      note_ env (showMessages (messages env) ++ " EXCEPTION: " ++ show (e :: SomeException))
      pure Nothing
    Right a -> pure a

currentScope :: Test String
currentScope = do
  msgs <- asks messages
  pure (showMessages msgs)

noteScoped :: String -> Test ()
noteScoped msg = do
  s <- currentScope
  note (s ++ ": " ++ msg)

ok :: Test ()
ok = Test (Just <$> putResult Passed)

skip :: Test ()
skip = Test (Nothing <$ putResult Skipped)

crash :: HasCallStack => String -> Test a
crash msg = do
  let trace = callStack
      msg' = msg ++ " " ++ prettyCallStack trace
  Test (Just <$> putResult Failed) >> noteScoped ("FAILURE " ++ msg') >> Test (pure Nothing)

putResult :: Status -> ReaderT Env IO ()
putResult passed = do
  msgs <- asks messages
  r <- liftIO . atomically $ newTMVar (msgs, passed)
  q <- asks results
  lift . atomically $ writeTQueue q (Just r)

instance MonadReader Env Test where
  ask = Test (Just <$> ask)
  local f (Test t) = Test (local f t)
  reader f = Test (Just <$> reader f)

instance Monad Test where
  fail = crash
  return a = Test (pure (Just a))
  Test a >>= f = Test $ do
    a <- a
    case a of
      Nothing -> pure Nothing
      Just a -> let Test t = f a in t

instance Functor Test where
  fmap = liftM

instance Applicative Test where
  pure = return
  (<*>) = ap

instance MonadIO Test where
  liftIO io = wrap $ Test (Just <$> liftIO io)

instance Alternative Test where
  empty = Test (pure Nothing)
  Test t1 <|> Test t2 = Test $ do
    env <- ask
    (rng1, rng2) <- liftIO . atomically $ do
      currentRng <- readTVar (rng env)
      let (rng1, rng2) = Random.split currentRng
      (,) <$> newTVar rng1 <*> newTVar rng2
    lift $ do
      _ <- runWrap (env { rng = rng1 }) t1
      runWrap (env { rng = rng2 }) t2

instance MonadPlus Test where
  mzero = empty
  mplus = (<|>)

fork :: Test a -> Test ()
fork t = void (fork' t)

fork' :: Test a -> Test (Test a)
fork' (Test t) = do
  env <- ask
  tmvar <- liftIO newEmptyTMVarIO
  liftIO . atomically $ writeTQueue (results env) (Just tmvar)
  r <- liftIO . A.async $ runWrap env t
  waiter <- liftIO . A.async $ do
    e <- A.waitCatch r
    _ <- atomically $ tryPutTMVar tmvar (messages env, Skipped)
    case e of
      Left _ -> pure Nothing
      Right a -> pure a
  pure $ do
    a <- liftIO (A.wait waiter)
    case a of Nothing -> empty
              Just a -> pure a
