-- | The `bundle` command: like `compile`, but a self-contained executable,
-- a minified unisonrt with the program in it (Unison.Runtime.Bundle,
-- unison-runtime/unisonrt/README.md).
--
-- Minifying is the same for every program, since a bundle is unisonrt with
-- a program added, so it is done once for each unisonrt: the minified copy
-- is kept in the cache directory, under the hash of the unisonrt it was
-- made from.
module Unison.Codebase.Editor.HandleInput.Bundle
  ( handleBundle,
    BundleProblem (..),
    bundleFromCodebase,
  )
where

import Control.Exception (displayException)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteString qualified as B
import Data.Text qualified as Text
import Minify (minifyExecutable)
import System.Directory
  ( XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    doesFileExist,
    findExecutable,
    getXdgDirectory,
    removeFile,
    renameFile,
  )
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Info (os)
import System.Process (readProcessWithExitCode)
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Output (Output (BundleFailed, BundleMinifyingRuntime, BundleNotMinified, BundleWritten))
import Unison.Codebase.Execute qualified as Codebase (codebaseToCodeLookup)
import Unison.Codebase.MainTerm qualified as MainTerm
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.Runtime qualified as Runtime
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Runtime (Error, Runtime)
import Unison.Runtime.Bundle (makeExecutable, signExecutable, writeBundle)
import Unison.Runtime.Version (runtimeVersion)
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as SyntaxName
import UnliftIO.Exception (tryAny)

-- | Writes the bundle, given how to compile the program to a .uc file
-- (the `compile` command's own code).
handleBundle :: (FilePath -> HQ.HashQualified Name -> Cli ()) -> FilePath -> HQ.HashQualified Name -> Cli ()
handleBundle compile output main = do
  runtime <- minifiedRuntime
  program <- withTempDirectory \dir -> do
    compile (dir </> "program") main
    liftIO (B.readFile (dir </> "program.uc"))
  liftIO (writeBundle runtime program output) >>= \case
    Left err -> Cli.returnEarly (BundleFailed (Text.pack err))
    Right () -> Cli.respond (BundleWritten output)
  where
    withTempDirectory :: (FilePath -> Cli a) -> Cli a
    withTempDirectory action = Cli.with (withSystemTempDirectory "ucm-bundle") action

-- | A minified unisonrt of this ucm's runtime version, from the cache or
-- made now.
minifiedRuntime :: Cli FilePath
minifiedRuntime =
  liftIO locateRuntime >>= \case
    Left err -> Cli.returnEarly (BundleFailed err)
    Right (Cached cached) -> pure cached
    Right missing@(Uncached runtime _ _) -> do
      Cli.respond (BundleMinifyingRuntime runtime)
      liftIO (minifyRuntime missing) >>= \case
        Right minified -> pure minified
        -- on a platform minify doesn't handle, say, the bundle is bigger
        Left why -> Cli.respond (BundleNotMinified why) $> runtime

-- | Where the minified copy of unisonrt is, or what is needed to make it.
data Located
  = Cached FilePath
  | -- | unisonrt, its bytes, and where its minified copy goes
    Uncached FilePath B.ByteString FilePath

locateRuntime :: IO (Either Text Located)
locateRuntime = runExceptT do
  runtime <- ExceptT findRuntime
  ExceptT (checkVersion runtime)
  bytes <- liftIO (B.readFile runtime)
  cache <- liftIO (getXdgDirectory XdgCache "unisonrt")
  let key = show (hash bytes :: Digest SHA256)
      cached = cache </> ("unisonrt-" ++ key ++ "-min")
  liftIO (doesFileExist cached) <&> \case
    True -> Cached cached
    False -> Uncached runtime bytes cached

minifyRuntime :: Located -> IO (Either Text FilePath)
minifyRuntime = \case
  Cached cached -> pure (Right cached)
  Uncached _ bytes cached -> do
    createDirectoryIfMissing True (takeDirectory cached)
    minifyExecutable bytes >>= \case
      Left err -> pure (Left ("I couldn't minify unisonrt: " <> Text.pack err))
      Right small -> do
        -- written aside and renamed, so that a cached copy is always whole
        let partial = cached ++ ".partial"
        B.writeFile partial small
        makeExecutable partial
        signExecutable partial >>= \case
          Left err -> removeFile partial $> Left (Text.pack err)
          Right () -> renameFile partial cached $> Right cached

data BundleProblem
  = BundleProblem Text
  | -- | compiling the program failed
    BundleCompileError Error

-- | `ucm bundle project/branch:path.to.main OUTPUT`: the bundle command,
-- without the interactive loop. Says on stdout when it minifies.
bundleFromCodebase :: Codebase IO Symbol Ann -> Runtime Symbol -> PP.ProjectPathNames -> FilePath -> IO (Either BundleProblem ())
bundleFromCodebase codebase runtime mainPath output = runExceptT do
  let problem = throwError . BundleProblem
  ids <-
    liftIO . Codebase.runTransaction codebase $
      Q.loadProjectByName mainPath.project >>= \case
        Nothing -> pure (Left ("I couldn't find the project " <> into @Text mainPath.project <> "."))
        Just project ->
          Q.loadProjectBranchByName project.projectId mainPath.branch <&> \case
            Nothing -> Left ("I couldn't find the branch " <> into @Text mainPath.branch <> " of " <> into @Text mainPath.project <> ".")
            Just branch -> Right (project.projectId, branch.branchId)
  (projectId, branchId) <- either problem pure ids
  mainName <- maybe (problem "The path has to lead to a definition, as in project/branch:path.to.main.") pure (Path.toName (PP.path mainPath))
  names <- liftIO (Branch.toNames . Branch.head <$> Codebase.expectProjectBranchRoot codebase projectId branchId)
  main <-
    liftIO $
      MainTerm.getMainTerm
        (Codebase.runTransaction codebase . Codebase.getTypeOfTerm codebase)
        names
        (HQ.NameOnly mainName)
        (Runtime.mainType runtime)
  ref <- case main of
    MainTerm.Success _ ref _ _ -> pure ref
    MainTerm.NotFound -> problem ("I couldn't find " <> SyntaxName.toText mainName <> ".")
    MainTerm.BadType _ -> problem (SyntaxName.toText mainName <> " has to have the type '{IO, Exception} ().")
    MainTerm.Ambiguous _ -> problem (SyntaxName.toText mainName <> " names more than one definition.")
  located <- liftIO locateRuntime >>= either problem pure
  minified <- case located of
    Cached cached -> pure cached
    Uncached rt _ _ -> do
      liftIO (putStrLn ("Minifying the runtime " ++ rt ++ " for bundles. This takes a little while, once for each build of it."))
      liftIO (minifyRuntime located) >>= \case
        Right small -> pure small
        Left why -> liftIO (putStrLn (Text.unpack why <> " The bundle has the whole runtime.")) $> rt
  let ppe = (PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)).suffixifiedPPE
      codeLookup = () <$ Codebase.codebaseToCodeLookup codebase
  program <-
    ExceptT . withSystemTempDirectory "ucm-bundle" $ \dir -> do
      let uc = dir </> "program.uc"
      Runtime.compileTo runtime Runtime.defaultCompileOpts codeLookup ppe ref uc >>= \case
        Just err -> pure (Left (BundleCompileError err))
        Nothing -> Right <$> B.readFile uc
  liftIO (writeBundle minified program output) >>= either (problem . Text.pack) pure

-- | unisonrt: the one UNISONRT names, else the one next to ucm, else the
-- one on the PATH.
findRuntime :: IO (Either Text FilePath)
findRuntime =
  lookupEnv "UNISONRT" >>= \case
    Just path ->
      doesFileExist path <&> \case
        True -> Right path
        False -> Left ("UNISONRT names " <> Text.pack path <> ", which doesn't exist.")
    Nothing -> do
      beside <- (</> exe) . takeDirectory <$> getExecutablePath
      doesFileExist beside >>= \case
        True -> pure (Right beside)
        False ->
          findExecutable "unisonrt" <&> \case
            Just path -> Right path
            Nothing ->
              Left $
                "I couldn't find unisonrt, the runtime a bundle is made from. It is "
                  <> "looked for next to ucm, then on the PATH; UNISONRT can name it."
  where
    exe = "unisonrt" ++ if os == "mingw32" then ".exe" else ""

-- | A bundle's program only runs on the runtime version that compiled it.
checkVersion :: FilePath -> IO (Either Text ())
checkVersion runtime = do
  tryAny (readProcessWithExitCode runtime ["--version"] "") <&> \case
    Left e -> Left ("I couldn't run " <> Text.pack runtime <> ": " <> Text.pack (displayException e))
    Right (_, out, _) -> versions (Text.strip (Text.pack out))
  where
    versions theirs =
      if theirs == runtimeVersion
        then Right ()
        else
          Left $
            Text.unlines
              [ "The unisonrt at " <> Text.pack runtime <> " runs programs of another version of Unison.",
                "  unisonrt: " <> theirs,
                "  this ucm: " <> runtimeVersion,
                "Build or install the two together."
              ]
