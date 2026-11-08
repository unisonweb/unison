-- | The `bundle` command: like `compile`, but a self-contained executable,
-- unisonrt with the program in it (Unison.Runtime.Bundle,
-- unison-runtime/unisonrt/README.md). Releases ship unisonrt minified, so
-- bundles are too.
module Unison.Codebase.Editor.HandleInput.Bundle
  ( handleBundle,
    BundleProblem (..),
    bundleFromCodebase,
  )
where

import Control.Exception (displayException)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Data.ByteString qualified as B
import Data.Text qualified as Text
import System.Directory (doesFileExist, findExecutable)
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
import Unison.Codebase.Editor.Output (Output (BundleFailed, BundleWritten))
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
import Unison.Runtime.Bundle (writeBundle)
import Unison.Runtime.Version (runtimeVersion)
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as SyntaxName
import UnliftIO.Exception (tryAny)

-- | Writes the bundle, given how to compile the program to a .uc file
-- (the `compile` command's own code).
handleBundle :: (FilePath -> HQ.HashQualified Name -> Cli ()) -> FilePath -> HQ.HashQualified Name -> Cli ()
handleBundle compile output main = do
  runtime <- bundleRuntime
  program <- withTempDirectory \dir -> do
    compile (dir </> "program") main
    liftIO (B.readFile (dir </> "program.uc"))
  liftIO (writeBundle runtime program output) >>= \case
    Left err -> Cli.returnEarly (BundleFailed (Text.pack err))
    Right () -> Cli.respond (BundleWritten output)
  where
    withTempDirectory :: (FilePath -> Cli a) -> Cli a
    withTempDirectory action = Cli.with (withSystemTempDirectory "ucm-bundle") action

-- | The unisonrt a bundle starts from, of this ucm's runtime version. A
-- release ships it minified next to ucm, so it is used as it is (a
-- development build's is the whole runtime, and so are its bundles).
bundleRuntime :: Cli FilePath
bundleRuntime = liftIO locateRuntime >>= either (Cli.returnEarly . BundleFailed) pure

locateRuntime :: IO (Either Text FilePath)
locateRuntime = runExceptT do
  runtime <- ExceptT findRuntime
  ExceptT (checkVersion runtime)
  pure runtime

data BundleProblem
  = BundleProblem Text
  | -- | compiling the program failed
    BundleCompileError Error

-- | `ucm bundle project/branch:path.to.main OUTPUT`: the bundle command,
-- without the interactive loop.
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
  rtFile <- liftIO locateRuntime >>= either problem pure
  let ppe = (PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)).suffixifiedPPE
      codeLookup = () <$ Codebase.codebaseToCodeLookup codebase
  program <-
    ExceptT . withSystemTempDirectory "ucm-bundle" $ \dir -> do
      let uc = dir </> "program.uc"
      Runtime.compileTo runtime Runtime.defaultCompileOpts codeLookup ppe ref uc >>= \case
        Just err -> pure (Left (BundleCompileError err))
        Nothing -> Right <$> B.readFile uc
  liftIO (writeBundle rtFile program output) >>= either (problem . Text.pack) pure

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
