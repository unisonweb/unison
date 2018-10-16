{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module Unison.Codebase where

import Data.Maybe (catMaybes)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad (when, filterM)
import Data.Foldable (traverse_)
import Data.List (isSuffixOf, partition)
import Data.Text (Text)
import System.Directory (doesDirectoryExist,listDirectory,removeFile)
import qualified Data.Text as Text
import qualified Unison.Term as Term
import qualified Unison.DataDeclaration as DD
import qualified Unison.Hash as Hash
import qualified Unison.Codebase.Branch as Branch
import Unison.Reference (Reference (Builtin, Derived))
import qualified Unison.Codebase.Serialization.V0 as V0
import Unison.Codebase.Name (Name)
import Unison.Codebase.Branch (Branch)
import System.FilePath (FilePath, (</>), takeBaseName)
import Unison.Result (Result, Note)
import Unison.UnisonFile (UnisonFile')
import Unison.Hash (Hash)
import qualified Data.ByteString as BS
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Unison.Type as Type
import qualified Unison.Codebase.Serialization as S
import qualified Data.Map as Map
import qualified Unison.Builtin as Builtin
import Unison.Var (Var)

type DataDeclaration v a = DD.DataDeclaration' v a
type EffectDeclaration v a = DD.EffectDeclaration' v a
type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a
type Decl v a = Either (EffectDeclaration v a) (DataDeclaration v a)

data Codebase m v a =
  Codebase { getTerm :: Hash -> m (Maybe (Term v a))
           , getTypeOfTerm :: Reference -> m (Maybe (Type v a))
           , putTerm :: Hash -> Term v a -> Type v a -> m ()

           , getTypeDeclaration :: Hash -> m (Maybe (Decl v a))
           , putTypeDeclaration :: Hash -> Decl v a -> m ()

           , branches :: m [Name]
           , getBranch :: Name -> m (Maybe Branch)
           -- thought: this merges the given branch with the existing branch
           -- or creates a new branch if there's no branch with that name
           , mergeBranch :: Name -> Branch -> m Branch
           }

data Session m v a
  = Session { branch :: m Name
            , switchBranch :: Name -> m ()
            -- Await new .ubf files
            , watchBranches :: FilePath -> m (FilePath, Branch)
            -- Await the next .u file change in the given directory,
            -- and return the path of the thing that changed, its contents,
            -- and the results of parsing / typechecking.
            , watch :: FilePath -> m (FilePath, Text, Result (Note v a) (UnisonFile' v a)) }

data Err = InvalidBranchFile FilePath String deriving Show

branchFromFile :: (MonadIO m, MonadError Err m) => FilePath -> m Branch
branchFromFile ubf = do
  bytes <- liftIO $ BS.readFile ubf
  case Get.runGetS V0.getBranch bytes of
    Left err -> throwError $ InvalidBranchFile ubf err
    Right branch -> pure branch

branchToFile :: FilePath -> Branch -> IO ()
branchToFile ubf b =
  BS.writeFile ubf (Put.runPutS (V0.putBranch b))

branchFromFile' :: FilePath -> IO (Maybe Branch)
branchFromFile' ubf = go =<< runExceptT (branchFromFile ubf)
  where
    go (Left e) = do
      liftIO $ putStrLn (show e)
      pure Nothing
    go (Right b) = pure (Just b)

-- todo: might want to have richer return type that reflects merges that may have been done
branchFromDirectory :: FilePath -> IO (Maybe Branch)
branchFromDirectory dir = do
  exists <- doesDirectoryExist dir
  case exists of
    False -> pure Nothing
    True -> do
      bos <- traverse branchFromFile' =<< filesInPathMatchingSuffix dir ".ubf"
      pure $ case catMaybes bos of
        [] -> Nothing
        bos -> Just (mconcat bos)

-- todo: change this to use System.FilePath.takeExtension
filesInPathMatchingSuffix :: FilePath -> String -> IO [FilePath]
filesInPathMatchingSuffix path suffix = doesDirectoryExist path >>= \ok ->
  if ok then filter (suffix `isSuffixOf`) <$> listDirectory path
  else pure []

isValidBranchDirectory :: FilePath -> IO Bool
isValidBranchDirectory path =
  not . null <$> filesInPathMatchingSuffix path ".ubf"

termPath, typePath, declPath :: FilePath -> Hash -> FilePath
termPath path h = path </> "terms" </> Hash.base58s h </> "compiled.ub"
typePath path h = path </> "terms" </> Hash.base58s h </> "type.ub"
declPath path h = path </> "types" </> Hash.base58s h </> "compiled.ub"
branchesPath :: FilePath -> FilePath
branchesPath path = path </> "branches"
branchPath :: FilePath -> Text -> FilePath
branchPath path name = branchesPath path </> Text.unpack name

-- todo: builtin data decls (optional, unit, pair) should just have a regular
-- hash-based reference, rather than being Reference.Builtin
-- and we should verify that this doesn't break the runtime
codebase1 :: forall v a. Var v
          => a -> S.Format v -> S.Format a -> FilePath -> Codebase IO v a
codebase1 builtinTypeAnnotation
          (S.Format getV putV) (S.Format getA putA) path = let
  getTerm h = S.getFromFile (V0.getTerm getV getA) (termPath path h)
  putTerm h e typ = do
    S.putWithParentDirs (V0.putTerm putV putA) (termPath path h) e
    S.putWithParentDirs (V0.putType putV putA) (typePath path h) typ
  getTypeOfTerm r = case r of
    (Builtin _) -> pure $
      fmap (const builtinTypeAnnotation) <$>
      Map.lookup r Builtin.builtins0
    Derived h ->
      S.getFromFile (V0.getType getV getA) (typePath path h)
  getDecl h =
    S.getFromFile (V0.getEither (V0.getEffectDeclaration getV getA)
                                (V0.getDataDeclaration getV getA))
                  (declPath path h)
  putDecl h decl =
    S.putWithParentDirs (V0.putEither (V0.putEffectDeclaration putV putA)
                                      (V0.putDataDeclaration putV putA))
                        (declPath path h)
                        decl
  branches = map Text.pack <$> do
    files <- listDirectory (branchesPath path)
    filterM isValidBranchDirectory files
  getBranch name = branchFromDirectory (branchPath path name)

  -- delete any leftover branch files "before" this one,
  -- and write this one if it doesn't already exist.
  overwriteBranch :: Name -> Branch -> IO ()
  overwriteBranch name branch = do
    let newBranchHash = Hash.base58s . Branch.toHash $ branch
    (match, nonmatch) <-
      partition (\s -> newBranchHash == takeBaseName s) <$>
         filesInPathMatchingSuffix (branchPath path name) ".ubf"
    let
      isBefore :: Branch -> FilePath -> IO Bool
      isBefore b ubf = maybe False (`Branch.before` b) <$> branchFromFile' ubf
    -- delete any existing .ubf files
    traverse_ removeFile =<< filterM (isBefore branch) nonmatch
    -- save new branch data under <base58>.ubf
    when (null match) $
      branchToFile (branchPath path name </> newBranchHash <> ".ubf") branch

  mergeBranch name branch = do
    target <- getBranch name
    let newBranch = case target of
          -- merge with existing branch if present
          Just existing -> Branch.merge branch existing
          -- or save new branch
          Nothing -> branch
    overwriteBranch name newBranch
    pure newBranch
  in Codebase getTerm getTypeOfTerm putTerm getDecl putDecl branches getBranch mergeBranch
