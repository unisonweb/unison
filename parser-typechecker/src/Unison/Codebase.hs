{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Unison.Codebase where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad (when, filterM)
import Data.Foldable (traverse_)
import Data.List (isPrefixOf, isSuffixOf, partition)
import Data.Text (Text)
import System.Directory (listDirectory,removeFile)
import qualified Data.Text as Text
import qualified Unison.Term as Term
import qualified Unison.DataDeclaration as DD
import qualified Unison.Hash as Hash
import qualified Unison.Codebase.Branch as Branch
import Unison.Reference (Reference)
import qualified Unison.Codebase.Serialization.V0 as V0
import Unison.Codebase.Code (Code)
import Unison.Codebase.Name (Name)
import Unison.Codebase.Branch (Branch)
import System.FilePath (FilePath, (</>))
import Unison.Result (Result, Note)
import Unison.UnisonFile (UnisonFile')
import qualified Data.ByteString as BS
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put

type DataDeclaration v a = DD.DataDeclaration' v a
type Term v a = Term.AnnotatedTerm v a

data Codebase m v a =
  Codebase { getCode :: Reference -> m (Maybe (Code v a))
           , putCode :: Code v a -> m Reference
           , branches :: m [Name]
           , getBranch :: Name -> m (Maybe Branch)
           -- thought: this merges the given branch with the existing branch
           -- or creates a new branch if there's no branch with that name
           , mergeBranch :: Name -> Branch -> m ()
           }

data Session m v a
  = Session { branch :: m Name
            , switchBranch :: Name -> m ()
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

codebase1 :: FilePath -> Codebase IO v a
codebase1 path = let
  filesInPathMatchingSuffix path suffix =
    filter (suffix `isSuffixOf`) <$> listDirectory path
  getCode _r = error "todo"
  putCode _code = error "todo"
  branches = error "todo"
  getBranch _name = error "todo"
  -- given a name and a branch, serialize given branch with
  overwriteBranch name branch = do
    let newBranchHash = Hash.base58 . Branch.toHash $ branch
    (match, nonmatch) <- partition (Text.unpack newBranchHash `isPrefixOf`) <$>
                          filesInPathMatchingSuffix path ".ubf"
    let
      isBefore :: Branch -> FilePath -> IO Bool
      isBefore b ubf = maybe False (`Branch.before` b) <$> branchFromFile' ubf
    -- delete any existing .ubf files
    traverse_ removeFile =<< filterM (isBefore branch) nonmatch
    -- save new branch data under <base58>.ubf
    when (null match) $
      branchToFile (path </> Text.unpack name </> Text.unpack newBranchHash <> ".ubf") branch

  mergeBranch name branch = do
    target <- getBranch name
    overwriteBranch name $ case target of
        -- merge with existing branch if present
        Just existing -> Branch.merge branch existing
        -- or save new branch
        Nothing -> branch
  in Codebase getCode putCode branches getBranch mergeBranch
