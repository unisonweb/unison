{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.FileCodebase.Reserialize where

import Unison.Prelude

import Data.Monoid.Generic
import Control.Lens
import Control.Monad.State (evalStateT, StateT)

import           UnliftIO.Directory             ( doesFileExist )
import qualified Unison.Codebase.Causal        as Causal
import           Unison.Codebase.Branch         ( Branch(..) )
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Reference              as Reference
import Unison.Reference (Reference)
import qualified Unison.Referent               as Referent
import           Unison.Var                     ( Var )
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Star3             as Star3
import Unison.Codebase.FileCodebase.Common
import Unison.DataDeclaration as DD
import Unison.Codebase (BuiltinAnnotation, CodebasePath)
import qualified Unison.Term as Term
import qualified Unison.Type as Type

data SyncedEntities = SyncedEntities
  { _syncedTerms :: Set Reference.Id
  , _syncedDecls :: Set Reference.Id
  } deriving Generic
  deriving Semigroup via GenericSemigroup SyncedEntities
  deriving Monoid via GenericMonoid SyncedEntities

makeLenses ''SyncedEntities

-- Copy (merge) all dependents of `branch` from `srcPath` into `destPath`,
-- and set `branch` as the new root in `destPath`.
-- 
-- As a refresher, in the normal course of using `ucm` and updating the 
-- namespace, we call Branch.sync to write the updated root to disk.
-- Branch.sync takes a few parameters:
--  - `exists`: checks if a branch file already exists in the codebase, 
--              so we can skip it and the rest of its history.
--  - `serializeRaw`: given a Branch.Hash, writes a Branch.Raw to disk
--  - `serializeEdits`: given an EditsHash, writes an `m Patch` to disk
--
-- In this module, our `serializeRaw` (called `serialize`) is given double duty, 
-- to both write out the Branch.Raw as usual, and to take a look at the decls 
-- and terms referenced by each Raw, and write them and their dependencies 
-- (via normal serialization) to the destination codebase. 
-- 
-- The dependents, type, and type mention indices are populated like with the 
-- primary local codebase, by adding to them as each definition is serialized.
syncToDirectory
  :: forall m v a
   . (MonadIO m)
  => Var v
  => BuiltinAnnotation a
  => S.Format v
  -> S.Format a
  -> CodebasePath
  -> CodebasePath
  -> Branch m
  -> m ()
syncToDirectory fmtV fmtA srcPath destPath branch@(Branch c) = do
  flip evalStateT mempty $
    Branch.sync
      (hashExists destPath)
      serialize
      (serializeEdits destPath)
      (Branch.transform lift branch)
  updateCausalHead (branchHeadDir destPath) c
 where
  serialize rh rawBranch = do
    writeBranch $ Causal.rawHead rawBranch
    serializeRawBranch destPath rh rawBranch
  writeBranch :: Branch.Raw -> StateT SyncedEntities m ()
  writeBranch (Branch.Raw terms types _ _) = do
    -- Write all decls & transitive dependencies 🤞
    traverse_ putDecl'
      . mapMaybe Reference.toId
      . toList
      $ Star3.fact types
    -- Write all terms & transitive dependencies 🤞
    traverse_ putTerm'
      . mapMaybe Reference.toId
      . mapMaybe Referent.toTermReference
      . toList
      $ Star3.fact terms
  getDecl' = getDecl (S.get fmtV) (S.get fmtA) srcPath
  getTerm' = getTerm (S.get fmtV) (S.get fmtA) srcPath
  getTypeOfTerm' = getTypeOfTerm (S.get fmtV) (S.get fmtA) srcPath
  getWatch' = getWatch (S.get fmtV) (S.get fmtA) srcPath
  putWatch' = putWatch (S.put fmtV) (S.put fmtA) destPath
  -- copyHelper is responsible for making sure we don't repeat work
  putDecl' = doFileOnce destPath syncedDecls declPath go where
    go i = do
      d <- fromMaybe noDecl <$> getDecl' i
      putDecl (S.put fmtV) (S.put fmtA) destPath i d
      traverse_ tryPutDependency (DD.declDependencies d)
      where
      noDecl = error $
        "😞 I was trying to copy a type from" ++
        "\n\t" ++ declPath destPath i ++
        "\nbut there was nothing there."
  putTerm' :: Reference.Id -> StateT SyncedEntities m ()
  putTerm' = doFileOnce destPath syncedTerms termPath go where
    go i = do
      tm <- fromMaybe noTerm <$> getTerm' i
      tp <- fromMaybe noType <$> getTypeOfTerm' i
      putTerm (S.put fmtV) (S.put fmtA) destPath i tm tp
      mayTest <- getWatch' UF.TestWatch i
      maybe (pure ()) (putWatch' UF.TestWatch i) mayTest
      traverse_ tryPutDependency
        (Term.dependencies tm <> Type.dependencies tp)
      where
      noTerm = error $
        "😞 I was trying to copy a term from" ++
        "\n\t" ++ termPath destPath i ++
        "\nbut there was nothing there."
      noType = error $
        "😞 I was trying to copy a type signature from" ++
        "\n\t" ++ typePath destPath i ++
        "\nbut there was nothing there."
  serialize :: Causal.Serialize (StateT SyncedEntities m) Branch.Raw Branch.Raw
  -- Decide if this is a term reference or decl reference, and then dispatch.
  -- These disk accesses are guarded by `copyHelper`, above.
  tryPutDependency :: Reference -> StateT SyncedEntities m ()
  tryPutDependency Reference.Builtin{} = pure ()
  tryPutDependency (Reference.DerivedId i) =
    ifM (isTerm i) (putTerm' i) $
      ifM (isDecl i) (putDecl' i) $
        fail $ "😞 I was trying to copy the definition of " ++ show i
            ++ ", but I couldn't find it as a type _or_ a term."
  isTerm = doesFileExist . termPath srcPath
  isDecl = doesFileExist . declPath srcPath
