{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.FileCodebase.Reserialize where

import Unison.Prelude
import           UnliftIO.Directory             ( doesPathExist
                                                )
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase                ( Codebase )
import qualified Unison.Codebase.Causal        as Causal
import           Unison.Codebase.Branch         ( Branch )
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Reference              as Reference
import           Unison.Referent                (
                                                 pattern Ref
                                                , pattern Con
                                                )
import           Unison.Var                     ( Var )
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Star3             as Star3
import Control.Error (rightMay)
import Unison.Codebase.FileCodebase.Common

-- Create a codebase structure at `destPath` if none exists, and
-- copy (merge) all codebase elements from the current codebase into it.
syncToDirectory
  :: forall m v a
   . (MonadIO m)
  => Var v
  => Codebase.BuiltinAnnotation a
  => S.Format v
  -> S.Format a
  -> Codebase m v a
  -> CodebasePath
  -> Branch m
  -> m (Branch m)
syncToDirectory fmtV fmtA codebase destPath branch = do
  b <- (liftIO . exists) destPath
  if b then do
--    let code = codebase1 fmtV fmtA localPath
    remoteRoot <- fromMaybe Branch.empty . rightMay <$> getRootBranch destPath
    Branch.sync (hashExists destPath) serialize (serializeEdits destPath) branch
    merged <- Branch.merge branch remoteRoot
    putRootBranch destPath merged
    pure merged
  else do
    Branch.sync (hashExists destPath) serialize (serializeEdits destPath) branch
    updateCausalHead (branchHeadDir destPath) $ Branch._history branch
    pure branch
 where
  serialize :: Causal.Serialize m Branch.Raw Branch.Raw
  serialize rh rawBranch = do
    writeBranch $ Causal.rawHead rawBranch
    serializeRawBranch destPath rh rawBranch
  calamity i =
    error
      $  "Calamity! Somebody deleted "
      <> show i
      <> " from the codebase while I wasn't looking."
  writeBranch :: Branch.Raw -> m ()
  writeBranch (Branch.Raw terms types _ _) = do
    for_ (toList $ Star3.fact types) $ \case
      Reference.DerivedId i -> do
        alreadyExists <- liftIO . doesPathExist $ declPath destPath i
        unless alreadyExists $ do
          mayDecl <- Codebase.getTypeDeclaration codebase i
          maybe (calamity i) (putDecl (S.put fmtV) (S.put fmtA) destPath i) mayDecl
      Reference.Builtin{} -> pure ()
    -- Write all terms
    for_ (toList $ Star3.fact terms) $ \case
      Ref r@(Reference.DerivedId i) -> do
        alreadyExists <- liftIO . doesPathExist $ termPath destPath i
        unless alreadyExists $ do
          mayTerm <- Codebase.getTerm codebase i
          mayType <- Codebase.getTypeOfTerm codebase r
          fromMaybe (calamity i)
                    (putTerm (S.put fmtV) (S.put fmtA) destPath i <$> mayTerm <*> mayType)
          -- If the term is a test, write the cached value too.
          mayTest <- Codebase.getWatch codebase UF.TestWatch i
          maybe (pure ()) (putWatch (S.put fmtV) (S.put fmtA) destPath UF.TestWatch i) mayTest
      Ref Reference.Builtin{} -> pure ()
      Con{} -> pure ()
