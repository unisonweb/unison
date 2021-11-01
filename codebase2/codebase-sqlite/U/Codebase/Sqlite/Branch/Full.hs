{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module U.Codebase.Sqlite.Branch.Full where

import U.Codebase.Reference (Reference')
import U.Codebase.Referent (Referent')
import U.Codebase.Sqlite.DbId (BranchObjectId, CausalHashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalBranchChildId, LocalDefnId, LocalPatchObjectId, LocalTextId)
import qualified U.Util.Map as Map
import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.Set as Set
import Control.Lens (Traversal, Traversal')
import Unison.Prelude

type LocalBranch = Branch' LocalTextId LocalDefnId LocalPatchObjectId LocalBranchChildId

type DbBranch = Branch' TextId ObjectId PatchObjectId (BranchObjectId, CausalHashId)

type Referent'' t h = Referent' (Reference' t h) (Reference' t h)

data Branch' t h p c = Branch
  { terms :: Map t (Map (Referent'' t h) (MetadataSetFormat' t h)),
    types :: Map t (Map (Reference' t h) (MetadataSetFormat' t h)),
    patches :: Map t p,
    children :: Map t c
  }
  deriving (Show, Generic)

branchHashes_ :: Traversal (Branch' t h p c) (Branch' t h' p c) h h'
branchHashes_ _f _ = undefined
  -- Branch <$> traverse (\m -> Map.mapKeys)

branchCausalHashes_ :: Traversal (Branch' t h p c) (Branch' t h p c') c c'
branchCausalHashes_ f Branch{..} =
  Branch terms types patches <$> traverse f children


-- convertBranch :: (DB m, MonadState MigrationState m) => DbBranch -> m DbBranch
-- convertBranch dbBranch = _

-- branch plan
-- ===========
-- 1. function that reads a DbBranch out of codebase
-- ==> loadDbBranchByObjectId
-- 2. function which remaps references in a Branch ✅
-- ==> Chris's work
-- 3. function that takes DbBranch to (LocalIds, LocalBranch)
-- ==> dbToLocalBranch (todo)
-- 4. function that takes a DbBranch to a Hashing.V2.Branch
-- ==> Unison.Codebase.SqliteCodebase.MigrateSchema12.DbHelpers.dbBranchHash
-- 5. saveBranchHash
-- 6. saveBranchObject
-- ===============
-- end branch plan

-- causal plan'
-- ===========
-- 1.

-- database has a root CausalHashId
-- from CausalHashId, we can look up ValueHashId and
--
-- old object id --db--> old hash --mem--> new hash --db--> new object id

--
-- Branch
--  { terms :: Map TextId (Map (Referent'' TextId ObjectId) (MetadataSetFormat' TextId ObjectId)),
--    types :: Map TextId (Map (Reference' TextId ObjectId) (MetadataSetFormat' TextId ObjectId)),
--    patches :: Map TextId PatchObjectId,
--    children :: Map TextId (BranchObjectId, CausalHashId)
--  }


type LocalMetadataSet = MetadataSetFormat' LocalTextId LocalDefnId

type DbMetadataSet = MetadataSetFormat' TextId ObjectId

data MetadataSetFormat' t h = Inline (Set (Reference' t h))
  deriving Show

metadataSetFormatReferences_ ::
  (Ord t, Ord h) =>
  Traversal' (MetadataSetFormat' t h) (Reference' t h)
metadataSetFormatReferences_ f (Inline refs) =
  fmap (Inline . Set.fromList) . traverse f . Set.toList $ refs

quadmap :: forall t h p c t' h' p' c'. (Ord t', Ord h') => (t -> t') -> (h -> h') -> (p -> p') -> (c -> c') -> Branch' t h p c -> Branch' t' h' p' c'
quadmap ft fh fp fc (Branch terms types patches children) =
  Branch
    (Map.bimap ft doTerms terms)
    (Map.bimap ft doTypes types)
    (Map.bimap ft fp patches)
    (Map.bimap ft fc children)
  where
    doTerms = Map.bimap (bimap (bimap ft fh) (bimap ft fh)) doMetadata
    doTypes = Map.bimap (bimap ft fh) doMetadata
    doMetadata (Inline s) = Inline . Set.map (bimap ft fh) $ s
