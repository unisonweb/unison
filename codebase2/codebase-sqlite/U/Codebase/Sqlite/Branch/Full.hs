{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module U.Codebase.Sqlite.Branch.Full where

import Control.Lens (Traversal, Traversal')
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Set as Set
import U.Codebase.Reference (Reference')
import U.Codebase.Referent (Referent')
import U.Codebase.Sqlite.DbId (BranchObjectId, CausalHashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalBranchChildId, LocalDefnId, LocalPatchObjectId, LocalTextId)
import qualified U.Util.Map as Map
import Unison.Prelude

-- |
-- @
-- Branch
--   { terms :: Map LocalTextId (Map LocalReferent LocalMetadataSet),
--     types :: Map LocalTextId (Map LocalReference LocalMetadataSet),
--     patches :: Map LocalTextId LocalPatchObjectId,
--     children :: Map LocalTextId LocalBranchChildId
--   }
-- @
type LocalBranch = Branch' LocalTextId LocalDefnId LocalPatchObjectId LocalBranchChildId

-- |
-- @
-- Branch
--   { terms :: Map TextId (Map Referent DbMetadataSet),
--     types :: Map TextId (Map Reference DbMetadataSet),
--     patches :: Map TextId PatchObjectId,
--     children :: Map TextId (BranchObjectId, CausalHashId)
--   }
-- @
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

termHashes_ :: Traversal (Branch' t h p c) (Branch' t h' p c) h h'
termHashes_ _f _ = undefined

typeHashes_ :: Traversal (Branch' t h p c) (Branch' t h' p c) h h'
typeHashes_ _f _ = undefined

patchHashes_ :: Traversal (Branch' t h p c) (Branch' t h p' c) p p'
patchHashes_ = undefined

childrenHashes_ :: Traversal (Branch' t h p c) (Branch' t h p c') c c'
childrenHashes_ = undefined

-- Branch <$> traverse (\m -> Map.mapKeys)

branchCausalHashes_ :: Traversal (Branch' t h p c) (Branch' t h p c') c c'
branchCausalHashes_ f Branch {..} =
  Branch terms types patches <$> traverse f children

type LocalMetadataSet = MetadataSetFormat' LocalTextId LocalDefnId

type DbMetadataSet = MetadataSetFormat' TextId ObjectId

data MetadataSetFormat' t h = Inline (Set (Reference' t h))
  deriving (Show)

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
