{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module U.Codebase.Sqlite.Branch.Full where

import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import U.Codebase.Reference (Reference')
import qualified U.Codebase.Reference as Reference
import U.Codebase.Referent (Referent')
import U.Codebase.Sqlite.DbId (BranchHashId, CausalHashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalBranchChildId, LocalDefnId, LocalPatchObjectId, LocalTextId)
import Unison.Prelude
import qualified Unison.Util.Map as Map
import qualified Unison.Util.Set as Set

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
type DbBranch = Branch' TextId ObjectId PatchObjectId (BranchHashId, CausalHashId)

type Referent'' t h = Referent' (Reference' t h) (Reference' t h)

data Branch' t h p c = Branch
  { terms :: Map t (Map (Referent'' t h) (MetadataSetFormat' t h)),
    types :: Map t (Map (Reference' t h) (MetadataSetFormat' t h)),
    patches :: Map t p,
    children :: Map t c
  }
  deriving (Show, Generic)

emptyBranch :: Branch' t h p c
emptyBranch = Branch Map.empty Map.empty Map.empty Map.empty

branchHashes_ :: (Ord h', Ord t, Ord h) => Traversal (Branch' t h p c) (Branch' t h' p c) h h'
branchHashes_ f Branch {..} = do
  newTerms <- for terms (Map.bitraversed both metadataSetFormatReferences_ . Reference.h_ %%~ f)
  newTypes <- for types (Map.bitraversed id metadataSetFormatReferences_ . Reference.h_ %%~ f)
  pure Branch {terms = newTerms, types = newTypes, patches, children}

patches_ :: Traversal (Branch' t h p c) (Branch' t h p' c) p p'
patches_ f Branch {..} = (\newPatches -> Branch terms types newPatches children) <$> traverse f patches

childrenHashes_ :: Traversal (Branch' t h p c) (Branch' t h p c') c c'
childrenHashes_ f Branch {..} = Branch terms types patches <$> traverse f children

branchCausalHashes_ :: Traversal (Branch' t h p c) (Branch' t h p c') c c'
branchCausalHashes_ f Branch {..} =
  Branch terms types patches <$> traverse f children

type LocalMetadataSet = MetadataSetFormat' LocalTextId LocalDefnId

type DbMetadataSet = MetadataSetFormat' TextId ObjectId

data MetadataSetFormat' t h = Inline (Set (Reference' t h))
  deriving (Show)

metadataSetFormatReferences_ ::
  (Ord t, Ord h, Ord h') =>
  Traversal (MetadataSetFormat' t h) (MetadataSetFormat' t h') (Reference' t h) (Reference' t h')
metadataSetFormatReferences_ f (Inline refs) = Inline <$> Set.traverse f refs

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
