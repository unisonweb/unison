{-# LANGUAGE RecordWildCards #-}

module U.Codebase.Sqlite.Branch.Full where

import Control.Lens
import Data.Bitraversable
import Data.Map.Strict qualified as Map
import U.Codebase.HashTags
import U.Codebase.Reference (Reference', TermReference', TypeReference')
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent')
import U.Codebase.Sqlite.DbId (BranchObjectId, CausalHashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalBranchChildId, LocalDefnId, LocalPatchObjectId, LocalTextId)
import Unison.Prelude
import Unison.Util.Map qualified as Map
import Unison.Util.Set qualified as Set

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

type DbBranchV3 = GBranchV3 TextId ObjectId (BranchObjectId, CausalHashId)

type HashBranch = Branch' Text ComponentHash PatchHash (BranchHash, CausalHash)

type Referent'' t h = Referent' (TermReference' t h) (TypeReference' t h)

data Branch' t h p c = Branch
  { terms :: !(Map t (Map (Referent'' t h) (MetadataSetFormat' t h))),
    types :: !(Map t (Map (TypeReference' t h) (MetadataSetFormat' t h))),
    patches :: !(Map t p),
    children :: !(Map t c)
  }
  deriving stock (Show, Generic)

-- | A V3 branch; see U.Codebase.BranchV3
data GBranchV3 t h c = BranchV3
  { children :: !(Map t c),
    terms :: !(Map t (Referent'' t h)),
    types :: !(Map t (TypeReference' t h))
  }
  deriving stock (Generic, Show)

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
quadmap ft fh fp fc branch =
  runIdentity $ quadmapM (Identity . ft) (Identity . fh) (Identity . fp) (Identity . fc) branch

quadmapM :: forall t h p c t' h' p' c' m. (Ord t', Ord h', Applicative m) => (t -> m t') -> (h -> m h') -> (p -> m p') -> (c -> m c') -> Branch' t h p c -> m (Branch' t' h' p' c')
quadmapM ft fh fp fc (Branch terms types patches children) =
  Branch
    <$> (Map.bitraverse ft doTerms terms)
    <*> (Map.bitraverse ft doTypes types)
    <*> (Map.bitraverse ft fp patches)
    <*> (Map.bitraverse ft fc children)
  where
    doTerms = Map.bitraverse (bitraverse (bitraverse ft fh) (bitraverse ft fh)) doMetadata
    doTypes = Map.bitraverse (bitraverse ft fh) doMetadata
    doMetadata (Inline s) = Inline <$> Set.traverse (bitraverse ft fh) s

-- | Traversal over text references in a branch
t_ :: (Ord t', Ord h) => Traversal (Branch' t h p c) (Branch' t' h p c) t t'
t_ f = quadmapM f pure pure pure

-- | Traversal over hash references in a branch
h_ :: (Ord t, Ord h') => Traversal (Branch' t h p c) (Branch' t h' p c) h h'
h_ f = quadmapM pure f pure pure

-- | Traversal over patch references in a branch
p_ :: (Ord t, Ord h) => Traversal (Branch' t h p c) (Branch' t h p' c) p p'
p_ f = quadmapM pure pure f pure

-- | Traversal over child references in a branch
c_ :: (Ord t, Ord h) => Traversal (Branch' t h p c) (Branch' t h p c') c c'
c_ f = quadmapM pure pure pure f
