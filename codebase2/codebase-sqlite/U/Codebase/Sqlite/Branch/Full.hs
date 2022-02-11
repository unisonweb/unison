{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module U.Codebase.Sqlite.Branch.Full where

import Data.Map (Map)
import Data.Set (Set)
import U.Codebase.Reference (Reference')
import U.Codebase.Referent (Referent')
import U.Codebase.Sqlite.DbId (BranchObjectId, CausalHashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalBranchChildId, LocalDefnId, LocalPatchObjectId, LocalTextId)
import qualified Unison.Util.Map as Map
import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.Set as Set

type LocalBranch = Branch' LocalTextId LocalDefnId LocalPatchObjectId LocalBranchChildId

type DbBranch = Branch' TextId ObjectId PatchObjectId (BranchObjectId, CausalHashId)

type Referent'' t h = Referent' (Reference' t h) (Reference' t h)

data Branch' t h p c = Branch
  { terms :: Map t (Map (Referent'' t h) (MetadataSetFormat' t h)),
    types :: Map t (Map (Reference' t h) (MetadataSetFormat' t h)),
    patches :: Map t p,
    children :: Map t c
  }
  deriving Show

type LocalMetadataSet = MetadataSetFormat' LocalTextId LocalDefnId

type DbMetadataSet = MetadataSetFormat' TextId ObjectId

data MetadataSetFormat' t h = Inline (Set (Reference' t h))
  deriving Show

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
