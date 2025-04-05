{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.SqliteCodebase.Branch.Dependencies where

import Data.Map qualified as Map
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.Set qualified as Set
import U.Codebase.HashTags (CausalHash, PatchHash)
import Unison.Codebase.Branch.Type as Branch
import Unison.Codebase.Causal qualified as Causal
import Unison.Codebase.Patch (Patch)
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Hash (Hash)
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Reference (Reference, pattern Derived)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Util.Relation qualified as R
import Unison.Util.Star2 qualified as Star2

type Branches m = [(CausalHash, m (Branch m))]

data Dependencies = Dependencies
  { patches :: Set PatchHash,
    terms :: Set Hash,
    decls :: Set Hash
  }
  deriving (Show)
  deriving (Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid Dependencies

data Dependencies' = Dependencies'
  { patches' :: [PatchHash],
    terms' :: [Hash],
    decls' :: [Hash]
  }
  deriving (Eq, Show)
  deriving (Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid Dependencies'

to' :: Dependencies -> Dependencies'
to' Dependencies {..} = Dependencies' (toList patches) (toList terms) (toList decls)

fromBranch :: (Applicative m) => Branch m -> (Branches m, Dependencies)
fromBranch (Branch c) = case c of
  Causal.One _hh _eh e -> fromBranch0 e
  Causal.Cons _hh _eh e (h, m) -> fromBranch0 e <> fromTails (Map.singleton h m)
  Causal.Merge _hh _eh e tails -> fromBranch0 e <> fromTails tails
  where
    fromTails m = ([(h, Branch <$> mc) | (h, mc) <- Map.toList m], mempty)

fromBranch0 :: (Applicative m) => Branch0 m -> (Branches m, Dependencies)
fromBranch0 b =
  ( fromChildren (b ^. Branch.children),
    fromTermsStar (b ^. Branch.terms)
      <> fromTypesStar (b ^. Branch.types)
      <> fromEdits (b ^. Branch.edits)
  )
  where
    fromChildren :: (Applicative m) => Map NameSegment (Branch m) -> Branches m
    fromChildren m = [(Branch.headHash b, pure b) | b <- toList m]
    references :: Branch.Star r NameSegment -> [r]
    references = toList . R.dom . Star2.d1
    mdValues :: Branch.Star r NameSegment -> [Reference]
    mdValues = toList . R.ran . Star2.d2
    fromTermsStar :: Branch.Star Referent NameSegment -> Dependencies
    fromTermsStar s = Dependencies mempty terms decls
      where
        terms =
          Set.fromList $
            [h | Referent.Ref (Derived h _) <- references s]
              ++ [h | (Derived h _) <- mdValues s]
        decls =
          Set.fromList $
            [h | Referent.Con (ConstructorReference (Derived h _i) _) _ <- references s]
    fromTypesStar :: Branch.Star Reference NameSegment -> Dependencies
    fromTypesStar s = Dependencies mempty terms decls
      where
        terms = Set.fromList [h | (Derived h _) <- mdValues s]
        decls = Set.fromList [h | (Derived h _) <- references s]
    fromEdits :: Map NameSegment (PatchHash, m Patch) -> Dependencies
    fromEdits m = Dependencies (Set.fromList . fmap fst $ toList m) mempty mempty
