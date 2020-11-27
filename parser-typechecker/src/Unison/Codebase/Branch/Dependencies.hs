{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Branch.Dependencies where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid.Generic
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Unison.Codebase.Branch (Branch (Branch), Branch0, EditHash)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import Unison.Codebase.Patch (Patch)
import Unison.NameSegment (NameSegment)
import Unison.Reference (Reference (DerivedId))
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Star3 as Star3

type Branches m = [(Branch.Hash, Maybe (m (Branch m)))]

data Dependencies = Dependencies
  { patches :: Set EditHash,
    terms :: Set Reference.Id,
    decls :: Set Reference.Id
  }
  deriving (Show)
  deriving (Generic)
  deriving (Semigroup) via GenericSemigroup Dependencies
  deriving (Monoid) via GenericMonoid Dependencies

data Dependencies' = Dependencies'
  { patches' :: [EditHash],
    terms' :: [Reference.Id],
    decls' :: [Reference.Id]
  }
  deriving (Show)

to' :: Dependencies -> Dependencies'
to' Dependencies {..} = Dependencies' (toList patches) (toList terms) (toList decls)

fromBranch :: Applicative m => Branch m -> (Branches m, Dependencies)
fromBranch (Branch c) = case c of
  Causal.One _hh e -> fromBranch0 e
  Causal.Cons _hh e (h, m) -> fromBranch0 e <> fromTails (Map.singleton h m)
  Causal.Merge _hh e tails -> fromBranch0 e <> fromTails tails
  where
    fromTails m = ([(h, Just (Branch <$> mc)) | (h, mc) <- Map.toList m], mempty)

fromRawCausal ::
  Causal.Raw Branch.Raw (Branches m, Dependencies) ->
  (Branches m, Dependencies)
fromRawCausal = \case
  Causal.RawOne e -> e
  Causal.RawCons e h -> e <> fromTails [h]
  Causal.RawMerge e hs -> e <> fromTails (toList hs)
  where
    fromTails ts = (fmap (,Nothing) ts, mempty)

fromBranch0 :: Applicative m => Branch0 m -> (Branches m, Dependencies)
fromBranch0 b =
  ( fromChildren (Branch._children b),
    fromTermsStar (Branch._terms b)
      <> fromTypesStar (Branch._types b)
      <> fromEdits (Branch._edits b)
  )
  where
    fromChildren :: Applicative m => Map NameSegment (Branch m) -> Branches m
    fromChildren m = [(Branch.headHash b, Just (pure b)) | b <- toList m]
    references :: Branch.Star r NameSegment -> [r]
    references = toList . R.dom . Star3.d1
    mdValues :: Branch.Star r NameSegment -> [Reference]
    mdValues = fmap snd . toList . R.ran . Star3.d3
    fromTermsStar :: Branch.Star Referent NameSegment -> Dependencies
    fromTermsStar s = Dependencies mempty terms decls
      where
        terms =
          Set.fromList $
            [i | Referent.Ref (DerivedId i) <- references s]
              ++ [i | DerivedId i <- mdValues s]
        decls =
          Set.fromList $
            [i | Referent.Con (DerivedId i) _ _ <- references s]
    fromTypesStar :: Branch.Star Reference NameSegment -> Dependencies
    fromTypesStar s = Dependencies mempty terms decls
      where
        terms = Set.fromList [i | DerivedId i <- mdValues s]
        decls = Set.fromList [i | DerivedId i <- references s]
    fromEdits :: Map NameSegment (EditHash, m Patch) -> Dependencies
    fromEdits m = Dependencies (Set.fromList . fmap fst $ toList m) mempty mempty
