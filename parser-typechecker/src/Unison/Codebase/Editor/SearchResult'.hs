{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.Editor.SearchResult' where

import Unison.Referent (Referent)
import Unison.Reference (Reference)
import qualified Unison.HashQualified' as HQ'
import qualified Data.Set as Set
import qualified Unison.DataDeclaration as DD
import qualified Unison.Codebase.Editor.DisplayThing as DT
import qualified Unison.Type as Type
import Data.Set (Set)
import Unison.DataDeclaration (Decl)
import Unison.Codebase.Editor.DisplayThing (DisplayThing)
import Unison.Type (Type)

data SearchResult' v a
  = Tm' (TermResult' v a)
  | Tp' (TypeResult' v a)
  deriving (Eq, Show)
data TermResult' v a =
  TermResult' HQ'.HashQualified (Maybe (Type v a)) Referent (Set HQ'.HashQualified)
  deriving (Eq, Show)
data TypeResult' v a =
  TypeResult' HQ'.HashQualified (DisplayThing (Decl v a)) Reference (Set HQ'.HashQualified)
  deriving (Eq, Show)

pattern Tm n t r as = Tm' (TermResult' n t r as)
pattern Tp n t r as = Tp' (TypeResult' n t r as)

tmReferent :: SearchResult' v a -> Maybe Referent
tmReferent = \case; Tm _ _ r _ -> Just r; _ -> Nothing
tpReference :: SearchResult' v a -> Maybe Reference
tpReference = \case; Tp _ _ r _ -> Just r; _ -> Nothing

foldResult' :: (TermResult' v a -> b) -> (TypeResult' v a -> b) -> SearchResult' v a -> b
foldResult' f g = \case
  Tm' tm -> f tm
  Tp' tp -> g tp

labeledDependencies :: Ord v => SearchResult' v a -> Set (Either Reference Referent)
labeledDependencies = \case
  Tm' (TermResult' _ t r _) ->
    Set.insert (Right r) $ maybe mempty (Set.map Left . Type.dependencies) t
  Tp' (TypeResult' _ d r _) ->
    Set.map Left . Set.insert r $ maybe mempty (DD.declDependencies) (DT.toMaybe d)
