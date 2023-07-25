{-# LANGUAGE PatternSynonyms #-}

module Unison.Server.SearchResult' where

import Data.Set qualified as Set
import Unison.Codebase.Editor.DisplayObject (DisplayObject)
import Unison.Codebase.Editor.DisplayObject qualified as DT
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DD
import Unison.HashQualified qualified as HQ
import Unison.HashQualified' qualified as HQ'
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Type (Type)
import Unison.Type qualified as Type

data SearchResult' v a
  = Tm' (TermResult' v a)
  | Tp' (TypeResult' v a)
  deriving (Eq, Show)

data TermResult' v a
  = TermResult'
      (HQ.HashQualified Name)
      (Maybe (Type v a))
      Referent
      (Set (HQ'.HashQualified Name))
  deriving (Eq, Show)

data TypeResult' v a
  = TypeResult'
      (HQ.HashQualified Name)
      (DisplayObject () (Decl v a))
      Reference
      (Set (HQ'.HashQualified Name))
  deriving (Eq, Show)

pattern Tm ::
  HQ.HashQualified Name ->
  Maybe (Type v a) ->
  Referent ->
  Set (HQ'.HashQualified Name) ->
  SearchResult' v a
pattern Tm n t r as = Tm' (TermResult' n t r as)

pattern Tp ::
  HQ.HashQualified Name ->
  DisplayObject () (Decl v a) ->
  Reference ->
  Set (HQ'.HashQualified Name) ->
  SearchResult' v a
pattern Tp n t r as = Tp' (TypeResult' n t r as)

tmReferent :: SearchResult' v a -> Maybe Referent
tmReferent = \case Tm _ _ r _ -> Just r; _ -> Nothing

tpReference :: SearchResult' v a -> Maybe Reference
tpReference = \case Tp _ _ r _ -> Just r; _ -> Nothing

foldResult' :: (TermResult' v a -> b) -> (TypeResult' v a -> b) -> SearchResult' v a -> b
foldResult' f g = \case
  Tm' tm -> f tm
  Tp' tp -> g tp

-- todo: comment me out, is this actually useful, given what we saw in ShowDefinitionI?
-- namely, that it doesn't include the Term's deps, just the Decl's and the
-- result Term/Type names.
labeledDependencies :: (Ord v) => SearchResult' v a -> Set LabeledDependency
labeledDependencies = \case
  Tm' (TermResult' _ t r _) ->
    Set.insert (LD.referent r) $ maybe mempty (Set.map LD.typeRef . Type.dependencies) t
  Tp' (TypeResult' _ d r _) ->
    maybe mempty (DD.labeledDeclDependenciesIncludingSelf r) (DT.toMaybe d)
