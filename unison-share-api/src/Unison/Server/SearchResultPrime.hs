{-# LANGUAGE PatternSynonyms #-}

module Unison.Server.SearchResultPrime where

import Unison.Codebase.Editor.DisplayObject (DisplayObject)
import Unison.DataDeclaration (Decl)
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Type (Type)

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

foldResult' :: (TermResult' v a -> b) -> (TypeResult' v a -> b) -> SearchResult' v a -> b
foldResult' f g = \case
  Tm' tm -> f tm
  Tp' tp -> g tp
