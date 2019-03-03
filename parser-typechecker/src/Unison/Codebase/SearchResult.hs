{-# LANGUAGE LambdaCase #-}

module Unison.Codebase.SearchResult where

import           Data.Set             (Set)
import           Unison.HashQualified (HashQualified)
import           Unison.Reference     (Reference)
import           Unison.Referent      (Referent)

data SearchResult = Tm TermResult | Tp TypeResult deriving (Eq, Ord, Show)

data TermResult = TermResult
  { termName    :: HashQualified
  , referent    :: Referent
  , termAliases :: Set HashQualified
  } deriving (Eq, Ord, Show)

data TypeResult = TypeResult
  { typeName    :: HashQualified
  , reference   :: Reference
  , typeAliases :: Set HashQualified
  } deriving (Eq, Ord, Show)

termResult :: HashQualified -> Referent -> Set HashQualified -> SearchResult
termResult hq r as = Tm (TermResult hq r as)

typeResult :: HashQualified -> Reference -> Set HashQualified -> SearchResult
typeResult hq r as = Tp (TypeResult hq r as)

name :: SearchResult -> HashQualified
name = \case
  Tm t -> termName t
  Tp t -> typeName t

aliases :: SearchResult -> Set HashQualified
aliases = \case
  Tm t -> termAliases t
  Tp t -> typeAliases t
