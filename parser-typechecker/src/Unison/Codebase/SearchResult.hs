{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.SearchResult where

import qualified Data.Set as Set
import Unison.HashQualified' (HashQualified)
import qualified Unison.HashQualified' as HQ
import Unison.Name (Name)
import Unison.Names2 (Names' (Names), Names0)
import qualified Unison.Names2 as Names
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Util.Relation as R

-- this Ord instance causes types < terms
data SearchResult = Tp TypeResult | Tm TermResult deriving (Eq, Ord, Show)

data TermResult = TermResult
  { termName :: HashQualified,
    referent :: Referent,
    termAliases :: Set HashQualified
  }
  deriving (Eq, Ord, Show)

data TypeResult = TypeResult
  { typeName :: HashQualified,
    reference :: Reference,
    typeAliases :: Set HashQualified
  }
  deriving (Eq, Ord, Show)

pattern Tm' hq r as = Tm (TermResult hq r as)

pattern Tp' hq r as = Tp (TypeResult hq r as)

termResult :: HashQualified -> Referent -> Set HashQualified -> SearchResult
termResult hq r as = Tm (TermResult hq r as)

termSearchResult :: Names0 -> Name -> Referent -> SearchResult
termSearchResult b n r =
  termResult (Names._hqTermName b n r) r (Names._hqTermAliases b n r)

typeResult :: HashQualified -> Reference -> Set HashQualified -> SearchResult
typeResult hq r as = Tp (TypeResult hq r as)

typeSearchResult :: Names0 -> Name -> Reference -> SearchResult
typeSearchResult b n r =
  typeResult (Names._hqTypeName b n r) r (Names._hqTypeAliases b n r)

name :: SearchResult -> HashQualified
name = \case
  Tm t -> termName t
  Tp t -> typeName t

aliases :: SearchResult -> Set HashQualified
aliases = \case
  Tm t -> termAliases t
  Tp t -> typeAliases t

-- | TypeResults yield a `Referent.Ref`
toReferent :: SearchResult -> Referent
toReferent (Tm (TermResult _ r _)) = r
toReferent (Tp (TypeResult _ r _)) = Referent.Ref r

truncateAliases :: Int -> SearchResult -> SearchResult
truncateAliases n = \case
  Tm (TermResult hq r as) -> termResult hq r (Set.map (HQ.take n) as)
  Tp (TypeResult hq r as) -> typeResult hq r (Set.map (HQ.take n) as)

-- | You may want to sort this list differently afterward.
fromNames :: Names0 -> [SearchResult]
fromNames b =
  map (uncurry (typeSearchResult b)) (R.toList . Names.types $ b)
    <> map (uncurry (termSearchResult b)) (R.toList . Names.terms $ b)

_fromNames :: Names0 -> [SearchResult]
_fromNames n0@(Names terms types) = typeResults <> termResults
  where
    typeResults =
      [ typeResult (Names._hqTypeName n0 name r) r (Names._hqTypeAliases n0 name r)
        | (name, r) <- R.toList types
      ]
    termResults =
      [ termResult (Names._hqTermName n0 name r) r (Names._hqTermAliases n0 name r)
        | (name, r) <- R.toList terms
      ]
