module Unison.Server.SearchResult where

import qualified Data.Set as Set
import Unison.HashQualified (HashQualified)
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import Unison.Names (Names (..))
import qualified Unison.Names as Names
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Util.Relation as R

data SearchResult = Tp TypeResult | Tm TermResult deriving (Eq, Ord)

data TermResult = TermResult
  { termName :: HashQualified Name,
    referent :: Referent,
    termAliases :: Set (HQ'.HashQualified Name)
  }
  deriving (Eq, Ord)

data TypeResult = TypeResult
  { typeName :: HashQualified Name,
    reference :: Reference,
    typeAliases :: Set (HQ'.HashQualified Name)
  }
  deriving (Eq, Ord)

pattern Tm' :: HashQualified Name -> Referent -> Set (HQ'.HashQualified Name) -> SearchResult
pattern Tm' hq r as = Tm (TermResult hq r as)

pattern Tp' :: HashQualified Name -> Reference -> Set (HQ'.HashQualified Name) -> SearchResult
pattern Tp' hq r as = Tp (TypeResult hq r as)

-- | Construct a term search result from a primary name, referent, and set of aliases.
termResult ::
  HashQualified Name -> Referent -> Set (HQ'.HashQualified Name) -> SearchResult
termResult hq r as = Tm (TermResult hq r as)

termSearchResult :: Names -> Name -> Referent -> SearchResult
termSearchResult b n r =
  termResult (HQ'.toHQ (Names._hqTermName b n r)) r (Names._hqTermAliases b n r)

-- | Construct a type search result from a primary name, reference, and set of aliases.
typeResult ::
  HashQualified Name -> Reference -> Set (HQ'.HashQualified Name) -> SearchResult
typeResult hq r as = Tp (TypeResult hq r as)

typeSearchResult :: Names -> Name -> Reference -> SearchResult
typeSearchResult b n r =
  typeResult (HQ'.toHQ (Names._hqTypeName b n r)) r (Names._hqTypeAliases b n r)

name :: SearchResult -> HashQualified Name
name = \case
  Tm t -> termName t
  Tp t -> typeName t

aliases :: SearchResult -> Set (HQ'.HashQualified Name)
aliases = \case
  Tm t -> termAliases t
  Tp t -> typeAliases t

-- | TypeResults yield a `Referent.Ref`
toReferent :: SearchResult -> Referent
toReferent (Tm (TermResult _ r _)) = r
toReferent (Tp (TypeResult _ r _)) = Referent.Ref r

truncateAliases :: Int -> SearchResult -> SearchResult
truncateAliases n = \case
  Tm (TermResult hq r as) -> termResult hq r (Set.map (HQ'.take n) as)
  Tp (TypeResult hq r as) -> typeResult hq r (Set.map (HQ'.take n) as)

-- | You may want to sort this list differently afterward.
fromNames :: Names -> [SearchResult]
fromNames b =
  map (uncurry (typeSearchResult b)) (R.toList . Names.types $ b)
    <> map (uncurry (termSearchResult b)) (R.toList . Names.terms $ b)

_fromNames :: Names -> [SearchResult]
_fromNames n0@(Names terms types) = typeResults <> termResults
  where
    typeResults =
      [ typeSearchResult n0 name r
        | (name, r) <- R.toList types
      ]
    termResults =
      [ termSearchResult n0 name r
        | (name, r) <- R.toList terms
      ]
