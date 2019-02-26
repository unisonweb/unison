module Unison.Codebase.SearchResult where

-- import           Data.List            (sortOn)
-- import           Data.Map             (Map)
import           Data.Set             (Set)
import           Unison.HashQualified (HashQualified)
import           Unison.Reference     (Reference)
import           Unison.Referent      (Referent)
import qualified Unison.Term          as Term
import qualified Unison.Type          as Type

type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a

data SearchResult v a = SearchResult
  { termResults :: [TermResult v a]
  , typeResults :: [TypeResult]
  }

data SearchResult' v a score = SearchResult'
  { termResults' :: [(score, TermResult v a)]
  , typeResults' :: [(score, TypeResult)]
  } deriving (Eq, Show) -- ABT.Term lacks Ord instance

data TermResult v a = TermResult
  { termName    :: HashQualified
  , referent    :: Referent
  , termType    :: Maybe (Type v a)
  , termAliases :: Set HashQualified
  } deriving (Eq, Show)

data TypeResult = TypeResult
  { typeName    :: HashQualified
  , reference   :: Reference
  , typeAliases :: [HashQualified]
  } deriving (Eq, Show)

-- May eventually want a version that includes the term or type,
-- but think about which fields this new case actually needs.

instance Semigroup (SearchResult v a) where
  left <> right = SearchResult (termResults left <> termResults right)
                               (typeResults left <> typeResults right)
instance Monoid (SearchResult v a) where
  mempty = SearchResult [] []
  mappend = (<>)


-- note: don't use Down
-- instance Ord score => Semigroup (SearchResult' v a score) where
--   left <> right = SearchResult'
--     (sortOn (Down . fst) $ termResults left <> termResults right)
--     (sortOn (Down . fst) $ typeResults left <> typeResults right)
-- instance Ord score => Monoid (SearchResult' v a score) where
--   mempty = SearchResult' [] []
--   mappend = (<>)

data SearchResult0' score = SearchResult0'
  { termResults0' :: [(score, TermResult0)]
  , typeResults0' :: [(score, TypeResult)]
  -- , byReferent0  :: Map Referent TermResult0
  -- , byReference0 :: Map Reference TypeResult
  }

data SearchResult0 = SearchResult0
  { termResults0 :: [TermResult0]
  , typeResults0 :: [TypeResult]
  -- , byReferent0  :: Map Referent TermResult0
  -- , byReference0 :: Map Reference TypeResult
  }

data TermResult0 = TermResult0
  { termName0    :: HashQualified
  , referent0    :: Referent
  , termAliases0 :: Set HashQualified
  }

-- instance Ord score => Semigroup (SearchResult0' score) where
--   left <> right = SearchResult0
--     (sortOn (Down . fst) $ termResults0 left <> termResults0 right)
--     (sortOn (Down . fst) $ typeResults0 left <> typeResults0 right)
--     -- (byReferent0 left <> byReferent0 right)
--     -- (byReference0 left <> byReference0 right)
-- instance Ord score => Monoid (SearchResult0' score) where
--   mempty = SearchResult0 mempty mempty -- mempty mempty
--   mappend = (<>)

instance Semigroup SearchResult0 where
  left <> right = SearchResult0
    (termResults0 left <> termResults0 right)
    (typeResults0 left <> typeResults0 right)

instance Monoid SearchResult0 where
  mempty = SearchResult0 mempty mempty -- mempty mempty
  mappend = (<>)
