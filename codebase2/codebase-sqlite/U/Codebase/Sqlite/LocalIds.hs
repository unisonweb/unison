module U.Codebase.Sqlite.LocalIds where

import Data.Vector (Vector)
import U.Codebase.Sqlite.DbId
import Data.Bitraversable (Bitraversable(bitraverse))
import Data.Bifoldable (Bifoldable(bifoldMap))
import Data.Bifunctor (Bifunctor(bimap))

-- |A mapping between index ids that are local to an object and the ids in the database
data LocalIds' t h = LocalIds
  { textLookup :: Vector t,
    defnLookup :: Vector h
  }

type LocalIds = LocalIds' TextId ObjectId
type WatchLocalIds = LocalIds' TextId HashId

instance Bitraversable LocalIds' where
  bitraverse f g (LocalIds t d) = LocalIds <$> traverse f t <*> traverse g d
instance Bifoldable LocalIds' where
  bifoldMap f g (LocalIds t d) = foldMap f t <> foldMap g d
instance Bifunctor LocalIds' where
  bimap f g (LocalIds t d) = LocalIds (f <$> t) (g <$> d)
