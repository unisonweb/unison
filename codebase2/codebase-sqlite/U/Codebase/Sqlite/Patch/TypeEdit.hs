module U.Codebase.Sqlite.Patch.TypeEdit where

import Data.Bifoldable (Bifoldable (bifoldMap))
import Data.Bitraversable (Bitraversable (bitraverse))
import U.Codebase.Reference (Reference')
import qualified U.Codebase.Sqlite.DbId as Db
import U.Codebase.Sqlite.LocalIds (LocalDefnId, LocalTextId)
import Control.Lens
import qualified U.Codebase.Reference as Reference

type LocalTypeEdit = TypeEdit' LocalTextId LocalDefnId

type TypeEdit = TypeEdit' Db.TextId Db.ObjectId

data TypeEdit' t h = Replace (Reference' t h) | Deprecate
  deriving (Eq, Ord, Show)

_Replace :: Prism (TypeEdit' t h) (TypeEdit' t' h') (Reference' t h) (Reference' t' h')
_Replace = prism Replace project
  where
    project :: TypeEdit' t h -> Either (TypeEdit' t' h') (Reference' t h)
    project (Replace ref) = Right ref
    project Deprecate = Left Deprecate

h_ :: Traversal (TypeEdit' t h) (TypeEdit' t h') h h'
h_ = _Replace . Reference.h_

instance Bifunctor TypeEdit' where
  bimap f g (Replace r) = Replace (bimap f g r)
  bimap _ _ Deprecate = Deprecate

instance Bifoldable TypeEdit' where
  bifoldMap f g (Replace r) = bifoldMap f g r
  bifoldMap _ _ Deprecate = mempty

instance Bitraversable TypeEdit' where
  bitraverse f g (Replace r) = Replace <$> bitraverse f g r
  bitraverse _ _ Deprecate = pure Deprecate
