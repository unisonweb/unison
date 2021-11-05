module U.Codebase.Sqlite.Patch.TermEdit where

import Data.Bifoldable (Bifoldable (bifoldMap))
import Data.Bitraversable (Bitraversable (bitraverse))
import U.Codebase.Reference (Reference')
import qualified U.Codebase.Referent as Referent
import qualified U.Codebase.Sqlite.DbId as Db
import U.Codebase.Sqlite.LocalIds (LocalDefnId, LocalTextId)
import Control.Lens
import qualified U.Codebase.Reference as Reference

type TermEdit = TermEdit' Db.TextId Db.ObjectId

type LocalTermEdit = TermEdit' LocalTextId LocalDefnId

type Referent' t h = Referent.Referent' (Reference' t h) (Reference' t h)

data TermEdit' t h = Replace (Referent' t h) Typing | Deprecate
  deriving (Eq, Ord, Show)

_Replace :: Prism (TermEdit' t h) (TermEdit' t' h') (Referent' t h, Typing) (Referent' t' h', Typing)
_Replace = prism embed project
  where
    project :: TermEdit' t h -> Either (TermEdit' t' h') (Referent' t h, Typing)
    project (Replace ref typ) = Right (ref, typ)
    project Deprecate = Left Deprecate

    embed :: (Referent' t' h', Typing) -> TermEdit' t' h'
    embed (ref, typ) = Replace ref typ

h_ :: Traversal (TermEdit' t h) (TermEdit' t h') h h'
h_ f = _Replace . _1 . Referent.refs_ . Reference.h_ %%~ f

-- Replacements with the Same type can be automatically propagated.
-- Replacements with a Subtype can be automatically propagated but may result in dependents getting more general types, so requires re-inference.
-- Replacements of a Different type need to be manually propagated by the programmer.
data Typing = Same | Subtype | Different
  deriving (Eq, Ord, Show)

instance Bifunctor TermEdit' where
  bimap f g (Replace r t) = Replace (bimap (bimap f g) (bimap f g) r) t
  bimap _ _ Deprecate = Deprecate

instance Bifoldable TermEdit' where
  bifoldMap f g (Replace r _t) = bifoldMap (bifoldMap f g) (bifoldMap f g) r
  bifoldMap _ _ Deprecate = mempty

instance Bitraversable TermEdit' where
  bitraverse f g (Replace r t) = Replace <$> bitraverse (bitraverse f g) (bitraverse f g) r <*> pure t
  bitraverse _ _ Deprecate = pure Deprecate
