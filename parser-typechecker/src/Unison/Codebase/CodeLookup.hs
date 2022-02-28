module Unison.Codebase.CodeLookup where

import Unison.Prelude

import Control.Monad.Morph (MFunctor (..))
import qualified Data.Set as Set
import Unison.DataDeclaration (Decl)
import qualified Unison.DataDeclaration as DD
import qualified Unison.Reference as Reference
import Unison.Term (Term)
import qualified Unison.Term as Term
import qualified Unison.Util.Set as Set
import Unison.Var (Var)

data CodeLookup v m a
  = CodeLookup {
      getTerm :: Reference.Id -> m (Maybe (Term v a)),
      getTypeDeclaration :: Reference.Id -> m (Maybe (Decl v a))
   }

instance MFunctor (CodeLookup v) where
  hoist f (CodeLookup tm tp) = CodeLookup (f . tm) (f . tp)

instance (Ord v, Functor m) => Functor (CodeLookup v m) where
  fmap f cl = CodeLookup tm ty where
    tm id = fmap (Term.amap f) <$> getTerm cl id
    ty id = fmap md <$> getTypeDeclaration cl id
    md (Left e) = Left (f <$> e)
    md (Right d) = Right (f <$> d)

instance Monad m => Semigroup (CodeLookup v m a) where
  (<>) = mappend

instance Monad m => Monoid (CodeLookup v m a) where
  mempty = CodeLookup (const $ pure Nothing) (const $ pure Nothing)
  c1 `mappend` c2 = CodeLookup tm ty where
    tm id = do
      o <- getTerm c1 id
      case o of Nothing -> getTerm c2 id; Just _ -> pure o
    ty id = do
      o <- getTypeDeclaration c1 id
      case o of Nothing -> getTypeDeclaration c2 id; Just _ -> pure o

-- todo: can this be implemented in terms of TransitiveClosure.transitiveClosure?
-- todo: add some tests on this guy?
transitiveDependencies
  :: (Monad m, Var v)
  => CodeLookup v m a
  -> Set Reference.Id
  -> Reference.Id
  -> m (Set Reference.Id)
transitiveDependencies code seen0 rid = if Set.member rid seen0
  then pure seen0
  else
    let seen = Set.insert rid seen0
        getIds = Set.mapMaybe Reference.toId
    in getTerm code rid >>= \case
      Just t ->
        foldM (transitiveDependencies code) seen (getIds $ Term.dependencies t)
      Nothing ->
        getTypeDeclaration code rid >>= \case
          Nothing        -> pure seen
          Just (Left ed) -> foldM (transitiveDependencies code)
                                  seen
                                  (getIds $ DD.dependencies (DD.toDataDecl ed))
          Just (Right dd) -> foldM (transitiveDependencies code)
                                   seen
                                   (getIds $ DD.dependencies dd)
