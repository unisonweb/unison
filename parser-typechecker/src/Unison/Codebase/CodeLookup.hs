module Unison.Codebase.CodeLookup where

import Control.Applicative
import qualified Data.Map                      as Map
import           Unison.UnisonFile              ( UnisonFile )
import qualified Unison.UnisonFile              as UF
import qualified Unison.Term                    as Term
import           Unison.Term                    ( AnnotatedTerm )
import           Unison.Var                     ( Var )
import qualified Unison.Reference as Reference
import qualified Unison.Typechecker.TypeLookup as TL

type Decl v a = TL.Decl v a

fromUnisonFile :: (Var v, Monad m) => UnisonFile v a -> CodeLookup m v a
fromUnisonFile uf = CodeLookup tm ty where
  tm id = pure $ Map.lookup id termMap
  ty id = pure $ Map.lookup id typeMap1 <|> Map.lookup id typeMap2
  typeMap1 = Map.fromList [ (id, Right dd) |
                            (_, (Reference.DerivedId id, dd)) <-
                            Map.toList (UF.dataDeclarations uf) ]
  typeMap2 = Map.fromList [ (id, Left ad) |
                            (_, (Reference.DerivedId id, ad)) <-
                            Map.toList (UF.effectDeclarations uf) ]
  tmm = Map.fromList (UF.terms uf)
  termMap = Map.fromList [ (id, e) |
                            (_, (Reference.DerivedId id, e)) <-
                            Map.toList (Term.hashComponents tmm) ]

data CodeLookup m v a
  = CodeLookup {
      getTerm :: Reference.Id -> m (Maybe (AnnotatedTerm v a)),
      getTypeDeclaration :: Reference.Id -> m (Maybe (Decl v a))
   }

instance (Ord v, Functor m) => Functor (CodeLookup m v) where
  fmap f cl = CodeLookup tm ty where
    tm id = fmap (Term.amap f) <$> getTerm cl id
    ty id = fmap md <$> getTypeDeclaration cl id
    md (Left e) = Left (f <$> e)
    md (Right d) = Right (f <$> d)

instance Monad m => Semigroup (CodeLookup m v a) where
  (<>) = mappend

instance Monad m => Monoid (CodeLookup m v a) where
  mempty = CodeLookup (const $ pure Nothing) (const $ pure Nothing)
  c1 `mappend` c2 = CodeLookup tm ty where
    tm id = do
      o <- getTerm c1 id
      case o of Nothing -> getTerm c2 id; Just _ -> pure o
    ty id = do
      o <- getTypeDeclaration c1 id
      case o of Nothing -> getTypeDeclaration c2 id; Just _ -> pure o
