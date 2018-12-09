module Unison.Typechecker.TypeLookup where

import Data.Map (Map)
import qualified Data.Map as Map
import Unison.Reference (Reference)
import Unison.Type (AnnotatedType)
import qualified Unison.DataDeclaration as DD
import qualified Unison.Referent as Referent
import Unison.Referent (Referent)

type Type v a = AnnotatedType v a
type DataDeclaration v a = DD.DataDeclaration' v a
type EffectDeclaration v a = DD.EffectDeclaration' v a
type Decl v a = Either (EffectDeclaration v a) (DataDeclaration v a)

-- Used for typechecking.
data TypeLookup v a =
  TypeLookup { typeOfTerms :: Map Reference (Type v a)
             , dataDecls :: Map Reference (DataDeclaration v a)
             , effectDecls :: Map Reference (EffectDeclaration v a) }
  deriving Show

typeOfReferent :: TypeLookup v a -> Referent -> Maybe (Type v a)
typeOfReferent tl r = case r of
  Referent.Ref r -> typeOfTerm tl r
  Referent.Con r cid -> typeOfDataConstructor tl r cid
  Referent.Req r cid -> typeOfEffectConstructor tl r cid

typeOfReferent' :: TypeLookup v a -> Referent -> Either Referent (Type v a)
typeOfReferent' tl r = case typeOfReferent tl r of
  Nothing -> Left r
  Just a -> Right a

typeOfDataConstructor :: TypeLookup v a -> Reference -> Int -> Maybe (Type v a)
typeOfDataConstructor tl r cid = go =<< Map.lookup r (dataDecls tl)
  where go dd = DD.typeOfConstructor dd cid

typeOfEffectConstructor :: TypeLookup v a -> Reference -> Int -> Maybe (Type v a)
typeOfEffectConstructor tl r cid = go =<< Map.lookup r (effectDecls tl)
  where go dd = DD.typeOfConstructor (DD.toDataDecl dd) cid

typeOfTerm :: TypeLookup v a -> Reference -> Maybe (Type v a)
typeOfTerm tl r = Map.lookup r (typeOfTerms tl)

typeOfTerm' :: TypeLookup v a -> Reference -> Either Reference (Type v a)
typeOfTerm' tl r = case Map.lookup r (typeOfTerms tl) of
  Nothing -> Left r
  Just a -> Right a

instance Semigroup (TypeLookup v a) where (<>) = mappend

instance Monoid (TypeLookup v a) where
  mempty = TypeLookup mempty mempty mempty
  mappend (TypeLookup a b c) (TypeLookup a2 b2 c2) =
    TypeLookup (a <> a2) (b <> b2) (c <> c2)

instance Functor (TypeLookup v) where
  fmap f tl =
    TypeLookup
      (fmap f <$> typeOfTerms tl)
      (fmap f <$> dataDecls tl)
      (fmap f <$> effectDecls tl)
