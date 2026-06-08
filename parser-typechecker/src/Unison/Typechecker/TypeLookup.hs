module Unison.Typechecker.TypeLookup where

import Data.Map qualified as Map
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration (DataDeclaration, EffectDeclaration)
import Unison.DataDeclaration qualified as DD
import Unison.Prelude
import Unison.Reference (TermReference, TypeReference)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.OpaqueDeclaration (OpaqueDeclaration)
import Unison.OpaqueDeclaration qualified as OpaqueDeclaration
import Unison.Type (Type)
import Unison.TypeAlias (TypeAlias)
import Unison.TypeAlias qualified as TypeAlias
import Unison.Var (Var)

-- Used for typechecking.
data TypeLookup v a = TypeLookup
  { typeOfTerms :: Map TermReference (Type v a),
    dataDecls :: Map TypeReference (DataDeclaration v a),
    effectDecls :: Map TypeReference (EffectDeclaration v a),
    typeAliases :: Map TypeReference (TypeAlias v a),
    opaqueDecls :: Map TypeReference (OpaqueDeclaration v a)
  }
  deriving (Show)

typeOfReferent :: TypeLookup v a -> Referent -> Maybe (Type v a)
typeOfReferent tl r = case r of
  Referent.Ref r -> typeOfTerm tl r
  Referent.Con r CT.Data -> typeOfDataConstructor tl r
  Referent.Con r CT.Effect -> typeOfEffectConstructor tl r

-- bombs if not found
unsafeConstructorType :: TypeLookup v a -> TypeReference -> CT.ConstructorType
unsafeConstructorType tl r =
  fromMaybe
    (error $ "no constructor type for " <> show r)
    (constructorType tl r)

constructorType :: TypeLookup v a -> TypeReference -> Maybe CT.ConstructorType
constructorType tl r =
  (const CT.Data <$> Map.lookup r (dataDecls tl))
    <|> (const CT.Effect <$> Map.lookup r (effectDecls tl))

typeOfDataConstructor :: TypeLookup v a -> ConstructorReference -> Maybe (Type v a)
typeOfDataConstructor tl (ConstructorReference r cid) = go =<< Map.lookup r (dataDecls tl)
  where
    go dd = DD.typeOfConstructor dd cid

typeOfEffectConstructor :: TypeLookup v a -> ConstructorReference -> Maybe (Type v a)
typeOfEffectConstructor tl (ConstructorReference r cid) = go =<< Map.lookup r (effectDecls tl)
  where
    go dd = DD.typeOfConstructor (DD.toDataDecl dd) cid

typeOfTerm :: TypeLookup v a -> TermReference -> Maybe (Type v a)
typeOfTerm tl r = Map.lookup r (typeOfTerms tl)

typeOfTerm' :: TypeLookup v a -> TermReference -> Either TermReference (Type v a)
typeOfTerm' tl r = case Map.lookup r (typeOfTerms tl) of
  Nothing -> Left r
  Just a -> Right a

instance Semigroup (TypeLookup v a) where
  TypeLookup a b c d e <> TypeLookup a2 b2 c2 d2 e2 =
    TypeLookup (a <> a2) (b <> b2) (c <> c2) (d <> d2) (e <> e2)

instance Monoid (TypeLookup v a) where
  mempty = TypeLookup mempty mempty mempty mempty mempty

-- TypeAlias.amap requires @Ord v@, so this is a named function rather
-- than a Functor instance.
amap :: (Var v) => (a -> a') -> TypeLookup v a -> TypeLookup v a'
amap f tl =
  TypeLookup
    (fmap f <$> typeOfTerms tl)
    (fmap f <$> dataDecls tl)
    (fmap f <$> effectDecls tl)
    (TypeAlias.amap f <$> typeAliases tl)
    (OpaqueDeclaration.amap f <$> opaqueDecls tl)
