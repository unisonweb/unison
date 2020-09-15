{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}

module Unison.Codebase.V1.DataDeclaration where


import qualified Data.Set                      as Set
import           Prelude                 hiding ( cycle )
import           Unison.Codebase.V1.Reference               ( Reference )
import           Unison.Codebase.V1.Type                    ( Type )
import qualified Unison.Codebase.V1.Type                   as Type
import qualified Unison.Codebase.V1.ConstructorType as CT
import Data.Text (Text)
import Data.Set (Set)

type Decl v a = Either (EffectDeclaration v a) (DataDeclaration v a)

data DeclOrBuiltin v a =
  Builtin CT.ConstructorType | Decl (Decl v a)

asDataDecl :: Decl v a -> DataDeclaration v a
asDataDecl = either toDataDecl id

declDependencies :: Ord v => Decl v a -> Set Reference
declDependencies = either (dependencies . toDataDecl) dependencies

constructorType :: Decl v a -> CT.ConstructorType
constructorType = \case
  Left{} -> CT.Effect
  Right{} -> CT.Data

data Modifier = Structural | Unique Text
  deriving (Eq, Ord, Show)

data DataDeclaration v a = DataDeclaration {
  modifier :: Modifier,
  annotation :: a,
  bound :: [v],
  constructors' :: [(a, v, Type v a)]
}

newtype EffectDeclaration v a = EffectDeclaration {
  toDataDecl :: DataDeclaration v a
}

withEffectDecl
  :: (DataDeclaration v a -> DataDeclaration v' a')
  -> (EffectDeclaration v a -> EffectDeclaration v' a')
withEffectDecl f e = EffectDeclaration (f . toDataDecl $ e)

withEffectDeclM :: Functor f
                => (DataDeclaration v a -> f (DataDeclaration v' a'))
                -> EffectDeclaration v a
                -> f (EffectDeclaration v' a')
withEffectDeclM f = fmap EffectDeclaration . f . toDataDecl

constructors :: DataDeclaration v a -> [(v, Type v a)]
constructors (DataDeclaration _ _ _ ctors) = [(v,t) | (_,v,t) <- ctors ]

constructorTypes :: DataDeclaration v a -> [Type v a]
constructorTypes = (snd <$>) . constructors

dependencies :: Ord v => DataDeclaration v a -> Set Reference
dependencies dd =
  Set.unions (Type.dependencies <$> constructorTypes dd)
