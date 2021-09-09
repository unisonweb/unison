{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}

module Unison.Hashing.V1.DataDeclaration
  ( DataDeclaration (..),
    EffectDeclaration (..),
    Decl,
    Modifier(..),
    asDataDecl,
    constructorType,
    constructorTypes,
    declConstructorReferents,
    declDependencies,
    dependencies,
  )
where

import Unison.Prelude


import qualified Data.Set as Set
import Prelude.Extras (Show1)
import Unison.Hashing.V1.Reference (Reference)
import qualified Unison.Hashing.V1.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.Hashing.V1.Type (Type)
import qualified Unison.Hashing.V1.Type as Type
import qualified Unison.ConstructorType as CT
import Unison.Hashable (Hashable1)
import qualified Unison.Hashable as Hashable
import qualified Unison.Referent' as Referent'
import Prelude hiding (cycle)

type Decl v a = Either (EffectDeclaration v a) (DataDeclaration v a)

data DeclOrBuiltin v a =
  Builtin CT.ConstructorType | Decl (Decl v a)
  deriving (Eq, Show)

asDataDecl :: Decl v a -> DataDeclaration v a
asDataDecl = either toDataDecl id

declDependencies :: Ord v => Decl v a -> Set Reference
declDependencies = either (dependencies . toDataDecl) dependencies

constructorType :: Decl v a -> CT.ConstructorType
constructorType = \case
  Left{} -> CT.Effect
  Right{} -> CT.Data

data Modifier = Structural | Unique Text --  | Opaque (Set Reference)
  deriving (Eq, Ord, Show)

data DataDeclaration v a = DataDeclaration {
  modifier :: Modifier,
  annotation :: a,
  bound :: [v],
  constructors' :: [(a, v, Type v a)]
} deriving (Eq, Show, Functor)

newtype EffectDeclaration v a = EffectDeclaration {
  toDataDecl :: DataDeclaration v a
} deriving (Eq,Show,Functor)

constructorTypes :: DataDeclaration v a -> [Type v a]
constructorTypes = (snd <$>) . constructors

constructors :: DataDeclaration v a -> [(v, Type v a)]
constructors (DataDeclaration _ _ _ ctors) = [(v,t) | (_,v,t) <- ctors ]

-- -- This function is unsound, since the `rid` and the `decl` have to match.
-- -- It should probably be hashed directly from the Decl, once we have a
-- -- reliable way of doing that. —AI
-- declConstructorReferents :: Reference.Id -> Decl v a -> [Referent.Id]
-- declConstructorReferents rid decl =
--   [ Referent'.Con' rid i ct | i <- constructorIds (asDataDecl decl) ]
--   where ct = constructorType decl

constructorIds :: DataDeclaration v a -> [Int]
constructorIds dd = [0 .. length (constructors dd) - 1]


dependencies :: Ord v => DataDeclaration v a -> Set Reference
dependencies dd =
  Set.unions (Type.dependencies <$> constructorTypes dd)

data F a
  = Type (Type.F a)
  | LetRec [a] a
  | Constructors [a]
  | Modified Modifier a
  deriving (Functor, Foldable, Show, Show1)

instance Hashable1 F where
  hash1 hashCycle hash e =
    let (tag, hashed) = (Hashable.Tag, Hashable.Hashed)
      -- Note: start each layer with leading `2` byte, to avoid collisions with
      -- terms, which start each layer with leading `1`. See `Hashable1 Term.F`
    in Hashable.accumulate $ tag 2 : case e of
      Type t -> [tag 0, hashed $ Hashable.hash1 hashCycle hash t]
      LetRec bindings body ->
        let (hashes, hash') = hashCycle bindings
        in [tag 1] ++ map hashed hashes ++ [hashed $ hash' body]
      Constructors cs ->
        let (hashes, _) = hashCycle cs
        in tag 2 :  map hashed hashes
      Modified m t ->
        [tag 3, Hashable.accumulateToken m, hashed $ hash t]

instance Hashable.Hashable Modifier where
  tokens Structural = [Hashable.Tag 0]
  tokens (Unique txt) = [Hashable.Tag 1, Hashable.Text txt]