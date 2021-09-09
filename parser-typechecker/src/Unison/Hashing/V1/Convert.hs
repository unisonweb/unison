{-# LANGUAGE ViewPatterns #-}

module Unison.Hashing.V1.Convert where

import Control.Lens (over, _3)
import qualified Control.Lens as Lens
import Control.Monad.Validate (Validate)
import qualified Control.Monad.Validate as Validate
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Unison.ABT as ABT
import qualified Unison.DataDeclaration as Memory.DD
import Unison.Hash (Hash)
import qualified Unison.Hashing.V1.DataDeclaration as Hashing.DD
import qualified Unison.Hashing.V1.Reference as Hashing.Reference
import qualified Unison.Hashing.V1.Type as Hashing.Type
import qualified Unison.Names.ResolutionResult as Names
import qualified Unison.Reference as Memory.Reference
import qualified Unison.Referent as Memory.Referent
import qualified Unison.Type as Memory.Type
import Unison.Var (Var)

data ResolutionFailure v a
  = TermResolutionFailure v a (Set Memory.Referent.Referent)
  | TypeResolutionFailure v a (Set Memory.Reference.Reference)
  | CycleResolutionFailure Hash
  deriving (Eq, Ord, Show)

type ResolutionResult v a r = Validate (Seq (ResolutionFailure v a)) r

convertResolutionResult :: Names.ResolutionResult v a r -> ResolutionResult v a r
convertResolutionResult = \case
  Left e -> Validate.refute (fmap f e)
  Right a -> pure a
  where
    f = \case
      Names.TermResolutionFailure v a rs -> TermResolutionFailure v a rs
      Names.TypeResolutionFailure v a rs -> TypeResolutionFailure v a rs

hashDecls ::
  (Eq v, Var v) =>
  (Hash -> Maybe Hashing.Reference.Size) ->
  Map v (Memory.DD.DataDeclaration v a) ->
  ResolutionResult v a [(v, Memory.Reference.Id, Memory.DD.DataDeclaration v a)]
hashDecls f memDecls = do
  hashingDecls <- traverse m2hDecl memDecls
  hashingResult <- convertResolutionResult $ Hashing.DD.hashDecls hashingDecls
  pure $ map h2mDeclResult hashingResult
  where
    lookupHash :: Hash -> ResolutionResult v a Hashing.Reference.Size
    lookupHash h = case f h of
      Just size -> pure size
      Nothing -> Validate.refute . pure $ CycleResolutionFailure h

    m2hDecl :: Ord v => Memory.DD.DataDeclaration v a -> ResolutionResult v a (Hashing.DD.DataDeclaration v a)
    m2hDecl (Memory.DD.DataDeclaration mod ann bound ctors) =
      Hashing.DD.DataDeclaration (m2hModifier mod) ann bound
        <$> traverse (Lens.mapMOf _3 m2hType) ctors

    m2hType :: Ord v => Memory.Type.Type v a -> ResolutionResult v a (Hashing.Type.Type v a)
    m2hType = ABT.transformM \case
      Memory.Type.Ref ref -> Hashing.Type.Ref <$> m2hReference ref
      Memory.Type.Arrow a1 a1' -> pure $ Hashing.Type.Arrow a1 a1'
      Memory.Type.Ann a1 ki -> pure $ Hashing.Type.Ann a1 ki
      Memory.Type.App a1 a1' -> pure $ Hashing.Type.App a1 a1'
      Memory.Type.Effect a1 a1' -> pure $ Hashing.Type.Effect a1 a1'
      Memory.Type.Effects a1s -> pure $ Hashing.Type.Effects a1s
      Memory.Type.Forall a1 -> pure $ Hashing.Type.Forall a1
      Memory.Type.IntroOuter a1 -> pure $ Hashing.Type.IntroOuter a1

    m2hReference :: Memory.Reference.Reference -> ResolutionResult v a (Hashing.Reference.Reference)
    m2hReference = \case
      Memory.Reference.Builtin t -> pure $ Hashing.Reference.Builtin t
      Memory.Reference.DerivedId d -> Hashing.Reference.DerivedId <$> m2hReferenceId d

    m2hReferenceId :: Memory.Reference.Id -> ResolutionResult v a (Hashing.Reference.Id)
    m2hReferenceId (Memory.Reference.Id h i _n) = Hashing.Reference.Id h i <$> lookupHash h

h2mModifier :: Hashing.DD.Modifier -> Memory.DD.Modifier
h2mModifier = \case
  Hashing.DD.Structural -> Memory.DD.Structural
  Hashing.DD.Unique text -> Memory.DD.Unique text

m2hModifier :: Memory.DD.Modifier -> Hashing.DD.Modifier
m2hModifier = \case
  Memory.DD.Structural -> Hashing.DD.Structural
  Memory.DD.Unique text -> Hashing.DD.Unique text

h2mDeclResult :: Ord v => (v, Hashing.Reference.Id, Hashing.DD.DataDeclaration v a) -> (v, Memory.Reference.Id, Memory.DD.DataDeclaration v a)
h2mDeclResult (v, id, dd) = (v, h2mReferenceId id, h2mDecl dd)

h2mDecl :: Ord v => Hashing.DD.DataDeclaration v a -> Memory.DD.DataDeclaration v a
h2mDecl (Hashing.DD.DataDeclaration mod ann bound ctors) =
  Memory.DD.DataDeclaration (h2mModifier mod) ann bound (over _3 h2mType <$> ctors)

h2mType :: Ord v => Hashing.Type.Type v a -> Memory.Type.Type v a
h2mType = ABT.transform \case
  Hashing.Type.Ref ref -> Memory.Type.Ref (h2mReference ref)
  Hashing.Type.Arrow a1 a1' -> Memory.Type.Arrow a1 a1'
  Hashing.Type.Ann a1 ki -> Memory.Type.Ann a1 ki
  Hashing.Type.App a1 a1' -> Memory.Type.App a1 a1'
  Hashing.Type.Effect a1 a1' -> Memory.Type.Effect a1 a1'
  Hashing.Type.Effects a1s -> Memory.Type.Effects a1s
  Hashing.Type.Forall a1 -> Memory.Type.Forall a1
  Hashing.Type.IntroOuter a1 -> Memory.Type.IntroOuter a1

h2mReference :: Hashing.Reference.Reference -> Memory.Reference.Reference
h2mReference = \case
  Hashing.Reference.Builtin t -> Memory.Reference.Builtin t
  Hashing.Reference.DerivedId d -> Memory.Reference.DerivedId (h2mReferenceId d)

h2mReferenceId :: Hashing.Reference.Id -> Memory.Reference.Id
h2mReferenceId (Hashing.Reference.Id h i n) = Memory.Reference.Id h i n


