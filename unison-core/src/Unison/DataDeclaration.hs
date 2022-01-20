{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}

module Unison.DataDeclaration
  ( DataDeclaration (..),
    EffectDeclaration (..),
    Decl,
    DeclOrBuiltin(..),
    Modifier(..),
    allVars,
    asDataDecl,
    bindReferences,
    constructorNames,
    constructors,
    constructorType,
    constructorTypes,
    constructorVars,
    constructorIds,
    declConstructorReferents,
    declDependencies,
    declFields,
    dependencies,
    generateRecordAccessors,
    unhashComponent,
    mkDataDecl',
    mkEffectDecl',
    typeOfConstructor,
    withEffectDeclM,
    amap,
    updateDependencies,
  )
where

import Unison.Prelude

import Control.Lens (over, _3)
import Control.Monad.State (evalState)
import Data.Bifunctor (bimap, first, second)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Prelude.Extras (Show1)
import qualified Unison.ABT as ABT
import Unison.ConstructorReference (GConstructorReference(..))
import qualified Unison.ConstructorType as CT
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import qualified Unison.Name as Name
import qualified Unison.Names.ResolutionResult as Names
import qualified Unison.Pattern as Pattern
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Unison.Referent' as Referent'
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.Var.RefNamed as Var
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

withEffectDeclM :: Functor f
                => (DataDeclaration v a -> f (DataDeclaration v' a'))
                -> EffectDeclaration v a
                -> f (EffectDeclaration v' a')
withEffectDeclM f = fmap EffectDeclaration . f . toDataDecl

-- propose to move this code to some very feature-specific module —AI
generateRecordAccessors
  :: (Semigroup a, Var v)
  => [(v, a)]
  -> v
  -> Reference
  -> [(v, Term v a)]
generateRecordAccessors fields typename typ =
  join [ tm t i | (t, i) <- fields `zip` [(0::Int)..] ]
  where
  argname = Var.uncapitalize typename
  tm (fname, ann) i =
    [(Var.namespaced [typename, fname], get),
     (Var.namespaced [typename, fname, Var.named "set"], set),
     (Var.namespaced [typename, fname, Var.named "modify"], modify)]
    where
    -- example: `point -> case point of Point x _ -> x`
    get = Term.lam ann argname $ Term.match ann
      (Term.var ann argname)
      [Term.MatchCase pat Nothing rhs]
      where
      pat = Pattern.Constructor ann (ConstructorReference typ 0) cargs
      cargs = [ if j == i then Pattern.Var ann else Pattern.Unbound ann
              | (_, j) <- fields `zip` [0..]]
      rhs = ABT.abs' ann fname (Term.var ann fname)
    -- example: `x point -> case point of Point _ y -> Point x y`
    set = Term.lam' ann [fname', argname] $ Term.match ann
      (Term.var ann argname)
      [Term.MatchCase pat Nothing rhs]
      where
      fname' = Var.named . Var.name $
               Var.freshIn (Set.fromList $ [argname] <> (fst <$> fields)) fname
      pat = Pattern.Constructor ann (ConstructorReference typ 0) cargs
      cargs = [ if j == i then Pattern.Unbound ann else Pattern.Var ann
              | (_, j) <- fields `zip` [0..]]
      rhs = foldr (ABT.abs' ann) (Term.constructor ann (ConstructorReference typ 0) `Term.apps'` vargs)
                  [ f | ((f, _), j) <- fields `zip` [0..], j /= i ]
      vargs = [ if j == i then Term.var ann fname' else Term.var ann v
              | ((v, _), j) <- fields `zip` [0..]]
    -- example: `f point -> case point of Point x y -> Point (f x) y`
    modify = Term.lam' ann [fname', argname] $ Term.match ann
      (Term.var ann argname)
      [Term.MatchCase pat Nothing rhs]
      where
      fname' = Var.named . Var.name $
               Var.freshIn (Set.fromList $ [argname] <> (fst <$> fields))
                           (Var.named "f")
      pat = Pattern.Constructor ann (ConstructorReference typ 0) cargs
      cargs = replicate (length fields) $ Pattern.Var ann
      rhs = foldr (ABT.abs' ann) (Term.constructor ann (ConstructorReference typ 0) `Term.apps'` vargs)
                  (fst <$> fields)
      vargs = [ if j == i
                then Term.apps' (Term.var ann fname') [Term.var ann v]
                else Term.var ann v
              | ((v, _), j) <- fields `zip` [0..]]

constructorTypes :: DataDeclaration v a -> [Type v a]
constructorTypes = (snd <$>) . constructors

-- what is declFields? —AI
declFields :: Var v => Decl v a -> Either [Int] [Int]
declFields = bimap cf cf . first toDataDecl
  where
  cf = fmap fields . constructorTypes
  fields (Type.ForallsNamed' _ ty) = fields ty
  fields (Type.Arrows' spine) = length spine - 1
  fields _ = 0

typeOfConstructor :: DataDeclaration v a -> ConstructorId -> Maybe (Type v a)
typeOfConstructor dd i = constructorTypes dd `atMay` i

constructors :: DataDeclaration v a -> [(v, Type v a)]
constructors (DataDeclaration _ _ _ ctors) = [(v,t) | (_,v,t) <- ctors ]

constructorVars :: DataDeclaration v a -> [v]
constructorVars dd = fst <$> constructors dd

constructorNames :: Var v => DataDeclaration v a -> [Text]
constructorNames dd = Var.name <$> constructorVars dd

-- This function is unsound, since the `rid` and the `decl` have to match.
-- It should probably be hashed directly from the Decl, once we have a
-- reliable way of doing that. —AI
declConstructorReferents :: Reference.Id -> Decl v a -> [Referent.Id]
declConstructorReferents rid decl =
  [ Referent'.Con' (ConstructorReference rid i) ct | i <- constructorIds (asDataDecl decl) ]
  where ct = constructorType decl

constructorIds :: DataDeclaration v a -> [ConstructorId]
constructorIds dd = [0 .. length (constructors dd) - 1]

-- | All variables mentioned in the given data declaration.
-- Includes both term and type variables, both free and bound.
allVars :: Ord v => DataDeclaration v a -> Set v
allVars (DataDeclaration _ _ bound ctors) = Set.unions $
  Set.fromList bound : [ Set.insert v (Set.fromList $ ABT.allVars tp) | (_,v,tp) <- ctors ]

-- | All variables mentioned in the given declaration.
-- Includes both term and type variables, both free and bound.
allVars' :: Ord v => Decl v a -> Set v
allVars' = allVars . either toDataDecl id

bindReferences :: Var v
          => Set v
          -> Map Name.Name Reference
          -> DataDeclaration v a
          -> Names.ResolutionResult v a (DataDeclaration v a)
bindReferences keepFree names (DataDeclaration m a bound constructors) = do
  constructors <- for constructors $ \(a, v, ty) ->
    (a,v,) <$> Type.bindReferences keepFree names ty
  pure $ DataDeclaration m a bound constructors

dependencies :: Ord v => DataDeclaration v a -> Set Reference
dependencies dd =
  Set.unions (Type.dependencies <$> constructorTypes dd)

mkEffectDecl'
  :: Modifier -> a -> [v] -> [(a, v, Type v a)] -> EffectDeclaration v a
mkEffectDecl' m a b cs = EffectDeclaration (DataDeclaration m a b cs)

mkDataDecl'
  :: Modifier -> a -> [v] -> [(a, v, Type v a)] -> DataDeclaration v a
mkDataDecl' = DataDeclaration

data F a
  = Type (Type.F a)
  | LetRec [a] a
  | Constructors [a]
  | Modified Modifier a
  deriving (Functor, Foldable, Show, Show1)

updateDependencies :: Ord v => Map Reference Reference -> Decl v a -> Decl v a
updateDependencies typeUpdates decl = back $ dataDecl
  { constructors' = over _3 (Type.updateDependencies typeUpdates)
                      <$> constructors' dataDecl
  }
 where
  dataDecl = either toDataDecl id decl
  back     = either (const $ Left . EffectDeclaration) (const Right) decl


-- | This converts `Reference`s it finds that are in the input `Map`
-- back to free variables.
--
-- In the result map, any of the references inside the Decls which are keys of the input map;
-- have been replaced with the corresponding output `v`s in the output `Decl`s,
-- which are fresh with respect to all input Decls.
unhashComponent
  :: forall v a. Var v => Map Reference (Decl v a) -> Map Reference (v, Decl v a)
unhashComponent m
  = let
      usedVars :: Set v
      usedVars = foldMap allVars' m
      -- We assign fresh names to each reference/decl pair.
      -- We haven't modified the decls yet, but we will, further below.
      m' :: Map Reference (v, Decl v a)
      m' = evalState (Map.traverseWithKey assignVar m) usedVars where
        assignVar r d = (,d) <$> ABT.freshenS (Var.refNamed r)
      unhash1 :: ABT.Term Type.F v a -> ABT.Term Type.F v a
      unhash1  = ABT.rebuildUp' go
       where
        go e@(Type.Ref' r) = case Map.lookup r m' of
          Nothing -> e
          Just (v,_)  -> Type.var (ABT.annotation e) v
        go e = e
      unhash2 (Right dd@DataDeclaration{}) = Right $ unhash3 dd
      unhash2 (Left (EffectDeclaration dd)) =
        Left . EffectDeclaration $ unhash3 dd
      unhash3 dd@DataDeclaration {..} =
        dd { constructors' = fmap (over _3 unhash1) constructors' }
    in
      second unhash2 <$> m'

amap :: (a -> a2) -> Decl v a -> Decl v a2
amap f (Left e) = Left (f <$> e)
amap f (Right d) = Right (f <$> d)

