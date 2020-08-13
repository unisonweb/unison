{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}

module Unison.DataDeclaration where

import Unison.Prelude

import Control.Lens (_3, over)
import Control.Monad.State (evalState)

import Data.Bifunctor (first, second)
import qualified Unison.Util.Relation as Rel
import           Unison.Hash                    ( Hash )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Prelude                 hiding ( cycle )
import           Prelude.Extras                 ( Show1 )
import qualified Unison.ABT                    as ABT
import           Unison.Hashable                (Hashable,  Accumulate
                                                , Hashable1
                                                )
import qualified Unison.Hashable               as Hashable
import qualified Unison.Name                   as Name
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import qualified Unison.Reference.Util         as Reference.Util
import qualified Unison.Referent               as Referent
import qualified Unison.Term                   as Term
import           Unison.Term                    ( Term )
import           Unison.Type                    ( TypeR )
import qualified Unison.Type                   as Type
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import           Unison.Names3                 (Names0)
import qualified Unison.Names3                 as Names
import qualified Unison.Pattern                as Pattern
import qualified Unison.ConstructorType as CT

type ConstructorId = Term.ConstructorId

type Decl v a = Either (EffectDeclaration v a) (DataDeclaration v a)
type DeclR r v a = Either (EffectDeclarationR r v a) (DataDeclarationR r v a)

type DeclOrBuiltin v a = DeclOrBuiltinR Reference v a
data DeclOrBuiltinR r v a =
  Builtin CT.ConstructorType | Decl (DeclR r v a)
  deriving (Eq, Show)

asDataDecl :: DeclR r v a -> DataDeclarationR r v a
asDataDecl = either toDataDecl id

declDependencies :: (Ord r, Ord v) => DeclR r v a -> Set r
declDependencies = either (dependencies . toDataDecl) dependencies

constructorType :: DeclR r v a -> CT.ConstructorType
constructorType = \case
  Left{} -> CT.Effect
  Right{} -> CT.Data

data Modifier = Structural | Unique Text --  | Opaque (Set Reference)
  deriving (Eq, Ord, Show)

type DataDeclaration v a = DataDeclarationR Reference v a
data DataDeclarationR r v a = DataDeclaration {
  modifier :: Modifier,
  annotation :: a,
  bound :: [v],
  constructors' :: [(a, v, TypeR r v a)]
} deriving (Eq, Show, Functor)

type EffectDeclaration v a = EffectDeclarationR Reference v a
newtype EffectDeclarationR r v a = EffectDeclaration {
  toDataDecl :: DataDeclarationR r v a
} deriving (Eq, Show, Functor)

withEffectDecl
  :: (DataDeclarationR r v a -> DataDeclarationR r' v' a')
  -> (EffectDeclarationR r v a -> EffectDeclarationR r' v' a')
withEffectDecl f e = EffectDeclaration (f . toDataDecl $ e)

withEffectDeclM :: Functor f
                => (DataDeclarationR r v a -> f (DataDeclarationR r' v' a'))
                -> EffectDeclarationR r v a
                -> f (EffectDeclarationR r' v' a')
withEffectDeclM f = fmap EffectDeclaration . f . toDataDecl

-- todo: move this out of this module. it is an implementation detail of the
--       FileParser, and used in a corresponding check in DeclPrinter
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
      pat = Pattern.Constructor ann typ 0 cargs
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
      pat = Pattern.Constructor ann typ 0 cargs
      cargs = [ if j == i then Pattern.Unbound ann else Pattern.Var ann
              | (_, j) <- fields `zip` [0..]]
      rhs = foldr (ABT.abs' ann) (Term.constructor ann typ 0 `Term.apps'` vargs)
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
      pat = Pattern.Constructor ann typ 0 cargs
      cargs = replicate (length fields) $ Pattern.Var ann
      rhs = foldr (ABT.abs' ann) (Term.constructor ann typ 0 `Term.apps'` vargs)
                  (fst <$> fields)
      vargs = [ if j == i
                then Term.apps' (Term.var ann fname') [Term.var ann v]
                else Term.var ann v
              | ((v, _), j) <- fields `zip` [0..]]

constructorTypes :: DataDeclarationR r v a -> [TypeR r v a]
constructorTypes = (snd <$>) . constructors

typeOfConstructor :: DataDeclarationR r v a -> ConstructorId -> Maybe (TypeR r v a)
typeOfConstructor dd i = constructorTypes dd `atMay` i

constructors :: DataDeclarationR r v a -> [(v, TypeR r v a)]
constructors (DataDeclaration _ _ _ ctors) = [(v,t) | (_,v,t) <- ctors ]

constructorVars :: DataDeclarationR r v a -> [v]
constructorVars dd = fst <$> constructors dd

constructorNames :: Var v => DataDeclarationR r v a -> [Text]
constructorNames dd = Var.name <$> constructorVars dd

-- todo: delete this implementation detail of SlimCopyRegenerateIndex
declConstructorReferents :: Reference.Id -> Decl v a -> [Referent.Id]
declConstructorReferents rid decl =
  [ Referent.Con' rid i ct | i <- constructorIds (asDataDecl decl) ]
  where ct = constructorType decl

constructorIds :: DataDeclarationR r v a -> [Int]
constructorIds dd = [0 .. length (constructors dd) - 1]

-- | All variables mentioned in the given data declaration.
-- Includes both term and type variables, both free and bound.
allVars :: Ord v => DataDeclarationR r v a -> Set v
allVars (DataDeclaration _ _ bound ctors) = Set.unions $
  Set.fromList bound : [ Set.insert v (Set.fromList $ ABT.allVars tp) | (_,v,tp) <- ctors ]

-- | All variables mentioned in the given declaration.
-- Includes both term and type variables, both free and bound.
allVars' :: Ord v => DeclR r v a -> Set v
allVars' = allVars . either toDataDecl id

-- todo: move this out, as it depends on Names0
bindNames :: Var v
          => Set v
          -> Names0
          -> DataDeclaration v a
          -> Names.ResolutionResult v a (DataDeclaration v a)
bindNames keepFree names (DataDeclaration m a bound constructors) = do
  constructors <- for constructors $ \(a, v, ty) ->
    (a,v,) <$> Type.bindNames keepFree names ty
  pure $ DataDeclaration m a bound constructors

dependencies :: (Ord r, Ord v) => DataDeclarationR r v a -> Set r
dependencies dd =
  Set.unions (Type.dependencies <$> constructorTypes dd)

third :: (a -> b) -> (x,y,a) -> (x,y,b)
third f (x,y,a) = (x, y, f a)

-- implementation of dataDeclToNames and effectDeclToNames
toNames0 :: Var v => CT.ConstructorType -> v -> Reference.Id -> DataDeclaration v a -> Names0
toNames0 ct typeSymbol (Reference.DerivedId -> r) dd =
  -- constructor names
  foldMap names (constructorVars dd `zip` [0 ..])
  -- name of the type itself
  <> Names.names0 mempty (Rel.singleton (Name.fromVar typeSymbol) r)
  where
  names (ctor, i) =
    Names.names0 (Rel.singleton (Name.fromVar ctor) (Referent.Con r i ct)) mempty

dataDeclToNames :: Var v => v -> Reference.Id -> DataDeclaration v a -> Names0
dataDeclToNames = toNames0 CT.Data

effectDeclToNames :: Var v => v -> Reference.Id -> EffectDeclaration v a -> Names0
effectDeclToNames typeSymbol r ed = toNames0 CT.Effect typeSymbol r $ toDataDecl ed

dataDeclToNames' :: Var v => (v, (Reference.Id, DataDeclaration v a)) -> Names0
dataDeclToNames' (v,(r,d)) = dataDeclToNames v r d

effectDeclToNames' :: Var v => (v, (Reference.Id, EffectDeclaration v a)) -> Names0
effectDeclToNames' (v, (r, d)) = effectDeclToNames v r d

mkEffectDecl'
  :: Modifier -> a -> [v] -> [(a, v, TypeR r v a)] -> EffectDeclarationR r v a
mkEffectDecl' m a b cs = EffectDeclaration (DataDeclaration m a b cs)

mkEffectDecl :: Modifier -> [v] -> [(v, TypeR r v ())] -> EffectDeclarationR r v ()
mkEffectDecl m b cs = mkEffectDecl' m () b $ map (\(v, t) -> ((), v, t)) cs

mkDataDecl'
  :: Modifier -> a -> [v] -> [(a, v, TypeR r v a)] -> DataDeclarationR r v a
mkDataDecl' = DataDeclaration

mkDataDecl :: Modifier -> [v] -> [(v, TypeR r v ())] -> DataDeclarationR r v ()
mkDataDecl m b cs = mkDataDecl' m () b $ map (\(v,t) -> ((),v,t)) cs

constructorArities :: DataDeclarationR r v a -> [Int]
constructorArities (DataDeclaration _ _a _bound ctors) =
  Type.arity . (\(_,_,t) -> t) <$> ctors

data F r a
  = Type (Type.F' r a)
  | LetRec [a] a
  | Constructors [a]
  | Modified Modifier a
  deriving (Functor, Foldable, Show, Show1)

instance Hashable r => Hashable1 (F r) where
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

{-
  type UpDown = Up | Down

  type List a = Nil | Cons a (List a)

  type Ping p = Ping (Pong p)
  type Pong p = Pong (Ping p)

  type Foo a f = Foo Int (Bar a)
  type Bar a f = Bar Long (Foo a)
-}

hash :: (Eq v, Var v, Ord h, Accumulate h, Hashable r)
     => [(v, ABT.Term (F r) v ())] -> [(v, h)]
hash recursiveDecls = zip (fst <$> recursiveDecls) hashes where
  hashes = ABT.hash <$> toLetRec recursiveDecls

toLetRec :: Ord v => [(v, ABT.Term (F r) v ())] -> [ABT.Term (F r) v ()]
toLetRec decls = do1 <$> vs
 where
  (vs, decls') = unzip decls
  -- we duplicate this letrec once (`do1`)
  -- for each of the mutually recursive types
  do1 v = ABT.cycle (ABT.absChain vs . ABT.tm $ LetRec decls' (ABT.var v))

unsafeUnwrapType :: (Show r, Var v) => ABT.Term (F r) v a -> TypeR r v a
unsafeUnwrapType typ = ABT.transform f typ
  where f (Type t) = t
        f _ = error $ "Tried to unwrap a type that wasn't a type: " ++ show typ

toABT :: Var v => DataDeclarationR r v () -> ABT.Term (F r) v ()
toABT dd = ABT.tm $ Modified (modifier dd) dd'
  where
  dd' = ABT.absChain (bound dd) $ ABT.cycle
          (ABT.absChain
            (fst <$> constructors dd)
            (ABT.tm . Constructors $ ABT.transform Type <$> constructorTypes dd))

updateDependencies :: (Ord r, Ord v) => Map r r -> DeclR r v a -> DeclR r v a
updateDependencies typeUpdates decl = back $ dataDecl
  { constructors' = over _3 (Type.updateDependencies typeUpdates)
                      <$> constructors' dataDecl
  }
 where
  dataDecl = either toDataDecl id decl
  back     = either (const $ Left . EffectDeclaration) (const Right) decl


-- This converts `Reference`s it finds that are in the input `Map`
-- back to free variables
unhashComponent
  :: forall v a. Var v => Map Reference (Decl v a) -> Map Reference (v, Decl v a)
unhashComponent m
  = let
      usedVars = foldMap allVars' m
      m' :: Map Reference (v, Decl v a)
      m' = evalState (Map.traverseWithKey assignVar m) usedVars where
        assignVar r d = (,d) <$> ABT.freshenS (Var.refNamed r)
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

-- Implementation detail of `hashDecls`, works with unannotated data decls
hashDecls0 :: (Eq v, Var v) => Map v (DataDeclaration v ()) -> [(v, Reference.Id)]
hashDecls0 decls =
  let abts = toABT <$> decls
      ref r = ABT.tm (Type (Type.Ref (Reference.DerivedId r)))
      cs = Reference.Util.hashComponents ref abts
  in  [ (v, r) | (v, (r, _)) <- Map.toList cs ]

-- | compute the hashes of these user defined types and update any free vars
--   corresponding to these decls with the resulting hashes
--
--   data List a = Nil | Cons a (List a)
--   becomes something like
--   (List, #xyz, [forall a. #xyz a, forall a. a -> (#xyz a) -> (#xyz a)])
--
-- NOTE: technical limitation, this implementation gives diff results if ctors
-- have the same FQN as one of the types. TODO: assert this and bomb if not
-- satisfied, or else do local mangling and unmangling to ensure this doesn't
-- affect the hash.
hashDecls
  :: (Eq v, Var v)
  => Map v (DataDeclaration v a)
  -> Names.ResolutionResult v a [(v, Reference.Id, DataDeclaration v a)]
hashDecls decls = do
  -- todo: make sure all other external references are resolved before calling this
  let varToRef = hashDecls0 (void <$> decls)
      varToRef' = second Reference.DerivedId <$> varToRef
      decls'   = bindTypes <$> decls
      bindTypes dd = dd { constructors' = over _3 (Type.bindExternal varToRef') <$> constructors' dd }
      typeNames0 = Names.names0 mempty
                 $ Rel.fromList (first Name.fromVar <$> varToRef')
      -- normalize the order of the constructors based on a hash of their types
      sortCtors dd = dd { constructors' = sortOn hash3 $ constructors' dd }
      hash3 (_, _, typ) = ABT.hash typ :: Hash
  decls' <- fmap sortCtors <$> traverse (bindNames mempty typeNames0) decls'
  pure  [ (v, r, dd) | (v, r) <- varToRef, Just dd <- [Map.lookup v decls'] ]
