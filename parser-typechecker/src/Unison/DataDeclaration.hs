{-# LANGUAGE DeriveAnyClass #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}

module Unison.DataDeclaration where

import Data.List (sortOn)
import Unison.Hash (Hash)
import           Data.Functor
import           Data.Map (Map)
import qualified Data.Map as Map
import           Prelude hiding (cycle)
import           Prelude.Extras (Show1)
import qualified Unison.ABT as ABT
import           Unison.Hashable (Accumulate, Hashable1)
import qualified Unison.Hashable as Hashable
import           Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Term as Term
import           Unison.Term (AnnotatedTerm)
import           Unison.Type (AnnotatedType)
import qualified Unison.Type as Type
import           Unison.Var (Var)
import Data.Text (Text)
import qualified Unison.Var as Var
import Unison.Names (Names)
import Unison.Names as Names
-- import Debug.Trace

type DataDeclaration v = DataDeclaration' v ()

data DataDeclaration' v a = DataDeclaration {
  annotation :: a,
  bound :: [v],
  constructors' :: [(a, v, AnnotatedType v a)]
} deriving (Show, Functor)

generateConstructorRefs
  :: (Reference -> Int -> Reference)
  -> Reference.Id
  -> Int
  -> [(Int, Reference)]
generateConstructorRefs hashCtor rid n =
  (\i -> (i, hashCtor (Reference.DerivedPrivate_ rid) i)) <$> [0 .. n]

-- Returns references to the constructors,
-- along with the terms for those references and their types.
constructorTerms
  :: (Reference -> Int -> Reference)
  -> (a -> Reference -> Int -> AnnotatedTerm v a)
  -> Reference.Id
  -> DataDeclaration' v a
  -> [(Reference.Id, AnnotatedTerm v a, AnnotatedType v a)]
constructorTerms hashCtor f rid dd =
  (\((a, _, t), (i, re@(Reference.DerivedId r))) -> (r, f a re i, t)) <$> zip
    (constructors' dd)
    (generateConstructorRefs hashCtor rid (length $ constructors dd))

dataConstructorTerms
  :: Ord v
  => Reference.Id
  -> DataDeclaration' v a
  -> [(Reference.Id, AnnotatedTerm v a, AnnotatedType v a)]
dataConstructorTerms = constructorTerms Term.hashConstructor Term.constructor

effectConstructorTerms
  :: Ord v
  => Reference.Id
  -> EffectDeclaration' v a
  -> [(Reference.Id, AnnotatedTerm v a, AnnotatedType v a)]
effectConstructorTerms rid ed =
  constructorTerms Term.hashRequest Term.request rid $ toDataDecl ed

constructorTypes :: DataDeclaration' v a -> [AnnotatedType v a]
constructorTypes = (snd <$>) . constructors

constructors :: DataDeclaration' v a -> [(v, AnnotatedType v a)]
constructors (DataDeclaration _ _ ctors) = [(v,t) | (_,v,t) <- ctors ]

constructorVars :: DataDeclaration' v a -> [v]
constructorVars dd = fst <$> constructors dd

constructorNames :: Var v => DataDeclaration' v a -> [Text]
constructorNames dd = Var.name <$> constructorVars dd

bindBuiltins :: Var v => Names v x -> DataDeclaration' v a -> DataDeclaration' v a
bindBuiltins names (DataDeclaration a bound constructors) =
  DataDeclaration a bound (third (Names.bindType names) <$> constructors)

third :: (a -> b) -> (x,y,a) -> (x,y,b)
third f (x,y,a) = (x, y, f a)

-- implementation of dataDeclToNames and effectDeclToNames
toNames0 :: Var v => v
                  -> Reference
                  -> (a -> Reference -> Int -> AnnotatedTerm v a)
                  -> DataDeclaration' v a
                  -> Names v a
toNames0 typeSymbol r f dd = let
  names ((ctor, typ), i) = let
    name = mconcat [Var.qualifiedName typeSymbol, ".", Var.qualifiedName ctor]
    in Names.fromTerms [(name, (f (ABT.annotation typ) r i, typ))] <>
       Names.fromPatterns [(name, (r,i))]
  in foldMap names (constructors dd `zip` [0 ..]) <>
     Names.fromTypesV [(typeSymbol,r)]

dataDeclToNames :: Var v => v -> Reference -> DataDeclaration' v a -> Names v a
dataDeclToNames typeSymbol r dd = toNames0 typeSymbol r Term.constructor dd

effectDeclToNames :: Var v => v -> Reference -> EffectDeclaration' v a -> Names v a
effectDeclToNames typeSymbol r ed = toNames0 typeSymbol r Term.request $ toDataDecl ed

dataDeclToNames' :: Var v => (v, (Reference, DataDeclaration' v a)) -> Names v a
dataDeclToNames' (v,(r,d)) = dataDeclToNames v r d

effectDeclToNames' :: Var v => (v, (Reference, EffectDeclaration' v a)) -> Names v a
effectDeclToNames' (v,(r,d)) = effectDeclToNames v r d

type EffectDeclaration v = EffectDeclaration' v ()

newtype EffectDeclaration' v a = EffectDeclaration {
  toDataDecl :: DataDeclaration' v a
} deriving (Show,Functor)

withEffectDecl :: (DataDeclaration' v a -> DataDeclaration' v' a') -> (EffectDeclaration' v a -> EffectDeclaration' v' a')
withEffectDecl f e = EffectDeclaration (f . toDataDecl $ e)

mkEffectDecl' :: a -> [v] -> [(a, v, AnnotatedType v a)] -> EffectDeclaration' v a
mkEffectDecl' a b cs = EffectDeclaration (DataDeclaration a b cs)

mkEffectDecl :: [v] -> [(v, AnnotatedType v ())] -> EffectDeclaration' v ()
mkEffectDecl b cs = mkEffectDecl' () b $ map (\(v,t) -> ((),v,t)) cs

mkDataDecl' :: a -> [v] -> [(a, v, AnnotatedType v a)] -> DataDeclaration' v a
mkDataDecl' a b cs = DataDeclaration a b cs

mkDataDecl :: [v] -> [(v, AnnotatedType v ())] -> DataDeclaration' v ()
mkDataDecl b cs = mkDataDecl' () b $ map (\(v,t) -> ((),v,t)) cs

constructorArities :: DataDeclaration' v a -> [Int]
constructorArities (DataDeclaration _a _bound ctors) =
  Type.arity . (\(_,_,t) -> t) <$> ctors

-- type List a = Nil | Cons a (List a)
-- cycle (abs "List" (LetRec [abs "a" (cycle (absChain ["Nil","Cons"] (Constructors [List a, a -> List a -> List a])))] "List"))
-- absChain [a] (cycle ())"List")

data F a
  = Type (Type.F a)
  | LetRec [a] a
  | Constructors [a]
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
        in [tag 2] ++ map hashed hashes

{-
  type UpDown = Up | Down

  type List a = Nil | Cons a (List a)

  type Ping p = Ping (Pong p)
  type Pong p = Pong (Ping p)

  type Foo a f = Foo Int (Bar a)
  type Bar a f = Bar Long (Foo a)
-}

hash :: (Eq v, Var v, Ord h, Accumulate h)
     => [(v, ABT.Term F v ())] -> [(v, h)]
hash recursiveDecls = zip (fst <$> recursiveDecls) hashes where
  hashes = ABT.hash <$> toLetRec recursiveDecls

toLetRec :: Ord v => [(v, ABT.Term F v ())] -> [ABT.Term F v ()]
toLetRec decls =
  do1 <$> vs
  where
    (vs, decls') = unzip decls
    -- we duplicate this letrec once (`do1`) for each of the mutually recursive types
    do1 v = ABT.cycle (ABT.absChain vs . ABT.tm $ LetRec decls' (ABT.var v))

unsafeUnwrapType :: (Var v) => ABT.Term F v a -> AnnotatedType v a
unsafeUnwrapType typ = ABT.transform f typ
  where f (Type t) = t
        f _ = error $ "Tried to unwrap a type that wasn't a type: " ++ show typ

toABT :: Var v => DataDeclaration v -> ABT.Term F v ()
toABT dd =
  ABT.absChain (bound dd) (ABT.cycle (ABT.absChain (fst <$> constructors dd)
                                                   (ABT.tm $ Constructors stuff)))
  where stuff = ABT.transform Type . snd <$> constructors dd

fromABT :: Var v => ABT.Term F v () -> DataDeclaration' v ()
fromABT (ABT.AbsN' bound (
           ABT.Cycle' names (ABT.Tm' (Constructors stuff)))) =
  DataDeclaration ()
    bound
    [((), v, unsafeUnwrapType t) | (v, t) <- names `zip` stuff]
fromABT a = error $ "ABT not of correct form to convert to DataDeclaration: " ++ show a

-- Implementation detail of `hashDecls`, works with unannotated data decls
hashDecls0
  :: (Eq v, Var v)
  => Map v (DataDeclaration' v ())
  -> [(v, Reference)]
hashDecls0 decls = let
  abts  = toABT <$> decls
  ref r = ABT.tm (Type (Type.Ref r))
  cs = Reference.hashComponents ref abts
  in [(v,r) | (v, (r,_)) <- Map.toList cs ]

-- | compute the hashes of these user defined types and update any free vars
--   corresponding to these decls with the resulting hashes
--
--   data List a = Nil | Cons a (List a)
--   becomes something like
--   (List, #xyz, [forall a. #xyz a, forall a. a -> (#xyz a) -> (#xyz a)])
hashDecls
  :: (Eq v, Var v)
  => Map v (DataDeclaration' v a)
  -> [(v, Reference, DataDeclaration' v a)]
hashDecls decls =
  let varToRef = hashDecls0 (void <$> decls)
      decls'   = bindDecls decls (Names.fromTypesV varToRef)
  in  [ (v, r, dd) | (v, r) <- varToRef, Just dd <- [Map.lookup v decls'] ]

bindDecls :: Var v => Map v (DataDeclaration' v a) -> Names v x -> Map v (DataDeclaration' v a)
bindDecls decls refs = sortCtors . bindBuiltins refs <$> decls where
  -- normalize the order of the constructors based on a hash of their types
  sortCtors dd = DataDeclaration (annotation dd) (bound dd) (sortOn hash3 $ constructors' dd)
  hash3 (_,_,typ) = ABT.hash typ :: Hash
