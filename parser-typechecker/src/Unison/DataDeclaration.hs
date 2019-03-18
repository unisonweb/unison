{-# LANGUAGE DeriveAnyClass #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language TypeApplications #-}
{-# Language ViewPatterns #-}

module Unison.DataDeclaration where

import           Safe                           ( atMay )
import           Data.List                      ( sortOn )
import           Unison.Hash                    ( Hash )
import           Data.Functor
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Prelude                 hiding ( cycle )
import           Prelude.Extras                 ( Show1 )
import qualified Unison.ABT                    as ABT
import           Unison.Hashable                ( Accumulate
                                                , Hashable1
                                                )
import qualified Unison.Hashable               as Hashable
import qualified Unison.Name                   as Name
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import           Unison.Referent                ( Referent )
import qualified Unison.Referent               as Referent
import qualified Unison.Term                   as Term
import           Unison.Term                    ( AnnotatedTerm
                                                , AnnotatedTerm2
                                                )
import           Unison.Type                    ( AnnotatedType )
import qualified Unison.Type                   as Type
import           Unison.Var                     ( Var )
import           Data.Text                      ( Text )
import qualified Unison.Var                    as Var
import           Unison.Names                   ( Names )
import           Unison.Names                  as Names
import           Unison.Symbol                  ( Symbol )
import qualified Unison.Pattern                as Pattern

type ConstructorId = Int

type DataDeclaration v = DataDeclaration' v ()

data DataDeclaration' v a = DataDeclaration {
  annotation :: a,
  bound :: [v],
  constructors' :: [(a, v, AnnotatedType v a)]
} deriving (Eq, Show, Functor)

generateConstructorRefs
  :: (Reference -> ConstructorId -> Reference)
  -> Reference.Id
  -> Int
  -> [(ConstructorId, Reference)]
generateConstructorRefs hashCtor rid n =
  (\i -> (i, hashCtor (Reference.DerivedId rid) i)) <$> [0 .. n]

-- Returns references to the constructors,
-- along with the terms for those references and their types.
constructorTerms
  :: (Reference -> ConstructorId -> Reference)
  -> (a -> Reference -> ConstructorId -> AnnotatedTerm v a)
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

typeOfConstructor :: DataDeclaration' v a -> ConstructorId -> Maybe (AnnotatedType v a)
typeOfConstructor dd i = constructorTypes dd `atMay` i

constructors :: DataDeclaration' v a -> [(v, AnnotatedType v a)]
constructors (DataDeclaration _ _ ctors) = [(v,t) | (_,v,t) <- ctors ]

constructorVars :: DataDeclaration' v a -> [v]
constructorVars dd = fst <$> constructors dd

constructorNames :: Var v => DataDeclaration' v a -> [Text]
constructorNames dd = Var.name <$> constructorVars dd

bindBuiltins :: Var v => Names -> DataDeclaration' v a -> DataDeclaration' v a
bindBuiltins names (DataDeclaration a bound constructors) =
  DataDeclaration a bound (third (Names.bindType names) <$> constructors)

dependencies :: Ord v => DataDeclaration' v a -> Set Reference
dependencies dd =
  Set.unions (Type.dependencies <$> constructorTypes dd)

third :: (a -> b) -> (x,y,a) -> (x,y,b)
third f (x,y,a) = (x, y, f a)

-- implementation of dataDeclToNames and effectDeclToNames
toNames0
  :: Var v
  => v
  -> Reference
  -> (Reference -> ConstructorId -> Referent)
  -> DataDeclaration' v a
  -> Names
toNames0 typeSymbol r f dd =
  let names (ctor, i) =
        let name = Name.unsafeFromVar ctor in Names.fromTerms [(name, f r i)]
  in  foldMap names (constructorVars dd `zip` [0 ..])
        <> Names.fromTypesV [(typeSymbol, r)]

dataDeclToNames :: Var v => v -> Reference -> DataDeclaration' v a -> Names
dataDeclToNames typeSymbol r dd = toNames0 typeSymbol r Referent.Con dd

effectDeclToNames :: Var v => v -> Reference -> EffectDeclaration' v a -> Names
effectDeclToNames typeSymbol r ed =
  toNames0 typeSymbol r Referent.Con $ toDataDecl ed

dataDeclToNames' :: Var v => (v, (Reference, DataDeclaration' v a)) -> Names
dataDeclToNames' (v,(r,d)) = dataDeclToNames v r d

effectDeclToNames' :: Var v => (v, (Reference, EffectDeclaration' v a)) -> Names
effectDeclToNames' (v, (r, d)) = effectDeclToNames v r d

type EffectDeclaration v = EffectDeclaration' v ()

newtype EffectDeclaration' v a = EffectDeclaration {
  toDataDecl :: DataDeclaration' v a
} deriving (Eq,Show,Functor)

withEffectDecl :: (DataDeclaration' v a -> DataDeclaration' v' a') -> (EffectDeclaration' v a -> EffectDeclaration' v' a')
withEffectDecl f e = EffectDeclaration (f . toDataDecl $ e)

mkEffectDecl'
  :: a -> [v] -> [(a, v, AnnotatedType v a)] -> EffectDeclaration' v a
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
toLetRec decls = do1 <$> vs
 where
  (vs, decls') = unzip decls
  -- we duplicate this letrec once (`do1`)
  -- for each of the mutually recursive types
  do1 v = ABT.cycle (ABT.absChain vs . ABT.tm $ LetRec decls' (ABT.var v))

unsafeUnwrapType :: (Var v) => ABT.Term F v a -> AnnotatedType v a
unsafeUnwrapType typ = ABT.transform f typ
  where f (Type t) = t
        f _ = error $ "Tried to unwrap a type that wasn't a type: " ++ show typ

toABT :: Var v => DataDeclaration v -> ABT.Term F v ()
toABT dd = ABT.absChain
  (bound dd)
  (ABT.cycle
    (ABT.absChain (fst <$> constructors dd) (ABT.tm $ Constructors stuff))
  )
  where stuff = ABT.transform Type . snd <$> constructors dd

fromABT :: Var v => ABT.Term F v () -> DataDeclaration' v ()
fromABT (ABT.AbsN' bound (ABT.Cycle' names (ABT.Tm' (Constructors stuff)))) =
  DataDeclaration
    ()
    bound
    [ ((), v, unsafeUnwrapType t) | (v, t) <- names `zip` stuff ]
fromABT a =
  error $ "ABT not of correct form to convert to DataDeclaration: " ++ show a

-- Implementation detail of `hashDecls`, works with unannotated data decls
hashDecls0 :: (Eq v, Var v) => Map v (DataDeclaration' v ()) -> [(v, Reference)]
hashDecls0 decls =
  let abts = toABT <$> decls
      ref r = ABT.tm (Type (Type.Ref r))
      cs = Reference.hashComponents ref abts
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
  => Map v (DataDeclaration' v a)
  -> [(v, Reference, DataDeclaration' v a)]
hashDecls decls =
  let varToRef = hashDecls0 (void <$> decls)
      decls'   = bindDecls decls (Names.fromTypesV varToRef)
  in  [ (v, r, dd) | (v, r) <- varToRef, Just dd <- [Map.lookup v decls'] ]

unitRef, pairRef, optionalRef :: Reference
(unitRef, pairRef, optionalRef) = let
  decls = builtinDataDecls @ Symbol
  [(_,unit,_)] = filter (\(v, _,_) -> v == Var.named "()") decls
  [(_,pair,_)] = filter (\(v, _,_) -> v == Var.named "Pair") decls
  [(_,opt,_)] = filter (\(v, _,_) -> v == Var.named "Optional") decls
  in (unit, pair, opt)

builtinDataDecls :: Var v => [(v, Reference, DataDeclaration' v ())]
builtinDataDecls = hashDecls $
  Map.fromList [
    (v "()", unit), (v "Pair", pair), (v "Optional", opt) ]
  where
  v name = Var.named name
  var name = Type.var() (v name)
  arr = Type.arrow'
  -- see note on `hashDecls` above for why ctor must be called `().()`.
  unit = DataDeclaration () [] [((), v "().()", var "()")]
  pair = DataDeclaration () [v "a", v "b"] [
    ((), v "Pair.Pair", Type.foralls() [v"a",v"b"]
         (var "a" `arr` (var "b" `arr` Type.apps' (var "Pair") [var "a", var "b"])))
   ]
  opt = DataDeclaration () [v "a"] [
    ((), v "Optional.None", Type.foralls() [v "a"]
      (              Type.app' (var "Optional") (var "a"))),
    ((), v "Optional.Some", Type.foralls() [v "a"]
      (var "a" `arr` Type.app' (var "Optional") (var "a")))
   ]

pattern UnitRef <- (unUnitRef -> True)
pattern PairRef <- (unPairRef -> True)
pattern OptionalRef <- (unOptionalRef -> True)
pattern TupleType' ts <- (unTupleType -> Just ts)
pattern TupleTerm' xs <- (unTupleTerm -> Just xs)
pattern TuplePattern ps <- (unTuplePattern -> Just ps)

unitType, pairType, optionalType :: Ord v => a -> AnnotatedType v a
unitType a = Type.ref a unitRef
pairType a = Type.ref a pairRef
optionalType a = Type.ref a optionalRef

unitTerm :: Var v => a -> AnnotatedTerm v a
unitTerm ann = Term.constructor ann unitRef 0

tupleConsTerm :: (Ord v, Semigroup a)
              => AnnotatedTerm2 vt at ap v a
              -> AnnotatedTerm2 vt at ap v a
              -> AnnotatedTerm2 vt at ap v a
tupleConsTerm hd tl =
  Term.apps' (Term.constructor (ABT.annotation hd) pairRef 0) [hd, tl]

tupleTerm :: (Var v, Monoid a) => [AnnotatedTerm v a] -> AnnotatedTerm v a
tupleTerm = foldr tupleConsTerm (unitTerm mempty)

-- delayed terms are just lambdas that take a single `()` arg
-- `force` calls the function
forceTerm :: Var v => a -> a -> AnnotatedTerm v a -> AnnotatedTerm v a
forceTerm a au e = Term.app a e (unitTerm au)

delayTerm :: Var v => a -> AnnotatedTerm v a -> AnnotatedTerm v a
delayTerm a e = Term.lam a (Var.named "()") e

unTupleTerm :: Term.AnnotatedTerm2 vt at ap v a -> Maybe [Term.AnnotatedTerm2 vt at ap v a]
unTupleTerm t = case t of
  Term.Apps' (Term.Constructor' PairRef 0) [fst, snd] -> (fst :) <$> unTupleTerm snd
  Term.Constructor' UnitRef 0 -> Just []
  _ -> Nothing

unTupleType :: Var v => Type.AnnotatedType v a -> Maybe [Type.AnnotatedType v a]
unTupleType t = case t of
  Type.Apps' (Type.Ref' PairRef) [fst, snd] -> (fst :) <$> unTupleType snd
  Type.Ref' UnitRef -> Just []
  _ -> Nothing

unTuplePattern :: Pattern.PatternP loc -> Maybe [Pattern.PatternP loc]
unTuplePattern p = case p of
  Pattern.ConstructorP _ PairRef 0 [fst, snd] -> (fst : ) <$> unTuplePattern snd
  Pattern.ConstructorP _ UnitRef 0 [] -> Just []
  _ -> Nothing

unUnitRef,unPairRef,unOptionalRef :: Reference -> Bool
unUnitRef = (== unitRef)
unPairRef = (== pairRef)
unOptionalRef = (== optionalRef)

bindDecls
  :: Var v
  => Map v (DataDeclaration' v a)
  -> Names
  -> Map v (DataDeclaration' v a)
bindDecls decls refs = sortCtors . bindBuiltins refs <$> decls
 where
  -- normalize the order of the constructors based on a hash of their types
  sortCtors dd =
    DataDeclaration (annotation dd) (bound dd) (sortOn hash3 $ constructors' dd)
  hash3 (_, _, typ) = ABT.hash typ :: Hash
