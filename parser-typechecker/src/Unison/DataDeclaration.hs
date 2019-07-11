{-# LANGUAGE DeriveAnyClass #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language TypeApplications #-}
{-# Language ViewPatterns #-}

module Unison.DataDeclaration where

import Control.Lens (_3, over)
import Data.Bifunctor (first)
import Data.Traversable (for)
import qualified Unison.Util.Relation as Rel
import           Safe                           ( atMay )
import           Data.List                      ( sortOn, elemIndex, find )
import           Unison.Hash                    ( Hash )
import           Control.Monad                  ( join )
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
import qualified Unison.Reference.Util         as Reference.Util
import qualified Unison.Referent               as Referent
import qualified Unison.Term                   as Term
import           Unison.Term                    ( AnnotatedTerm
                                                , AnnotatedTerm2
                                                )
import           Unison.Type                    ( Type )
import qualified Unison.Type                   as Type
import           Unison.Var                     ( Var )
import           Data.Text                      ( Text )
import qualified Unison.Var                    as Var
import           Unison.Names3                 (Names0)
import qualified Unison.Names3                 as Names
import           Unison.Symbol                  ( Symbol )
import qualified Unison.Pattern                as Pattern
import qualified Unison.ConstructorType as CT

type ConstructorId = Int

type DataDeclaration v = DataDeclaration' v ()
type Decl v a = Either (EffectDeclaration' v a) (DataDeclaration' v a)

declDependencies :: Ord v => Decl v a -> Set Reference
declDependencies = either (dependencies . toDataDecl) dependencies

constructorType :: Decl v a -> CT.ConstructorType
constructorType = \case
  Left{} -> CT.Effect
  Right{} -> CT.Data

data Modifier = Structural | Unique Text -- | Opaque (Set Reference)
  deriving (Eq, Ord, Show)

data DataDeclaration' v a = DataDeclaration {
  modifier :: Modifier,
  annotation :: a,
  bound :: [v],
  constructors' :: [(a, v, Type v a)]
} deriving (Eq, Show, Functor)

generateConstructorRefs
  :: (Reference -> ConstructorId -> Reference)
  -> Reference.Id
  -> Int
  -> [(ConstructorId, Reference)]
generateConstructorRefs hashCtor rid n =
  (\i -> (i, hashCtor (Reference.DerivedId rid) i)) <$> [0 .. n]

generateRecordAccessors
  :: (Semigroup a, Var v)
  => [(v, a)]
  -> v
  -> Reference
  -> [(v, AnnotatedTerm v a)]
generateRecordAccessors fields typename typ =
  join [ tm t i | (t, i) <- fields `zip` [(0::Int)..] ]
  where
  argname = Var.uncapitalize typename
  tm (fname, ann) i =
    [(Var.namespaced $ [typename, fname], get),
     (Var.namespaced $ [typename, fname, Var.named "set"], set),
     (Var.namespaced $ [typename, fname, Var.named "modify"], modify)]
    where
    -- example: `point -> case point of Point x _ -> x`
    get = Term.lam ann argname $ Term.match ann
      (Term.var ann argname)
      [Term.MatchCase pat Nothing rhs]
      where
      pat = Pattern.ConstructorP ann typ 0 cargs
      cargs = [ if j == i then Pattern.VarP ann else Pattern.UnboundP ann
              | (_, j) <- fields `zip` [0..]]
      rhs = ABT.abs' ann fname (Term.var ann fname)
    -- example: `x point -> case point of Point _ y -> Point x y`
    set = Term.lam' ann [fname', argname] $ Term.match ann
      (Term.var ann argname)
      [Term.MatchCase pat Nothing rhs]
      where
      fname' = Var.named . Var.name $
               Var.freshIn (Set.fromList $ [argname] <> (fst <$> fields)) fname
      pat = Pattern.ConstructorP ann typ 0 cargs
      cargs = [ if j == i then Pattern.UnboundP ann else Pattern.VarP ann
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
      pat = Pattern.ConstructorP ann typ 0 cargs
      cargs = replicate (length fields) $ Pattern.VarP ann
      rhs = foldr (ABT.abs' ann) (Term.constructor ann typ 0 `Term.apps'` vargs)
                  (fst <$> fields)
      vargs = [ if j == i
                then Term.apps' (Term.var ann fname') [Term.var ann v]
                else Term.var ann v
              | ((v, _), j) <- fields `zip` [0..]]

-- Returns references to the constructors,
-- along with the terms for those references and their types.
constructorTerms
  :: (Reference -> ConstructorId -> Reference)
  -> (a -> Reference -> ConstructorId -> AnnotatedTerm v a)
  -> Reference.Id
  -> DataDeclaration' v a
  -> [(Reference.Id, AnnotatedTerm v a, Type v a)]
constructorTerms hashCtor f rid dd =
  (\((a, _, t), (i, re@(Reference.DerivedId r))) -> (r, f a re i, t)) <$> zip
    (constructors' dd)
    (generateConstructorRefs hashCtor rid (length $ constructors dd))

dataConstructorTerms
  :: Ord v
  => Reference.Id
  -> DataDeclaration' v a
  -> [(Reference.Id, AnnotatedTerm v a, Type v a)]
dataConstructorTerms = constructorTerms Term.hashConstructor Term.constructor

effectConstructorTerms
  :: Ord v
  => Reference.Id
  -> EffectDeclaration' v a
  -> [(Reference.Id, AnnotatedTerm v a, Type v a)]
effectConstructorTerms rid ed =
  constructorTerms Term.hashRequest Term.request rid $ toDataDecl ed

constructorTypes :: DataDeclaration' v a -> [Type v a]
constructorTypes = (snd <$>) . constructors

typeOfConstructor :: DataDeclaration' v a -> ConstructorId -> Maybe (Type v a)
typeOfConstructor dd i = constructorTypes dd `atMay` i

constructors :: DataDeclaration' v a -> [(v, Type v a)]
constructors (DataDeclaration _ _ _ ctors) = [(v,t) | (_,v,t) <- ctors ]

constructorVars :: DataDeclaration' v a -> [v]
constructorVars dd = fst <$> constructors dd

constructorNames :: Var v => DataDeclaration' v a -> [Text]
constructorNames dd = Var.name <$> constructorVars dd

bindNames :: Var v
          => Set v
          -> Names0
          -> DataDeclaration' v a
          -> Names.ResolutionResult v a (DataDeclaration' v a)
bindNames keepFree names (DataDeclaration m a bound constructors) = do
  constructors <- for constructors $ \(a, v, ty) ->
    (a,v,) <$> Type.bindNames keepFree names ty
  pure $ DataDeclaration m a bound constructors

dependencies :: Ord v => DataDeclaration' v a -> Set Reference
dependencies dd =
  Set.unions (Type.dependencies <$> constructorTypes dd)

third :: (a -> b) -> (x,y,a) -> (x,y,b)
third f (x,y,a) = (x, y, f a)

-- implementation of dataDeclToNames and effectDeclToNames
toNames0 :: Var v => CT.ConstructorType -> v -> Reference -> DataDeclaration' v a -> Names0
toNames0 ct typeSymbol r dd =
  -- constructor names
  foldMap names (constructorVars dd `zip` [0 ..])
  -- name of the type itself
  <> Names.names0 mempty (Rel.singleton (Name.fromVar typeSymbol) r)
  where
  names (ctor, i) =
    Names.names0 (Rel.singleton (Name.fromVar ctor) (Referent.Con r i ct)) mempty

dataDeclToNames :: Var v => v -> Reference -> DataDeclaration' v a -> Names0
dataDeclToNames typeSymbol r dd = toNames0 CT.Data typeSymbol r dd

effectDeclToNames :: Var v => v -> Reference -> EffectDeclaration' v a -> Names0
effectDeclToNames typeSymbol r ed = toNames0 CT.Effect typeSymbol r $ toDataDecl ed

dataDeclToNames' :: Var v => (v, (Reference, DataDeclaration' v a)) -> Names0
dataDeclToNames' (v,(r,d)) = dataDeclToNames v r d

effectDeclToNames' :: Var v => (v, (Reference, EffectDeclaration' v a)) -> Names0
effectDeclToNames' (v, (r, d)) = effectDeclToNames v r d

type EffectDeclaration v = EffectDeclaration' v ()

newtype EffectDeclaration' v a = EffectDeclaration {
  toDataDecl :: DataDeclaration' v a
} deriving (Eq,Show,Functor)

withEffectDecl :: (DataDeclaration' v a -> DataDeclaration' v' a') -> (EffectDeclaration' v a -> EffectDeclaration' v' a')
withEffectDecl f e = EffectDeclaration (f . toDataDecl $ e)

withEffectDeclM :: Functor f
                => (DataDeclaration' v a -> f (DataDeclaration' v' a'))
                -> EffectDeclaration' v a
                -> f (EffectDeclaration' v' a')
withEffectDeclM f = fmap EffectDeclaration . f . toDataDecl

mkEffectDecl'
  :: Modifier -> a -> [v] -> [(a, v, Type v a)] -> EffectDeclaration' v a
mkEffectDecl' m a b cs = EffectDeclaration (DataDeclaration m a b cs)

mkEffectDecl :: Modifier -> [v] -> [(v, Type v ())] -> EffectDeclaration' v ()
mkEffectDecl m b cs = mkEffectDecl' m () b $ map (\(v,t) -> ((),v,t)) cs

mkDataDecl' :: Modifier -> a -> [v] -> [(a, v, Type v a)] -> DataDeclaration' v a
mkDataDecl' m a b cs = DataDeclaration m a b cs

mkDataDecl :: Modifier -> [v] -> [(v, Type v ())] -> DataDeclaration' v ()
mkDataDecl m b cs = mkDataDecl' m () b $ map (\(v,t) -> ((),v,t)) cs

constructorArities :: DataDeclaration' v a -> [Int]
constructorArities (DataDeclaration _ _a _bound ctors) =
  Type.arity . (\(_,_,t) -> t) <$> ctors

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
        in [tag 2] ++ map hashed hashes
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

unsafeUnwrapType :: (Var v) => ABT.Term F v a -> Type v a
unsafeUnwrapType typ = ABT.transform f typ
  where f (Type t) = t
        f _ = error $ "Tried to unwrap a type that wasn't a type: " ++ show typ

toABT :: Var v => DataDeclaration v -> ABT.Term F v ()
toABT dd = ABT.tm $ Modified (modifier dd) dd'
  where
  dd' = ABT.absChain (bound dd) $ ABT.cycle
          (ABT.absChain
            (fst <$> constructors dd)
            (ABT.tm . Constructors $ ABT.transform Type <$> constructorTypes dd))

-- Implementation detail of `hashDecls`, works with unannotated data decls
hashDecls0 :: (Eq v, Var v) => Map v (DataDeclaration' v ()) -> [(v, Reference)]
hashDecls0 decls =
  let abts = toABT <$> decls
      ref r = ABT.tm (Type (Type.Ref r))
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
  => Map v (DataDeclaration' v a)
  -> Names.ResolutionResult v a [(v, Reference, DataDeclaration' v a)]
hashDecls decls = do
  -- todo: make sure all other external references are resolved before calling this
  let varToRef = hashDecls0 (void <$> decls)
      decls'   = bindTypes <$> decls
      bindTypes dd = dd { constructors' = over _3 (Type.bindExternal varToRef) <$> constructors' dd }
      typeNames0 = Names.names0 mempty
                 $ Rel.fromList (first Name.fromVar <$> varToRef)
      -- normalize the order of the constructors based on a hash of their types
      sortCtors dd = dd { constructors' = sortOn hash3 $ constructors' dd }
      hash3 (_, _, typ) = ABT.hash typ :: Hash
  decls' <- fmap sortCtors <$> traverse (bindNames mempty typeNames0) decls'
  pure  [ (v, r, dd) | (v, r) <- varToRef, Just dd <- [Map.lookup v decls'] ]


unitRef, pairRef, optionalRef, testResultRef :: Reference
(unitRef, pairRef, optionalRef, testResultRef) =
  let decls          = builtinDataDecls @Symbol
      [(_, unit, _)] = filter (\(v, _, _) -> v == Var.named "()") decls
      [(_, pair, _)] = filter (\(v, _, _) -> v == Var.named "Pair") decls
      [(_, opt , _)] = filter (\(v, _, _) -> v == Var.named "Optional") decls
      [(_, testResult, _)] =
        filter (\(v, _, _) -> v == Var.named "Test.Result") decls
  in  (unit, pair, opt, testResult)

constructorId :: Reference -> Text -> Maybe Int
constructorId ref name = do
  (_,_,dd) <- find (\(_,r,_) -> r == ref) (builtinDataDecls @Symbol)
  elemIndex name $ constructorNames dd

okConstructorId, failConstructorId :: Int
Just okConstructorId = constructorId testResultRef "Test.Result.Ok"
Just failConstructorId = constructorId testResultRef "Test.Result.Fail"

failResult :: (Ord v, Monoid a) => a -> Text -> AnnotatedTerm v a
failResult ann msg =
  Term.app ann (Term.request ann testResultRef failConstructorId)
               (Term.text ann msg)

builtinDataDecls :: Var v => [(v, Reference, DataDeclaration' v ())]
builtinDataDecls = case hashDecls $ Map.fromList
  [ (v "()"             , unit)
  , (v "Pair"           , pair)
  , (v "Optional"       , opt)
  , (v "Test.Result"    , tr)
  ] of Right a -> a; Left e -> error $ "builtinDataDecls: " <> show e
 where
  v name = Var.named name
  var name = Type.var () (v name)
  arr  = Type.arrow'
  -- see note on `hashDecls` above for why ctor must be called `().()`.
  unit = DataDeclaration Structural () [] [((), v "().()", var "()")]
  pair = DataDeclaration
    Structural
    ()
    [v "a", v "b"]
    [ ( ()
      , v "Pair.Pair"
      , Type.foralls
        ()
        [v "a", v "b"]
        (     var "a"
        `arr` (var "b" `arr` Type.apps' (var "Pair") [var "a", var "b"])
        )
      )
    ]
  opt = DataDeclaration
    Structural
    ()
    [v "a"]
    [ ( ()
      , v "Optional.None"
      , Type.foralls () [v "a"] (Type.app' (var "Optional") (var "a"))
      )
    , ( ()
      , v "Optional.Some"
      , Type.foralls ()
                     [v "a"]
                     (var "a" `arr` Type.app' (var "Optional") (var "a"))
      )
    ]
  tr = DataDeclaration
    (Unique "70621e539cd802b2ad53105697800930411a3ebc")
    ()
    []
    [ ((), v "Test.Result.Fail", Type.text () `arr` var "Test.Result")
    , ((), v "Test.Result.Ok"  , Type.text () `arr` var "Test.Result")
    ]

pattern UnitRef <- (unUnitRef -> True)
pattern PairRef <- (unPairRef -> True)
pattern OptionalRef <- (unOptionalRef -> True)
pattern TupleType' ts <- (unTupleType -> Just ts)
pattern TupleTerm' xs <- (unTupleTerm -> Just xs)
pattern TuplePattern ps <- (unTuplePattern -> Just ps)

unitType, pairType, optionalType, testResultType
  :: Ord v => a -> Type v a
unitType a = Type.ref a unitRef
pairType a = Type.ref a pairRef
testResultType a = Type.app a (Type.vector a) (Type.ref a testResultRef)
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

unTupleType :: Var v => Type v a -> Maybe [Type v a]
unTupleType t = case t of
  Type.Apps' (Type.Ref' PairRef) [fst, snd] -> (fst :) <$> unTupleType snd
  Type.Ref' UnitRef -> Just []
  _ -> Nothing

unTuplePattern :: Pattern.PatternP loc -> Maybe [Pattern.PatternP loc]
unTuplePattern p = case p of
  Pattern.ConstructorP _ PairRef 0 [fst, snd] -> (fst : ) <$> unTuplePattern snd
  Pattern.ConstructorP _ UnitRef 0 [] -> Just []
  _ -> Nothing

unUnitRef,unPairRef,unOptionalRef:: Reference -> Bool
unUnitRef = (== unitRef)
unPairRef = (== pairRef)
unOptionalRef = (== optionalRef)

