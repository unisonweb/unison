{-# LANGUAGE DeriveAnyClass #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveGeneric #-}

module Unison.DataDeclaration where

import           Data.Bifunctor (second)
import           Data.Map (Map, intersectionWith)
import qualified Data.Map as Map
import           Prelude hiding (cycle)
import           Prelude.Extras (Show1)
import           Unison.ABT (absChain, cycle)
import qualified Unison.ABT as ABT
import           Unison.Hashable (Accumulate, Hashable1)
import qualified Unison.Hashable as Hashable
import           Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import           Unison.Type (Type)
import qualified Unison.Type as Type
import           Unison.Typechecker.Components (components)
import           Unison.Var (Var)

data DataDeclaration v = DataDeclaration {
  bound :: [v], -- todo: do we actually use the names? or just the length
  constructors :: [(v, Type v)]
} deriving (Show)

bindBuiltins :: Var v => [(v, Type v)] -> DataDeclaration v -> DataDeclaration v
bindBuiltins typeEnv (DataDeclaration bound constructors) =
  DataDeclaration bound (second (Type.bindBuiltins typeEnv) <$> constructors)

newtype EffectDeclaration v = EffectDeclaration {
  toDataDecl :: DataDeclaration v
} deriving (Show)

withEffectDecl :: (DataDeclaration v -> DataDeclaration v') -> (EffectDeclaration v -> EffectDeclaration v')
withEffectDecl f e = EffectDeclaration (f . toDataDecl $ e)

mkEffectDecl :: [v] -> [(v, Type v)] -> EffectDeclaration v
mkEffectDecl = (EffectDeclaration .) . DataDeclaration

constructorArities :: DataDeclaration v -> [Int]
constructorArities (DataDeclaration _bound ctors) =
  Type.arity . snd <$> ctors

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
    do1 v = cycle (absChain vs . ABT.tm $ LetRec decls' (ABT.var v))

unsafeUnwrapType :: (Var v) => ABT.Term F v () -> Type v
unsafeUnwrapType typ = ABT.transform f typ
  where f (Type t) = t
        f _ = error $ "Tried to unwrap a type that wasn't a type: " ++ show typ

toABT :: Var v => DataDeclaration v -> ABT.Term F v ()
toABT (DataDeclaration bound constructors) =
  absChain bound (cycle (absChain (fst <$> constructors)
                                  (ABT.tm $ Constructors stuff)))
  where stuff = ABT.transform Type . snd <$> constructors

fromABT :: Var v => ABT.Term F v () -> DataDeclaration v
fromABT (ABT.AbsN' bound (
           ABT.Cycle' names (ABT.Tm' (Constructors stuff)))) =
  DataDeclaration
    bound
    [(v, unsafeUnwrapType t) | (v, t) <- names `zip` stuff]
fromABT a = error $ "ABT not of correct form to convert to DataDeclaration: " ++ show a

hashDecls :: (Eq v, Var v)
          => Map v (DataDeclaration v) -> [(v, Reference, DataDeclaration v)]
hashDecls decls =
  reverse . snd . foldl f ([], []) $ components abts
  where
    f (m, newDecls) cycle =
      let substed = second (ABT.substs m) <$> cycle
          hs = second Reference.Derived <$> hash substed -- hash substed :: [(v, Hash)] --> [(v, Reference)]
          newM = second toRef <$> hs
          joined = intersectionWith (,) (Map.fromList hs) (Map.fromList substed)
      in (newM ++ m,
          [(v, r, fromABT d) | (v, (r, d)) <- Map.toList joined] ++ newDecls)
    abts = second toABT <$> Map.toList decls
    toRef = ABT.tm . Type . Type.Ref
