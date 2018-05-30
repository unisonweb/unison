{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveGeneric #-}

module Unison.DataDeclaration where

import Prelude hiding (cycle)
import Unison.Type (Type)
import qualified Unison.Type as Type
import Unison.ABT (absChain, cycle)
import Unison.Var (Var)
import Unison.Hashable (Accumulate, Hashable1)
import qualified Unison.Hashable as Hashable
import qualified Unison.ABT as ABT

data DataDeclaration v = DataDeclaration {
  bound :: [v],
  constructors :: [(v, Type v)]
} deriving (Show)

-- type List a = Nil | Cons a (List a)
-- cycle (abs "List" (LetRec [abs "a" (cycle (absChain ["Nil","Cons"] (Constructors [List a, a -> List a -> List a])))] "List"))
-- absChain [a] (cycle ())"List")

data F a
  = Type (Type.F a)
  | LetRec [a] a
  | Constructors [a]
  deriving (Functor, Foldable)

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
     => [(v, DataDeclaration v)] -> [(v, h)]
hash recursiveDecls = zip (fst <$> recursiveDecls) hashes where
  hashes = ABT.hash <$> toLetRec recursiveDecls

toLetRec :: Ord v => [(v, DataDeclaration v)] -> [ABT.Term F v ()]
toLetRec decls =
  do1 <$> vs
  where
    vs = fst <$> decls
    decls' = toABT . snd <$> decls
    -- we duplicate this letrec once (`do1`) for each of the mutually recursive types
    do1 v = cycle (absChain vs . ABT.tm $ LetRec decls' (ABT.var v))

toABT :: Ord v => DataDeclaration v -> ABT.Term F v ()
toABT (DataDeclaration bound constructors) =
  absChain bound (cycle (absChain (fst <$> constructors)
                                  (ABT.tm $ Constructors stuff)))
  where stuff = ABT.transform Type . snd <$> constructors
