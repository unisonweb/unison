{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Various orphan instances and functions that we don't want to appear in client
module Unison.ABT.Extra where

import Control.Applicative
import Data.Bytes.Serial (Serial(..), Serial1(..))
import Data.Bytes.VarInt (VarInt(..))
import Data.List hiding (cycle)
import Data.Ord
import Data.Vector ((!))
import Prelude hiding (abs,cycle)
import Unison.ABT
import Unison.Symbol.Extra () -- `Serial` instances
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Unison.Digest as Digest

-- | We ignore annotations in the `Term`, as these should never affect the
-- meaning of the term.
hash :: forall f a . (Foldable f, Digest.Digestable1 f) => Term f a -> Digest.Hash
hash t = hash' [] t where
  hash' :: [Either [V] V] -> Term f a -> Digest.Hash
  hash' env (Term _ _ t) = case t of
    Var v -> maybe die hashInt ind
      where lookup (Left cycle) = elem v cycle
            lookup (Right v') = v == v'
            ind = findIndex lookup env
            -- env not likely to be very big, prefer to encode in one byte if possible
            hashInt :: Int -> Digest.Hash
            hashInt i = Digest.run (serialize (VarInt i))
            die = error $ "unknown var in environment: " ++ show v
    Cycle (AbsN' vs t) -> hash' (Left vs : env) t
    Cycle t -> hash' env t
    Abs v t -> hash' (Right v : env) t
    Tm t -> Digest.digest1 (hashCycle env) (hash' env) $ t

  hashCycle :: [Either [V] V] -> [Term f a] -> Digest.DigestM (Term f a -> Digest.Hash)
  hashCycle env@(Left cycle : envTl) ts | length cycle == length ts =
    let
      permute p xs = case Vector.fromList xs of xs -> map (xs !) p
      hashed = map (\(i,t) -> ((i,t), hash' env t)) (zip [0..] ts)
      pt = map fst (sortBy (comparing snd) hashed)
      (p,ts') = unzip pt
    in case map Right (permute p cycle) ++ envTl of
      env -> Foldable.traverse_ (serialize . hash' env) ts'
          *> pure (hash' env)
  hashCycle env ts = Foldable.traverse_ (serialize . hash' env) ts *> pure (hash' env)

-- | Use the `hash` function to efficiently remove duplicates from the list, preserving order.
distinct :: (Foldable f, Digest.Digestable1 f) => [Term f a] -> [Term f a]
distinct ts = map fst (sortBy (comparing snd) m)
  where m = Map.elems (Map.fromList (map hash ts `zip` (ts `zip` [0 :: Int .. 1])))

-- | Use the `hash` function to remove elements from `t1s` that exist in `t2s`, preserving order.
subtract :: (Foldable f, Digest.Digestable1 f) => [Term f a] -> [Term f a] -> [Term f a]
subtract t1s t2s =
  let skips = Set.fromList (map hash t2s)
  in filter (\t -> Set.notMember (hash t) skips) t1s

instance (Foldable f, Serial a, Serial1 f) => Serial (Term f a) where
  serialize (Term _ a e) = serialize a *> case e of
    Var v -> Put.putWord8 0 *> serialize v
    Cycle body -> Put.putWord8 1 *> serialize body
    Abs v body -> Put.putWord8 2 *> serialize v *> serialize body
    Tm v -> Put.putWord8 3 *> serializeWith serialize v

  deserialize = do
    ann <- deserialize
    b <- Get.getWord8
    case b of
      0 -> annotatedVar ann <$> deserialize
      1 -> cycle' ann <$> deserialize
      2 -> abs' ann <$> deserialize <*> deserialize
      3 -> tm' ann <$> deserializeWith deserialize
      _ -> fail ("unknown byte tag, expected one of {0,1,2}, got: " ++ show b)

