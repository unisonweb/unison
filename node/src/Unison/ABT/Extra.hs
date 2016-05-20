{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Various orphan instances and functions that we don't want to appear in client
module Unison.ABT.Extra where

import Control.Applicative
import Data.Bytes.Serial (Serial(..), Serial1(..))
import Data.List hiding (cycle)
import Data.Ord
import Prelude hiding (abs,cycle)
import Unison.ABT
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put

instance (Foldable f, Serial a, Serial v, Ord v, Serial1 f) => Serial (Term f v a) where
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
