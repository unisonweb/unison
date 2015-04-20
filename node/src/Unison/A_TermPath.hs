{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.A_TermPath where

import Control.Applicative
import Data.Aeson.TH
import Data.Traversable
import Data.Vector ((!?), (//))
import qualified Data.Aeson as J
import qualified Data.Vector as Vector

data E
  = Fn -- ^ Points at function in a function application
  | Arg -- ^ Points at the argument of a function application
  | Body -- ^ Points at the body of a lambda
  | Index !Int -- ^ Points at the index of a vector
  deriving (Eq,Ord,Show)

newtype Path = Path [E] deriving (Eq,Ord)

instance Show Path where show (Path es) = show es

deriveJSON defaultOptions ''E

instance J.FromJSON Path where
  parseJSON (J.Array es) = Path . Vector.toList <$> traverse J.parseJSON es
  parseJSON j = fail $ "Path.parseJSON expected Object, got: " ++ show j

instance J.ToJSON Path where
  toJSON (Path es) = J.toJSON es
