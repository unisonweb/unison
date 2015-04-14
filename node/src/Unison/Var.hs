{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Var where

import Control.Applicative
import Data.Aeson

newtype Var = I Int deriving (Eq,Ord)

instance Show Var where
  show (I i) | i <= 0    = "t" ++ show (abs i)
  show (I i) | otherwise = "x" ++ show i

succ :: Var -> Var
succ (I i) = I (i + 1)

decr :: Var -> Var
decr (I i) = I (i - 1)

minv :: Var -> Var -> Var
minv (I i) (I j) = I (min i j)

nest :: Var -> Var -> Var
nest (I i) (I j) = I (i + j)

bound1 :: Var
bound1 = I 1

instance FromJSON Var where
  parseJSON j = I <$> parseJSON j

instance ToJSON Var where
  toJSON (I i) = toJSON i
