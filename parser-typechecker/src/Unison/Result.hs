{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Result where

import           Control.Monad
import           Control.Monad.Fail (MonadFail(..))
import           Control.Monad.Trans ( lift )
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer (Writer, runWriter, MonadWriter(..))
import           Data.Maybe
import           Data.Sequence ( Seq )
import qualified Unison.Parser as Parser
import           Unison.Paths ( Path )
import           Unison.Term ( AnnotatedTerm )
import qualified Unison.Typechecker.Context as Context

data Result notes a = Result { notes :: notes, result :: Maybe a }
  deriving Show

type Term v loc = AnnotatedTerm v loc

data Note v loc
  = Parsing (Parser.Err v)
  | InvalidPath Path (Term v loc) -- todo: move me!
  | UnknownSymbol v loc
  | TypeError (Context.ErrorNote v loc)
  | TypeInfo (Context.InfoNote v loc)
  deriving Show

isSuccess :: Result note a -> Bool
isSuccess r = isJust $ result r

isFailure :: Result note a -> Bool
isFailure r = isNothing $ result r

toMaybe :: Result note a -> Maybe a
toMaybe = result

toEither :: Result notes a -> Either notes a
toEither r = case result r of
  Nothing -> Left $ notes r
  Just a  -> Right a

fromParsing :: Either (Parser.Err v) a -> Result (Seq (Note v loc)) a
fromParsing (Left  e) = Result (pure $ Parsing e) Nothing
fromParsing (Right a) = pure a

fromTrans :: MaybeT (Writer notes) a -> Result notes a
fromTrans (runWriter . runMaybeT -> (r, n)) = Result n r

toTrans :: Monoid notes => Result notes a -> MaybeT (Writer notes) a
toTrans (Result notes may) = do
  lift $ tell notes
  MaybeT (pure may)

instance Functor (Result notes) where
  fmap f (Result notes res) = Result notes (f <$> res)

instance Monoid notes => Applicative (Result notes) where
  pure = return
  (<*>) = ap

instance Monoid notes => Monad (Result notes) where
  return a = Result mempty (Just a)
  Result notes Nothing >>= _f = Result notes Nothing
  Result notes (Just a) >>= f = case f a of
    Result notes2 b -> Result (notes `mappend` notes2) b

instance Monoid notes => MonadWriter notes (Result notes) where
  tell note = fromTrans $ tell note
  listen (Result notes may) = Result notes ((,notes) <$> may)
  pass = fromTrans . pass . toTrans

instance Monoid notes => MonadFail (Result notes) where
  fail _ = Result mempty Nothing

