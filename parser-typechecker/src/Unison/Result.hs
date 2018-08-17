{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Result where

import           Control.Monad
import           Control.Monad.Fail (MonadFail(..))
import           Control.Monad.Trans ( lift )
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer (Writer, runWriter, MonadWriter(..))
import qualified Data.Foldable as Foldable
import           Data.Maybe
import           Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import qualified Unison.Parser as Parser
import           Unison.Paths ( Path )
import           Unison.Term ( AnnotatedTerm )
import qualified Unison.Typechecker.Context as Context

data Result note a = Result { notes :: Seq note, result :: Maybe a }
  deriving Show

type Term v loc = AnnotatedTerm v loc

data Note v loc
  = Parsing (Parser.Err v)
  | InvalidPath Path (Term v loc) -- todo: move me!
  | UnknownSymbol v loc
--  | UnknownReference Reference
  | Typechecking (Context.Note v loc) deriving Show
  -- WithinLocals (Note v loc)

isSuccess :: Result note a -> Bool
isSuccess r = isJust $ result r

isFailure :: Result note a -> Bool
isFailure r = isNothing $ result r

toMaybe :: Result note a -> Maybe a
toMaybe = result

toEither :: Result note a -> Either [note] a
toEither r = case result r of
  Nothing -> Left (Foldable.toList $ notes r)
  Just a  -> Right a

fromParsing :: Either (Parser.Err v) a -> Result (Note v loc) a
fromParsing (Left  e) = Result (pure $ Parsing e) Nothing
fromParsing (Right a) = pure a

failNote :: note -> Result note a
failNote note = do
  tell $ Seq.singleton note
  Control.Monad.Fail.fail ""

fromTrans :: MaybeT (Writer (Seq note)) a -> Result note a
fromTrans (runWriter . runMaybeT -> (r, n)) = Result n r

toTrans :: Result note a -> MaybeT (Writer (Seq note)) a
toTrans (Result notes may) = do
  lift $ tell notes
  MaybeT (pure may)

instance Functor (Result note) where
  fmap = liftM

instance Applicative (Result note) where
  pure = return
  (<*>) = ap

instance Monad (Result note) where
  return a = Result mempty (Just a)
  Result notes Nothing >>= _f = Result notes Nothing
  Result notes (Just a) >>= f = case f a of
    Result notes2 b -> Result (notes `mappend` notes2) b

instance (MonadWriter (Seq note)) (Result note) where
  tell note = fromTrans $ tell note
  listen (Result notes may) = Result notes ((,notes) <$> may)
  pass = fromTrans . pass . toTrans

instance MonadFail (Result note) where
  fail _ = Result Seq.empty Nothing
