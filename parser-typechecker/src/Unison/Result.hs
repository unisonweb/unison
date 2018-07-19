module Unison.Result where

import Data.Maybe
import Control.Monad
import Data.Sequence (Seq)
import qualified Data.Foldable as Foldable
import qualified Unison.Typechecker.Context as Context
import Unison.Paths (Path)
import Unison.Reference (Reference)
import Unison.Term (AnnotatedTerm)

data Result v loc a = Result { notes :: Seq (Note v loc), result :: Maybe a }

type Term v loc = AnnotatedTerm v loc

data Note v loc
  = Parsing String
  | InvalidPath Path (Term v loc) -- todo: move me!
  | UnknownSymbol v loc
  | UnknownReference Reference
  | Typechecking (Context.Note v loc) deriving Show
  -- WithinLocals (Note v loc)

isSuccess :: Result v loc a -> Bool
isSuccess r = isJust $ result r

isFailure :: Result v loc a -> Bool
isFailure r = isNothing $ result r

toMaybe :: Result v loc a -> Maybe a
toMaybe = result

toEither :: Result v loc a -> Either [Note v loc] a
toEither r = case result r of
  Nothing -> Left (Foldable.toList $ notes r)
  Just a -> Right a

fromParsing :: Either String a -> Result v loc a
fromParsing (Left e) = Result (pure $ Parsing e) Nothing
fromParsing (Right a) = pure a

instance Functor (Result v loc) where
  fmap = liftM

instance Applicative (Result v loc) where
  pure = return
  (<*>) = ap

instance Monad (Result v loc) where
  return a = Result mempty (Just a)
  Result notes Nothing >>= _f = Result notes Nothing
  Result notes (Just a) >>= f = case f a of
    Result notes2 b -> Result (notes `mappend` notes2) b
