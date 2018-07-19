module Unison.Result where

import Control.Monad
import Data.Sequence (Seq)
import qualified Unison.Typechecker.Context as Context
import Unison.Paths (Path)
import Unison.Term (AnnotatedTerm)

data Result v loc a = Result { notes :: Seq (Note v loc), result :: Maybe a }
type Term v loc = AnnotatedTerm v loc

data Note v loc
  = Parsing String
  | InvalidPath Path (Term v loc) -- todo: move me!
  | UnknownSymbol v loc
  | Typechecking (Context.Note v loc)
  -- WithinLocals (Note v loc)

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
