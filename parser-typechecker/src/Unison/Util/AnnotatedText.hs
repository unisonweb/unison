module Unison.Util.AnnotatedText where

import           Control.Arrow     (second)
import           Data.Sequence     (Seq)
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Data.String       (IsString (..))
import           Unison.Lexer      (Line)
import           Unison.Util.Range (Range)

newtype AnnotatedDocument a = AnnotatedDocument (Seq (Section a))
newtype AnnotatedText a = AnnotatedText (Seq (String, a)) deriving (Eq, Ord)

data Section a
  = Text (AnnotatedText (Maybe a))
  | Blockquote (AnnotatedExcerpt a)
  deriving (Eq, Ord)

data AnnotatedExcerpt a = AnnotatedExcerpt
  { lineOffset  :: Line
  , text        :: String
  , annotations :: Set (Range, a)
  } deriving (Eq, Ord, Show)

markup :: Ord a => AnnotatedExcerpt a -> Set (Range, a) -> AnnotatedExcerpt a
markup a r = a { annotations = r `Set.union` (annotations a) }

instance Ord a => IsString (AnnotatedExcerpt a) where
  fromString s = AnnotatedExcerpt 1 s mempty

instance Semigroup (AnnotatedDocument a) where
  (<>) = mappend

instance Monoid (AnnotatedDocument a) where
  mempty = AnnotatedDocument mempty
  mappend (AnnotatedDocument chunks) (AnnotatedDocument chunks') =
    AnnotatedDocument (chunks <> chunks')

instance Functor AnnotatedText where
  fmap f (AnnotatedText chunks) = AnnotatedText $ (second f <$> chunks)
