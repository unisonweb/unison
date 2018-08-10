{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Unison.Util.AnnotatedText where

import           Control.Arrow (second)
import           Data.Foldable (asum, foldl')
import           Data.Sequence (Seq)
import           Data.Sequence (Seq ((:|>)))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (IsString (..))
import           Data.Void (Void)
import           Safe (lastMay)
import           Unison.Lexer (Line, Pos (..))
import           Unison.Util.Monoid (intercalateMap)
import           Unison.Util.Range (Range (..))

newtype AnnotatedDocument a = AnnotatedDocument (Seq (Section a))

data Section a
  = Text (AnnotatedText (Maybe a))
  | Describe a
  | Blockquote (AnnotatedExcerpt a)
  deriving (Eq, Ord)

newtype AnnotatedText a = AnnotatedText (Seq (String, a)) deriving (Eq, Ord)

data AnnotatedExcerpt a = AnnotatedExcerpt
  { lineOffset  :: Line
  , text        :: String
  , annotations :: Set (Range, a)
  } deriving (Eq, Ord, Show)

newtype Rendered a = Rendered { rawRender :: Seq String } deriving (Eq)

sectionToDoc :: Section a -> AnnotatedDocument a
sectionToDoc = AnnotatedDocument . pure

textToDoc :: AnnotatedText (Maybe a) -> AnnotatedDocument a
textToDoc = AnnotatedDocument . pure . Text

excerptToDoc :: AnnotatedExcerpt a -> AnnotatedDocument a
excerptToDoc = AnnotatedDocument . pure . Blockquote

trailingNewLine :: AnnotatedText a -> Bool
trailingNewLine (AnnotatedText (init :|> (s,_))) =
  case lastMay s of
         Just '\n' -> True
         Just _    -> False
         _         -> trailingNewLine (AnnotatedText init)
trailingNewLine _ = False

markup :: Ord a => AnnotatedExcerpt a -> Set (Range, a) -> AnnotatedExcerpt a
markup a r = a { annotations = r `Set.union` annotations a }

renderTextUnstyled :: AnnotatedText a -> Rendered Void
renderTextUnstyled (AnnotatedText chunks) = foldl' go mempty chunks
  where go r (text, _) = r <> fromString text

splitAndRender :: Ord a
               => Int
               -> (AnnotatedExcerpt a -> Rendered b)
               -> AnnotatedExcerpt a -> Rendered b
splitAndRender n f e = intercalateMap "    .\n" f $ snipWithContext n e

_deoffsetRange :: Line -> Range -> Range
_deoffsetRange lineOffset (Range (Pos startLine startCol) (Pos endLine endCol)) =
  Range (Pos (startLine - lineOffset + 1) startCol)
        (Pos (endLine - lineOffset + 1) endCol)

-- | drops lines and replaces with "." if there are more than `n` unannotated
-- | lines in a row.
snipWithContext :: Ord a => Int -> AnnotatedExcerpt a -> [AnnotatedExcerpt a]
snipWithContext margin source =
  case foldl' whileWithinMargin
              (Nothing, mempty, mempty)
              (Set.toList $ annotations source) of
    (Nothing, _, _) -> []
    (Just (Range (Pos startLine' _) (Pos endLine' _)), group', rest') ->
      let dropLineCount = startLine' - lineOffset source
          takeLineCount = endLine' - startLine' + 1
          text', text2' :: [String]
          (text', text2') =
            splitAt takeLineCount (drop dropLineCount (lines (text source)))
      in AnnotatedExcerpt startLine' (unlines text') group'
        : snipWithContext
            margin (AnnotatedExcerpt (endLine' + 1) (unlines text2') rest')
  where
    withinMargin :: Range -> Range -> Bool
    withinMargin (Range _start1 (Pos end1 _)) (Range (Pos start2 _) _end2) =
      end1 + margin >= start2

    whileWithinMargin :: Ord a
                      => (Maybe Range, Set (Range, a), Set (Range, a))
                      -> (Range, a)
                      -> (Maybe Range, Set (Range, a), Set (Range, a))
    whileWithinMargin (r0, taken, rest) a@(r1,_) =
      case r0 of
        Nothing -> -- haven't processed any annotations yet
          (Just r1, Set.singleton a, mempty)
        Just r0 ->
          -- if all annotations so far can be joined without .. separations
          if null rest
          -- if this one can be joined to the new region without .. separation
          then if withinMargin r0 r1
            -- add it to the first set and grow the compare region
            then (Just $ r0 <> r1, Set.insert a taken, mempty)
            -- otherwise add it to the second set
            else (Just r0, taken, Set.singleton a)
          -- once we've added to the second set, anything more goes there too
          else (Just r0, taken, Set.insert a rest)

instance IsString (AnnotatedDocument a) where
  fromString = AnnotatedDocument . pure . fromString

instance IsString (Section a) where
  fromString = Text . fromString

instance IsString (AnnotatedText (Maybe a)) where
  fromString s = AnnotatedText . pure $ (s, Nothing)

instance Ord a => IsString (AnnotatedExcerpt a) where
  fromString s = AnnotatedExcerpt 1 s mempty

instance Semigroup (AnnotatedDocument a) where
  (<>) = mappend

instance Monoid (AnnotatedDocument a) where
  mempty = AnnotatedDocument mempty
  mappend (AnnotatedDocument chunks) (AnnotatedDocument chunks') =
    AnnotatedDocument (chunks <> chunks')

instance Semigroup (AnnotatedText a) where
  (<>) = mappend

instance Monoid (AnnotatedText a) where
  mempty = AnnotatedText mempty
  mappend (AnnotatedText chunks) (AnnotatedText chunks') =
    AnnotatedText (chunks <> chunks')

instance Functor AnnotatedText where
  fmap f (AnnotatedText chunks) = AnnotatedText (second f <$> chunks)

instance Show (Rendered a) where
  show (Rendered chunks) = asum chunks

instance Semigroup (Rendered a) where
  (<>) = mappend

instance Monoid (Rendered a) where
  mempty = Rendered mempty
  mappend (Rendered chunks) (Rendered chunks') = Rendered (chunks <> chunks')

instance IsString (Rendered a) where
  fromString s = Rendered (pure s)
