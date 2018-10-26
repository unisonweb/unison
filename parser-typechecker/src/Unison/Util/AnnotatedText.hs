{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Unison.Util.AnnotatedText where

import           Data.Foldable      (asum, foldl')
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Sequence      (Seq ((:|>)))
import qualified Data.Sequence      as Seq
import           Data.String        (IsString (..))
import           Data.Void          (Void)
import           Safe               (lastMay)
import           Unison.Lexer       (Line, Pos (..))
import           Unison.Util.Monoid (intercalateMap)
import           Unison.Util.Range  (Range (..))

newtype AnnotatedDocument a = AnnotatedDocument (Seq (Section a))
  deriving (Functor, Semigroup, Monoid)

-- Prose with subsequences that may have an annotation.
-- A textual reference to an annotation style.
-- Quoted text (indented, with source line numbers) with annotated portions.

-- The reason the current API deals with a bunch of different types instead of
-- a single one is because not all of the combinators make sense on all of the
-- types.

-- Question: Should every bit of text be forced to have an annotation?
-- Answer: No.  That doesn't make sense for the Excerpt text — especially
--              in the context of multiple rendering options.
data Section a
  = Text (AnnotatedText (Maybe a))
  -- | NoBreak (AnnotatedText (Maybe a))
  | Blockquote (AnnotatedExcerpt a)
  deriving (Functor)

newtype AnnotatedText a = AnnotatedText (Seq (String, a))
  deriving (Functor, Foldable, Semigroup, Monoid)

data AnnotatedExcerpt a = AnnotatedExcerpt
  { lineOffset  :: Line
  , text        :: String
  , annotations :: Map Range a
  } deriving (Functor)

newtype Rendered a = Rendered { rawRender :: Seq String }
  deriving (Semigroup, Monoid)

sectionToDoc :: Section a -> AnnotatedDocument a
sectionToDoc = AnnotatedDocument . pure

pairToDoc :: (String, Maybe a) -> AnnotatedDocument a
pairToDoc (str, a) = textToDoc . AnnotatedText . Seq.singleton $ (str, a)

pairToDoc' :: (String, a) -> AnnotatedDocument a
pairToDoc' (str, a) = pairToDoc (str, Just a)

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

markup :: AnnotatedExcerpt a -> Map Range a -> AnnotatedExcerpt a
markup a r = a { annotations = r `Map.union` annotations a }

renderTextUnstyled :: AnnotatedText a -> Rendered Void
renderTextUnstyled (AnnotatedText chunks) = foldl' go mempty chunks
  where go r (text, _) = r <> fromString text

textLength :: AnnotatedText a -> Int
textLength = length . show . renderTextUnstyled

textEmpty :: AnnotatedText a -> Bool
textEmpty = (==0) . textLength

splitAndRender :: Int
               -> (AnnotatedExcerpt a -> Rendered b)
               -> AnnotatedExcerpt a -> Rendered b
splitAndRender n f e = intercalateMap "    .\n" f $ snipWithContext n e

-- | drops lines and replaces with "." if there are more than `n` unannotated
-- | lines in a row.
snipWithContext :: Int -> AnnotatedExcerpt a -> [AnnotatedExcerpt a]
snipWithContext margin source =
  case foldl' whileWithinMargin
              (Nothing, mempty, mempty)
              (Map.toList $ annotations source) of
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

    whileWithinMargin :: (Maybe Range, Map Range a, Map Range a)
                      -> (Range, a)
                      -> (Maybe Range, Map Range a, Map Range a)
    whileWithinMargin (r0, taken, rest) (r1,a1) =
      case r0 of
        Nothing -> -- haven't processed any annotations yet
          (Just r1, Map.singleton r1 a1, mempty)
        Just r0 ->
          -- if all annotations so far can be joined without .. separations
          if null rest
          -- if this one can be joined to the new region without .. separation
          then if withinMargin r0 r1
            -- add it to the first set and grow the compare region
            then (Just $ r0 <> r1, Map.insert r1 a1 taken, mempty)
            -- otherwise add it to the second set
            else (Just r0, taken, Map.singleton r1 a1)
          -- once we've added to the second set, anything more goes there too
          else (Just r0, taken, Map.insert r1 a1 rest)

instance IsString (AnnotatedDocument a) where
  fromString = AnnotatedDocument . pure . fromString

instance IsString (Section a) where
  fromString = Text . fromString

instance IsString (AnnotatedText (Maybe a)) where
  fromString s = AnnotatedText . pure $ (s, Nothing)

instance IsString (AnnotatedExcerpt a) where
  fromString s = AnnotatedExcerpt 1 s mempty

instance Show (Rendered a) where
  show (Rendered chunks) = asum chunks

instance IsString (Rendered a) where
  fromString s = Rendered (pure s)
