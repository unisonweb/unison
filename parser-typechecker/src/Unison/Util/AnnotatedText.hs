{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}

module Unison.Util.AnnotatedText where

import           Data.Foldable      (asum, foldl')
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Sequence      (Seq ((:|>)))
import qualified Data.Sequence      as Seq
import           Data.String        (IsString (..))
import           Data.Void          (Void)
import           Safe               (headMay, lastMay)
import           Unison.Lexer       (Line, Pos (..))
import           Unison.Util.Monoid (intercalateMap)
import           Unison.Util.Range  (Range (..), inRange)

type AnnotatedText a = AnnotatedText' (Maybe a)
newtype AnnotatedText' a = AnnotatedText' (Seq (String, a))
  deriving (Functor, Foldable, Semigroup, Monoid)

  -- Quoted text (indented, with source line numbers) with annotated portions.
data AnnotatedExcerpt a = AnnotatedExcerpt
  { lineOffset  :: Line
  , text        :: String
  , annotations :: Map Range a
  } deriving (Functor)

newtype Rendered a = Rendered { rawRender :: Seq String }
  deriving (Semigroup, Monoid)

pairToText :: (String, a) -> AnnotatedText' a
pairToText (str, a) = AnnotatedText' . Seq.singleton $ (str, a)

pairToText' :: (String, a) -> AnnotatedText a
pairToText' (str, a) = pairToText (str, Just a)

trailingNewLine :: AnnotatedText a -> Bool
trailingNewLine (AnnotatedText' (init :|> (s,_))) =
  case lastMay s of
         Just '\n' -> True
         Just _    -> False
         _         -> trailingNewLine (AnnotatedText' init)
trailingNewLine _ = False

markup :: AnnotatedExcerpt a -> Map Range a -> AnnotatedExcerpt a
markup a r = a { annotations = r `Map.union` annotations a }

renderTextUnstyled :: AnnotatedText' a -> Rendered Void
renderTextUnstyled (AnnotatedText' chunks) = foldl' go mempty chunks
  where go r (text, _) = r <> fromString text

textLength :: AnnotatedText' a -> Int
textLength = length . show . renderTextUnstyled

textEmpty :: AnnotatedText' a -> Bool
textEmpty = (==0) . textLength

condensedExcerptToText :: Int -> AnnotatedExcerpt a -> AnnotatedText a
condensedExcerptToText margin e =
  intercalateMap "    .\n" excerptToText $ snipWithContext margin e

excerptToText :: forall a. AnnotatedExcerpt a -> AnnotatedText a
excerptToText e =
  track (Pos line1 1) [] (Map.toList $ annotations e) (renderLineNumber line1) (text e)
  where
    line1 :: Int
    line1 = lineOffset e
    renderLineNumber :: Int -> AnnotatedText a
    renderLineNumber n = fromString $ " " ++ spaces ++ sn ++ " | "
      where sn = show n
            spaces = replicate (lineNumberWidth - length sn) ' '
            lineNumberWidth = 4

    -- step through the source characters and annotations
    track _ _ _ rendered "" = rendered
    track _ _ _ rendered "\n" = rendered <> "\n"
    track pos@(Pos line col) stack annotations rendered _input@(c:rest) =
      let
        (poppedAnnotations, remainingAnnotations) =  span (inRange pos . fst) annotations
        -- drop any stack entries that will be closed after this char
        -- and add new stack entries
        stack' = foldl' pushColor stack0 poppedAnnotations
          where pushColor s (Range _ end, style) = (style, end) : s
                stack0 = dropWhile ((<=pos) . snd) stack
        maybeColor = fst <$> headMay stack'
        -- on new line, advance pos' vertically and set up line header
        -- additions :: AnnotatedText (Maybe a)
        pos' :: Pos
        (additions, pos') =
          if c == '\n'
          then ("\n" <> renderLineNumber (line + 1), Pos (line + 1) 1)
          else (pairToText ([c], maybeColor), Pos line (col + 1))
      in track pos' stack' remainingAnnotations (rendered <> additions) rest

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

instance IsString (AnnotatedText a) where
  fromString s = AnnotatedText' . pure $ (s, Nothing)

instance IsString (AnnotatedExcerpt a) where
  fromString s = AnnotatedExcerpt 1 s mempty

instance Show (Rendered a) where
  show (Rendered chunks) = asum chunks

instance IsString (Rendered a) where
  fromString s = Rendered (pure s)
