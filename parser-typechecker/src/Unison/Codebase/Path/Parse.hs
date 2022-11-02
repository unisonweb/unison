{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Path.Parse
  ( parsePath',
    parsePathImpl',
    parseSplit',
    definitionNameSegment,
    parseHQSplit,
    parseHQSplit',
    parseShortHashOrHQSplit',
    wordyNameSegment,
  )
where

import Control.Lens (over, _1)
import qualified Control.Lens as Lens
import Data.Bifunctor (first)
import Data.List.Extra (stripPrefix)
import qualified Data.Text as Text
import Unison.Codebase.Path
import qualified Unison.HashQualified' as HQ'
import Unison.NameSegment (NameSegment (NameSegment))
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude hiding (empty, toList)
import qualified Unison.ShortHash as SH
import qualified Unison.Syntax.Lexer as Lexer

-- .libs.blah.poo is Absolute
-- libs.blah.poo is Relative
-- Left is some parse error tbd
parsePath' :: String -> Either String Path'
parsePath' p = case parsePathImpl' p of
  Left e -> Left e
  Right (p, "") -> Right p
  Right (p, rem) -> case parseSegment rem of
    Right (seg, "") -> Right (unsplit' (p, NameSegment . Text.pack $ seg))
    Right (_, rem) ->
      Left ("extra characters after " <> show p <> ": " <> show rem)
    Left e -> Left e

-- implementation detail of parsePath' and parseSplit'
-- foo.bar.baz.34 becomes `Right (foo.bar.baz, "34")
-- foo.bar.baz    becomes `Right (foo.bar, "baz")
-- baz            becomes `Right (, "baz")
-- foo.bar.baz#a8fj becomes `Left`; we don't hash-qualify paths.
-- TODO: Get rid of this thing.
parsePathImpl' :: String -> Either String (Path', String)
parsePathImpl' p = case p of
  "." -> Right (Path' . Left $ absoluteEmpty, "")
  '.' : p -> over _1 (Path' . Left . Absolute . fromList) <$> segs p
  p -> over _1 (Path' . Right . Relative . fromList) <$> segs p
  where
    go f p = case f p of
      Right (a, "") -> case Lens.unsnoc (NameSegment.segments' $ Text.pack a) of
        Nothing -> Left "empty path"
        Just (segs, last) -> Right (NameSegment <$> segs, Text.unpack last)
      Right (segs, '.' : rem) ->
        let segs' = NameSegment.segments' (Text.pack segs)
         in Right (NameSegment <$> segs', rem)
      Right (segs, rem) ->
        Left $ "extra characters after " <> segs <> ": " <> show rem
      Left e -> Left e
    segs p = go parseSegment p

parseSegment :: String -> Either String (String, String)
parseSegment s =
  first show
    . (Lexer.wordyId0 <> Lexer.symbolyId0)
    <> unit'
    <> const (Left ("I expected an identifier but found " <> s))
    $ s

wordyNameSegment, definitionNameSegment :: String -> Either String NameSegment
wordyNameSegment s = case Lexer.wordyId0 s of
  Left e -> Left (show e)
  Right (a, "") -> Right (NameSegment (Text.pack a))
  Right (a, rem) ->
    Left $ "trailing characters after " <> show a <> ": " <> show rem

-- Parse a name segment like "()"
unit' :: String -> Either String (String, String)
unit' s = case stripPrefix "()" s of
  Nothing -> Left $ "Expected () but found: " <> s
  Just rem -> Right ("()", rem)

unit :: String -> Either String NameSegment
unit s = case unit' s of
  Right (_, "") -> Right $ NameSegment "()"
  Right (_, rem) -> Left $ "trailing characters after (): " <> show rem
  Left _ -> Left $ "I don't know how to parse " <> s

definitionNameSegment s = wordyNameSegment s <> symbolyNameSegment s <> unit s
  where
    symbolyNameSegment s = case Lexer.symbolyId0 s of
      Left e -> Left e
      Right (a, "") -> Right (NameSegment (Text.pack a))
      Right (a, rem) ->
        Left $ "trailing characters after " <> show a <> ": " <> show rem

-- parseSplit' wordyNameSegment "foo.bar.baz" returns Right (foo.bar, baz)
-- parseSplit' wordyNameSegment "foo.bar.+" returns Left err
-- parseSplit' definitionNameSegment "foo.bar.+" returns Right (foo.bar, +)
parseSplit' ::
  (String -> Either String NameSegment) ->
  String ->
  Either String Split'
parseSplit' lastSegment p = do
  (p', rem) <- parsePathImpl' p
  seg <- lastSegment rem
  pure (p', seg)

parseShortHashOrHQSplit' :: String -> Either String (Either SH.ShortHash HQSplit')
parseShortHashOrHQSplit' s =
  case Text.breakOn "#" $ Text.pack s of
    ("", "") -> error $ "encountered empty string parsing '" <> s <> "'"
    (n, "") -> do
      (p, rem) <- parsePathImpl' (Text.unpack n)
      seg <- definitionNameSegment rem
      pure $ Right (p, HQ'.NameOnly seg)
    ("", sh) -> do
      sh <- maybeToRight (shError s) . SH.fromText $ sh
      pure $ Left sh
    (n, sh) -> do
      (p, rem) <- parsePathImpl' (Text.unpack n)
      seg <- definitionNameSegment rem
      hq <-
        maybeToRight (shError s)
          . fmap (\sh -> (p, HQ'.HashQualified seg sh))
          . SH.fromText
          $ sh
      pure $ Right hq
  where
    shError s = "couldn't parse shorthash from " <> s

parseHQSplit :: String -> Either String HQSplit
parseHQSplit s = case parseHQSplit' s of
  Right (Path' (Right (Relative p)), hqseg) -> Right (p, hqseg)
  Right (Path' Left {}, _) ->
    Left $ "Sorry, you can't use an absolute name like " <> s <> " here."
  Left e -> Left e

parseHQSplit' :: String -> Either String HQSplit'
parseHQSplit' s = case Text.breakOn "#" $ Text.pack s of
  ("", "") -> error $ "encountered empty string parsing '" <> s <> "'"
  ("", _) -> Left "Sorry, you can't use a hash-only reference here."
  (n, "") -> do
    (p, rem) <- parsePath n
    seg <- definitionNameSegment rem
    pure (p, HQ'.NameOnly seg)
  (n, sh) -> do
    (p, rem) <- parsePath n
    seg <- definitionNameSegment rem
    maybeToRight (shError s)
      . fmap (\sh -> (p, HQ'.HashQualified seg sh))
      . SH.fromText
      $ sh
  where
    shError s = "couldn't parse shorthash from " <> s
    parsePath n = do
      x <- parsePathImpl' $ Text.unpack n
      pure $ case x of
        (Path' (Left e), "") | e == absoluteEmpty -> (relativeEmpty', ".")
        x -> x
