{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

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

import Unison.Prelude hiding (empty, toList)

import Unison.Codebase.Path

import Control.Lens (_1, over)
import qualified Control.Lens as Lens
import Data.Bifunctor (first, bimap)
import Data.List.Extra (stripPrefix)
import qualified Data.Text as Text
import qualified Unison.HashQualified' as HQ'
import qualified Unison.Lexer as Lexer
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (NameSegment))
import qualified Unison.ShortHash as SH

-- .libs.blah.poo is Absolute
-- libs.blah.poo is Relative
-- Left is some parse error tbd
parsePath' :: String -> Either String UnknownPath
parsePath' p = case parsePathImpl' p of
  Left  e        -> Left e
  Right (p, "" ) -> Right p
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
parsePathImpl' :: String -> Either String (UnknownPath, String)
parsePathImpl' p = case p of
  "."     -> Right (Path' . Left $ absoluteEmpty, "")
  '.' : p -> over _1 (Path' . Left . Absolute . fromList) <$> segs p
  p       -> over _1 (Path' . Right . Relative . fromList) <$> segs p
 where
  go p = case parseSegment p of
    Right (a, "") -> case Lens.unsnoc (Name.segments' $ Text.pack a) of
      Nothing           -> Left "empty path"
      Just (segs, last) -> Right (NameSegment <$> segs, Text.unpack last)
    Right (segs, '.' : rem) ->
      let segs' = Name.segments' (Text.pack segs)
      in  Right (NameSegment <$> segs', rem)
    Right (segs, rem) ->
      Left $ "extra characters after " <> segs <> ": " <> show rem
    Left e -> Left e

parseSegment :: String -> Either String (String, String)
parseSegment s =
  first show
    .  (Lexer.wordyId <> Lexer.symbolyId)
    <> unit'
    <> const (Left ("I expected an identifier but found " <> s))
    $  s

wordyNameSegment, definitionNameSegment :: String -> Either String NameSegment
wordyNameSegment s = case Lexer.wordyId0 s of
  Left e -> Left (show e)
  Right (a, "") -> Right (NameSegment (Text.pack a))
  Right (a, rem) ->
    Left $ "trailing characters after " <> show a <> ": " <> show rem

-- Parse a name segment like "()"
unit' :: String -> Either String (String, String)
unit' s = case stripPrefix "()" s of
  Nothing  -> Left $ "Expected () but found: " <> s
  Just rem -> Right ("()", rem)

unit :: String -> Either String NameSegment
unit s = case unit' s of
  Right (_, "" ) -> Right $ NameSegment "()"
  Right (_, rem) -> Left $ "trailing characters after (): " <> show rem
  Left  _        -> Left $ "I don't know how to parse " <> s


definitionNameSegment s = wordyNameSegment s <> symbolyNameSegment s <> unit s
 where
  symbolyNameSegment s = case Lexer.symbolyId0 s of
    Left  e       -> Left (show e)
    Right (a, "") -> Right (NameSegment (Text.pack a))
    Right (a, rem) ->
      Left $ "trailing characters after " <> show a <> ": " <> show rem

-- parseSplit' wordyNameSegment "foo.bar.baz" returns Right (foo.bar, baz)
-- parseSplit' wordyNameSegment "foo.bar.+" returns Left err
-- parseSplit' definitionNameSegment "foo.bar.+" returns Right (foo.bar, +)
parseSplit' :: (String -> Either String NameSegment)
            -> String
            -> Either String (Split t)
parseSplit' lastSegment p = do
  (p', rem) <- parsePathImpl' p
  seg <- lastSegment rem
  pure (p', seg)

parseShortHashOrHQSplit' :: String -> Either String (Either SH.ShortHash (HQSplit t))
parseShortHashOrHQSplit' s =
  case Text.breakOn "#" $ Text.pack s of
    ("","") -> error $ "encountered empty string parsing '" <> s <> "'"
    (n,"") -> do
      (p, rem) <- parsePathImpl' (Text.unpack n)
      seg <- definitionNameSegment rem
      pure $ Right (p, HQ'.NameOnly seg)
    ("", sh) -> do
      sh <- maybeToRight (shError s) . SH.fromText $ sh
      pure $ Left sh
    (n, sh) -> do
      (p, rem) <- parsePathImpl' (Text.unpack n)
      seg <- definitionNameSegment rem
      hq <- maybeToRight (shError s) .
        fmap (\sh -> (p, HQ'.HashQualified seg sh)) .
        SH.fromText $ sh
      pure $ Right hq
  where
  shError s = "couldn't parse shorthash from " <> s

parseHQSplit :: String -> Either String (HQSplit 'Relative)
parseHQSplit s = do
  parseHQSplit' s >>= \case
    Left (AbsolutePath{}, _) ->
      Left $ "Sorry, you can't use an absolute name like " <> s <> " here."
    Right (relPath, hqseg) -> Right (relPath, hqseg)

parseHQSplit' :: String -> Either String (Either (HQSplit 'Absolute) (HQSplit 'Relative))
parseHQSplit' s = case Text.breakOn "#" $ Text.pack s of
  ("", "") -> error $ "encountered empty string parsing '" <> s <> "'"
  ("", _ ) -> Left "Sorry, you can't use a hash-only reference here."
  (n , "") -> do
    (p, rem) <- parsePath n
    seg      <- definitionNameSegment rem
    let makeSplit :: Path t -> HQSplit t
        makeSplit p = (p, HQ'.NameOnly seg)
    pure (bimap makeSplit makeSplit p)
  (n, shText) -> do
    (p, rem) <- parsePath n
    seg      <- definitionNameSegment rem
    case SH.fromText shText of
      Nothing -> Left $ "couldn't parse shorthash from " <> s
      Just sh' -> do
        let makeSplit :: Path t -> HQSplit t
            makeSplit p = (p, HQ'.HashQualified seg sh')
        pure (bimap makeSplit makeSplit p)
 where
  parsePath n = do
    parsePathImpl' (Text.unpack n) >>= \case
      (Left (AbsolutePath Empty), "") -> pure (Right emptyRelative, ".")
      x -> pure x
