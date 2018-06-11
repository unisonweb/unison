{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# Language BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | These are Haskell-style layout combinators for parsec 3 by Edward Kmett,
-- first seen on StackOverflow <http://stackoverflow.com/a/3023615/33796>.
-- Should be fairly self-explanatory, with the following notes:
--
-- * You must use the provided `space` combinator to parse spaces.  This interacts poorly with
-- the "Text.Parsec.Token" modules, unfortunately.
--
-- * Uses \"\{\" and \"\}\" for explicit blocks.  This is hard-coded for the time being.

module Text.Parsec.Layout
    ( vblock'
    , vblockIncrement
    , vblockNextToken
    , semi
    , vsemi
    , space
    , spaced
    , LayoutEnv
    , defaultLayoutEnv
    , HasLayoutEnv(..)
    , maybeFollowedBy
    , virtual_rbrace
    , virtual_lbrace_increment
    , virtual_lbrace_nextToken
    , withoutLayout
    ) where

import Control.Applicative ((<$>))
import Control.Monad

import Data.Char (isSpace)

import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim hiding (State)
import Text.Parsec.Char hiding (space)
import qualified Text.Parsec.Char as Parsec.Char

import Debug.Trace
import Text.Parsec (anyChar)

pTrace :: Stream s m Char => [Char] -> ParsecT s u m ()
pTrace s = pt <|> return ()
    where pt = try $
               do
                 x <- try $ many anyChar
                 trace (s++": " ++ show x) $ try $ char 'z'
                 fail x

tracingEnabled :: Bool
tracingEnabled = False

traced :: (Stream s m Char, HasLayoutEnv u) =>
          [Char] -> ParsecT s u m b -> ParsecT s u m b
traced s p = if not tracingEnabled then p else do
  pTrace s
  ctx <- getEnv
  let !_ = trace ("ctx (before): " ++ show ctx) ()
  a <- p <|> trace (s ++ " backtracked") (fail s)
  ctx <- getEnv
  let !_ = trace ("ctx (after): " ++ show ctx) ()
  let !_ = trace (s ++ " succeeded") ()
  pure a

data LayoutContext = NoLayout | Layout Int deriving (Eq,Ord,Show)

-- | Keeps track of necessary context for layout parsers.
data LayoutEnv = Env
    { envLayout :: [LayoutContext]
    , envBol :: Bool -- if true, must run offside calculation
    } deriving (Show)

-- | For embedding layout information into a larger parse state.  Instantiate
-- this class if you need to use this together with other user state.
class HasLayoutEnv u where
    getLayoutEnv :: u -> LayoutEnv
    setLayoutEnv :: LayoutEnv -> u -> u

instance HasLayoutEnv LayoutEnv where
    getLayoutEnv = id
    setLayoutEnv = const

-- | A fresh layout.
defaultLayoutEnv :: LayoutEnv
defaultLayoutEnv = Env [] True

pushContext :: (HasLayoutEnv u, Stream s m c) => LayoutContext -> ParsecT s u m ()
pushContext ctx = modifyEnv $ \env -> env { envLayout = ctx:envLayout env }

modifyEnv :: (HasLayoutEnv u, Monad m) => (LayoutEnv -> LayoutEnv) -> ParsecT s u m ()
modifyEnv f = modifyState (\u -> setLayoutEnv (f $ getLayoutEnv u) u)

getEnv :: (HasLayoutEnv u, Monad m) => ParsecT s u m LayoutEnv
getEnv = getLayoutEnv <$> getState

popContext :: (HasLayoutEnv u, Stream s m c) => String -> ParsecT s u m ()
popContext loc = do
    (_:xs) <- envLayout <$> getEnv
    modifyEnv $ \env' -> env' { envLayout = xs }
  <|> unexpected ("empty context for " ++ loc)

getIndentation :: (HasLayoutEnv u, Stream s m c) => ParsecT s u m Int
getIndentation = depth . envLayout <$> getEnv where
    depth :: [LayoutContext] -> Int
    depth (Layout n:_) = n
    depth _ = 0

-- Pushes a column onto the layout stack determined by the column where
-- the next token begins. Ex:
--
--   let
--     x = 42
--
-- The column of `x` is pushed after `let` is parsed.
-- This may be less than the previous column at the top
-- of the layout stack, which allows for things like:
--
--   foo x y z = case x of
--     42 -> ...
--
-- Here, the `=` introduces a layout block at column 13 (the start of the `case`),
-- and the `of` introduces a layout block at column 3 (the start of the `42`).
pushNextTokenContext :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m ()
pushNextTokenContext = traced "pushNextTokenContext" $ do
  _ <- Parsec.Char.spaces
  col <- sourceColumn <$> getPosition
  pushContext . Layout $ col

maybeFollowedBy :: Stream s m c => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m a
t `maybeFollowedBy` x = do t' <- t; optional x; return t'

withoutLayout :: (HasLayoutEnv u, Stream s m c) => String -> ParsecT s u m a -> ParsecT s u m a
withoutLayout endMsg p =
  pushContext NoLayout *> (p <* popContext endMsg)

-- | @(\``maybeFollowedBy`\` space)@
spaced :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
spaced t = t `maybeFollowedBy` space

data Layout = VSemi | VBrace | Other Char deriving (Eq,Ord,Show)

-- TODO: Parse C-style #line pragmas out here
layout :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m Layout
layout = try $ do
    bol <- envBol <$> getEnv
    whitespace False (cont bol)
  where
    cont :: (HasLayoutEnv u, Stream s m Char) => Bool -> Bool -> ParsecT s u m Layout
    cont True = offside
    cont False = onside

    -- TODO: Parse nestable {-# LINE ... #-} pragmas in here
    whitespace :: (HasLayoutEnv u, Stream s m Char) =>
        Bool -> (Bool -> ParsecT s u m Layout) -> ParsecT s u m Layout
    whitespace x k =
            try (string "{-" >> nested k >>= whitespace True)
        <|> try comment
        <|> do newline; whitespace True offside
        <|> do tab; whitespace True k
        <|> do (satisfy isSpace <?> "space"); whitespace True k
        <|> k x

    comment :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m Layout
    comment = do
        string "--"
        many (satisfy ('\n'/=))
        newline
        whitespace True offside

    nested :: (HasLayoutEnv u, Stream s m Char) =>
        (Bool -> ParsecT s u m Layout) ->
        ParsecT s u m (Bool -> ParsecT s u m Layout)
    nested k =
            try (do string "-}"; return k)
        <|> try (do string "{-"; k' <- nested k; nested k')
        <|> do newline; nested offside
        <|> do anyChar; nested k

    offside :: (HasLayoutEnv u, Stream s m Char) => Bool -> ParsecT s u m Layout
    offside x = do
        p <- getPosition
        pos <- compare (sourceColumn p) <$> getIndentation
        isEof <- (True <$ smartEof) <|> pure False
        let returnVBrace = do
              popContext "the offside rule"
              modifyEnv $ \env -> env { envBol = True }
              return VBrace
        case pos of
          LT -> returnVBrace
          EQ -> if isEof then returnVBrace else return VSemi
          GT -> onside x

    -- we remained onside.
    -- If we skipped any comments, or moved to a new line and stayed onside, we return a single a ' ',
    -- otherwise we provide the next char
    onside :: (HasLayoutEnv u, Stream s m Char) => Bool -> ParsecT s u m Layout
    onside True = return $ Other ' '
    onside False = do
        modifyEnv $ \env -> env { envBol = False }
        Other <$> anyChar

layoutSatisfies :: (HasLayoutEnv u, Stream s m Char) => (Layout -> Bool) -> ParsecT s u m ()
layoutSatisfies p = guard . p =<< layout

inLayout :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m Bool
inLayout = do
  env <- getEnv
  pure $ case envLayout env of
    [] -> True
    (NoLayout:_) -> False
    (Layout _:_) -> True

pushIncrementedContext :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m ()
pushIncrementedContext = traced "pushIncrementedContext" $ do
  env <- getEnv
  case envLayout env of
    [] -> pushContext (Layout 2)
    (Layout n : _) -> pushContext (Layout (n + 1))
    (NoLayout : _) -> pure ()

vblockIncrement :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
vblockIncrement = vblock' pushIncrementedContext virtual_rbrace

vblockNextToken :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
vblockNextToken = vblock' pushNextTokenContext virtual_rbrace

virtual_lbrace_increment :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m ()
virtual_lbrace_increment = pushIncrementedContext

virtual_lbrace_nextToken :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m ()
virtual_lbrace_nextToken = pushNextTokenContext

virtual_rbrace :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m ()
virtual_rbrace = traced "virtual_rbrace" $ try (void $ lookAhead semi) <|> do
  allow <- inLayout
  when allow $
    (traced "eof" $ smartEof) <|> (traced "outdent" $ try (layoutSatisfies (VBrace ==) <?> "outdent"))

smartEof :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m ()
smartEof = try $ many (satisfy isSpace) *> eof

-- | Consumes one or more spaces, comments, and onside newlines in a layout rule.
space :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m String
space = do
    try $ layoutSatisfies (Other ' ' ==)
    return " "
  <?> "space"

vsemi :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m String
vsemi = traced "vsemi" $ (do
    try $ layoutSatisfies p
    return ";"
  <?> "semicolon")
  where
    p VSemi = True
    p _ = False

-- | Recognize a semicolon including a virtual semicolon in layout.
semi :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m String
semi = do
    try $ layoutSatisfies p
    return ";"
  <?> "semicolon"
  where
    p (Other ';') = True
    p _ = False

vblock' :: (HasLayoutEnv u, Stream s m Char)
        => ParsecT s u m l
        -> ParsecT s u m r
        -> ParsecT s u m a
        -> ParsecT s u m a
vblock' virtual_lbrace virtual_rbrace p = do
  prevEnvBol <- envBol <$> getEnv
  modifyEnv (\env -> env { envBol = True })
  a <- between (spaced virtual_lbrace) (spaced virtual_rbrace) p
  modifyEnv (\env -> env { envBol = prevEnvBol })
  pure a
