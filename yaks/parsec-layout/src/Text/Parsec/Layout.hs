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
    ( block
    , vblock
    , vblock'
    , semi
    , vsemi
    , space
    , spaced
    , LayoutEnv
    , defaultLayoutEnv
    , HasLayoutEnv(..)
    , maybeFollowedBy
    , virtual_rbrace
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
                 trace (s++": " ++x) $ try $ char 'z'
                 fail x

traced :: (Stream s m Char, HasLayoutEnv u) =>
          [Char] -> ParsecT s u m b -> ParsecT s u m b
traced s p = do
  pTrace s
  ctx <- getEnv
  let !_ = trace ("ctx: " ++ show ctx) ()
  a <- p --  <|> trace (s ++ " backtracked") (fail s)
  -- let !x = trace (s ++ " succeeded") ()
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
--   let
--     x = 42
-- The column of `x` is pushed after `let` is parsed.
pushNextTokenContext :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m ()
pushNextTokenContext = traced "pushNextTokenContext" $ do
  indent <- getIndentation
  _ <- Parsec.Char.spaces
  col <- sourceColumn <$> getPosition
  pushContext . Layout $ max (indent+1) col

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
        case pos of
            LT -> do
                popContext "the offside rule"
                modifyEnv $ \env -> env { envBol = True }
                return VBrace
            EQ -> return VSemi
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

pushIncrementedContext :: (HasLayoutEnv u, Stream s m c) => ParsecT s u m ()
pushIncrementedContext = do
  env <- getEnv
  case envLayout env of
    [] -> pushContext (Layout 1)
    (Layout n : _) -> pushContext (Layout (n + 1))
    (NoLayout : _) -> pure ()

virtual_lbrace :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m ()
virtual_lbrace = pushNextTokenContext

virtual_rbrace :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m ()
virtual_rbrace = try (void $ lookAhead semi) <|> do
  allow <- inLayout
  when allow $ eof <|> try (layoutSatisfies (VBrace ==) <?> "outdent")

-- | Consumes one or more spaces, comments, and onside newlines in a layout rule.
space :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m String
space = traced "space" $ (do
    try $ layoutSatisfies (Other ' ' ==)
    return " "
  <?> "space")

vsemi :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m String
vsemi = do
    try $ layoutSatisfies p
    return ";"
  <?> "semicolon"
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

lbrace :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m String
lbrace = do
    char '{'
    pushContext NoLayout
    return "{"

rbrace :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m String
rbrace = do
    char '}'
    popContext "a right brace"
    return "}"

vblock' :: (HasLayoutEnv u, Stream s m Char)
        => ParsecT s u m ()
        -> ParsecT s u m a
        -> ParsecT s u m a
vblock' virtual_rbrace p = do
  prevEnvBol <- envBol <$> getEnv
  modifyEnv (\env -> env { envBol = True })
  a <- between (spaced virtual_lbrace) (spaced virtual_rbrace) p
  modifyEnv (\env -> env { envBol = prevEnvBol })
  pure a

vblock :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
vblock = vblock' virtual_rbrace

block :: (HasLayoutEnv u, Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
block p = braced p <|> vbraced p where
  braced s = between (try (spaced lbrace)) (spaced rbrace) s
  vbraced s = between (spaced virtual_lbrace) (spaced virtual_rbrace) s
  -- NB: virtual_lbrace here doesn't use current column for offside calc, instead
  -- uses 1 column greater than whatever column is at top of layout stack
  virtual_lbrace = do
    allow <- inLayout
    when allow pushIncrementedContext
