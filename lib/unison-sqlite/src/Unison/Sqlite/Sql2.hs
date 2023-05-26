{-# LANGUAGE TemplateHaskell #-}

module Unison.Sqlite.Sql2
  ( Sql2 (..),
    sql2,

    -- * Exported for testing
    Param (..),
    internalParseSql,
  )
where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Char as Char
import Data.Generics.Labels ()
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sqlite.Simple
import qualified Database.SQLite.Simple.ToField as Sqlite.Simple
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Builder
import qualified Text.Builder as Text (Builder)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import Unison.Prelude

-- | A SQL query.
data Sql2 = Sql2
  { query :: Text,
    params :: [Sqlite.Simple.SQLData]
  }
  deriving stock (Show)

-- Template haskell, don't ask.

query__ :: Sql2 -> Text
query__ (Sql2 x _) = x

params__ :: Sql2 -> [Sqlite.Simple.SQLData]
params__ (Sql2 _ x) = x

-- | A quasi-quoter for producing a 'Sql2' from a SQL query string, using the Haskell variables in scope for each named
-- parameter.
--
-- For example, the query
--
-- @
-- let qux = 5 :: Int
--
-- [sql2|
--   SELECT foo
--   FROM bar
--   WHERE baz = :qux
-- |]
-- @
--
-- would produce a value like
--
-- @
-- Sql2
--   { query = "SELECT foo FROM bar WHERE baz = ?"
--   , params = [SQLInteger 5]
--   }
-- @
--
-- which, of course, will require a @qux@ with a 'Sqlite.Simple.ToField' instance in scope.
--
-- There are three valid syntaxes for interpolating a variable:
--
--   * @:colon@, which denotes a single-field variable
--   * @\@at@, followed by 1+ bare @\@@, which denotes a multi-field variable
--   * @\$dollar@, which denotes an entire 'Sql2' fragment
--
-- As an example of the second, consider a variable @plonk@ with a two-field 'Sqlite.Simple.ToRow' instance. A query
-- that interpolates @plonk@ might look like:
--
-- @
-- [sql2|
--   SELECT foo
--   FROM bar
--   WHERE stuff = \@plonk
--     AND other = \@
-- |]
-- @
--
-- As an example of the third,
--
-- @
-- let foo = [sql2| bar |] in [sql2| $foo baz |]
-- @
--
-- is equivalent to
--
-- @
-- [sql2| bar baz |]
-- @
sql2 :: TH.QuasiQuoter
sql2 = TH.QuasiQuoter sql2QQ undefined undefined undefined

sql2QQ :: String -> TH.Q TH.Exp
sql2QQ input =
  case internalParseSql (Text.pack input) of
    Left err -> fail err
    Right lumps -> do
      (sqlPieces, paramsPieces) <- unzip <$> for lumps (either outerLump innerLump)
      [|
        Sql2
          (fold $(pure (TH.ListE sqlPieces)))
          (fold $(pure (TH.ListE paramsPieces)))
        |]
  where
    -- Take an outer lump like
    --
    --   ("foo ? ? ?", [FieldParam "bar", RowParam "qux" 2])
    --
    -- and resolve each parameter (field or row) to its corresponding list of SQLData, ultimately returning a pair like
    --
    --   ("foo ? ? ?", fold [[SQLInteger 5], [SQLInteger 6, SQLInteger 7]]) :: (Text, [SQLData])
    outerLump :: (Text, [Param]) -> TH.Q (TH.Exp, TH.Exp)
    outerLump (s, params) =
      (,) <$> TH.lift s <*> [|fold $(TH.ListE <$> for params paramToSqlData)|]
      where
        paramToSqlData :: Param -> TH.Q TH.Exp
        paramToSqlData = \case
          FieldParam var ->
            TH.lookupValueName (Text.unpack var) >>= \case
              Nothing -> fail ("Not in scope: " ++ Text.unpack var)
              Just name -> [|[Sqlite.Simple.toField $(TH.varE name)]|]
          RowParam var _count ->
            TH.lookupValueName (Text.unpack var) >>= \case
              Nothing -> fail ("Not in scope: " ++ Text.unpack var)
              Just name -> [|Sqlite.Simple.toRow $(TH.varE name)|]

    innerLump :: Text -> TH.Q (TH.Exp, TH.Exp)
    innerLump var =
      TH.lookupValueName (Text.unpack var) >>= \case
        Nothing -> fail ("Not in scope: " ++ Text.unpack var)
        Just name -> (,) <$> [|query__ $(TH.varE name)|] <*> [|params__ $(TH.varE name)|]

-- | Parse a SQL string, and return the list of lumps, where each Left is a prettefied SQL string along with the named
-- parameters it contains, and each Right is the name of a query to be interpolated.
--
-- Exported only for testing.
internalParseSql :: Text -> Either String [Either (Text, [Param]) Text]
internalParseSql input =
  case runP (parser <* Megaparsec.eof) (Text.strip input) of
    Left err -> Left (Megaparsec.errorBundlePretty err)
    Right ((), lumps) -> Right (map unlump (reverse lumps))
  where
    unlump = \case
      OuterLump sql params -> Left (Text.Builder.run sql, reverse params)
      InnerLump query -> Right query

-- Parser state.
--
-- A simple query (without query interpolation) is a single "outer lump", which contains:
--
--   * The SQL parsed so far
--   * A list of parameter names in reverse order
--
-- For example, if we were partway through parsing the query
--
--   SELECT foo
--   FROM bar
--   WHERE baz = :bonk AND qux = 'monk'
--
-- then we would have an outer lump that looks like
--
--   OuterLump
--     "SELECT foo FROM bar WHERE baz = ? AND "
--     [FieldParam "bonk"]
--
-- There are two ways to specify parameters:
--
--   1. Field parameters like ":bonk", which get turned into a single SQLite parameter (via `toField`)
--   2. Row parameters like "@whonk", followed by 1+ "@", which get turned into that many SQLite parameters (via
--      `toRow`)
--
-- Why keep the SQL parsed so far:
--
--   1. We need to replace variables with question marks.
--   2. We can make the query slightly prettier by replacing all runs of 1+ characters of whitespace with a single
--      space. This lets us write vertically aligned SQL queries at arbitrary indentations in Haskell quasi-quoters,
--      but not have to look at a bunch of "\n        " in debug logs and such.
--   3. We strip comments.
--
-- More generally, the full parser state tracks a list of alternating "outer" and "inner" lumps (in reverse order),
-- where an inner lump is simply a single Haskell variable name, and denotes a sql query to be interpolated into the
-- query.
--
-- Example:
--
--   [sql| one $two :three $four |]
--        ^    ^   ^       ^    ^
--        |    |   |       |    |
--        |    |   |       |    ` OuterLump " " []
--        |    |   |       |
--        |    |   |       ` InnerLump "four"
--        |    |   |
--        |    |   ` OuterLump " ? " ["three"]
--        |    |
--        |    ` InnerLump "two"
--        |
--        ` OuterLump " one " []
--
data Lump
  = OuterLump !Text.Builder ![Param]
  | InnerLump !Text -- \$foo ==> InnerLump "foo"

data Param
  = FieldParam !Text -- :foo ==> FieldParam "foo"
  | RowParam !Text !Int -- @bar @ @ ==> RowParam "bar" 3
  deriving stock (Eq, Show)

type P a =
  State.StateT [Lump] (Megaparsec.Parsec Void Text) a

runP :: P a -> Text -> Either (Megaparsec.ParseErrorBundle Text Void) (a, [Lump])
runP p =
  Megaparsec.runParser (State.runStateT p []) ""

-- Parser for a SQL query (stored in the parser state).
parser :: P ()
parser = do
  fragmentParser >>= \case
    Comment -> parser
    NonParam fragment -> outer fragment pure
    AtParam param ->
      outer
        qmark
        -- Either we parsed a bare "@", in which case we want to bump the int count of the latest field we walked over
        -- (which must be a RowField, otherwise the query is invalid as it begins some string of @-params with a bare
        -- @), or we parsed a new "@foo@ row param
        let param1 = Text.Builder.run param
         in if Text.null param1
              then \case
                RowParam name count : params -> do
                  let !count' = count + 1
                  pure (RowParam name count' : params)
                _ -> fail ("Invalid query: encountered unnamed-@ without a preceding named-@, like `@foo`")
              else \params -> pure (RowParam param1 1 : params)
    ColonParam param -> field param
    DollarParam param -> inner param
    Whitespace -> outer (Text.Builder.char ' ') pure
    EndOfInput -> pure ()
  where
    outer :: Text.Builder -> ([Param] -> P [Param]) -> P ()
    outer s g = do
      State.get >>= \case
        OuterLump sql params : lumps -> do
          let !sql' = sql <> s
          params' <- g params
          State.put (OuterLump sql' params' : lumps)
        lumps -> do
          params <- g []
          State.put (OuterLump s params : lumps)
      parser

    inner :: Text.Builder -> P ()
    inner param = do
      State.modify' (InnerLump (Text.Builder.run param) :)
      parser

    field :: Text.Builder -> P ()
    field param =
      outer qmark \params -> pure (FieldParam (Text.Builder.run param) : params)

    qmark = Text.Builder.char '?'

-- A single fragment, where a list of fragments (always ending in EndOfFile) makes a whole query.
--
-- The query
--
--   SELECT foo
--   FROM   bar
--   WHERE  baz = :bonk AND qux = 'monkey monk'
--
-- corresponds to the fragments
--
--   [ NonParam "SELECT"
--   , Whitespace
--   , NonParam "foo"
--   , Whitespace
--   , NonParam "FROM"
--   , Whitespace
--   , NonParam "bar"
--   , Whitespace
--   , NonParam "WHERE"
--   , Whitespace
--   , NonParam "baz"
--   , Whitespace
--   , NonParam "="
--   , Whitespace
--   , ColonParam "bonk"
--   , Whitespace
--   , NonParam "AND"
--   , Whitespace
--   , NonParam "qux"
--   , Whitespace
--   , NonParam "="
--   , Whitespace
--   , NonParam "'monkey monk'"
--   , EndOfInput
--   ]
--
-- Any sequence of consecutive NonParam fragments in such a list is equivalent to a single NonParam fragment with the
-- contents concatenated. How the non-parameter stuff between parameters is turned into 1+ NonParam fragments is just a
-- consequence of how we parse these SQL strings: identify strings and such, but otherwise make no attempt to
-- understand the structure of the query.
--
-- A parsed query can be reconstructed by simply concatenating all fragments together, with a colon character ':'
-- prepended to each Param fragment.
data Fragment
  = Comment -- we toss these, so we don't bother remembering the contents
  | NonParam Text.Builder
  | AtParam Text.Builder -- builder may be empty
  | ColonParam Text.Builder -- builder is non-empty
  | DollarParam Text.Builder -- builder is non-empty
  | Whitespace
  | EndOfInput

fragmentParser :: P Fragment
fragmentParser =
  asum
    [ Whitespace <$ whitespaceP,
      NonParam <$> betwixt "string" '\'',
      NonParam <$> betwixt "identifier" '"',
      NonParam <$> betwixt "identifier" '`',
      NonParam <$> bracketedIdentifierP,
      Comment <$ lineCommentP,
      Comment <$ blockCommentP,
      ColonParam <$> colonParamP,
      AtParam <$> atParamP,
      DollarParam <$> dollarParamP,
      NonParam <$> unstructuredP,
      EndOfInput <$ Megaparsec.eof
    ]
  where
    -- It's not clear if there is *no* syntax for escaping a literal ] character from an identifier between brackets
    -- that looks like [this], but the documentation here doesn't mention any, and (brief) experimentation at the
    -- sqlite3 repl didn't reveal any.
    --
    -- So this parser is simple: left bracket, stuff, right bracket.
    bracketedIdentifierP :: P Text.Builder
    bracketedIdentifierP = do
      x <- char '['
      ys <- Megaparsec.takeWhile1P (Just "identifier") (/= ']')
      z <- char ']'
      pure (x <> Text.Builder.text ys <> z)

    lineCommentP :: P ()
    lineCommentP = do
      _ <- Megaparsec.string "--"
      _ <- Megaparsec.takeWhileP (Just "comment") (/= '\n')
      -- Eat whitespace after a line comment just so we don't end up with [Whitespace, Comment, Whitespace] fragments,
      -- which would get serialized as two consecutive spaces
      whitespaceP

    blockCommentP :: P ()
    blockCommentP = do
      _ <- Megaparsec.string "/*"
      let loop = do
            _ <- Megaparsec.takeWhileP (Just "comment") (/= '*')
            Megaparsec.string "*/" <|> (Megaparsec.anySingle >> loop)
      _ <- loop
      -- See whitespace-eating comment above
      whitespaceP

    unstructuredP :: P Text.Builder
    unstructuredP = do
      x <- Megaparsec.anySingle
      xs <-
        Megaparsec.takeWhileP
          (Just "sql")
          \c ->
            not (Char.isSpace c)
              && c /= '\''
              && c /= '"'
              && c /= ':'
              && c /= '@'
              && c /= '$'
              && c /= '`'
              && c /= '['
              && c /= '-'
              && c /= '/'
      pure (Text.Builder.char x <> Text.Builder.text xs)

    -- Parse either "@foobar" or just "@"
    atParamP :: P Text.Builder
    atParamP = do
      _ <- Megaparsec.char '@'
      haskellVariableP <|> pure mempty

    colonParamP :: P Text.Builder
    colonParamP = do
      _ <- Megaparsec.char ':'
      haskellVariableP

    dollarParamP :: P Text.Builder
    dollarParamP = do
      _ <- Megaparsec.char '$'
      haskellVariableP

    haskellVariableP :: P Text.Builder
    haskellVariableP = do
      x <- Megaparsec.satisfy (\c -> Char.isAlpha c || c == '_')
      xs <- Megaparsec.takeWhileP (Just "parameter") \c -> Char.isAlphaNum c || c == '_' || c == '\''
      pure (Text.Builder.char x <> Text.Builder.text xs)

    whitespaceP :: P ()
    whitespaceP = do
      void (Megaparsec.takeWhile1P (Just "whitepsace") Char.isSpace)

-- @betwixt name c@ parses a @c@-surrounded string of arbitrary characters (naming the parser @name@), where two @c@s
-- in a row inside the string is the syntax for a single @c@. This is simply how escaping works in SQLite for
-- single-quoted things (strings), double-quoted things (usually identifiers, but weirdly, SQLite lets you quote
-- strings this way sometimes, probably because people don't know about single-quote syntax), and backtick-quoted
-- things (identifiers).
--
-- That is,
--
--   - 'foo''bar' denotes the string foo'bar
--   - "foo""bar" denotes the identifier foo"bar
--   - `foo``bar` denotes the idetifier foo`bar
--
-- This function returns the quoted thing *with* the surrounding quotes, and *retaining* any double-quoted things
-- within. For example, @betwixt "" '`'@ applied to the string "`foo``bar`" will return the full string "`foo``bar`".
--
-- This implementation is stolen from our own Travis Staton's @hasql-interpolate@ package, but tweaked a bit.
betwixt :: String -> Char -> P Text.Builder
betwixt name quote = do
  startQuote <- quoteP
  let loop sofar = do
        content <- Megaparsec.takeWhileP (Just name) (/= quote)
        Megaparsec.notFollowedBy Megaparsec.eof
        let escapedQuoteAndMore = do
              escapedQuote <- Megaparsec.try ((<>) <$> quoteP <*> quoteP)
              loop (sofar <> Text.Builder.text content <> escapedQuote)
        let allDone = do
              endQuote <- quoteP
              pure (sofar <> Text.Builder.text content <> endQuote)
        escapedQuoteAndMore <|> allDone
  loop startQuote
  where
    quoteP =
      char quote

char :: Char -> P Text.Builder
char c =
  Megaparsec.char c $> Text.Builder.char c
