{-# LANGUAGE TemplateHaskell #-}

module Unison.Sqlite.Sql
  ( Sql (..),
    sql,

    -- * Exported for testing
    Param (..),
    ParsedLump (..),
    internalParseSql,
  )
where

import Control.Monad.Trans.State.Strict qualified as State
import Data.Char qualified as Char
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Text qualified as Text
import Database.SQLite.Simple qualified as Sqlite.Simple
import Database.SQLite.Simple.ToField qualified as Sqlite.Simple
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Text.Builder qualified
import Text.Builder qualified as Text (Builder)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec
import Unison.Prelude

-- | A SQL query.
data Sql = Sql
  { query :: Text,
    params :: [Sqlite.Simple.SQLData]
  }
  deriving stock (Show)

-- Template haskell, don't ask.

query__ :: Sql -> Text
query__ (Sql x _) = x

params__ :: Sql -> [Sqlite.Simple.SQLData]
params__ (Sql _ x) = x

-- | A quasi-quoter for producing a 'Sql' from a SQL query string, using the Haskell variables in scope for each named
-- parameter.
--
-- For example, the query
--
-- @
-- let qux = 5 :: Int
--
-- [sql|
--   SELECT foo
--   FROM bar
--   WHERE baz = :qux
-- |]
-- @
--
-- would produce a value like
--
-- @
-- Sql
--   { query = "SELECT foo FROM bar WHERE baz = ?"
--   , params = [SQLInteger 5]
--   }
-- @
--
-- which, of course, will require a @qux@ with a 'Sqlite.Simple.ToField' instance in scope.
--
-- There are five valid syntaxes for interpolating a variable:
--
--   * @:colon@, which denotes a single-field variable
--   * @\@at@, followed by 1+ bare @\@@, which denotes a multi-field variable
--   * @\$dollar@, which denotes an entire 'Sql' fragment
--   * @IN :colon@, which denotes an @IN@ expression, where the right-hand side is a list of scalars
--   * @VALUES :colon@, which denotes an entire @VALUES@ literal (1+ tuples)
--
-- As an example of the @\@at@ syntax, consider a variable @plonk@ with a two-field 'Sqlite.Simple.ToRow' instance. A
-- query that interpolates @plonk@ might look like:
--
-- @
-- [sql|
--   SELECT foo
--   FROM bar
--   WHERE stuff = \@plonk
--     AND other = \@
-- |]
-- @
--
-- As an example of @$dollar@ syntax,
--
-- @
-- let foo = [sql| bar |] in [sql| $foo baz |]
-- @
--
-- splices @foo@ into the second fragment, and is equivalent to
--
-- @
-- [sql| bar baz |]
-- @
--
-- As an example of @IN :colon@ syntax, the query
--
-- @
-- [sql| IN :foo |]
-- @
--
-- will require a list "foo" to be in scope, whose elements have `ToField` instances, and will expand to SQL that looks
-- like
--
-- @
-- IN (?, ?, ?, ?)
-- @
--
-- depending on how man elements "foo" has.
--
-- As an example of @VALUES :colon@ syntax, the query
--
-- @
-- [sql| VALUES :foo |]
-- @
--
-- will require a non-empty list "foo" to be in scope, whose elements have `ToRow` instances, and will expand to
-- SQL that looks like
--
-- @
-- VALUES (?, ?), (?, ?), (?, ?)
-- @
--
-- depending on how many elements "foo" has, and how wide its rows are.
sql :: TH.QuasiQuoter
sql = TH.QuasiQuoter sqlQQ undefined undefined undefined

sqlQQ :: String -> TH.Q TH.Exp
sqlQQ input =
  case internalParseSql (Text.pack input) of
    Left err -> fail err
    Right lumps -> do
      (sqlPieces, paramsPieces) <- unzip <$> for lumps unlump
      [|
        Sql
          (mconcat $(pure (TH.ListE sqlPieces)))
          (mconcat $(pure (TH.ListE paramsPieces)))
        |]
  where
    unlump :: ParsedLump -> TH.Q (TH.Exp, TH.Exp)
    unlump = \case
      ParsedOuterLump s params -> outerLump s params
      ParsedInnerLump s -> innerLump s
      ParsedInLump s -> inLump s
      ParsedValuesLump s -> valuesLump s

    -- Take an outer lump like
    --
    --   "foo ? ? ?"
    --   [FieldParam "bar", RowParam "qux" 2]
    --
    -- and resolve each parameter (field or row) to its corresponding list of SQLData, ultimately returning a pair like
    --
    --   "foo ? ? ?"
    --   mconcat [[SQLInteger 5], [SQLInteger 6, SQLInteger 7]])
    outerLump :: Text -> [Param] -> TH.Q (TH.Exp, TH.Exp)
    outerLump s params =
      (,) <$> TH.lift s <*> [|mconcat $(TH.ListE <$> for params paramToSqlData)|]
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

    inLump :: Text -> TH.Q (TH.Exp, TH.Exp)
    inLump var =
      TH.lookupValueName (Text.unpack var) >>= \case
        Nothing -> fail ("Not in scope: " ++ Text.unpack var)
        Just name -> (,) <$> [|inSql $(TH.varE name)|] <*> [|map Sqlite.Simple.toField $(TH.varE name)|]

    valuesLump :: Text -> TH.Q (TH.Exp, TH.Exp)
    valuesLump var =
      TH.lookupValueName (Text.unpack var) >>= \case
        Nothing -> fail ("Not in scope: " ++ Text.unpack var)
        Just name -> (,) <$> [|valuesSql $(TH.varE name)|] <*> [|foldMap Sqlite.Simple.toRow $(TH.varE name)|]

inSql :: (Sqlite.Simple.ToField a) => [a] -> Text
inSql scalars =
  Text.Builder.run ("IN (" <> b_commaSep (map (\_ -> b_qmark) scalars) <> b_rparen)

valuesSql :: (Sqlite.Simple.ToRow a) => List.NonEmpty a -> Text
valuesSql values =
  Text.Builder.run $
    "VALUES " <> b_commaSep (replicate (length values) (valueSql columns))
  where
    columns :: Int
    columns =
      length (Sqlite.Simple.toRow (List.NonEmpty.head values))

    valueSql :: Int -> Text.Builder
    valueSql columns =
      b_lparen <> b_commaSep (replicate columns b_qmark) <> b_rparen

data ParsedLump
  = ParsedOuterLump !Text ![Param]
  | ParsedInnerLump !Text
  | ParsedInLump !Text
  | ParsedValuesLump !Text
  deriving stock (Eq, Show)

-- | Parse a SQL string, and return the list of lumps. Exported only for testing.
internalParseSql :: Text -> Either String [ParsedLump]
internalParseSql input =
  case runP (parser <* Megaparsec.eof) (Text.strip input) of
    Left err -> Left (Megaparsec.errorBundlePretty err)
    Right ((), lumps) -> Right (map unlump (reverse lumps))
  where
    unlump = \case
      OuterLump sql params -> ParsedOuterLump (Text.Builder.run sql) (reverse params)
      InnerLump query -> ParsedInnerLump query
      InLump param -> ParsedInLump param
      ValuesLump param -> ParsedValuesLump param

-- Parser state.
--
-- A simple query, without query interpolation and without a VALUES param, is a single "outer lump", which contains:
--
--   * The SQL parsed so far, with
--       * params replaced by question marks (e.g. ":foo" becomes "?")
--       * run of whitespace normalized to one space
--       * comments stripped
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
-- We can also interpolate entire sql fragments, which we represent as an "inner lump". And finally, we can interpolate
-- VALUES literals whose length is only known at runtime, which we represent as a "values lump"
--
-- Putting it all together, here's a visual example. Note, too, that the lumps are actually stored in reverse order in
-- the parser state (because we simply cons lumps as we crawl along, which we reverse at the end).
--
--   [sql| one $two :three IN :four VALUES :five |]
--        ^    ^   ^       ^       ^^           ^
--        |    |   |       |       ||           |
--        |    |   |       |       ||           OuterLump " " []
--        |    |   |       |       ||
--        |    |   |       |       |ValuesLump "five"
--        |    |   |       |       |
--        |    |   |       |       OuterLump " " []
--        |    |   |       |
--        |    |   |       InLump "four"
--        |    |   |
--        |    |   OuterLump " ? " ["three"]
--        |    |
--        |    InnerLump "two"
--        |
--        OuterLump " one " []
--
data Lump
  = OuterLump !Text.Builder ![Param]
  | InnerLump !Text -- "$foo" ==> InnerLump "foo"
  | InLump !Text -- "IN :foo" ==> InLump "foo"
  | ValuesLump !Text -- "VALUES :foo" ==> ValuesLump "foo"

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
        b_qmark
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
    ColonParam param -> outer b_qmark \params -> pure (FieldParam (Text.Builder.run param) : params)
    DollarParam param -> do
      State.modify' (InnerLump (Text.Builder.run param) :)
      parser
    InParam param -> do
      State.modify' (InLump (Text.Builder.run param) :)
      parser
    ValuesParam param -> do
      State.modify' (ValuesLump (Text.Builder.run param) :)
      parser
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
  | NonParam !Text.Builder
  | AtParam !Text.Builder -- "@foo" ==> "foo"; "@" ==> ""
  | ColonParam !Text.Builder -- ":foo" ==> "foo"
  | DollarParam !Text.Builder -- "$foo" ==> "foo"
  | InParam !Text.Builder -- "IN :foo" ==> "foo"
  | ValuesParam !Text.Builder -- "VALUES :foo" ==> "foo"
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
      InParam <$> inParamP,
      ValuesParam <$> valuesParamP,
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
              && c /= '\'' -- 'string'
              && c /= '"' -- "identifier"
              && c /= ':' -- :param
              && c /= '@' -- @param
              && c /= '$' -- \$param
              && c /= '`' -- `identifier`
              && c /= '[' -- [identifier]
              && c /= '-' -- -- comment (maybe)
              && c /= '/' -- /* comment */ (maybe)
              && c /= 'I' -- IN :param (maybe)
              && c /= 'V' -- VALUES :param (maybe)
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

    inParamP :: P Text.Builder
    inParamP = do
      -- Use try (backtracking), so we can parse both:
      --
      --   * "IN :foo", an "in param", i.e. foo is a list of columns
      --   * "IN (...)", just normal unstructured IN expression, which can contain a subquery, function, etc
      --
      Megaparsec.try do
        _ <- Megaparsec.string "IN"
        whitespaceP
        colonParamP

    valuesParamP :: P Text.Builder
    valuesParamP = do
      -- Use try (backtracking), so we can parse both:
      --
      --   * "VALUES :foo", a "values param", i.e. foo is a non-empty list of rows
      --   * "VALUES (:foo)" or similar, just normal unstructured SQL with params n' stuff, or not
      --
      Megaparsec.try do
        _ <- Megaparsec.string "VALUES"
        whitespaceP
        colonParamP

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

-- Few common text builders

b_qmark :: Text.Builder
b_qmark = Text.Builder.char '?'

b_lparen :: Text.Builder
b_lparen = Text.Builder.char '('

b_rparen :: Text.Builder
b_rparen = Text.Builder.char ')'

b_commaSep :: [Text.Builder] -> Text.Builder
b_commaSep =
  Text.Builder.intercalate (Text.Builder.text ", ")
