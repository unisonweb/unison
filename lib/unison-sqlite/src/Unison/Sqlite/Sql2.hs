{-# LANGUAGE TemplateHaskell #-}

module Unison.Sqlite.Sql2
  ( Sql2 (..),
    sql2,

    -- * Exported for testing
    internalParseSql,
  )
where

import Control.Lens ((%=), (<>=))
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Char as Char
import Data.Generics.Labels ()
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sqlite.Simple
import qualified Database.SQLite.Simple.ToField as Sqlite.Simple
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
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
-- There are three valid syntaxes for interpolating a variable: @:colon@, @\@at@, and @\$dollar@.
sql2 :: TH.QuasiQuoter
sql2 = TH.QuasiQuoter sql2QQ undefined undefined undefined

sql2QQ :: String -> TH.Q TH.Exp
sql2QQ input =
  case internalParseSql (Text.pack input) of
    Left err -> fail err
    Right (query, params0) -> do
      let params :: [TH.Q TH.Exp]
          params =
            map
              ( \var ->
                  TH.lookupValueName (Text.unpack var) >>= \case
                    Nothing -> fail ("Not in scope: " ++ Text.unpack var)
                    Just name -> [|Sqlite.Simple.toField $(TH.varE name)|]
              )
              params0
      [|Sql2 query $(TH.listE params)|]

-- | Parse a SQL string, and return the prettefied SQL string along with the named parameters it contains.
--
-- Exported only for testing.
internalParseSql :: Text -> Either String (Text, [Text])
internalParseSql input =
  case runP (parser <* Megaparsec.eof) (Text.strip input) of
    Left err -> Left (Megaparsec.errorBundlePretty err)
    Right ((), S {sql, params}) -> Right (Text.Builder.run sql, reverse params)

-- Parser state: the SQL parsed so far, and a list of parameter names (in reverse order).
--
-- For example, if we were partway through parsing the query
--
--   SELECT foo
--   FROM bar
--   WHERE baz = :bonk AND qux = 'monk'
--                         ^
--
-- then we would have the state
--
--   S
--     { sql = "SELECT foo FROM bar WHERE baz = ? AND "
--     , params = ["bonk"]
--     }
--
-- Why keep the SQL parsed so far:
--
--   1. We need to replace variables with question marks.
--   2. We can make the query slightly prettier by replacing all runs of 1+ characters of whitespace with a single
--      space. This lets us write vertically aligned SQL queries at arbitrary indentations in Haskell quasi-quoters,
--      but not have to look at a bunch of "\n        " in debug logs and such.
--   3. We strip comments.
data S = S
  { sql :: !Text.Builder,
    params :: [Text]
  }
  deriving stock (Generic)

type P a =
  State.StateT S (Megaparsec.Parsec Void Text) a

runP :: P a -> Text -> Either (Megaparsec.ParseErrorBundle Text Void) (a, S)
runP p =
  Megaparsec.runParser (State.runStateT p (S mempty [])) ""

-- Parser for a SQL query (stored in the parser state).
parser :: P ()
parser = do
  fragmentParser >>= \case
    Comment -> parser
    NonParam fragment -> do
      #sql <>= fragment
      parser
    Param param -> do
      #sql <>= Text.Builder.char '?'
      #params %= (Text.Builder.run param :)
      parser
    Whitespace -> do
      #sql <>= Text.Builder.char ' '
      parser
    EndOfInput -> pure ()

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
--   , Param "bonk"
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
  | Param Text.Builder
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
      Param <$> paramP,
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

    paramP :: P Text.Builder
    paramP = do
      _ <- Megaparsec.satisfy (\c -> c == ':' || c == '@' || c == '$')
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
