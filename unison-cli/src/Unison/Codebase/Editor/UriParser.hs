module Unison.Codebase.Editor.UriParser
  ( readRemoteNamespaceParser,
    parseReadShareLooseCode,
    writeRemoteNamespace,
  )
where

import Data.Char (isAlphaNum)
import Data.Text qualified as Text
import Data.These (These)
import Data.Void
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Unison.Codebase.Editor.RemoteRepo
  ( ReadRemoteNamespace (..),
    ReadShareLooseCode (..),
    ShareCodeserver (DefaultCodeserver),
    ShareUserHandle (..),
  )
import Unison.Codebase.Path qualified as Path
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Project (ProjectBranchName, ProjectBranchSpecifier (..), ProjectName, projectAndBranchNamesParser)
import Unison.Syntax.Lexer qualified
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Util.Pretty qualified as P
import Unison.Util.Pretty.MegaParsec qualified as P

type P = P.Parsec Void Text.Text

readRemoteNamespaceParser :: ProjectBranchSpecifier branch -> P (ReadRemoteNamespace (These ProjectName branch))
readRemoteNamespaceParser specifier =
  ReadShare'ProjectBranch <$> projectAndBranchNamesParserInTheContextOfAlsoParsingLooseCodePaths specifier
    <|> ReadShare'LooseCode <$> readShareLooseCode

projectAndBranchNamesParserInTheContextOfAlsoParsingLooseCodePaths ::
  ProjectBranchSpecifier branch ->
  P (These ProjectName branch)
projectAndBranchNamesParserInTheContextOfAlsoParsingLooseCodePaths specifier =
  P.try do
    projectAndBranch <- projectAndBranchNamesParser specifier
    -- we don't want to succeed parsing the 'foo' off of 'foo.bar', leaving '.bar' behind
    P.notFollowedBy (C.char '.')
    pure projectAndBranch

parseReadShareLooseCode :: String -> String -> Either (P.Pretty P.ColorText) ReadShareLooseCode
parseReadShareLooseCode label input =
  let printError err = P.lines [P.string "I couldn't parse this as a share path.", P.prettyPrintParseError input err]
   in first printError (P.parse readShareLooseCode label (Text.pack input))

-- >>> P.parseMaybe writeRemoteNamespace "unisonweb.base._releases.M4"
-- Just (WriteRemoteNamespaceShare (WriteShareRemoteNamespace {server = ShareRepo, repo = "unisonweb", path = base._releases.M4}))
writeRemoteNamespace :: P (These ProjectName ProjectBranchName)
writeRemoteNamespace =
  (projectAndBranchNamesParserInTheContextOfAlsoParsingLooseCodePaths ProjectBranchSpecifier'Name)

-- >>> P.parseMaybe readShareLooseCode ".unisonweb.base._releases.M4"
-- >>> P.parseMaybe readShareLooseCode "unisonweb.base._releases.M4"
-- Nothing
-- Just (ReadShareLooseCode {server = DefaultCodeserver, repo = ShareUserHandle {shareUserHandleToText = "unisonweb"}, path = base._releases.M4})
readShareLooseCode :: P ReadShareLooseCode
readShareLooseCode = do
  P.label "read share loose code" $
    ReadShareLooseCode
      <$> pure DefaultCodeserver
      -- <*> sch <- P.optional shortBranchHash
      <*> shareUserHandle
      <*> (Path.fromList <$> P.many (C.char '.' *> nameSegment))

-- | We're lax in our share user rules here, Share is the source of truth
-- for this stuff and can provide better error messages if required.
--
-- >>> P.parseMaybe shareUserHandle "unison"
-- Just (ShareUserHandle {shareUserHandleToText = "unison"})
--
-- >>> P.parseMaybe shareUserHandle "unison-1337"
-- Just (ShareUserHandle {shareUserHandleToText = "unison-1337"})
shareUserHandle :: P ShareUserHandle
shareUserHandle = do
  ShareUserHandle . Text.pack <$> P.some (P.satisfy \c -> isAlphaNum c || c == '-' || c == '_')

data Scheme = Ssh | Https
  deriving (Eq, Ord, Show)

data User = User Text
  deriving (Eq, Ord, Show)

data HostInfo = HostInfo Text (Maybe Text)
  deriving (Eq, Ord, Show)

nameSegment :: P NameSegment
nameSegment =
  NameSegment.unsafeParseText . Text.pack
    <$> ( (:)
            <$> P.satisfy Unison.Syntax.Lexer.wordyIdStartChar
            <*> P.many (P.satisfy Unison.Syntax.Lexer.wordyIdChar)
        )
