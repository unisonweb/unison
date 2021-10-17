{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Unison.CommandLine.Globbing where
import Unison.Codebase (Codebase)
import Unison.NameSegment (NameSegment (NameSegment))
import Unison.Codebase.Branch (Branch0)
import qualified Unison.Codebase.Path as Path
import Data.Text (Text)
import Control.Lens as Lens
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.NameSegment as NameSegment
import qualified Data.Text as Text
import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char
import Control.Applicative (liftA3)

-- Glob paths are always relative.
type GlobPath = [Either NameSegment GlobArg]
data GlobArg =
  GlobArg {namespacePrefix :: Text, namespaceSuffix :: Text}

toNamespacePredicate :: Either NameSegment GlobArg -> (NameSegment -> Bool)
toNamespacePredicate globArg (NameSegment.toText -> ns') = 
  case globArg of
    Left (NameSegment.toText -> ns) -> ns == ns'
    Right (GlobArg prefix suffix) -> prefix `Text.isPrefixOf` ns' && suffix `Text.isSuffixOf` ns'

unglob :: forall m. GlobPath -> Branch0 m -> [Path.Absolute]
unglob gp branch = Path.Absolute . Path.fromList <$> unglobToNameSegments gp branch

unglobToNameSegments :: forall m. GlobPath -> Branch0 m -> [[NameSegment]]
unglobToNameSegments [] _ = [[]]
unglobToNameSegments (x:xs) b =
      let nextBranches :: [(NameSegment, (Branch0 m))]
          nextBranches = b ^@.. childBranchesByKey (toNamespacePredicate x)
       in foldMap (\(ns, b) -> (ns:) <$> unglobToNameSegments xs b) nextBranches
  where
    childBranchesByKey :: (NameSegment -> Bool) -> IndexedTraversal' NameSegment (Branch0 m) (Branch0 m)
    childBranchesByKey keyPredicate = Branch.currentChildren . indices keyPredicate

parseGlobs :: [String] -> Codebase m v a -> [Either Text GlobArg]
parseGlobs = _

-- globParser :: Parsec Void String GlobPath
-- globParser = do
--   isAbsolute <- Maybe.isJust <$> optional "."
--   _



globArgParser :: Parsec Void String (Either NameSegment GlobArg)
globArgParser = do
  let nsChar = ((char '\\' *> char '*') <|> satisfy (/= '.'))
  let globSegmentP = 
        liftA3 (,,) (many nsChar) (char '*') (many nsChar) 
          <&> \(prefix, _star, suffix) -> GlobArg (Text.pack prefix) (Text.pack suffix)
  (Right <$> try globSegmentP) <|> (Left . NameSegment . Text.pack <$> some anyChar)
