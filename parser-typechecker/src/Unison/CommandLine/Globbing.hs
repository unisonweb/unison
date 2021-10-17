{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Unison.CommandLine.Globbing where
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
import qualified Data.Maybe as Maybe
import Data.Bifunctor (second)
import qualified Unison.Util.Star3 as Star3
import qualified Unison.Util.Relation as Relation
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Unison.Util.Monoid as Monoid
import qualified Data.Either as Either

data TargetType = Type | Term | Namespace
  deriving (Eq, Ord)

-- Glob paths are always relative.
type GlobPath = [Either NameSegment GlobArg]
data GlobArg =
  GlobArg {namespacePrefix :: Text, namespaceSuffix :: Text}

toPredicate :: Either NameSegment GlobArg -> (NameSegment -> Bool)
toPredicate globArg (NameSegment.toText -> ns') =
  case globArg of
    Left (NameSegment.toText -> ns) -> ns == ns'
    Right (GlobArg prefix suffix) -> prefix `Text.isPrefixOf` ns' && suffix `Text.isSuffixOf` ns'

unglob :: Set TargetType -> GlobPath -> Branch0 m -> [(TargetType, Path.Absolute)]
unglob targets gp branch = second (Path.Absolute . Path.fromList) <$> unglobToNameSegments targets gp branch

unglobToNameSegments :: forall m. Set TargetType -> GlobPath -> Branch0 m -> [(TargetType, [NameSegment])]
unglobToNameSegments targets [] _ =
  if Set.member Namespace targets
    then [(Namespace, [])]
    else []
unglobToNameSegments targets (x:xs) b =
     Monoid.whenM (Set.member Term targets) matchingTerms
  <> Monoid.whenM (Set.member Type targets) matchingTypes
  <> recursiveMatches
  where
    nextBranches :: [(NameSegment, (Branch0 m))]
    nextBranches = b ^@.. childBranchesByKey (toPredicate x)
    recursiveMatches, matchingTerms, matchingTypes :: ([(TargetType, [NameSegment])])
    recursiveMatches =
      (foldMap (\(ns, b) -> second (ns:) <$> unglobToNameSegments targets xs b) nextBranches)
    matchingTerms = matchingNamesInStar Term (toPredicate x) (Branch._terms b)
    matchingTypes = matchingNamesInStar Type (toPredicate x) (Branch._terms b)
    childBranchesByKey :: (NameSegment -> Bool) -> IndexedTraversal' NameSegment (Branch0 m) (Branch0 m)
    childBranchesByKey keyPredicate = Branch.currentChildren . indices keyPredicate
    matchingNamesInStar :: TargetType -> (NameSegment -> Bool) -> Branch.Star a NameSegment -> [(TargetType, [NameSegment])]
    matchingNamesInStar typ predicate star =
      star & Star3.d1
           & Relation.ran
           & Set.toList
           & filter predicate
           & pure @[]
           & fmap (typ,)

expandGlobs :: Set TargetType -> Branch0 m -> String -> [String]
expandGlobs targets branch s = Either.fromRight [s] $ do
  globPath <- runParser globParser "arguments" s
  pure . fmap (Path.convert . snd) $ unglob targets globPath branch

globParser :: Parsec Void String GlobPath
globParser = do
  _isAbsolute <- Maybe.isJust <$> optional "."
  globArgParser `sepBy`  "."

globArgParser :: Parsec Void String (Either NameSegment GlobArg)
globArgParser = do
  let nsChar = ((char '\\' *> char '*') <|> satisfy (/= '.'))
  let globSegmentP =
        liftA3 (,,) (many nsChar) (char '*') (many nsChar)
          <&> \(prefix, _star, suffix) -> GlobArg (Text.pack prefix) (Text.pack suffix)
  (Right <$> try globSegmentP) <|> (Left . NameSegment . Text.pack <$> some anyChar)
