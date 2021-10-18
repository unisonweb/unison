{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Unison.CommandLine.Globbing where
import Unison.NameSegment (NameSegment (NameSegment))
import Unison.Codebase.Branch (Branch0)
import qualified Unison.Codebase.Path as Path
import Data.Text (Text)
import Control.Lens as Lens hiding (noneOf)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.NameSegment as NameSegment
import qualified Data.Text as Text
import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char
import qualified Data.Maybe as Maybe
import qualified Unison.Util.Star3 as Star3
import qualified Unison.Util.Relation as Relation
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Unison.Util.Monoid as Monoid
import qualified Data.Either as Either
import Unison.Prelude (traceShowId, trace)
import Control.Applicative (liftA2)
import Control.Monad (guard)
import Control.Error (hush)

data TargetType = Type | Term | Namespace
  deriving (Eq, Ord, Show)

-- Glob paths are always relative.
type GlobPath = [Either NameSegment GlobArg]
data GlobArg = GlobArg
    { namespacePrefix :: Text
    , namespaceSuffix :: Text
    } deriving (Show)

globPredicate :: Either NameSegment GlobArg -> (NameSegment -> Bool)
globPredicate globArg (NameSegment.toText -> ns') =
  case globArg of
    Left (NameSegment.toText -> ns) -> ns == ns'
    Right (GlobArg prefix suffix) -> prefix `Text.isPrefixOf` ns' && suffix `Text.isSuffixOf` ns'

unglob :: Set TargetType -> GlobPath -> Branch0 m -> [Path.Relative]
unglob targets gp branch = (Path.Relative . Path.fromList) <$> unglobToNameSegments targets gp branch

unglobToNameSegments :: forall m. Set TargetType -> GlobPath -> Branch0 m -> [[NameSegment]]
unglobToNameSegments targets [] _ =
  if Set.member Namespace targets
    then [[]] -- Return an empty path, which will be built up by the parents.
    else []   -- Return zero paths.
unglobToNameSegments targets (x:xs) b =
     Monoid.whenM (Set.member Term targets) matchingTerms
  <> Monoid.whenM (Set.member Type targets) matchingTypes
  <> recursiveMatches
  where
    nextBranches :: [(NameSegment, (Branch0 m))]
    nextBranches = b ^@.. childBranchesByKey (globPredicate x)
    recursiveMatches, matchingTerms, matchingTypes :: ([[NameSegment]])
    recursiveMatches =
      (foldMap (\(ns, b) -> (ns:) <$> unglobToNameSegments targets xs b) nextBranches)
    matchingTerms = traceShowId $ trace "terms" $ matchingNamesInStar (globPredicate x) (Branch._terms b)
    matchingTypes = traceShowId $ trace "types" $ matchingNamesInStar (globPredicate x) (Branch._types b)
    childBranchesByKey :: (NameSegment -> Bool) -> IndexedTraversal' NameSegment (Branch0 m) (Branch0 m)
    childBranchesByKey keyPredicate = Branch.currentChildren . indices keyPredicate
    matchingNamesInStar :: (NameSegment -> Bool) -> Branch.Star a NameSegment -> [[NameSegment]]
    matchingNamesInStar predicate star =
      star & Star3.d1
           & Relation.ran
           & Set.toList
           & filter predicate
           & pure @[]

expandGlobs :: forall m. Set TargetType -> Branch0 m -> Path.Absolute -> String -> [String]
expandGlobs Empty _branch _currentPath s = [s]
expandGlobs targets rootBranch currentPath s = Maybe.fromMaybe [s] $ do
  (isAbsolute, globPath) <- hush $ runParser globParser "arguments" s
  -- If we don't have any actual globs, we can fail to fall back to the original argument.
  guard (any Either.isRight globPath)
  let currentBranch :: Branch0 m
      currentBranch
        | isAbsolute = rootBranch
        | otherwise = Branch.getAt0 (Path.unabsolute currentPath) rootBranch
  let paths = unglob targets globPath currentBranch
  let relocatedPaths | isAbsolute = (Path.Absolute . Path.unrelative) <$> paths
                     | otherwise = Path.resolve currentPath <$> paths
  pure (Path.convert <$> relocatedPaths)

globParser :: Parsec Void String (Bool, GlobPath)
globParser = do
  isAbsolute <- Maybe.isJust <$> optional "."
  (isAbsolute,) <$> (globArgParser `sepBy`  "." <* eof)

-- We unintuitively use '?' for glob patterns right now since they're not valid in names.
globArgParser :: Parsec Void String (Either NameSegment GlobArg)
globArgParser = do
  let nsChar = ((char '\\' *> char '?') <|> noneOf ['.', '?'] )
  let globSegmentP =
        liftA2 GlobArg (Text.pack <$> manyTill nsChar "?") (Text.pack <$> many nsChar)
  (Right <$> try globSegmentP) <|> (Left . NameSegment . Text.pack <$> some (satisfy (/= '.')))
