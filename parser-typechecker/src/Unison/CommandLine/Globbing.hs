{-| Provides Globbing for selecting types, terms and namespaces using wildcards.
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Unison.CommandLine.Globbing
  ( expandGlobs
  , TargetType(..)
  ) where
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
import Control.Applicative (liftA2)
import Control.Monad (guard)
import Control.Error (hush)

-- | Possible targets which a glob may select.
data TargetType
  = Type
  | Term
  | Namespace
  deriving (Eq, Ord, Show)

-- | Glob paths are always relative to some branch.
type GlobPath = [Either NameSegment GlobArg]

-- | Represents a name segment containing a glob pattern
--   e.g. start?end -> GlobArg "start" "end"
data GlobArg = GlobArg
    { namespacePrefix :: Text
    , namespaceSuffix :: Text
    } deriving (Show)

-- | Constructs a namespace "matcher" from a 'GlobArg'
globPredicate :: Either NameSegment GlobArg -> (NameSegment -> Bool)
globPredicate globArg (NameSegment.toText -> ns') =
  case globArg of
    Left (NameSegment.toText -> ns) -> ns == ns'
    Right (GlobArg prefix suffix) -> prefix `Text.isPrefixOf` ns' && suffix `Text.isSuffixOf` ns'

-- | Expands a glob into a list of paths which lead to valid targets.
expandGlobToPaths :: Set TargetType -> GlobPath -> Branch0 m -> [Path.Relative]
expandGlobToPaths targets gp branch = (Path.Relative . Path.fromList) <$> expandGlobToNameSegments targets gp branch

-- | Helper for 'expandGlobToPaths'
expandGlobToNameSegments :: forall m. Set TargetType -> GlobPath -> Branch0 m -> [[NameSegment]]
expandGlobToNameSegments _targets [] _branch = []
expandGlobToNameSegments targets [segment] branch =
         Monoid.whenM (Set.member Term targets) matchingTerms
      <> Monoid.whenM (Set.member Type targets) matchingTypes
      <> Monoid.whenM (Set.member Namespace targets) matchingNamespaces
  where
    predicate = globPredicate segment
    matchingNamespaces, matchingTerms, matchingTypes :: [[NameSegment]]
    matchingNamespaces = branch ^.. matchingChildBranches predicate . asIndex . to (pure @[])
    matchingTerms = matchingNamesInStar predicate (Branch._terms branch)
    matchingTypes = matchingNamesInStar predicate (Branch._types branch)
    matchingNamesInStar :: (NameSegment -> Bool) -> Branch.Star a NameSegment -> [[NameSegment]]
    matchingNamesInStar predicate star =
      star & Star3.d1
           & Relation.ran
           & Set.toList
           & filter predicate
           & fmap (pure @[])
expandGlobToNameSegments targets (x:xs) b = recursiveMatches
  where
    nextBranches :: [(NameSegment, (Branch0 m))]
    nextBranches = b ^@.. matchingChildBranches (globPredicate x)
    recursiveMatches :: ([[NameSegment]])
    recursiveMatches =
      (foldMap (\(ns, b) -> (ns:) <$> expandGlobToNameSegments targets xs b) nextBranches)

-- | Find all child branches whose name matches a predicate.
matchingChildBranches :: (NameSegment -> Bool) -> IndexedTraversal' NameSegment (Branch0 m) (Branch0 m)
matchingChildBranches keyPredicate = Branch.currentChildren . indices keyPredicate

-- | Expand a single glob pattern into all matching targets of the specified types.
expandGlobs :: forall m. Set TargetType
            -> Branch0 m -- ^ Root branch
            -> Path.Absolute -- ^ UCM's current path
            -> String -- ^ The glob string, e.g. .base.List.?.doc
            -> [String] -- ^ Fully expanded, absolute paths. E.g. [".base.List.map"]
expandGlobs Empty _rootBranch _currentPath s = [s]
expandGlobs targets rootBranch currentPath s = Maybe.fromMaybe [s] $ do
  (isAbsolute, globPath) <- hush $ runParser globbedPathParser "arguments" s
  -- If we don't have any actual globs, we can fail to fall back to the original argument.
  guard (any Either.isRight globPath)
  let currentBranch :: Branch0 m
      currentBranch
        | isAbsolute = rootBranch
        | otherwise = Branch.getAt0 (Path.unabsolute currentPath) rootBranch
  let paths = expandGlobToPaths targets globPath currentBranch
  let relocatedPaths | isAbsolute = (Path.Absolute . Path.unrelative) <$> paths
                     | otherwise = Path.resolve currentPath <$> paths
  pure (Path.convert <$> relocatedPaths)

-- | Ad-hoc parser for paths which may contain globs in them.
globbedPathParser :: Parsec Void String (Bool, GlobPath)
globbedPathParser = do
  isAbsolute <- Maybe.isJust <$> optional "."
  (isAbsolute,) <$> (globArgParser `sepBy`  "." <* eof)

-- | Parses a single name segment into a GlobArg or a bare segment according to whether
-- there's a glob.
-- E.g.
--   "toList" -> Left (NameSegment "toList")
--   "to?" -> Left (GlobArg "to" "")
-- We unintuitively use '?' for glob patterns right now since they're not valid in names.
globArgParser :: Parsec Void String (Either NameSegment GlobArg)
globArgParser = do
  let nsChar = ((char '\\' *> char '?') <|> noneOf ['.', '?'] )
  let globSegmentP =
        liftA2 GlobArg (Text.pack <$> manyTill nsChar "?") (Text.pack <$> many nsChar)
  (Right <$> try globSegmentP) <|> (Left . NameSegment . Text.pack <$> some (satisfy (/= '.')))
