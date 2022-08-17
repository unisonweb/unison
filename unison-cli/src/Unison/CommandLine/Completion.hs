{-
   This module defines tab-completion strategies for entering info via the CLI
-}
module Unison.CommandLine.Completion
  ( -- * Completers
    exactComplete,
    prefixCompleteTermOrType,
    prefixCompleteTerm,
    prefixCompleteType,
    prefixCompletePatch,
    noCompletions,
    prefixCompleteNamespace,
    -- Unused for now, but may be useful later
    prettyCompletion,
    fixupCompletion,
    haskelineTabComplete,
  )
where

import Control.Lens (ifoldMap)
import qualified Control.Lens as Lens
import Control.Lens.Cons (unsnoc)
import Data.List (isPrefixOf)
import qualified Data.List as List
import Data.List.Extra (nubOrdOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text as Text
import qualified System.Console.Haskeline as Line
import System.Console.Haskeline.Completion (Completion)
import qualified System.Console.Haskeline.Completion as Haskeline
import qualified U.Codebase.Branch as V2Branch
import qualified U.Codebase.Causal as V2Causal
import qualified U.Codebase.Reference as Reference
import qualified U.Codebase.Referent as Referent
import qualified U.Util.Monoid as Monoid
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import qualified Unison.CommandLine.InputPattern as IP
import qualified Unison.Hash as H
import qualified Unison.HashQualified' as HQ'
import Unison.NameSegment (NameSegment (NameSegment))
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude
import qualified Unison.ShortHash as SH
import qualified Unison.Util.Pretty as P
import Prelude hiding (readFile, writeFile)

-- | A completion func for use with Haskeline
haskelineTabComplete ::
  Monad m =>
  Map String IP.InputPattern ->
  Codebase m v a ->
  Path.Absolute ->
  Line.CompletionFunc m
haskelineTabComplete patterns codebase currentPath = Line.completeWordWithPrev Nothing " " $ \prev word ->
  -- User hasn't finished a command name, complete from command names
  if null prev
    then pure . exactComplete word $ Map.keys patterns
    else -- User has finished a command name; use completions for that command
    case words $ reverse prev of
      h : t -> fromMaybe (pure []) $ do
        p <- Map.lookup h patterns
        argType <- IP.argType p (length t)
        pure $ IP.suggestions argType word codebase currentPath
      _ -> pure []

-- | Things which we may want to complete for.
data CompletionType
  = NamespaceCompletion
  | TermCompletion
  | TypeCompletion
  | PatchCompletion
  deriving (Show, Eq, Ord)

-- | The empty completor.
noCompletions ::
  Monad m =>
  String ->
  Codebase m v a ->
  Path.Absolute ->
  m [System.Console.Haskeline.Completion.Completion]
noCompletions _ _ _ = pure []

-- | Finds names of the selected completion types within the path provided by the query.
--
-- Given a codebase with these terms:
--
-- @@
-- .base.List.map.doc
-- .base.List
-- .bar.foo
-- @@
--
-- We will return:
--
-- @@
-- .> cd bas<Tab>
-- base
--
-- .> cd base<Tab>
-- base
-- base.List
--
-- .> cd base.<Tab>
-- base.List
--
-- .> cd base.List.<Tab>
-- base.List.map
--
-- If conflicted, or if there's a # in the query, we expand completions into short-hashes.
-- This is also a convenient way to just see the shorthash for a given term.
--
-- .> view base.List.map#<Tab>
-- base.List.map#0q926sgnn6
completeWithinNamespace ::
  forall m v a.
  Monad m =>
  -- | The types of completions to return
  NESet CompletionType ->
  -- | The portion of this are that the user has already typed.
  String ->
  Codebase m v a ->
  Path.Absolute ->
  m [System.Console.Haskeline.Completion.Completion]
completeWithinNamespace compTypes query codebase currentPath = do
  shortHashLen <- Codebase.hashLength codebase
  Codebase.getShallowCausalFromRoot codebase Nothing absQueryPath >>= \case
    Nothing -> do
      pure []
    Just cb -> do
      b <- V2Causal.value cb
      let currentBranchSuggestions =
            namesInBranch shortHashLen b
              & fmap (\(isFinished, match) -> (isFinished, Text.unpack . Path.toText' $ queryPathPrefix Lens.:> NameSegment.NameSegment match))
              & filter (\(_isFinished, match) -> List.isPrefixOf query match)
              & fmap (\(isFinished, match) -> prettyCompletionWithQueryPrefix isFinished query match)

      childSuggestions <- getChildSuggestions shortHashLen b
      pure . nubOrdOn Haskeline.replacement . List.sortOn Haskeline.replacement $ currentBranchSuggestions <> childSuggestions
  where
    queryPathPrefix :: Path.Path'
    querySuffix :: NameSegment.NameSegment
    (queryPathPrefix, querySuffix) = parseLaxPath'Query (Text.pack query)
    absQueryPath :: Path.Absolute
    absQueryPath = Path.resolve currentPath queryPathPrefix
    getChildSuggestions :: Int -> V2Branch.Branch m -> m [Completion]
    getChildSuggestions shortHashLen b = do
      case querySuffix of
        "" -> pure []
        suffix -> do
          case Map.lookup (Cv.namesegment1to2 suffix) (V2Branch.children b) of
            Nothing -> pure []
            Just childCausal -> do
              childBranch <- V2Causal.value childCausal
              namesInBranch shortHashLen childBranch
                & fmap
                  ( \(isFinished, match) -> (isFinished, Text.unpack . Path.toText' $ queryPathPrefix Lens.:> suffix Lens.:> NameSegment.NameSegment match)
                  )
                & filter (\(_isFinished, match) -> List.isPrefixOf query match)
                & fmap (\(isFinished, match) -> prettyCompletionWithQueryPrefix isFinished query match)
                & pure
    namesInBranch :: Int -> V2Branch.Branch m -> [(Bool, Text)]
    namesInBranch hashLen b =
      let textifyHQ :: (V2Branch.NameSegment -> r -> HQ'.HashQualified V2Branch.NameSegment) -> Map V2Branch.NameSegment (Map r metadata) -> [(Bool, Text)]
          textifyHQ f xs =
            xs
              & hashQualifyCompletions f
              & fmap (HQ'.toTextWith V2Branch.unNameSegment)
              & fmap (True,)
       in ((False,) <$> dotifyNamespaces (fmap V2Branch.unNameSegment . Map.keys $ V2Branch.children b))
            <> Monoid.whenM (NESet.member TermCompletion compTypes) (textifyHQ (hqFromNamedV2Referent hashLen) $ V2Branch.terms b)
            <> Monoid.whenM (NESet.member TypeCompletion compTypes) (textifyHQ (hqFromNamedV2Reference hashLen) $ V2Branch.types b)
            <> Monoid.whenM (NESet.member PatchCompletion compTypes) (fmap ((True,) . V2Branch.unNameSegment) . Map.keys $ V2Branch.patches b)

    -- Regrettably there'shqFromNamedV2Referencenot a great spot to combinators for V2 references and shorthashes right now.
    hqFromNamedV2Referent :: Int -> V2Branch.NameSegment -> Referent.Referent -> HQ'.HashQualified V2Branch.NameSegment
    hqFromNamedV2Referent hashLen n r = HQ'.HashQualified n (SH.take hashLen $ v2ReferentToShortHash r)
    hqFromNamedV2Reference :: Int -> V2Branch.NameSegment -> Reference.Reference -> HQ'.HashQualified V2Branch.NameSegment
    hqFromNamedV2Reference hashLen n r = HQ'.HashQualified n (SH.take hashLen $ v2ReferenceToShortHash r)
    v2ReferentToShortHash :: Referent.Referent -> SH.ShortHash
    v2ReferentToShortHash = \case
      Referent.Ref r -> v2ReferenceToShortHash r
      Referent.Con r conId ->
        case v2ReferenceToShortHash r of
          SH.ShortHash h p _con -> SH.ShortHash h p (Just $ tShow conId)
          sh@(SH.Builtin {}) -> sh

    v2ReferenceToShortHash :: Reference.Reference -> SH.ShortHash
    v2ReferenceToShortHash (Reference.ReferenceBuiltin b) = SH.Builtin b
    v2ReferenceToShortHash (Reference.ReferenceDerived (Reference.Id h i)) = SH.ShortHash (H.base32Hex h) (showComponentPos i) Nothing
    showComponentPos :: Reference.Pos -> Maybe Text
    showComponentPos 0 = Nothing
    showComponentPos n = Just (tShow n)

    hashQualifyCompletions :: forall r metadata. (V2Branch.NameSegment -> r -> HQ'.HashQualified V2Branch.NameSegment) -> Map V2Branch.NameSegment (Map r metadata) -> [HQ'.HashQualified V2Branch.NameSegment]
    hashQualifyCompletions qualify defs = ifoldMap qualifyRefs defs
      where
        -- Qualify any conflicted definitions. If the query has a "#" in it, then qualify ALL
        -- completions.
        qualifyRefs :: V2Branch.NameSegment -> (Map r metadata) -> [HQ'.HashQualified V2Branch.NameSegment]
        qualifyRefs n refs
          | ((Text.isInfixOf "#" . NameSegment.toText) querySuffix) || length refs > 1 =
              refs
                & Map.keys
                <&> qualify n
          | otherwise = [HQ'.NameOnly n]

    -- If we're not completing namespaces, then all namespace completions should automatically
    -- drill-down by adding a trailing '.'
    dotifyNamespaces :: [Text] -> [Text]
    dotifyNamespaces namespaces =
      if not (NESet.member NamespaceCompletion compTypes)
        then fmap (<> ".") namespaces
        else namespaces

-- | A path parser which which is more lax with respect to well formed paths,
-- specifically we can determine a valid path prefix with a (possibly empty) suffix query.
-- This is used in tab-completion where the difference between `.base` and `.base.` is
-- relevant, but can't be detected when running something like 'Path.fromText''
--
-- >>> parseLaxPath'Query ".base."
-- (.base,"")
--
-- >>> parseLaxPath'Query ".base"
-- (.,"base")
--
-- >>> parseLaxPath'Query ".base.List"
-- (.base,"List")
--
-- >>> parseLaxPath'Query ""
-- (,"")
--
-- >>> parseLaxPath'Query "base"
-- (,"base")
--
-- >>> parseLaxPath'Query "base."
-- (base,"")
--
-- >>> parseLaxPath'Query "base.List"
-- (base,"List")
parseLaxPath'Query :: Text -> (Path.Path', NameSegment)
parseLaxPath'Query txt =
  case unsnoc (Text.splitOn "." txt) of
    -- This case is impossible due to the behaviour of 'splitOn'
    Nothing ->
      (Path.relativeEmpty', NameSegment "")
    -- ".base."
    -- ".base.List"
    Just ("" : pathPrefix, querySegment) -> (Path.AbsolutePath' . Path.Absolute . Path.fromList . fmap NameSegment $ pathPrefix, NameSegment querySegment)
    -- ""
    -- "base"
    -- "base.List"
    Just (pathPrefix, querySegment) ->
      ( Path.RelativePath' . Path.Relative . Path.fromList . fmap NameSegment $ pathPrefix,
        NameSegment querySegment
      )

-- | Completes a namespace argument by prefix-matching against the query.
prefixCompleteNamespace ::
  forall m v a.
  Monad m =>
  String ->
  Codebase m v a ->
  Path.Absolute -> -- Current path
  m [Line.Completion]
prefixCompleteNamespace = completeWithinNamespace (NESet.singleton NamespaceCompletion)

-- | Completes a term or type argument by prefix-matching against the query.
prefixCompleteTermOrType ::
  forall m v a.
  Monad m =>
  String ->
  Codebase m v a ->
  Path.Absolute -> -- Current path
  m [Line.Completion]
prefixCompleteTermOrType = completeWithinNamespace (NESet.fromList (TermCompletion NE.:| [TypeCompletion]))

-- | Completes a term argument by prefix-matching against the query.
prefixCompleteTerm ::
  forall m v a.
  Monad m =>
  String ->
  Codebase m v a ->
  Path.Absolute -> -- Current path
  m [Line.Completion]
prefixCompleteTerm = completeWithinNamespace (NESet.singleton TermCompletion)

-- | Completes a term or type argument by prefix-matching against the query.
prefixCompleteType ::
  forall m v a.
  Monad m =>
  String ->
  Codebase m v a ->
  Path.Absolute -> -- Current path
  m [Line.Completion]
prefixCompleteType = completeWithinNamespace (NESet.singleton TypeCompletion)

-- | Completes a patch argument by prefix-matching against the query.
prefixCompletePatch ::
  forall m v a.
  Monad m =>
  String ->
  Codebase m v a ->
  Path.Absolute -> -- Current path
  m [Line.Completion]
prefixCompletePatch = completeWithinNamespace (NESet.singleton PatchCompletion)

-- | Renders a completion option with the prefix matching the query greyed out.
prettyCompletionWithQueryPrefix ::
  Bool ->
  -- | query
  String ->
  -- | completion
  String ->
  Line.Completion
prettyCompletionWithQueryPrefix endWithSpace query s =
  let coloredMatch = P.hiBlack (P.string query) <> P.string (drop (length query) s)
   in Line.Completion s (P.toAnsiUnbroken coloredMatch) endWithSpace

-- discards formatting in favor of better alignment
-- prettyCompletion (s, p) = Line.Completion s (P.toPlainUnbroken p) True
-- preserves formatting, but Haskeline doesn't know how to align
prettyCompletion :: Bool -> (String, P.Pretty P.ColorText) -> Line.Completion
prettyCompletion endWithSpace (s, p) = Line.Completion s (P.toAnsiUnbroken p) endWithSpace

-- | Constructs a list of 'Completion's from a query and completion options by
-- filtering them for prefix matches. A completion will be selected if it's an exact match for
-- a provided option.
exactComplete :: String -> [String] -> [Line.Completion]
exactComplete q ss = go <$> filter (isPrefixOf q) ss
  where
    go s = prettyCompletionWithQueryPrefix (s == q) q s

-- workaround for https://github.com/judah/haskeline/issues/100
-- if the common prefix of all the completions is smaller than
-- the query, we make all the replacements equal to the query,
-- which will preserve what the user has typed
fixupCompletion :: String -> [Line.Completion] -> [Line.Completion]
fixupCompletion _q [] = []
fixupCompletion _q [c] = [c]
fixupCompletion q cs@(h : t) =
  let commonPrefix (h1 : t1) (h2 : t2) | h1 == h2 = h1 : commonPrefix t1 t2
      commonPrefix _ _ = ""
      overallCommonPrefix =
        foldl commonPrefix (Line.replacement h) (Line.replacement <$> t)
   in if not (q `isPrefixOf` overallCommonPrefix)
        then [c {Line.replacement = q} | c <- cs]
        else cs
