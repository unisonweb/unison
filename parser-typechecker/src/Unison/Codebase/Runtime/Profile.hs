{-# LANGUAGE ExistentialQuantification #-}

module Unison.Codebase.Runtime.Profile
  ( ProfTrie (..),
    Profile (..),
    SomeProfile (..),
    ProfileSpec (..),
    emptyProfile,
    singlePath,
    addPath,
    aggregatePruned,
    fullProfile,
    miniProfile,
    foldedProfile
  ) where

import Data.Bifunctor (second)
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.String

import Numeric

import Unison.PrettyPrintEnv
import Unison.Reference
import Unison.Referent
import Unison.Syntax.NamePrinter
import Unison.Util.Pretty as P

-- A `ProfTrie` maps non-empty paths to counts of occurrences of that
-- path. This allows tracking relative frequency of call stacks. The
-- path elements are parameterized to allow for more efficient `Map` keys
-- to be used during construction, and can be paired with a decoding to
-- `Reference` as below.
newtype ProfTrie k a = ProfT (Map k (a, ProfTrie k a))
  deriving (Functor)

-- A profile pairs the above arbitrary key based profile trie with a
-- decoding of the integers to references and a total sample count.
data Profile k = Prof !Int !(ProfTrie k Int) !(Map k Reference)

-- Abstracts over the exact key type used in a profile.
data SomeProfile = forall k. Ord k => SomeProf (Profile k)

data ProfileSpec = NoProf | MiniProf | FullProf String
  deriving (Eq, Ord, Show)

emptyProfile :: Profile k
emptyProfile = Prof 0 (ProfT M.empty) M.empty

-- Creates a singleton profile trie from a path.
singlePath :: Ord k => [k] -> (Int, ProfTrie k Int)
singlePath [] = (1, ProfT M.empty)
singlePath (i:is) = (0,) . ProfT $! M.singleton i (singlePath is)

addPath0 :: Ord k => [k] -> (Int, ProfTrie k Int) -> (Int, ProfTrie k Int)
addPath0 [] (m, p) = (,p) $! m+1
addPath0 (i:is) (m, ProfT p) = (m,) . ProfT $! M.alter f i p
  where
    f Nothing = Just $ singlePath is
    f (Just q) = Just $ addPath0 is q

-- Adds a path to a profile trie, incrementing the count for the given
-- path.
addPath :: Ord k => [k] -> ProfTrie k Int -> ProfTrie k Int
addPath [] p = p
addPath (i:is) (ProfT m) = ProfT $ M.alter f i m
  where
    f Nothing = Just $ singlePath is
    f (Just q) = Just $ addPath0 is q

data AggInfo k = Ag {
    -- inherited sample count
    inherited :: Int,
    -- total sample for all occurrences of a key
    allOccs :: Map k Int
  }

instance Ord k => Semigroup (AggInfo k) where
  Ag il al <> Ag ir ar =
    Ag (il + ir) (M.unionWith (+) al ar)

instance Ord k => Monoid (AggInfo k) where
  mempty = Ag 0 M.empty

aggregateWith ::
  Ord k =>
  (AggInfo k -> Int -> r) ->
  k ->
  (Int, ProfTrie k Int) ->
  (AggInfo k, (r, ProfTrie k r))
aggregateWith f k (m, ProfT t) =
  case M.traverseWithKey (aggregateWith f) t of
    (ag, t) -> (ag', (f ag' m, ProfT t))
      where
      ag' = ag <> Ag m (M.singleton k m)

prune0 ::
  Ord k =>
  Set k ->
  k ->
  (a, ProfTrie k a) ->
  Identity (Maybe (a, ProfTrie k a))
prune0 keep k (a, ProfT sub) =
  case M.traverseMaybeWithKey (prune0 keep) sub of
    Identity sub
      | null sub, k `S.notMember` keep -> pure Nothing
      | otherwise -> pure $ Just (a, ProfT sub)

prune :: Ord k => Set k -> ProfTrie k a -> ProfTrie k a
prune keep (ProfT m) = case M.traverseMaybeWithKey (prune0 keep) m of
  Identity sub -> ProfT sub

topN :: Ord k => Int -> Map k Int -> [(k, Int)]
topN n0 = M.foldlWithKey (ins n0) []
  where
    ins 0 _ _ _ = []
    ins _ [] k i = [(k, i)]
    ins n pss@((k1, j) : ps) k0 i
      | i > j = (k0, i) : pop (n-1) pss
      | otherwise = (k1, j) : ins (n-1) ps k0 i

    pop 0 _ = []
    pop _ [] = []
    pop n (p:ps) = p : pop (n-1) ps

fraction :: Int -> Int -> Double
fraction n d = fromIntegral n / fromIntegral d

fractions :: Int -> AggInfo k -> Int -> (Double, Double)
fractions total ag lo =
  (fraction (inherited ag) total, fraction lo total)

-- Given a total count and a profile trie, calculates local and inherited
-- cost fractions of the positions in the trie, and prunes it to the
-- hottest spots.
aggregatePruned ::
  Ord k => Int -> ProfTrie k Int -> ProfTrie k (Double, Double)
aggregatePruned total (ProfT t) =
  case M.traverseWithKey (aggregateWith (fractions total)) t of
    (ag, t)
      | top <- topN 10 (allOccs ag) ->
          prune (S.fromList $ fst <$> top) $ ProfT t

aggregate ::
  Ord k =>
  Int ->
  ProfTrie k Int ->
  ([(k, Double)], ProfTrie k (Double, Double))
aggregate total (ProfT t) =
  case M.traverseWithKey (aggregateWith (fractions total)) t of
    (ag, t) ->
      (second (flip fraction total) <$> topN 10 (allOccs ag), ProfT t)

aggregateTicks :: Ord k => ProfTrie k Int -> ProfTrie k Int
aggregateTicks (ProfT t) =
  ProfT . snd $ M.traverseWithKey (aggregateWith (const . inherited)) t

-- Folds over a profile trie. The mapping function receives a reversed
-- path to the node, which can be used e.g. to see the node's key and to
-- calculate the depth in the trie.
foldMapTrie :: Monoid m => ((k,[k]) -> v -> m) -> ProfTrie k v -> m
foldMapTrie f = descend []
  where
    descend ks (ProfT m) =
      M.foldMapWithKey
        (\k (v, sub) -> f (k,ks) v <> descend (k:ks) sub)
        m

showPercent :: Double -> String
showPercent d = pad $ showFFloat (Just 2) (100 * d) ""
  where
    pad s = replicate (6 - length s) ' ' <> s

dispProfEntry ::
  Ord k =>
  PrettyPrintEnv ->
  Map k Reference ->
  (k,[k]) ->
  (Double, Double) ->
  Pretty ColorText
dispProfEntry ppe refs (k,ks) (inh, self) =
  mconcat
    [ P.indentN 6 . fromString $ showPercent inh,
      P.indentN 4 . fromString $ showPercent self,
      P.indentN (2*ind + 4) $ dispKey ppe refs k,
      "\n"
    ]
  where
    ind = fromIntegral $ length ks

dispFunc :: PrettyPrintEnv -> Reference -> Pretty ColorText
dispFunc ppe =
  syntaxToColor . prettyHashQualified . termName ppe . Ref

dispKey ::
  Ord k => PrettyPrintEnv -> Map k Reference -> k -> Pretty ColorText
dispKey ppe refs k
  | Just r <- M.lookup k refs = dispFunc ppe r
  | otherwise = "<unknown>"

dispProfTrie ::
  Ord k =>
  PrettyPrintEnv ->
  Map k Reference ->
  ProfTrie k (Double, Double) ->
  Pretty ColorText
dispProfTrie ppe refs ag = foldMapTrie (dispProfEntry ppe refs) ag

dispTopEntry ::
  Ord k =>
  PrettyPrintEnv ->
  Map k Reference ->
  (k, Double) ->
  Pretty ColorText
dispTopEntry ppe refs (k, frac) =
  mconcat
    [ P.indentN 4 . fromString $ showPercent frac,
      P.indentN 6 dr,
      "\n"
    ]
  where
    dr :: Pretty ColorText
    dr | Just r <- M.lookup k refs = dispFunc ppe r
       | otherwise = "<unknown>"

dispTop ::
  Ord k =>
  PrettyPrintEnv ->
  Map k Reference ->
  [(k, Double)] ->
  Pretty ColorText
dispTop ppe refs = foldMap (dispTopEntry ppe refs)

profileTopHeader :: Pretty ColorText
profileTopHeader =
  "Hot Spots\n" <> "  Total Cost    Function\n"

profileTreeHeader :: Pretty ColorText
profileTreeHeader =
  P.lines
    [ P.indentN 2 "Profile Tree"
    , P.indentN 4 "Inherited    Self     Function"
    , ""
    ]

miniProfile :: Ord k => PrettyPrintEnv -> Profile k -> Pretty ColorText
miniProfile ppe (Prof total tr refs) =
  profileTreeHeader <>
  dispProfTrie ppe refs ag
  where
    ag = aggregatePruned total tr

fullProfile ::
  Ord k =>
  PrettyPrintEnv ->
  Profile k ->
  Pretty ColorText
fullProfile ppe (Prof total tr refs) =
  profileTopHeader <>
  dispTop ppe refs top <>
  "\n\n" <>
  profileTreeHeader <>
  dispProfTrie ppe refs ag
  where
    (top, ag) = aggregate total tr

foldedProfile ::
  Ord k =>
  PrettyPrintEnv ->
  Profile k ->
  String
foldedProfile ppe (Prof _ tr refs) =
  toPlainUnbroken $ foldMapTrie f tr
  where
    dk = dispKey ppe refs

    f (k,ks) n =
      mconcat
        [ foldl (\tx k -> dk k <> ";" <> tx) (dk k) ks
        , " "
        , fromString $ show n
        , "\n"
        ]
